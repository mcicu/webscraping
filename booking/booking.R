library(httr)
library(jsonlite)
library(rvest)
library(openxlsx)
library(dplyr)
library(future)
library(future.apply)
library(RSQLite)
library(readr)
library(stringi)

### configuration ###

set_config(add_headers(.headers = c(
  "Accept-Encoding" = "gzip, deflate",
  "User-Agent" = "PostmanRuntime/7.26.8")))

### constants ###
BOOKING_BASE_URL = "https://www.booking.com/searchresults.ro.html"
ORASE = c("Bucuresti", "Brasov", "Constanta", "Prahova")
DATA_CHECKIN = "2022-04-25"
DATA_CHECKOUT = "2022-04-27"

### functions ### 

buildSearchParamers = function(cityName, checkin_date, checkout_date, offset) {
  list(
    "lang" = "ro",
    "selected_currency" = "RON",
    "src" = "searchresults",
    "ss" = cityName,
    "checkin_year" = strftime(checkin_date, "%Y"),
    "checkin_month" = strftime(checkin_date, "%m"),
    "checkin_monthday" = strftime(checkin_date, "%d"),
    "checkout_year" = strftime(checkout_date, "%Y"),
    "checkout_month" = strftime(checkout_date, "%m"),
    "checkout_monthday" = strftime(checkout_date, "%d"),
    "group_adults" = 2,
    "group_children" = 0,
    "no_rooms" = 1,
    "from_sf" = 1,
    "nflt" = "ht_id=204", #bifa cautare doar hoteluri
    "offset" = offset #NR hoteluri deja returnate (paginare)
  )
}

countHotelStars = function(node) {
  node %>% html_nodes("[data-testid='rating-stars'] span") %>% length()
}

getHotels = function(cityName, checkin_date_str, checkout_date_str) {
  checkin_date = as.Date(checkin_date_str, format = "%Y-%m-%d")    #exemplu: checkin_date_str = "2022-01-12"
  checkout_date = as.Date(checkout_date_str, format = "%Y-%m-%d")  #exemplu: checkout_date_str = "2022-01-14"
  hotels = data.frame()
  
  while (TRUE) {
    print(paste0("Oras = ", cityName, "; Nr. intermediar hoteluri gasite = ", nrow(hotels)))
    
    searchParameters = buildSearchParamers(cityName, checkin_date, checkout_date, offset = nrow(hotels))
    bookingHTML = RETRY("GET", BOOKING_BASE_URL, query = searchParameters, times = 5, pause_cap = 3, pause_min = 2) %>% read_html()
    hotelNodes = bookingHTML %>% html_nodes("[data-testid='property-card']")
    
    if (length(hotelNodes) == 0)
      break; #nu au fost gasite alte hoteluri (paginare); iesim din while
    
    hotelNames = hotelNodes %>% html_nodes("[data-testid='title']") %>% html_text()
    hotelStars = sapply(hotelNodes, countHotelStars)
    hotelPrices = hotelNodes %>% 
                  html_nodes("[data-testid='price-and-discounted-price'] span:last-child") %>% 
                  html_text() %>% parse_number(locale = locale(grouping_mark = "."))
    hotelBreakfastValidations = hotelNodes %>%
                  html_node("[data-testid='recommended-units']") %>% 
                  html_text() %>% grepl(pattern = "dejun", ignore.case = TRUE)
    hotelRoomTypes = hotelNodes %>%
                  html_node("[data-testid='recommended-units']") %>% 
                  html_node("span.df597226dd") %>% #clasa acestui span s-ar putea schimba in viitor
                  html_text()
    
    foundHotels = data.frame(nume = hotelNames,
                             stele = hotelStars,
                             pret = hotelPrices,
                             moneda = 'RON',
                             oras = cityName,
                             mic_dejun = hotelBreakfastValidations,
                             tip_camera = hotelRoomTypes,
                             data_checkin = checkin_date_str,
                             data_checkout = checkout_date_str,
                             data_culegere = Sys.Date())
    foundHotels = subset(foundHotels, stele != 0)  #eliminare hoteluri cu 0 stele
    hotels = rbind(hotels, foundHotels)
  }
  
  print(paste0("Oras = ", cityName, "; Nr. final hoteluri gasite = ", nrow(hotels)))
  return(hotels)
}

### main execution ###
t1 = Sys.time()
booking_hotels = do.call(rbind, lapply(ORASE, function(cityName) { getHotels(cityName, DATA_CHECKIN, DATA_CHECKOUT) } ))
write.csv(booking_hotels, paste0("booking_hotels_", Sys.Date(), ".csv"), row.names = FALSE)

t2 = t1 - Sys.time()
t2

