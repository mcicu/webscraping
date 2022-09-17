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


### parallel run configuration ###
plan(multisession)
options(future.seed = TRUE)
options(future.rng.onMisuse = "ignore")

### configuration ###
set_config(add_headers(.headers = c(
  "Accept-Encoding" = "gzip, deflate",
  "User-Agent" = "PostmanRuntime/7.26.8")))

### constants ###
BOOKING_BASE_URL = "https://www.booking.com/searchresults.ro.html"
LISTA_ORASE = c("Bucuresti") #, "Brasov", "Constanta", "Prahova"
LISTA_SEJURURI = list(
  c(numar_persoane = 2, data_checkin = "2022-11-11", data_checkout = "2022-11-13"),
  c(numar_persoane = 2, data_checkin = "2022-11-18", data_checkout = "2022-11-20"),
  c(numar_persoane = 2, data_checkin = "2022-11-25", data_checkout = "2022-11-27"),
  c(numar_persoane = 2, data_checkin = "2022-11-09", data_checkout = "2022-11-10"),
  c(numar_persoane = 2, data_checkin = "2022-11-16", data_checkout = "2022-11-17"),
  c(numar_persoane = 2, data_checkin = "2022-11-23", data_checkout = "2022-11-24"),
  
  c(numar_persoane = 1, data_checkin = "2022-11-11", data_checkout = "2022-11-13"),
  c(numar_persoane = 1, data_checkin = "2022-11-18", data_checkout = "2022-11-20"),
  c(numar_persoane = 1, data_checkin = "2022-11-25", data_checkout = "2022-11-27"),
  c(numar_persoane = 1, data_checkin = "2022-11-09", data_checkout = "2022-11-10"),
  c(numar_persoane = 1, data_checkin = "2022-11-16", data_checkout = "2022-11-17"),
  c(numar_persoane = 1, data_checkin = "2022-11-23", data_checkout = "2022-11-24")
)

### functions ### 

buildSearchParamers = function(cityName, personCount, checkinDate, checkoutDate, offset) {
  list(
    "lang" = "ro",
    "selected_currency" = "RON",
    "src" = "searchresults",
    "ss" = cityName,
    "checkin_year" = strftime(checkinDate, "%Y"),
    "checkin_month" = strftime(checkinDate, "%m"),
    "checkin_monthday" = strftime(checkinDate, "%d"),
    "checkout_year" = strftime(checkoutDate, "%Y"),
    "checkout_month" = strftime(checkoutDate, "%m"),
    "checkout_monthday" = strftime(checkoutDate, "%d"),
    "group_adults" = personCount,
    "group_children" = 0,
    "no_rooms" = 1,
    "from_sf" = 1,
    "nflt" = "ht_id=204", #bifa cautare doar hoteluri
    "offset" = offset #NR hoteluri deja returnate (paginare)
  )
}

countRoomOccupancy = function(node) {
  node %>% html_nodes("i.bicon-occupancy") %>% length()
}

isBreakfastIncluded = function(node) {
  breakfastIncluded = FALSE
  roomConditions = node %>% html_nodes("li.bui-list__item") %>% html_text()
  for (roomCondition in roomConditions) {
    if (grepl(pattern = "Mic dejun.*inclus", x = roomCondition))
      breakfastIncluded = TRUE
  }
  
  return(breakfastIncluded)
}

isReimbursable = function(node) {
  reimbursable = TRUE
  roomConditions = node %>% html_nodes("li.bui-list__item") %>% html_text()
  for (roomCondition in roomConditions) {
    if (grepl(pattern = "Nerambursabil", x = roomCondition))
      reimbursable = FALSE
  }
  
  return(reimbursable)
}

getHotelData = function(hotelURL, hotelDistanceToCenter, personCount) {
  set_config(add_headers(.headers = c(
    "Accept-Encoding" = "gzip, deflate",
    "User-Agent" = "PostmanRuntime/7.26.8")))
  
  hotelPageHTML = RETRY("GET", hotelURL, times = 5, pause_cap = 3, pause_min = 2) %>% read_html()
  hotelName = hotelPageHTML %>% html_node(".pp-header__title") %>% html_text()
  hotelStars = hotelPageHTML %>% html_nodes("[data-testid='rating-stars'] span") %>% length()
  
  roomTypeNodes = hotelPageHTML %>% html_nodes("td.hprt-table-cell-roomtype")
  roomTypeNames = roomTypeNodes %>% html_node(".hprt-roomtype-icon-link") %>% html_text()
  roomSurfaces = roomTypeNodes %>% html_node("[data-name-en='room size']") %>% html_text()
  roomTypeRowCount = roomTypeNodes %>% html_attr("rowspan")
  
  roomOccupancies = sapply(hotelPageHTML %>% html_nodes("span.c-occupancy-icons__adults"), countRoomOccupancy)
  roomPrices = hotelPageHTML %>% html_nodes("span.prco-valign-middle-helper") %>%
                                  html_text() %>% parse_number(locale = locale(grouping_mark = "."))
  
  breakfastIncluded = sapply(hotelPageHTML %>% html_nodes("td.hprt-table-cell-conditions"), isBreakfastIncluded)
  reimbursable = sapply(hotelPageHTML %>% html_nodes("td.hprt-table-cell-conditions"), isReimbursable)
  
  
  hotelData = data.frame(nume = hotelName,
                         stele = hotelStars,
                         tip_camera = rep(roomTypeNames, roomTypeRowCount),
                         suprafata_camera = rep(roomSurfaces, roomTypeRowCount),
                         numar_persoane = roomOccupancies,
                         pret = roomPrices,
                         moneda = "RON",
                         mic_dejun_inclus = breakfastIncluded,
                         rambursabil = reimbursable,
                         distanta = hotelDistanceToCenter)
  
  hotelData = subset(hotelData, stele != 0) #eliminare inregistrari cu 0 stele
  hotelData = subset(hotelData, numar_persoane == personCount) #eliminare inregistrari cu capacitatea camerei diferita de numarul solicitat

  
  return(hotelData)
}


getHotels = function(cityName, personCount, checkinDateString, checkoutDateString) {
  checkinDate = as.Date(checkinDateString, format = "%Y-%m-%d")    #exemplu: checkin_date_str = "2022-01-12"
  checkoutDate = as.Date(checkoutDateString, format = "%Y-%m-%d")  #exemplu: checkout_date_str = "2022-01-14"
  
  hotelCount = 0
  hotelRecords = data.frame()
  
  while (TRUE) {
    print(paste0("Oras = ", cityName, "; Nr. pers = ", personCount, "; Perioada [", checkinDateString, ", ", checkoutDateString, "]; Nr. intermediar hoteluri gasite = ", hotelCount))
    print(paste0("Oras = ", cityName, "; Nr. pers = ", personCount, "; Perioada [", checkinDateString, ", ", checkoutDateString, "]; Nr. intermediar inregistrari gasite = ", nrow(hotelRecords)))
    
    searchParameters = buildSearchParamers(cityName, personCount, checkinDate, checkoutDate, offset = hotelCount)
    bookingHTML = RETRY("GET", BOOKING_BASE_URL, query = searchParameters, times = 5, pause_cap = 3, pause_min = 2) %>% read_html()
    hotelNodes = bookingHTML %>% html_nodes("[data-testid='property-card']")
    
    if (length(hotelNodes) <= 1)
      break; #nu au fost gasite alte hoteluri (paginare); iesim din while
    
    #if (nrow(hotelRecords) > 100)
    #  break; #limitam rezultatele pentru test; iesim din while
    
    hotelCount = hotelCount + length(hotelNodes)
    
    hotelURLs = hotelNodes %>% html_node("[data-testid='title-link']") %>% html_attr("href")
    hotelDistances = hotelNodes %>% html_node("[data-testid='distance']") %>% html_text()
    
    foundHotelRecords = do.call(rbind, future_lapply(1:length(hotelURLs), function(x) {return(getHotelData(hotelURLs[x], hotelDistances[x], personCount))}))
    hotelRecords = rbind(hotelRecords, foundHotelRecords)
  }
  
  #adaugam extra coloane pentru a stoca informatiile de colectare
  hotelRecords = cbind(hotelRecords, 
                       oras = cityName,
                       data_checkin = checkinDateString,
                       data_checkout = checkoutDateString,
                       zile_sejur = as.character(checkoutDate - checkinDate),
                       data_colectare = Sys.Date())
  
  print(paste0("Oras = ", cityName, "; Nr. pers = ", personCount, "; Perioada [", checkinDateString, ", ", checkoutDateString, "]; Nr. final hoteluri gasite = ", hotelCount))
  print(paste0("Oras = ", cityName, "; Nr. pers = ", personCount, "; Perioada [", checkinDateString, ", ", checkoutDateString, "]; Nr. final inregistrari gasite = ", nrow(hotelRecords)))
  return(hotelRecords)
}


#### MAIN EXECUTION ####
t1 = Sys.time()
bookingRecords = data.frame()

for (sejur in LISTA_SEJURURI) {
  personCount = sejur[["numar_persoane"]]
  checkinDateString = sejur[["data_checkin"]]
  checkoutDateString = sejur[["data_checkout"]]
  
  foundHotelRecords = do.call(rbind, lapply(LISTA_ORASE, function(cityName) { getHotels(cityName, personCount, checkinDateString, checkoutDateString) } ))
  bookingRecords = rbind(bookingRecords, foundHotelRecords)
}

print(paste0("Colectare completa, numar total inregistrari = ", nrow(bookingRecords)))
write.csv(bookingRecords, paste0("booking_hotels_", Sys.Date(), ".csv"), row.names = FALSE)

t2 = t1 - Sys.time()
t2

