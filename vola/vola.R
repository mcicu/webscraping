library(httr)
library(jsonlite)
library(rvest)


### Primul apel, pt initiere sesiune api.ith.toys ###
### Trimitem detaliile zborului in input ###

set_config(add_headers(.headers = c(
  "x-affiliate" = "vola",
  "content-type" = "application/json")))

build_request_input = function(from_airport_code, to_airport_code, departure_date, return_date) {
  json_input <- fromJSON(txt = "input_template.json")
  #zbor dus
  json_input$queries$departure[[1]] = departure_date; # ex: "2023-03-15"
  json_input$queries$origin$code[[1]] = from_airport_code; # ex: "OTP" (inseamna aeoroport Otopeni)
  json_input$queries$destination$code[[1]] = to_airport_code; # ex: "LTN" (inseamna aeroport Luton (Londra))
  
  #zbor intors
  json_input$queries$departure[[2]] = return_date; # ex: "2023-03-19"
  json_input$queries$origin$code[[2]] = to_airport_code; # ex: "LTN" (inseamna aeroport Luton (Londra))
  json_input$queries$destination$code[[2]] = from_airport_code; # ex: "OTP" (inseamna aeoroport Otopeni)
  return(json_input)
}


request_input = build_request_input("OTP", "LTN", "2023-03-20", "2023-03-25")
http_response = RETRY(verb = "POST", url = "https://api.ith.toys/gateway/travelOffers/",
      body = request_input, times = 1, encode = "json")

response_body = content(http_response, encoding = "json")
session_id = response_body$id



### Al doilea apel, apelam api.ith.toys cu sesiunea obtinuta din primul apel ###

URL_WITH_SESSION = paste0("https://api.ith.toys/gateway/travelOffers/", session_id)
http_response = RETRY(verb = "GET", url = URL_WITH_SESSION, times = 1, encode = "json")
flight_data = content(http_response, encoding = "json")


#flight_data$offers contine ofertele de pret;
# !!! Ocazional, al doilea apel nu returneaza oferte de pret, 
#     apelul trebuie refacut daca flight_data$offers nu contine oferte




