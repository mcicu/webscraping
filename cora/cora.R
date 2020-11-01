library(httr)
library(jsonlite)
library(rvest)

ID_CATEGORIE_ALIMENTE = 77148
SUBCATEGORII_ALIMENTE = c(77150, 77152, 77153, 77155, 77156, 77158, 77159, 
                          77161, 77162, 77164, 77165, 77169, 77170, 77172, 
                          77174, 77176, 77178, 77179, 77181, 77184, 77186,
                          77189, 77191, 77193, 77196, 77199, 77201, 77203,
                          77206, 77229)
CORA_PRODUCT_LIST_URL = "https://www.cora.ro/rest/product/list"
PRODUCT_URL_PREFIX = "https://www.cora.ro/"
DEFAULT_HEADERS = c("Content-Type" = "application/json", "Accept" = "application/json")


buildCoraProductListRequestObject = function(categoryId, subcategoryId, existingItems) {
  if (is.null(existingItems)) {
    existingItems = list()
  }
  
  requestPayload = list()
  requestPayload$filter = list()
  requestPayload$filter$filters = list()
  requestPayload$filter$categoryId = categoryId
  requestPayload$filter$subCategoryId = subcategoryId
  requestPayload$filter$count = 10
  requestPayload$pageNumber = 1
  requestPayload$itemsOnPage = existingItems #product ids already loaded
  return(requestPayload)
}

products = list()

for (subcategoryId in SUBCATEGORII_ALIMENTE) {
  hasMoreResults = TRUE
  
  while (hasMoreResults) {
    requestObject = buildCoraProductListRequestObject(categoryId = ID_CATEGORIE_ALIMENTE, subcategoryId = subcategoryId, existingItems = products$id)
    requestPayload = toJSON(requestObject, auto_unbox = TRUE)
    
    rawResponse = POST(url = CORA_PRODUCT_LIST_URL, body = requestPayload, add_headers(.headers = DEFAULT_HEADERS), encode = "json")
    
    if (rawResponse$status_code == 200) {
      textResponse = content(rawResponse, as = "text")
      responseObject = fromJSON(textResponse)
      responseProducts = responseObject$products
      
      print(paste("Produse incarcate = ", length(products$id), "; Subcategorie curenta =", subcategoryId))
      
      products$id = c(products$id, responseProducts$partnumber)
      products$name = c(products$name, responseProducts$name)
      products$price = c(products$price, responseProducts$price$amount)
      products$unitOfMeasure = c(products$unitOfMeasure, responseProducts$price$unitOfMeasure)
      products$availability = c(products$availability, responseProducts$availability)
      products$description = c(products$description, responseProducts$shortDescription)
      
      if(0 != length(responseProducts$seoUrl)) {
        products$URL = c(products$URL, paste0(PRODUCT_URL_PREFIX, responseProducts$seoUrl))
      }
      
      products$categoryId = c(products$categoryId, responseProducts$category$id)
      products$categoryName = c(products$categoryName, responseProducts$category$name)
      products$subcategoryId = c(products$subcategoryId, responseProducts$subCategory$id)
      products$subcategoryName = c(products$subcategoryName, responseProducts$subCategory$name)
      
      hasMoreResults = responseObject$hasMoreResults
    }
  }
}

write.csv(products, paste0("cora-", Sys.Date(), ".csv"))

