library(httr)
library(jsonlite)
library(rvest)
library(openxlsx)
library(dplyr)
library(future)
library(future.apply)

path0 = gsub("/Scripturi","",getwd())
# pentru scriere fisier mega cu date colectate
path1 = paste0(path0, "/Date_brute/Alimente")
# pentru scriere fisier de verificat
path2 = paste0(path0, "/Verificare")

ID_CATEGORIE_ALIMENTE = 77148
SUBCATEGORII_ALIMENTE = c(77150, 77152, 77153, 77155, 77156, 77158, 77159, 
                          77161, 77162, 77164, 77165, 77169, 77170, 77172, 
                          77174, 77176, 77178, 77179, 77181, 77184, 77186,
                          77189, 77191, 77193, 77196, 77199, 77201, 77203,
                          77206, 77229)

ID_CATEGORIE_CASA_FAMILIE = 77173
SUBCATEGORII_CASA_FAMILIE = c(77175, 77177, 77180, 77182, 77183, 77187, 77188, 
                              77190, 77192, 77194, 77195, 77197, 77205, 77207, 
                              77210, 77212, 77213, 77215, 77227)

DF_CATEGORII_ALIMENTE = data.frame(categoryId = ID_CATEGORIE_ALIMENTE, subcategoryId = SUBCATEGORII_ALIMENTE)
DF_CATEGORII_CASA_FAMILIE = data.frame(categoryId = ID_CATEGORIE_CASA_FAMILIE, subcategoryId = SUBCATEGORII_CASA_FAMILIE)

DF_CATEGORII = rbind(DF_CATEGORII_ALIMENTE, DF_CATEGORII_CASA_FAMILIE)

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

getCoraProducts = function(categoryId, subcategoryId) {
  
  CORA_PRODUCT_LIST_URL = "https://www.cora.ro/rest/product/list"
  PRODUCT_URL_PREFIX = "https://www.cora.ro/"
  DEFAULT_HEADERS = c("Content-Type" = "application/json", "Accept" = "application/json")
  
  products = data.frame()
  hasMoreResults = TRUE
  
  while (hasMoreResults) {
    requestObject = buildCoraProductListRequestObject(categoryId = categoryId, subcategoryId = subcategoryId, existingItems = products$ID)
    requestPayload = toJSON(requestObject, auto_unbox = TRUE)
    
    rawResponse = POST(url = CORA_PRODUCT_LIST_URL, body = requestPayload, add_headers(.headers = DEFAULT_HEADERS), encode = "json")
    
    if (rawResponse$status_code == 200) {
      textResponse = content(rawResponse, as = "text")
      responseObject = fromJSON(textResponse)
      responseProducts = responseObject$products
      
      print(paste("Produse incarcate = ", length(products$ID), "; categorie =", categoryId, ", subcategorie =", subcategoryId))
      
      foundProducts = list()
      foundProducts$ID = responseProducts$partnumber
      foundProducts$nume = responseProducts$name
      foundProducts$pret = responseProducts$price$amount
      foundProducts$um = responseProducts$price$unitOfMeasure
      foundProducts$instoc = responseProducts$availability
      foundProducts$descriere = responseProducts$shortDescription
      foundProducts$url = ifelse(0 != lengths(responseProducts$seoUrl), paste0(PRODUCT_URL_PREFIX, responseProducts$seoUrl), NA);
      foundProducts$idcategorie = responseProducts$category$id
      foundProducts$numecategorie = responseProducts$category$name
      foundProducts$idsubcateg = responseProducts$subCategory$id
      foundProducts$numesubcateg = responseProducts$subCategory$name
      
      hasMoreResults = responseObject$hasMoreResults
      products = bind_rows(products, as.data.frame(foundProducts))
    }
  }
  
  return(products);
}

t1 = Sys.time()

plan(multisession)
cora = do.call(rbind, 
               future_lapply(1:nrow(DF_CATEGORII), function(x) { return(getCoraProducts(DF_CATEGORII$categoryId[x], DF_CATEGORII$subcategoryId[x]))}))

# Testare
t2 = t1 - Sys.time()
t2
print("# 1. Timpul de rulare: aprox 10 min")
nrow(cora)
print("# 2. Nr de prod: aprox 8000 produse")

setwd(path1)
write.csv(cora, paste0("cora-", Sys.Date(), ".csv"))


##### Verificare ####################################################

#remove double white spaces
cora$ID = trimws(cora$ID)
cora$nume = trimws(cora$nume)

setwd(path2)
cora_v = read.xlsx("cora.xlsx") #113 prod
cora_v$ID = trimws(cora_v$ID)
cora_v$nume1 = trimws(cora_v$nume1)
cora_v$nume2 = trimws(cora_v$nume2)


# combinatia de join: ID+nume1, ID+nume2
cora_v = left_join(cora_v, cora[,c("ID", "pret")], by = c("ID" = "ID"))
# cora_v = left_join(cora_v, cora[,c("ID", "nume", "pret")], by = c("ID" = "ID", "nume1"="nume"))
# cora_v = left_join(cora_v, cora[,c("ID", "nume", "pret")], by = c("nume1" = "nume"))
sum(is.na(cora_v$pret))
nrow(cora_v)

names(cora_v)[ncol(cora_v)] = paste0(substr(Sys.Date(), 9,10),"_", months(Sys.Date()))
cora_v$sp = ""
names(cora_v)[ncol(cora_v)] = paste0("sp_",substr(Sys.Date(), 9,10),"_", months(Sys.Date()))

setwd(path2)
write.xlsx(cora_v, "cora.xlsx")
#write.xlsx(cora_v, paste0("cora_v", Sys.Date(),".xlsx" ))




