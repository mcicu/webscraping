library(httr)
library(jsonlite)
library(rvest)
library(openxlsx)
library(dplyr)
library(future)
library(future.apply)
library(RSQLite)

path0 = gsub("/Scripturi","",getwd())
# pentru scriere fisier mega cu date colectate
path1 = paste0(path0, "/Date_brute/Alimente")
# pentru scriere fisier de verificat
path2 = paste0(path0, "/Verificare")
# pentru DB
path3 = paste0(path0, "/DB")

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
  
  products = data.frame(
    id = character(), 
    nume = character(), 
    pret = double(),
    data = double(),
    magazin = character(),
    link = character(),
    categorie = character(),
    subcategorie = character(),
    descriere = character(),
    um = character(),
    instoc = character()
    )
  hasMoreResults = TRUE
  
  while (hasMoreResults) {
    requestObject = buildCoraProductListRequestObject(categoryId = categoryId, subcategoryId = subcategoryId, existingItems = products$id)
    requestPayload = toJSON(requestObject, auto_unbox = TRUE)
    rawResponse = POST(url = CORA_PRODUCT_LIST_URL, body = requestPayload, add_headers(.headers = DEFAULT_HEADERS), encode = "json")
    
    if (rawResponse$status_code == 200) {
      textResponse = content(rawResponse, as = "text")
      responseObject = fromJSON(textResponse)
      responseProducts = responseObject$products
      
      if (length(responseProducts) == 0) {
        hasMoreResults = FALSE
        break
      }
      
      print(paste("Produse incarcate = ", length(products$id), "; categorie =", categoryId, ", subcategorie =", subcategoryId))
      
      foundProducts = list()
      
      foundProducts$id = responseProducts$partnumber
      foundProducts$nume = responseProducts$name
      foundProducts$pret = responseProducts$price$amount
      foundProducts$data = replicate(length(foundProducts$id), Sys.Date())
      foundProducts$magazin = replicate(length(foundProducts$id), "cora")
      foundProducts$link = ifelse(0 != lengths(responseProducts$seoUrl), paste0(PRODUCT_URL_PREFIX, responseProducts$seoUrl), NA)
      foundProducts$categorie = ifelse(match("name", names(responseProducts$category)), responseProducts$category$name, NA)
      foundProducts$subcategorie = ifelse(match("name", names(responseProducts$subCategory)), responseProducts$subCategory$name, NA)
      foundProducts$descriere = responseProducts$shortDescription
      foundProducts$um = responseProducts$price$unitOfMeasure
      foundProducts$instoc = responseProducts$availability
      
      products = bind_rows(products, as.data.frame(foundProducts))
    }
  }
  
  return(products);
}

t1 = Sys.time()

plan(multisession)
cora = do.call(rbind, 
               future_lapply(1:nrow(DF_CATEGORII), function(x) { return(getCoraProducts(DF_CATEGORII$categoryId[x], DF_CATEGORII$subcategoryId[x]))}))
cora$data = Sys.Date()
cora$descriere = paste0(cora$descriere, ". UM: ", cora$um, ". InStoc: ", cora$instoc)
cora$um = NULL
cora$instoc = NULL
cora$nume = trimws(cora$nume)

t2 = t1 - Sys.time()

######## test ##################################################################

print(paste("# 1. Timpul de rulare: ", round(t2,2), " min"))
print(paste("# 2. Numarul de linii: ", nrow(cora)))
print(paste("# 3. Numarul de coloane: ", ncol(cora)))
print("# 4. Denumire coloane: ")
names(cora)
print(paste("# 4. Numarul de preturi lipsa: ", sum(is.na(cora$pret))))

################################################################################

setwd(path1)
write.csv(cora, paste0("cora_", Sys.Date(), ".csv"), row.names = FALSE)

######## clean #################################################################

cora = cora %>% mutate_all(as.character)
cora = cora %>% mutate_all(trimws)
id = cora %>% group_by(id) %>% summarise(n=n())
pret = cora %>% group_by(pret) %>% summarise(n=n())
data = cora %>% group_by(data) %>% summarise(n=n())
cora = unique(cora)
# !!!! neaparat data sa fie character


########  DB    ################################################################
setwd(path3)
alimdb <- dbConnect(RSQLite::SQLite(), "alimente.db")
dbSendQuery(alimdb, 'INSERT INTO alimente (id, nume, pret, data, magazin, link, categorie, subcategorie, descriere)
                     VALUES (:id, :nume, :pret, :data, :magazin, :link, :categorie, :subcategorie, :descriere);', cora)
a = dbGetQuery(alimdb, "SELECT * FROM alimente")

data_db = a %>% group_by(data) %>% summarise(n=n())

#dbExecute(alimdb, "DELETE FROM alimente WHERE data ==  18736.0")

dbDisconnect(alimdb)


##### Verificare ####################################################

#remove double white spaces
cora$id = trimws(cora$id)
cora$nume = trimws(cora$nume)

setwd(path2)
cora_v = read.xlsx("cora.xlsx") #113 prod
cora_v$id = trimws(cora_v$id)
cora_v$nume1 = trimws(cora_v$nume1)
cora_v$nume2 = trimws(cora_v$nume2)


# combinatia de join: ID+nume1, ID+nume2
cora_v = left_join(cora_v, cora[,c("id", "pret")], by = c("id" = "id"))
# cora_v = left_join(cora_v, cora[,c("ID", "nume", "pret")], by = c("ID" = "ID", "nume1"="nume"))
# cora_v = left_join(cora_v, cora[,c("ID", "nume", "pret")], by = c("nume1" = "nume"))
sum(is.na(cora_v$pret))
nrow(cora_v)

names(cora_v)[ncol(cora_v)] = paste0(substr(Sys.Date(), 9,10),"_", months(Sys.Date()))
cora_v$sp = ""
names(cora_v)[ncol(cora_v)] = paste0("sp_",substr(Sys.Date(), 9,10),"_", months(Sys.Date()))

setwd(path2)
write.xlsx(cora_v, "cora.xlsx", overwrite = TRUE)
#write.xlsx(cora_v, paste0("cora_v", Sys.Date(),".xlsx" ))




