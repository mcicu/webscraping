library(httr)
library(jsonlite)
library(rvest)
library(openxlsx)
library(dplyr)
library(future)
library(future.apply)
library(RSQLite)

### parallel run configuration ###
plan(multisession)

### global variables ###
path0 = gsub("/Scripturi","",getwd())
# pentru scriere fisier mega cu date colectate
path1 = paste0(path0, "/Date_brute/Alimente")
# pentru scriere fisier de verificat
path2 = paste0(path0, "/Verificare")
# pentru DB
path3 = paste0(path0, "/DB")

### constants ###
PAGINI_CATEGORII = c(
  "https://www.cora.ro/produse-proaspete.html",
  "https://www.cora.ro/bacanie.html"
)

### functions ### 
getSubcategories = function(mainCategoryURL) {
  categoryPageHTML = RETRY("GET", mainCategoryURL, times = 5, pause_cap = 3, pause_min = 2) %>% read_html()
  subcategoryAnchors = categoryPageHTML %>% html_nodes("span.block-category-link-inline > a")
  subcategories = data.frame(
    url = subcategoryAnchors %>% html_attr("href"),
    name = subcategoryAnchors %>% html_text())
  return(subcategories)
}

getProductDetails = function(productURL) {
  productPageHTML = RETRY("GET", productURL, times = 5, pause_cap = 3, pause_min = 2) %>% read_html()
  productInfoHTML = productPageHTML %>% html_node(".product-info-main")
  
  productDF = data.frame(
    id = paste0("P-", productInfoHTML %>% html_node("[itemprop='sku']") %>% html_text()),
    nume = productInfoHTML %>% html_node("[itemprop='name']") %>% html_text() %>% trimws(), 
    pret = productInfoHTML %>% html_node("[itemprop='price']") %>% html_attr("content"), 
    link = productURL,
    descriere = productInfoHTML %>% html_node("[itemprop='description']") %>% html_text(),
    um = productInfoHTML %>% html_node("[itemprop='unit_price_sales_unit']") %>% html_text()
  )
  
  return(productDF)
}

getProducts = function(subcategoryURL, subcategoryName) {
  print(paste0("subcategoryURL = ", subcategoryURL, ", subcategoryName = ", subcategoryName))
  pageIndex = 0
  productsDF = data.frame()
  while (TRUE) {
    pageIndex = pageIndex + 1
    print(paste0("Page = ", pageIndex))
    productsPageURL = paste0(subcategoryURL, "?p=", pageIndex)
    productsPageHTML = RETRY("GET", productsPageURL, times = 5, pause_cap = 3, pause_min = 2) %>% read_html()
    productListHTML = productsPageHTML %>% html_node("#amasty-shopby-product-list .product-items")
    
    if (is.na(productListHTML)) {
      break; #no more products can be found, stop the while loop
    }
    
    productURLs = productListHTML %>% html_nodes(".product-item-photo > a") %>% html_attr("href")
    foundProductsDF = do.call(rbind, future_lapply(productURLs, getProductDetails))
    foundProductsDF = cbind(foundProductsDF, subcategorie=subcategoryName, data = Sys.Date(), magazin = "cora") ## extra coloane
    productsDF = rbind(productsDF, foundProductsDF)
  }
  return(productsDF)
}


### main execution ###
t1 = Sys.time()
subcategories = do.call(rbind, lapply(PAGINI_CATEGORII, getSubcategories))
cora = do.call(rbind, lapply(1:nrow(subcategories), function(x) {return(getProducts(subcategories$url[x], subcategories$name[x]))}))
cora$descriere = paste0(cora$descriere, ". UM: ", cora$um)
cora$um = NULL

t2 = t1 - Sys.time()
t2

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
a = dbGetQuery(alimdb, "SELECT count(*) FROM alimente")
a

data_db = dbGetQuery(alimdb, "SELECT data, count(*) FROM alimente group by data")


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
cora_v = unique(cora_v)

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




