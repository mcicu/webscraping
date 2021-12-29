library(httr)  
library(rvest) 
library(jsonlite)
library(openxlsx)
library(dplyr)
library(future)
library(future.apply)
library(RSQLite)

### set parallel plan for future_* executions
plan(multisession, gc = TRUE, workers = 100)

path0 = gsub("/Scripturi","",getwd())
# pentru scriere fisier mega cu date colectate
path1 = paste0(path0, "/Date_brute/Alimente")
# pentru scriere fisier de verificat
path2 = paste0(path0, "/Verificare")
# pentru DB
path3 = paste0(path0, "/DB")

### functii
removeNewlineAndMultiWhitespaceFn = function(a) {
  b = gsub("(\\r|\\n|\\s)+", " ", a);
  return(b);
}

### cod

#### seteaza locatia in Bringo pentru Carrefour Orhideea (cookie-urile se salveaza automat)
#### nu se poate folosi http_session direct, ci doar GET/POST, datorita cookie-urilor

urlBringo = "https://www.bringo.ro"
urlCarrefour <- "https://www.bringo.ro/ro/store-details/carrefour_orhideea"

setLocationForCarrefourOrhideeea = function() {
  urlBringoRo = "https://www.bringo.ro/ro/"
  bringoFirstPage = GET(urlBringoRo)
  locationFormCSRFToken = bringoFirstPage %>% read_html() %>%
    html_node("[name=store_search\\[_token\\]]") %>% html_attr("value")
  
  form_data = list()
  form_data["store_search[address]"] = "Splaiul Independenței, Bucharest, Romania"
  form_data["store_search[streetNumber]"] = "210"
  form_data["store_search[street]"] = "Splaiul Independenței"
  form_data["store_search[latitude]"] = "44.4430707"
  form_data["store_search[longitude]"] = "26.0627016"
  form_data["store_search[locality]"] = "București"
  form_data["store_search[_token]"] = locationFormCSRFToken
  
  POST(url = urlBringoRo, body = form_data, encode = "form")
  print("--->>> Bringo location set to Carrefour Orhideea")
}

setLocationForCarrefourOrhideeea()

############################

getMainCategories = function() {
  ### get all categories - disabled 
  #carrefourCategoriesPage = GET(url = urlCarrefour) %>% read_html()
  #categoryURLPaths <- carrefourCategoriesPage %>%
  #                 html_nodes("div.bringo-category-list-box a.box-inner") %>% 
  #                 html_attr("href") 
  #categoryURLs = paste0(urlBringo, categoryURLPaths)
  
  categoryURLs = c("https://www.bringo.ro/ro/store/carrefour_orhideea/263",   #Lactate, branzeturi si oua
                   "https://www.bringo.ro/ro/store/carrefour_orhideea/250",   #Legume si fructe
                   "https://www.bringo.ro/ro/store/carrefour_orhideea/251",   #Macelarie si Peste
                   "https://www.bringo.ro/ro/store/carrefour_orhideea/256",   #Bacanie
                   "https://www.bringo.ro/ro/store/carrefour_orhideea/249",   #Brutarie, patiserie, cofetarie
                   "https://www.bringo.ro/ro/store/carrefour_orhideea/257",   #Dulciuri
                   "https://www.bringo.ro/ro/store/carrefour_orhideea/253",   #Curatenie si Intretinere
                   "https://www.bringo.ro/ro/store/carrefour_orhideea/23743", #Din Romania
                   "https://www.bringo.ro/ro/store/carrefour_orhideea/287",   #Mezeluri
                   "https://www.bringo.ro/ro/store/carrefour_orhideea/3345",  #Igiena si sanatate
                   "https://www.bringo.ro/ro/store/carrefour_orhideea/252",   #Cosmetice dama
                   "https://www.bringo.ro/ro/store/carrefour_orhideea/286",   #Articole menaj
                   "https://www.bringo.ro/ro/store/carrefour_orhideea/4235",  #Sanatate & Protectie
                   "https://www.bringo.ro/ro/store/carrefour_orhideea/3361")  #Cosmetice barbati
  
  return(categoryURLs)
}

sortimente = getMainCategories()

## subsortimente
link_subsort = c()
for (i in 1:length(sortimente)) {
  subsort = GET(sortimente[i]) %>% read_html() %>%
            html_nodes("div.bringo-product-listing-category-menu>a") %>% 
            html_attr("href") 
  subsort = subsort[-1]
  link_subsort = c(link_subsort, subsort)
}
link_subsort = paste0(urlBringo, link_subsort)


## pagini subsortimente
link_subsort_pages = c()
for (i in 1:length(link_subsort)) {
  pageCount = GET(link_subsort[i]) %>% read_html() %>%
    html_node("ul.pagination>*:nth-last-child(2)") %>% html_text()
  
  if (is.na(pageCount)) {
    # avem doar o singura pagina cu produse
    link_subsort_pages = c(link_subsort_pages, link_subsort[i])
  } else {
    # avem mai multe pagini cu produse
    for (pageIndex in 1:pageCount) {
      link_subsort_page = paste0(link_subsort[i], "?page=", pageIndex)
      link_subsort_pages = c(link_subsort_pages, link_subsort_page)
    }
  }
}



## pagini produse
getBringoProductLinks = function(pageURL) {
  print(paste("Pagina produse: ", pageURL));
  productURL = tryCatch(GET(pageURL) %>% read_html() %>%
                        html_nodes("div.top-product-listing-box>a.bringo-product-name") %>% 
                        html_attr("href"),error = function(e){NA})
  
  if (is.na(productURL)) 
    return();
  return(productURL);
}

link_prod = do.call(c, future_lapply(link_subsort_pages, getBringoProductLinks))
link_prod = paste0(urlBringo, link_prod)

## preluare detalii produse
t1 = Sys.time()

getBringoProducts = function(productURL) {
  print(paste("Produs: ", productURL))
  
  tryCatch({
    productPage = RETRY("GET", productURL, times = 20, pause_cap = 3, pause_min = 2) %>% read_html()
    
    nume = productPage %>% html_node("div.bringo-product-details .product-name") %>% 
                      html_text() %>% removeNewlineAndMultiWhitespaceFn()
    pret = productPage %>% html_node("div.bringo-product-details .product-price") %>% 
                      html_text() %>% removeNewlineAndMultiWhitespaceFn() %>%
                      gsub(pattern = " RON", replacement = "") %>% gsub(pattern = ",", replacement = ".")
    descriere = productPage %>% html_node("#productDetailsTabContent #details") %>%
                           html_text() %>% removeNewlineAndMultiWhitespaceFn()
    cat_info = productPage %>% html_nodes("div.bringo-breadcrumb .section") %>%
                          html_text()
    
    id = sub(".*(Numar produs:.*)", "\\1", descriere) %>% sub(pattern = "\\D*(\\d+).*", replacement = "\\1")
    
    if (length(nume) == 0)
      nume = NA
    if (length(pret) == 0)
      pret = NA
    if (length(descriere) == 0)
      descriere = NA
    if (length(cat_info) != 4) {
      categorie = NA
      subcategorie = NA
    } else{
      categorie = cat_info[3]
      subcategorie = cat_info[4]
    }
    
    informatiiProdus = data.frame(id = id, nume = nume, pret = pret, data = Sys.Date(), magazin = "carrefour",
                                  link = productURL,categorie = categorie, subcategorie = subcategorie,descriere = descriere)
    return(informatiiProdus)
  })
}

carrefour = do.call(rbind, future_lapply(link_prod, getBringoProducts))

t2 = t1 - Sys.time()

######## test ##################################################################

print(paste("# 1. Timpul de rulare: ", round(t2,2)))
print(paste("# 2. Numarul de linii: ", nrow(carrefour)))
print(paste("# 3. Numarul de coloane: ", ncol(carrefour)))
print("# 4. Denumire coloane: ")
names(carrefour)
print(paste("# 4. Numarul de preturi lipsa: ", sum(is.na(carrefour$pret))))

################################################################################

setwd(path1)
write.csv(carrefour, paste0("carrefour_", Sys.Date(), ".csv"), row.names = FALSE)

######## clean #################################################################

carrefour = carrefour %>% mutate_all(as.character)
carrefour = carrefour %>% mutate_all(trimws)
id = carrefour %>% group_by(id) %>% summarise(n=n())
pret = carrefour %>% group_by(pret) %>% summarise(n=n())
data = carrefour %>% group_by(data) %>% summarise(n=n())
carrefour = unique(carrefour)
# !!!! neaparat data sa fie character


########  DB    ################################################################
setwd(path3)
alimdb <- dbConnect(RSQLite::SQLite(), "alimente.db")
dbSendQuery(alimdb, 'INSERT INTO alimente (id, nume, pret, data, magazin, link, categorie, subcategorie, descriere)
                     VALUES (:id, :nume, :pret, :data, :magazin, :link, :categorie, :subcategorie, :descriere);', carrefour)
a = dbGetQuery(alimdb, "SELECT * FROM alimente")

data_db = a %>% group_by(data) %>% summarise(n=n())

#dbExecute(alimdb, "DELETE FROM alimente WHERE data ==  18736.0")

dbDisconnect(alimdb)


##### Verificare ####################################################


#remove double white spaces
carrefour$id = trimws(carrefour$id)
carrefour$nume = trimws(carrefour$nume)
carrefour = unique(carrefour)

setwd(path2)
carr_v = read.xlsx("carrefour.xlsx") #101 prod
carr_v$id = trimws(carr_v$id)
carr_v$nume1 = trimws(carr_v$nume1)
carr_v$nume2 = trimws(carr_v$nume2)


# join dupa:id, nume1, nume2
carr_v = left_join(carr_v, carrefour[,c("id", "pret")], by = c("id" = "id"))
#carr_v = left_join(carr_v, carrefour[,c("ID", "nume", "pret")], by = c("ID" = "ID", "nume1"="nume"))
sum(is.na(carr_v$pret))
nrow(carr_v)

names(carr_v)[ncol(carr_v)] = paste0(substr(Sys.Date(), 9,10),"_", months(Sys.Date()))
carr_v$sp = ""
names(carr_v)[ncol(carr_v)] = paste0("sp_",substr(Sys.Date(), 9,10),"_", months(Sys.Date()))

setwd(path2)
write.xlsx(carr_v, "carrefour.xlsx")
#write.xlsx(mega_v, paste0("mega_v_", Sys.Date(),".xlsx" ))




#### matching cu produse selectate pt verificare
df = unique(df)
carr = read.xlsx("E:/_Lucrari_INS/Proiect_Preturi_Online/1.Colectare/Scripturi/carrefour.xlsx")
carr$nume2_bringo = as.character(carr$nume2_bringo)
carr$nume1_carrefour = as.character(carr$nume1_carrefour)
carr$nume1_carrefour = trimws(carr$nume1_carrefour)

df$nume = as.character(df$nume)

# nume1 + nume2 + id
jcarr = left_join(carr, df[,c(1,2)], by = c("nume2_bringo"="nume"))



dt = paste0(substr(df$data[1],9,10),  "_", months(df$data[1]))
names(jcarr)[ncol(jcarr)] = dt


setwd(path2)
write.xlsx(jcarr, paste0("carrefourbringo_v_", Sys.Date(), ".xlsx"))



