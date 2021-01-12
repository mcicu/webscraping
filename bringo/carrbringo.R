
library(httr)  
library(rvest) 
library(jsonlite)
library(openxlsx)
library(dplyr)
library(future)
library(future.apply)


path0 = gsub("/Scripturi","",getwd())
# pentru scriere fisier mega cu date colectate
path1 = paste0(path0, "/Date_brute/Alimente")
path1
# pentru scriere fisier de verificat
path2 = paste0(path0, "/Verificare")
path2


### functii
removeNewlineAndMultiWhitespaceFn = function(a) {
  b = gsub("(\\r|\\n|\\s)+", " ", a);
  return(b);
}

### cod

urlCarrefour <- "https://www.bringo.ro/ro/store-details/carrefour_orhideea"

sortimente <- html_session(urlCarrefour) %>% read_html() %>%
                 html_nodes("div.row>div.col-lg-3>a.box-inner") %>% 
                 html_attr("href") 

bringo = "https://www.bringo.ro"

sortimente = paste0(bringo, sortimente)
sortimente = c("https://www.bringo.ro/ro/store/carrefour_orhideea/act-for-food",                   
 "https://www.bringo.ro/ro/store/carrefour_orhideea/sanatate-protectie" ,                                  
 "https://www.bringo.ro/ro/store/carrefour_orhideea/legume-si-fructe-4" ,            
 "https://www.bringo.ro/ro/store/carrefour_orhideea/lactate-branzeturi-si-oua-2",    
 "https://www.bringo.ro/ro/store/carrefour_orhideea/mezeluri-7" ,                    
 "https://www.bringo.ro/ro/store/carrefour_orhideea/macelarie-si-peste" ,            
 "https://www.bringo.ro/ro/store/carrefour_orhideea/produse-bio-4" ,                 
 "https://www.bringo.ro/ro/store/carrefour_orhideea/marci-carrefour"  ,              
 "https://www.bringo.ro/ro/store/carrefour_orhideea/produse-congelate" ,                                     
 "https://www.bringo.ro/ro/store/carrefour_orhideea/bacanie-1" ,                     
 "https://www.bringo.ro/ro/store/carrefour_orhideea/brutarie-patiserie"  ,           
 "https://www.bringo.ro/ro/store/carrefour_orhideea/sarate-1" ,                      
 "https://www.bringo.ro/ro/store/carrefour_orhideea/dulciuri" ,                      
 "https://www.bringo.ro/ro/store/carrefour_orhideea/mancare-gatita-3" ,              
 "https://www.bringo.ro/ro/store/carrefour_orhideea/diete-speciale-2",
 "https://www.bringo.ro/ro/store/carrefour_orhideea/curatenie-si-intretinere" ,      
 "https://www.bringo.ro/ro/store/carrefour_orhideea/igiena-si-sanatate-7",           
 "https://www.bringo.ro/ro/store/carrefour_orhideea/cosmetice-dama",                 
 "https://www.bringo.ro/ro/store/carrefour_orhideea/cosmetice-barbati-2")  


## subsortimente
link_subsort = c()
for (i in 1:length(sortimente)) {
  subsort = html_session(sortimente[i]) %>% read_html() %>%
            html_nodes("div.bringo-product-listing-category-menu>a") %>% 
            html_attr("href") 
  subsort = subsort[-1]
  link_subsort = c(link_subsort, subsort)
}
link_subsort = paste0(bringo, link_subsort)


## pagini subsortimente
link_subsort_pages = c()
for (i in 1:length(link_subsort)) {
  pageCount = html_session(link_subsort[i]) %>% read_html() %>%
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
  productURL = html_session(pageURL) %>% read_html() %>%
    html_nodes("div.top-product-listing-box>a.bringo-product-name") %>% 
    html_attr("href")
  
  return(productURL);
}

plan(multisession)
link_prod = do.call(c, future_lapply(link_subsort_pages, getBringoProductLinks))
link_prod = paste0(bringo, link_prod)

## preluare detalii produse
t1 = Sys.time()

getBringoProducts = function(productURL) {
  print(paste("Produs: ", productURL))
  
  pageHTML = html_session(productURL) %>% read_html()
  
  nume = pageHTML %>% html_nodes("div.bringo-product-details .product-name") %>% 
        html_text() %>% removeNewlineAndMultiWhitespaceFn()
  pret = pageHTML %>% html_nodes("div.bringo-product-details .product-price") %>% 
        html_text() %>% removeNewlineAndMultiWhitespaceFn() %>%
        gsub(pattern = " RON", replacement = "") %>% gsub(pattern = ",", replacement = ".")
  descriere = pageHTML %>% html_nodes("#productDetailsTabContent") %>%
        html_text() %>% removeNewlineAndMultiWhitespaceFn()
  cat_info = pageHTML %>% html_nodes("div.row>div.container>a.section") %>%
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
  
  informatiiProdus = data.frame(ID = id, nume = nume, pret = pret, link_prod = productURL, data = Sys.Date(),
                                descriere = descriere, categorie = categorie, subcategorie = subcategorie)
  return(informatiiProdus)
}

plan(multisession)
df_produse = do.call(rbind, future_lapply(link_prod, getBringoProducts))

t2 = t1 - Sys.time()
# ~30 minute
t2 
Sys.time()


setwd(path1)
write.csv(df_produse, paste0("carrefourbringo_", Sys.Date(), ".csv"))

##### Verificare ####################################################

df = df_produse
df1 = df_produse

#remove double white spaces
df$ID = trimws(df$ID)
df$nume = trimws(df$nume)
df = unique(df)

setwd(path2)
carr_v = read.xlsx("carrefour.xlsx") #101 prod
carr_v$ID = trimws(carr_v$ID)
carr_v$nume1_carrefour = trimws(carr_v$nume1_carrefour)
carr_v$nume2_bringo = trimws(carr_v$nume2_bringo)


# join dupa:id, nume1, nume2
carr_v = left_join(carr_v, df[,c("ID", "pret")], by = c("ID" = "ID"))
carr_v = left_join(carr_v, df[,c("ID", "nume", "pret")], by = c("ID" = "ID", "nume1"="nume"))
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



