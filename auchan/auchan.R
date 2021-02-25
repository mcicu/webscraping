
library(httr)
library(rvest)
library(jsonlite)
library(openxlsx)
library(dplyr)
library(future)
library(future.apply)


##### configuratie #####

# penntru INS
# set_config(use_proxy("http://proxy.insro.local", 8080))

plan(multisession)

##### constante ######

path0 = gsub("/Scripturi","",getwd())
# pentru scriere fisier mega cu date colectate
path1 = paste0(path0, "/Date_brute/Alimente")
# pentru scriere fisier de verificat
path2 = paste0(path0, "/Verificare")

AUCHAN_URL = "https://www.auchan.ro"
AUCHAN_STORE_PREFIX = "https://www.auchan.ro/store"

##### functii #####

get_subcategories = function(category_link, category_name) {
  subcategory_html_nodes = html_session(category_link) %>% read_html() %>% html_nodes(css = "ul.category-path-selected>li>span>a.category-path-not-selected")
  subcategory_links = subcategory_html_nodes %>% html_attr("href") %>% 
                          gsub(pattern = "\\?.*", replacement = "") %>%
                          { ifelse(nchar(.) > 0, paste0(AUCHAN_URL, .), NA) }
                        
  subcategory_names <- subcategory_html_nodes %>% html_text()
  
  if (length(subcategory_links) == 0)
    return(NULL)
  
  output = data.frame(
    subcategory_link = subcategory_links, 
    subcategory_name = subcategory_names,
    category_name = category_name,
    stringsAsFactors = FALSE)
  
  return(output)
}

get_page_count <- function(link) {
  last_page_link = html_session(link) %>% read_html() %>% html_node(css = "div.pagination-container>ul.pagination>li.last>a.link") %>% html_attr("href")
  page_grep = regexpr(pattern = "page=[0-9]+", text = last_page_link)
  last_page_number = substr(x = last_page_link, start = page_grep + 5, stop = page_grep + attr(page_grep, "match.length") - 1)
  
  if (is.na(last_page_number)) 
    return(0)
  return(last_page_number)
}

get_subcategory_pages = function(subcategory_link, subcategory_name, category_name, page_count) {
  page_count = as.integer(page_count)
  output = data.frame(
    subcategory_page_link = paste0(subcategory_link, "?page=", 0:page_count),
    subcategory_name = subcategory_name,
    category_name = category_name,
    stringsAsFactors = FALSE
  )
  
  return(output);
}

get_products = function(page_link, category_name, subcategory_name) {
  
  css_product_code = "div.row.productGrid form.list_add_to_cart_form input[name=\"productCodePost\"]"
  css_product_name = "div.row.productGrid form.list_add_to_cart_form input[name=\"productNamePost\"]"
  css_product_price = "div.row.productGrid form.list_add_to_cart_form input[name=\"productPostPrice\"]"
  css_product_url = "div.row.productGrid form.list_add_to_cart_form input[name=\"productPostUrl\"]"
  
  auchan_page = html_session(page_link)
  while(auchan_page$response$status_code != 200) {
    Sys.sleep(1)
    auchan_page = html_session(page_link)
  }
  
  html_page = auchan_page %>% read_html()
  product_codes = html_page %>% html_nodes(css = css_product_code) %>% html_attr("value")
  product_names = html_page %>% html_nodes(css = css_product_name) %>% html_attr("value")
  product_prices = html_page %>% html_nodes(css = css_product_price) %>% html_attr("value")
  product_urls = html_page %>% html_nodes(css = css_product_url) %>% html_attr("value")
  
  if (length(product_names) == 0) {
    return(NULL)
  }
  
  print(product_names)
  
  output = data.frame(
    id = product_codes,
    nume = product_names,
    pret = product_prices,
    link = paste0(AUCHAN_STORE_PREFIX, product_urls),
    categorie = category_name,
    subcateg = subcategory_name,
    data = Sys.Date(),
    magazin = "auchan",
    stringsAsFactors = FALSE
  )
  
  return(output)
}

##### executie #####

start_time = Sys.time()

category_html_nodes = html_session(AUCHAN_URL) %>% read_html() %>% html_nodes(css = "div.category-menu>li>span>a")
category_links <- category_html_nodes %>% html_attr("href") %>% { paste0(AUCHAN_URL, .) } %>% as.character()
category_names <- category_html_nodes %>% html_text() %>% as.character()
categories <- data.frame(category_name = category_names, category_link = category_links, stringsAsFactors = FALSE)

subcategories = do.call(rbind, future_lapply(1:nrow(categories), function(x) {get_subcategories(categories$category_link[x], categories$category_name[x])}))

subcategories$page_count = 0
subcategories$page_count = future_lapply(subcategories$subcategory_link, get_page_count)

auchan_pages = do.call(rbind, lapply(1:nrow(subcategories), function(x) {
  get_subcategory_pages(subcategories$subcategory_link[x], subcategories$subcategory_name[x], subcategories$category_name[x], subcategories$page_count[x])
  }))


auchan_products = do.call(rbind, future_lapply(1:nrow(auchan_pages), function(x) {
  get_products(auchan_pages$subcategory_page_link[x], auchan_pages$category_name[x], auchan_pages$subcategory_name[x]) 
  }))

end_time = Sys.time()

#Timp de rulare
end_time - start_time


setwd(path1)
write.csv(auchan_products, paste0("auchan_", Sys.Date(), ".csv"))


##### Verificare ####################################################

#remove double white spaces

setwd(path2)
auchan_v = read.xlsx("auchan.xlsx") #169 prod
auchan_v$ID = trimws(auchan_v$ID)
auchan_v$nume1 = trimws(auchan_v$nume1)
auchan_v$nume2 = trimws(auchan_v$nume2)


# join dupa:id, nume1, nume2
auchan_v = left_join(auchan_v, auchan[,c("ID", "pret")], by = c("ID" = "ID"))
sum(is.na(auchan_v$pret))
nrow(auchan_v)

names(auchan_v)[ncol(auchan_v)] = paste0(substr(Sys.Date(), 9,10),"_", months(Sys.Date()))
auchan_v$sp = ""
names(auchan_v)[ncol(auchan_v)] = paste0("sp_",substr(Sys.Date(), 9,10),"_", months(Sys.Date()))

setwd(path2)
write.xlsx(auchan_v, "auchan.xlsx")
#write.xlsx(mega_v, paste0("mega_v_", Sys.Date(),".xlsx" ))

