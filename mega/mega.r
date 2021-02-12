library(httr)
library(jsonlite)
library(rvest)
library(openxlsx)
library(dplyr)
library(future)
library(future.apply)

##### Constante #####

path0 = gsub("/Scripturi","",getwd())
# pentru scriere fisier mega cu date colectate
path1 = paste0(path0, "/Date_brute/Alimente")
# pentru scriere fisier de verificat
path2 = paste0(path0, "/Verificare")

CATEGORII = c("001", "002", "003", "004", "005", "006", "007", "008", "009", "010", "011", "012", "013", "014", "015")

##### Functii #####

build_request_object = function(category_id, page_number) {
  request_object = list()
  request_object$operationName = "GetCategoryProductSearch"
  request_object$variables = list(lang = "ro", 
                                  searchQuery = ":product", 
                                  sort = "product", 
                                  category = category_id,
                                  pageNumber = page_number,
                                  pageSize = 100,
                                  filterFlag = TRUE)
  request_object$query = "query GetCategoryProductSearch($lang: String, $searchQuery: String, $pageSize: Int, $pageNumber: Int, $category: String, $sort: String, $filterFlag: Boolean) {\n categoryProductSearch(lang: $lang, searchQuery: $searchQuery, pageSize: $pageSize, pageNumber: $pageNumber, category: $category, sort: $sort, filterFlag: $filterFlag) {\n products {\n  manufacturerName\n  manufacturerSubBrandName\n  code\n  name\n  description\n  url\n  price {\n  discountedPriceFormatted\n  unit\n  }\n  stock {\n  inStock\n  }\n  categories {\n  code\n  name\n  }\n }\n\n pagination {\n  currentPage\n  totalResults\n  totalPages\n  sort\n  }\n }\n}"
  
  return(request_object)
}

extract_category_info = function(product_categories) {
  info = list(category_id = NA, category_name = NA, subcategory_id = NA, subcategory_name = NA)
  for (i in 1:nrow(product_categories)) {
    if (nchar(product_categories$code[[i]]) == 3) {
      info$category_id = product_categories$code[[i]];
      info$category_name = product_categories$name[[i]];
    }
    
    if (nchar(product_categories$code[[i]]) == 6) {
      info$subcategory_id = product_categories$code[[i]];
      info$subcategory_name = product_categories$name[[i]];
    }
  }
  
  return(info);
}

get_mega_image_products = function(category_id) {
  
  API_MEGA_URL = "https://api.mega-image.ro"
  PRODUCT_URL_PREFIX = "https://www.mega-image.ro"
  DEFAULT_HEADERS = c("Content-Type" = "application/json", "Accept" = "application/json")
  
  products = data.frame()
  has_more_results = TRUE
  next_page = 0
  
  while (has_more_results) {
    request_object = build_request_object(category_id = category_id, page_number = next_page)
    request_payload = toJSON(request_object, auto_unbox = TRUE)
    
    raw_response = POST(url = API_MEGA_URL, body = request_payload, add_headers(.headers = DEFAULT_HEADERS), encode = "json")
    
    if (raw_response$status_code == 200) {
      text_response = content(raw_response, as = "text")
      response_object = fromJSON(text_response)
      response_products = response_object$data$categoryProductSearch$products
      response_pagination = response_object$data$categoryProductSearch$pagination
      
      print(paste("Produse incarcate = ", length(products$ID), "; categorie =", category_id))
      
      found_products = list()
      found_products$ID = response_products$code
      found_products$nume = response_products$name
      found_products$pret = response_products$price$discountedPriceFormatted %>%
                          gsub(pattern = " Lei", replacement = "") %>% gsub(pattern = ",", replacement = ".")
      found_products$um = response_products$price$unit %>% 
                          gsub(pattern = "piece", replacement = "bucata")
      
      found_products$producator = ifelse(is.na(response_products$manufacturerSubBrandName), 
                                         response_products$manufacturerName,
                                         paste(response_products$manufacturerName, response_products$manufacturerSubBrandName))
      
      found_products$instoc = response_products$stock$inStock
      found_products$descriere = response_products$description
      found_products$url = paste0(PRODUCT_URL_PREFIX, response_products$url)
      
      category_info = lapply(response_products$categories, extract_category_info)
      found_products$id_categorie = sapply(category_info, `[[`, "category_id")
      found_products$categorie = sapply(category_info, `[[`, "category_name")
      found_products$id_subcategorie = sapply(category_info, `[[`, "subcategory_id")
      found_products$subcategorie = sapply(category_info, `[[`, "subcategory_name")
      
      found_products$data_dl = Sys.Date()
      found_products$magazin = "mega_image"
      
      next_page = response_pagination$currentPage + 1;
      has_more_results = !next_page == response_pagination$totalPages;
      
      products = bind_rows(products, as.data.frame(found_products))
    }
  }
  
  return(products);
}

##### Executie #####

t1 = Sys.time()

plan(multisession)
mega = do.call(rbind, future_lapply(CATEGORII, get_mega_image_products))

#t1 = Sys.time()
t2 = Sys.time() - t1

print("# 1. Timpul de rulare: ")
t2
print("# 2. Nr de prod: ")
nrow(mega)

setwd(path1)
write.csv(mega, paste0("mega_", Sys.Date(),".csv" ), row.names = FALSE)


##### Verificare ####################################################

#remove double white spaces
mega$ID = trimws(mega$ID)
mega$nume = trimws(mega$nume)

setwd(path2)
mega_v = read.xlsx("mega.xlsx") #113 prod
mega_v$ID = trimws(mega_v$ID)
mega_v$nume1 = trimws(mega_v$nume1)
mega_v$nume2 = trimws(mega_v$nume2)


# join dupa:id, nume1, nume2
mega_v = left_join(mega_v, mega[,c("ID", "nume", "pret")], by = c("ID" = "ID", "nume1"="nume"))
sum(is.na(mega_v$pret))
nrow(mega_v)

names(mega_v)[ncol(mega_v)] = paste0(substr(Sys.Date(), 9,10),"_", months(Sys.Date()))
mega_v$sp = ""
names(mega_v)[ncol(mega_v)] = paste0("sp_",substr(Sys.Date(), 9,10),"_", months(Sys.Date()))

setwd(path2)
write.xlsx(mega_v, "mega.xlsx")
#write.xlsx(mega_v, paste0("mega_v_", Sys.Date(),".xlsx" ))


