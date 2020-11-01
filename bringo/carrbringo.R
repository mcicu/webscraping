library(httr)  
library(rvest) 
library(jsonlite)

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

sortimente = sortimente[1:17]
sortimente = sortimente[-c(12,13,14)]
sortimente = sortimente[-c(2,6, 7,10,11)]

bringo = "https://www.bringo.ro"

sortimente = paste0(bringo, sortimente)

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
link_prod = c()
for (i in 1:length(link_subsort_pages)) {
  prod = html_session(link_subsort_pages[i]) %>% read_html() %>%
              html_nodes("div.top-product-listing-box>a.bringo-product-name") %>% 
              html_attr("href")
  link_prod = c(link_prod, prod)
}
link_prod = paste0(bringo, link_prod)

## preluare detalii produse
nume = c()
pret = c()
descriere = c()
for (i in 1:length(link_prod)) {
  print(c(i, link_prod[i]))
  n = html_session(link_prod[i]) %>% read_html() %>%
        html_nodes("div.bringo-product-details .product-name") %>% 
        html_text() 
  p = html_session(link_prod[i]) %>% read_html() %>%
        html_nodes("div.bringo-product-details .product-price") %>% 
        html_text() 
  
  d = html_session(link_prod[i]) %>% read_html() %>%
        html_nodes("#productDetailsTabContent") %>%
        html_text()
  
  if (length(n) == 0)
    n = NA
  if (length(p) == 0)
    p = NA
  if (length(d) == 0)
    d = NA
  
  nume = c(nume, n)
  pret = c(pret, p)
  descriere = c(descriere, d)
}

nume = lapply(nume, removeNewlineAndMultiWhitespaceFn)
pret = lapply(pret, removeNewlineAndMultiWhitespaceFn)
descriere = lapply(descriere, removeNewlineAndMultiWhitespaceFn)
nume = as.character(nume)
pret = as.character(pret)
descriere = as.character(descriere)
df = data.frame(nume = nume, pret = pret, link_prod = link_prod, data=Sys.Date(), descriere=descriere)

write.csv(df, paste0("carrbringo-", Sys.Date(), ".csv"))
