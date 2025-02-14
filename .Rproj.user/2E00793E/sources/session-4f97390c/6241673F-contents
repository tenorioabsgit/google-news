news <- function(term) {
  url <- paste0("https://news.google.com/search?q=", term, "&hl=es-419&gl=US&ceid=US:es-419")
  nodeset <- read_html(url) %>% html_nodes("article")
  dplyr::bind_rows(lapply(nodeset, function(x) tibble::tibble(
    Title = x %>% html_node(".ipQwMb.ekueJc.RD0gLb") %>% html_text(), 
    Link = x %>% html_node(".ipQwMb.ekueJc.RD0gLb > a") %>% html_attr("href") %>% xml2::url_absolute(url), 
    Description = x %>% html_node("div.Da10Tb.Rai5ob > span") %>% html_text(), 
    Source = x %>% html_node("div.QmrVtf.RD0gLb.kybdz > div > a") %>% html_text(), 
    Time = x %>% html_node("div.QmrVtf.RD0gLb.kybdz > div > time") %>% html_attr("datetime")
  )))
}

term <- news("coronavirus")



## função em portugues brasil
news <- function(term) {
  url <- paste0("https://news.google.com/search?q=", term, "%20when%3A1y&hl=pt-BR&gl=BR&ceid=BR%3Apt-419")
  nodeset <- read_html(url) %>% html_nodes("article")
  dplyr::bind_rows(lapply(nodeset, function(x) tibble::tibble(
    Title = x %>% html_node(".ipQwMb.ekueJc.RD0gLb") %>% html_text(), 
    Link = x %>% html_node(".ipQwMb.ekueJc.RD0gLb > a") %>% html_attr("href") %>% xml2::url_absolute(url), 
    Description = x %>% html_node("div.Da10Tb.Rai5ob > span") %>% html_text(), 
    Source = x %>% html_node("div.QmrVtf.RD0gLb.kybdz > div > a") %>% html_text(), 
    Time = x %>% html_node("div.QmrVtf.RD0gLb.kybdz > div > time") %>% html_attr("datetime")
  )))
}

term <- news("coronavirus")
