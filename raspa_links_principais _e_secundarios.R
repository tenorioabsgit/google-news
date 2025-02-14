library(rvest)
library(dplyr)
library(stringr)
library(lubridate)
library(purrr)

# Função para realizar o web scraping de uma página do Google News e extrair links de notícias
scrape_google_news_page <- function(search_term, page_num) {
  base_url <- "https://news.google.com/search?q="
  date_filter <- "&hl=pt-BR&gl=BR&ceid=BR%3Apt-419&tbs=cdr%3A1%2Ccd_min%3A1%2F1%2F2018"
  url <- paste0(base_url, URLencode(search_term), date_filter, "&start=", (page_num - 1) * 10)
  
  page <- read_html(url)
  
  # Extrair links e datas das notícias
  links <- page %>% html_elements("a.WwrzSb") %>% html_attr("href")
  dates <- page %>% html_elements("time") %>% html_attr("datetime")
  
  # Ajustar links incompletos
  links <- ifelse(startsWith(links, "./"), paste0("https://news.google.com", substring(links, 2)), links)
  
  # Filtrar links com datas a partir de 01/01/2018
  filtered_links <- links[ymd_hms(dates) >= ymd("2018-01-01")]
  
  return(filtered_links)
}

# Função principal para realizar o web scraping de vários termos
scrape_google_news <- function(search_terms, num_pages = 1) {
  all_links <- c()
  
  for (term in search_terms) {
    for (page in 1:num_pages) {
      links <- scrape_google_news_page(term, page)
      all_links <- c(all_links, links)
    }
  }
  
  return(all_links)
}

# Função para extrair links de uma página de notícias
extract_links_from_news_page <- function(url) {
  tryCatch({
    page <- read_html(url)
    links <- page %>% 
      html_elements("a") %>% 
      html_attr("href") %>% 
      .[grepl("^https://", .)] %>%  # Filtrar links que começam com "https://"
      .[!grepl("google", .)] %>%    # Excluir links que contêm "google"
      unique()                      # Remover duplicados
    return(links)
  }, error = function(e) {
    message(paste("Erro ao acessar:", url))
    return(NULL)
  })
}

# Função para capturar links adicionais de cada resultado do Google News
scrape_additional_links <- function(google_news_results) {
  results_df <- data.frame(
    google_news_link = google_news_results,
    additional_links = character(length(google_news_results)),
    stringsAsFactors = FALSE
  )
  
  results_df$additional_links <- map(results_df$google_news_link, function(link) {
    additional_links <- extract_links_from_news_page(link)
    if (is.null(additional_links)) return(NA)
    paste(additional_links, collapse = ", ")
  })
  
  return(results_df)
}

# Exemplo de uso
termos_de_busca <- c("inteligência artificial", "mudanças climáticas")
resultados_google_news <- scrape_google_news(termos_de_busca, num_pages = 2)
resultados_completos <- scrape_additional_links(resultados_google_news)
print(resultados_completos)
