library(RSelenium)
library(rvest)
library(tibble)

# Iniciar o servidor Selenium
rD <- rsDriver(browser = "chrome", port = 4545L, verbose = FALSE)
remDr <- rD$client

# Função para extrair notícias do Google News usando Selenium
extrair_noticias_google_news <- function(termo_busca, pagina = 1) {
  
  # Construir a URL da busca
  url <- paste0("https://www.google.com/search?q=", URLencode(termo_busca), "&tbm=nws&start=", (pagina - 1) * 10)
  
  # Navegar para a página
  remDr$navigate(url)
  
  # Esperar o carregamento da página
  Sys.sleep(5)
  
  # Obter o HTML da página renderizada
  pagina_html <- read_html(remDr$getPageSource()[[1]])
  
  # Extrair os títulos das notícias
  titulos <- pagina_html %>%
    html_nodes("h3") %>%
    html_text()
  
  # Extrair os links das notícias
  links <- pagina_html %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    grep("^/url\\?q=", ., value = TRUE) %>%
    gsub("/url\\?q=", "", .) %>%
    gsub("&sa=.*", "", .)
  
  # Extrair as fontes das notícias
  fontes <- pagina_html %>%
    html_nodes(".wEwyrc.AVN2gc.uQIVzc.Sksgp") %>%
    html_text()
  
  # Extrair os trechos das notícias
  trechos <- pagina_html %>%
    html_nodes(".Y3v8qd") %>%
    html_text()
  
  # Criar um dataframe com os dados extraídos
  noticias_df <- tibble(
    Titulo = titulos,
    Link = if(length(links) > 0) links[1:length(titulos)] else NA,
    Fonte = if(length(fontes) > 0) fontes[1:length(titulos)] else NA,
    Trecho = if(length(trechos) > 0) trechos[1:length(titulos)] else NA
  )
  
  return(noticias_df)
}

# Exemplo de uso
resultado <- extrair_noticias_google_news("tecnologia", pagina = 1)

# Visualizar os resultados
print(resultado)

# Fechar a sessão e parar o servidor
remDr$close()
rD$server$stop()
