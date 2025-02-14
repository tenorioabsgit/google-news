# loading the packages:
library(dplyr) # for pipes and the data_frame function
library(rvest) # webscraping
library(stringr) # to deal with strings and to clean up our data
# extracting the whole website
google <- read_html("https://news.google.com/search?q=Atila%20Iamarino%20when%3A4y&hl=pt-BR&gl=BR&ceid=BR%3Apt-419")

article_all <- google %>% html_nodes("article")

times <- article_all %>%
  html_node("time") %>%
  html_text()

vehicles <- article_all %>%
  html_nodes("a.wEwyrc.AVN2gc.uQIVzc.Sksgp") %>%
  html_text()

headlines <- article_all %>%
  html_nodes("a.DY5T1d") %>%
  html_text()
tb_news <- tibble(headlines, vehicles, times)
