library(tidyverse)
library(kableExtra)
library(robotstxt)
library(rvest)
library(purrr)

url <- "https://en.wikipedia.org/wiki/List_of_justices_of_the_Supreme_Court_of_the_United_States"

justice_data <- url %>%
  httr::GET(config = httr::config(ssl_verifypeer = FALSE)) %>% 
  read_html() %>%
  html_nodes("table") %>%
  html_table() %>%
  purrr::pluck(2) %>%
  janitor::clean_names() %>%
  write_csv("problem_sets/ps5/data/justices.csv")

