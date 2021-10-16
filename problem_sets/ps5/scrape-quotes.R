library(tidyverse)
library(kableExtra)
library(robotstxt)

quotes_url <- "https://www.brainyquote.com/topics/wisdom-quotes"
robotstxt::paths_allowed(quotes_url)

quotes_html <- read_html(quotes_url)

quotes <- quotes_html %>%
  html_elements(".oncl_q") %>%
  html_text()

quotes <- quotes[which(quotes != " ")]

people <- quotes_html %>%
  html_elements(".oncl_a") %>%
  html_text()

# put in data frame with two variables (person and quote)
wisdom_quotes <- tibble(person = people, quote = quotes) %>% 
  mutate(quote = str_remove_all(quote, "\n\n"),
         # Prep quotes for markdown display when `results = "asis"`
         display = paste0('> *"', as.character(quote), '"* --' , as.character(person)))

write_csv(wisdom_quotes, "problem_sets/ps5/data/quotes.csv")

