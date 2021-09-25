library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(ggrepel)
library(dplyr)
library(tidyr)
library(stringr)
library(fivethirtyeight)

#import data
mad_men_lead <- mad_men %>%
  mutate(performer = iconv(mad_men$performer,"UTF-8", "UTF-8", sub = "")) %>% 
  select(performer, lead_notes) %>%
  mutate(lead_notes = strsplit(x = lead_notes, split = "; ")) %>%
  unnest(lead_notes) %>%
  mutate(lead_notes = strsplit(x = lead_notes, split = ", ")) %>%
  unnest_wider(lead_notes)  %>%
  rename(film = 2, year = 3) %>%
  mutate(year = as.integer(year)) %>%
  select(performer, film, year)  %>%  
  arrange(performer)

mad_men_support <- mad_men %>%
  mutate(performer = iconv(mad_men$performer,"UTF-8", "UTF-8", sub = "")) %>% 
  select(performer, support_notes) %>%
  mutate(support_notes = strsplit(x = support_notes, split = "; ")) %>%
  unnest(support_notes) %>%
  mutate(support_notes = strsplit(x = support_notes, split = ", ")) %>%
  unnest_wider(support_notes)  %>%
  rename(film = 2, year = 3) %>%
  mutate(year = as.integer(year)) %>%
  select(performer, film, year) %>%  
  arrange(performer)

mad_men_tidy <- mad_men %>% 
  mutate(performer = iconv(mad_men$performer,"UTF-8", "UTF-8", sub = "")) %>% 
  select(performer, show, show_start, charend, score) %>%
  pivot_longer(-performer & -show & -score, names_to = "Start/End", values_to = "year") %>%  
  arrange(performer)

#set up input
name_choices <- unique(mad_men_tidy$performer)

options_choice_names <- c("Supporting role in movies","Leading role in movies","Show roles with appearance in at least half the episodes")
options_choice_values <- c("lead_notes", "support_notes", "show")
names(options_choice_values) <- options_choice_names

############
#    ui    #
############
ui <- fluidPage(
  
  titlePanel("Mad Men Performers After the Show"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectizeInput(inputId = "id_name",
                     label = "Identify performer(s) to be shown:",
                     choices = name_choices,
                     selected = NULL,
                     multiple = TRUE),
      
      checkboxGroupInput(inputId = "options_name",
                   label = "Type of Appearance", 
                   choices = options_choice_names, 
                   selected = NULL)
      ),
  
  mainPanel(plotOutput("timeline"))
  
  )
)

############
# server   #
############
server <- function(input, output){
  
  name_data <- reactive({
    data <- mad_men_tidy %>%
      filter(performer %in% input$id_name)
  })
  
  lead_data <- reactive({
    data <- mad_men_lead %>%
      filter(performer %in% input$id_name)
  })
  support_data <- reactive({
    data <- mad_men_support %>%
      filter(performer %in% input$id_name)
  })
  
  plot_show <- reactive({
    show %in% input$options_name
  })
  

    output$timeline <- renderPlot({
      name_data() %>%
        ggplot() +
        geom_point(aes(y = performer,x = year)) +
        geom_line(aes(size = score, color = show,y = performer,x = year)) +
        geom_point(data = lead_data(),aes(y = performer,x = year, color = film)) + 
        geom_point(data = support_data(),aes(y = performer,x = year, color = film))
    })
  
  
}

shinyApp(ui = ui, server = server)


