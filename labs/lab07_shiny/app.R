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
mad_men_tidy <- mad_men %>% 
  mutate(performer = iconv(mad_men$performer,"UTF-8", "UTF-8", sub = "")) %>% 
  select(performer, show, show_start, charend, score) %>%
  pivot_longer(-performer & -show & -score, names_to = "Start/End", values_to = "year") %>%  
  arrange(performer)

#set up input
name_choices <- unique(mad_men_tidy$performer)

############
#    ui    #
############
ui <- fluidPage(
  title = "Mad Men Performers After the Show",
  
  sidebarLayout(
    selectizeInput(inputId = "id_name",
                   label = "Identify performer(s) to be shown:",
                   choices = name_choices,
                   selected = NULL,
                   multiple = TRUE)
  ,
  
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
  
  output$timeline <- renderPlot({
    name_data() %>%
      ggplot(aes(y = performer,x = year)) +
      geom_point() +
      geom_line(aes(color = show, size = score))
  })
  
}

shinyApp(ui = ui, server = server)


