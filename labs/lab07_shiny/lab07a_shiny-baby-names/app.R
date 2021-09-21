# Lab 7a: Shiny app (baby names)

# Load necessary packages
library(shiny)
library(tidyverse)
library(babynames) # for dataset

# Run to see all baby names from US Social Security Agency
# used at least 5 times from 1880 to 2017
# babynames %>%
#   select(name) %>% 
#   distinct() %>% 
#   arrange(name)

# Too many names for app; limit to names in this class
name_choices <- c("Brittney",
                  # Section 1
                  "Dhyey", "Gillian", "Hannah", "Helen",
                  "Henry", "Jason", "Kayla", "Lucas", 
                  "Priya", "Rohil", "Sara", "Tracy", 
                  "Vietta", "Ziji",
                  # Section 2
                  "Abbey", "Alexander", "Angela", "Angelica", 
                  "Becca", "Ben", "Caitlin", "Caroline", 
                  "Dan", "Dasha", "David", "Deven", 
                  "Diego", "Elizabeth", "Kalidas", "Karla", 
                  "Kauila", "Kedar", "Louis", "Luis", 
                  "Mahathi", "Rafael", "Skye", "Tyler")

# Define UI for app that creates a line plot for a given name
ui <- fluidPage(
  
  # Application title
  titlePanel("This is the title panel"),
  
  # Sidebar with a dropdown name input and radio button to choose
  # corresponding sex assigned at birth
  sidebarLayout(
    
    sidebarPanel(
      
      # Select Name
      selectInput(inputId = "nm",
                  label = "Name:",
                  choices = name_choices,
                  selected = "Brittney"),
      
      # Choose Sex
      radioButtons(inputId = "sx",
                   label = "Sex:",
                   choices = c("M", "F"),
                   selected = "F")
    ),
    
    # Show change in name use over time
    mainPanel(plotOutput("lineplot"))
  )
)

# Define server logic required to draw a lineplot
server <- function(input, output) {
  
  output$lineplot <- renderPlot({
    
    dat <- babynames %>%
      filter(name %in% input$nm, sex == input$sx) %>%
      group_by(name, year) %>%
      summarize(total = sum(n))
    
    ggplot(data = dat, aes(x = year, y = total)) +
      geom_line(color = "#0095b6", size = 2) +
      lims(x = c(1880, 2020)) +
      labs(x = "Year", 
           y = "Total number of births with this name",
           title = paste("Babies Named", input$nm)) + 
      theme_bw()
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
