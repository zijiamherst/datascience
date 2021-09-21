# This code reproduces only the histogram panel from the electric skateboards app

# Load necessary packages
library(shiny)
library(tidyverse)

# Import data
skateboards <- read_csv("electric_skateboards.txt")

#############################################################
# Define choice values and labels for widgets (user inputs) #
# - Define vectors for choice values and labels             #
# - Can then refer to them in server                        #
#############################################################

# For TAB 1 HISTOGRAM widgets:

## For selectInput, 'choices' object should be a NAMED LIST
hist_choice_values <- c("price", "range", "top_speed", "weight", "battery")
hist_choice_names <- c("Price", "Range", "Top Speed", "Weight", "Battery")
names(hist_choice_values) <- hist_choice_names

## For checkboxGroupInput
drv_choices <-  unique(skateboards$drive)


############
#    ui    #
############
ui <- navbarPage(
  
  title = "Electric Skateboards",
  
  # Tab 1: Histogram
  tabPanel(
    title = "Histogram",
    
    sidebarLayout(
      sidebarPanel(
        
        selectInput(inputId = "histvar",
                    label = "Choose a variable of interest to plot:",
                    choices = hist_choice_values,
                    selected = "price"),
        
        checkboxGroupInput(inputId = "drv",
                           label = "Include drive types:",
                           choices = drv_choices,
                           selected = drv_choices,
                           inline = TRUE)
      ),
      
      mainPanel(plotOutput(outputId = "hist"))
    )
  )
  
)

############
# server   #
############
server <- function(input, output){
  
  # TAB 1: HISTOGRAM
  data_for_hist <- reactive({
    data <- filter(skateboards, drive %in% input$drv)
  })
  
  output$hist <- renderPlot({
    ggplot(data = data_for_hist(), aes_string(x = input$histvar)) +
      geom_histogram(color = "#2c7fb8", fill = "#7fcdbb", alpha = 0.7) +
      labs(x = hist_choice_names[hist_choice_values == input$histvar],
           y = "Number of Skateboards")
  })
}

####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)