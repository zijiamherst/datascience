# This code reproduces only the scatterplot panel from the electric skateboards app

# Load necessary packages
library(shiny)
library(tidyverse)
library(ggrepel)

# Import data
skateboards <- read_csv("electric_skateboards.txt")

#############################################################
# Define choice values and labels for widgets (user inputs) #
# - Define vectors for choice values and labels             #
# - Can then refer to them in server                        #
#############################################################

# For TAB 2 SCATTERPLOT widgets:

## For radio button
size_choice_values <- c("price", "weight", "battery")
size_choice_names <- c("Price", "Weight", "Battery")
names(size_choice_values) <- size_choice_names

## For selectizeInput choices for skateboard name, pull directly from data
name_choices <- unique(skateboards$board)


############
#    ui    #
############
ui <- navbarPage(
  
  title = "Electric Skateboards",
  
  # Tab 2: Scatterplot
  tabPanel(
    title = "Scatterplot",
    
    sidebarLayout(
      
      sidebarPanel(
        radioButtons(inputId = "pt_size",
                     label = "Size points by:",
                     choices = size_choice_values,
                     selected = "weight"),
        
        selectizeInput(inputId = "id_name",
                       label = "Identify skateboard(s) in the scatterplot:",
                       choices = name_choices,
                       selected = NULL,
                       multiple = TRUE)
      ),
      
      mainPanel(plotOutput(outputId = "scatter"))
    )
  )
  
)

############
# server   #
############
server <- function(input, output){
  
  # TAB 2: INTERACTIVE SCATTERPLOT 
  output$scatter <- renderPlot({
    skateboards %>%
      filter(drive != "Direct") %>%
      ggplot(aes_string(x = "range", y = "top_speed", size = input$pt_size)) +
      geom_point(color = "#2c7fb8") +
      labs(title = "Electric Skateboards", 
           subtitle = "August 2018",
           x = "Range (miles)", 
           y = "Top Speed (mph)",
           size = size_choice_names[size_choice_values == input$pt_size]) +
      geom_label_repel(data = filter(skateboards, board %in% input$id_name),
                       aes(label = board), show.legend = FALSE) +
      facet_grid(~drive) 
    
  })
}

####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)