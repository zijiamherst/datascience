# This code reproduces only the table panel from the electric skateboards app

# Load necessary packages
library(shiny)
library(tidyverse)
library(DT)

# Import data
skateboards <- read_csv("electric_skateboards.txt")

#############################################################
# Define choice values and labels for widgets (user inputs) #
# - Define vectors for choice values and labels             #
# - Can then refer to them in server                        #
#############################################################

# For TAB 3 TABLE widgets: 

## For selectizeInput choices for company name, pull directly from data
cmpy_choices <- unique(skateboards$company)


############
#    ui    #
############
ui <- navbarPage(
  
  title = "Electric Skateboards",
  
  # Tab 3: Table
  tabPanel(
    title = "Table",
    
    sidebarLayout(
      sidebarPanel(
        selectizeInput(inputId = "cmpy",
                       label = "Choose one or more companies:",
                       choices = cmpy_choices,
                       selected = "DIYElectric",
                       multiple = TRUE)
      ),
      
      mainPanel(DT::dataTableOutput(outputId = "table"))
    )
  )
  
)

############
# server   #
############
server <- function(input, output){
  
  # TAB 3: TABLE
  data_for_table <- reactive({
    data <- filter(skateboards, company %in% input$cmpy)
  })
  
  output$table <- DT::renderDataTable({ 
    data_for_table()
  })
  
}

####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)