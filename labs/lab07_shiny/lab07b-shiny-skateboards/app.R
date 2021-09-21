# Load necessary packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(ggrepel)
#library(fivethirtyeight)

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


# For TAB 2 SCATTERPLOT widgets:

## For radio button
size_choice_values <- c("price", "weight", "battery")
size_choice_names <- c("Price", "Weight", "Battery")
names(size_choice_values) <- size_choice_names

## For selectizeInput choices for skateboard name, pull directly from data
name_choices <- unique(skateboards$board)


# For TAB 3 TABLE widgets: 

## For selectizeInput choices for company name, pull directly from data
cmpy_choices <- unique(skateboards$company)

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
  ),
  
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
  ),
  
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
  ),
  
  # Tab 4: Original graph
  tabPanel(
    title = "Original Graph",
    
    sidebarLayout(
      sidebarPanel(
        tags$div(
          HTML(paste("Original figure was presented by ",
                     tags$a(href="https://www.electricskateboardhq.com/boards-comparison/", 
                            "HQ Skateboard"),
                     sep = "")
          )
        )
      ),
      
      mainPanel(h3("Information overload!"),
                plotOutput(outputId = "original")
      )
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
  
  # TAB 3: TABLE
  data_for_table <- reactive({
    data <- filter(skateboards, company %in% input$cmpy)
  })
  
  output$table <- DT::renderDataTable({ 
    data_for_table()
  })
  
  # TAB 4: RE-CREATION OF ORIGINAL FIGURE (STATIC)
  output$original <- renderPlot({
    ggplot(data = skateboards, aes(x = range, y = top_speed, 
                                   color = company, 
                                   shape = drive, 
                                   size = weight)) +
      geom_point() +
      geom_text(aes(label = board), hjust = 0, nudge_x = 0.05, size = 3) +
      labs(title = "Electric Skateboards", 
           subtitle = "August 2018",
           x = "Range (miles)", 
           y = "Top Speed (mph)",
           shape = "Drive type", 
           size = "Weight of board") +
      guides(color = FALSE)
  })
}

####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)
