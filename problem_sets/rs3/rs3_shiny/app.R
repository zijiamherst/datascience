library(shiny)

ui <- fluidPage(
  sliderInput(inputId = "num", 
              label = "Choose a number", 
              value = 25, min = 1, max = 100),
  textInput(inputId = "title",
            label = "Name for graph"),
  actionButton("go", "Enter", value = "Title of Graph"),
  navlistPanel(  
    tabPanel(
      title = "Histogram",
      plotOutput("hist")),
    tabPanel(
      title = "Table",
      verbatimTextOutput("stats")))
)

server <- function(input, output) {
  rv <- reactiveValues(
    name = "Title of Graph",
    data = rnorm(25)
  )
  observeEvent(input$go, {rv$name <- isolate(input$title)
                          rv$data <- rnorm(input$num)})
  data <- reactive({
    rnorm(rv$data)
  })
  output$hist <- renderPlot({
    hist(rnorm(data()), 
    main = rv$name)
  })
  output$stats <- renderPrint({
    summary(data())
  })
}

shinyApp(ui = ui, server = server)