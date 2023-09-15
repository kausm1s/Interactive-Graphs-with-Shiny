library(shiny)
library(ggplot2)
library(shinyWidgets)
library(plotly)

# Define UI for app that draws histograms of iris features ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Histogram of Iris Features"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins for Sepal Length histogram ----
      sliderInput(inputId = "bins1",
                  label = "Number of bins for Sepal Length:",
                  min = 5,
                  max = 50,
                  value = 30),
      
      pickerInput(inputId = "sepalLengthColor",
                  label = "Sepal Length Color",
                  choices = list(
                    "select colour" = c("black", "red", "green", "blue","orange")
                  ),
                  selected = "#000000"),
      
      # Input: Slider for the number of bins for Sepal Width histogram ----
      sliderInput(inputId = "bins2",
                  label = "Number of bins for Sepal Width:",
                  min = 5,
                  max = 50,
                  value = 30),
      
      pickerInput(inputId = "sepalWidthColor",
                  label = "Sepal Width Color",
                  choices = list(
                    "select colour" = c("black", "red", "green", "blue","orange")
                  ),
                  selected = "#000000"),
      
      # Input: Slider for the number of bins for Petal Length histogram ----
      sliderInput(inputId = "bins3",
                  label = "Number of bins for Petal Length:",
                  min = 5,
                  max = 50,
                  value = 30),
      
      pickerInput(inputId = "petalLengthColor",
                  label = "Petal Length Color",
                  choices = list(
                    "select colour" = c("black", "red", "green", "blue","orange")
                  ),
                  selected = "#000000"),
      
      # Input: Slider for the number of bins for Petal Width histogram ----
      sliderInput(inputId = "bins4",
                  label = "Number of bins for Petal Width:",
                  min = 5,
                  max = 50,
                  value = 30),
      
      pickerInput(inputId = "petalWidthColor",
                  label = "Petal Width Color",
                  choices = list(
                    "select colour" = c("black", "red", "green", "blue","orange")
                  ),
                  selected = "#000000")
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histograms ----
      plotlyOutput("sepalLengthPlot", width = "100%", height = "500px"),
      plotlyOutput("sepalWidthPlot", width = "100%", height = "500px"),
      plotlyOutput("petalLengthPlot", width = "100%", height = "500px"),
      plotlyOutput("petalWidthPlot", width = "100%", height = "500px")
      
    )
  )
)

# Define server logic required to draw histograms of iris features ----
server <- function(input, output) {
  
  # Histogram of Sepal Length data ----
  # with requested number of bins
  output$sepalLengthPlot <- renderPlotly({
    
    x    <- iris$Sepal.Length
    bins <- seq(min(x), max(x), length.out = input$bins1 + 1)
    
    p <- ggplot(data = iris, aes(x = Sepal.Length)) + 
      geom_histogram(breaks = bins, fill = input$sepalLengthColor ,color="orange") +
      xlab("Sepal Length") + 
      ylab("Count") +
      ggtitle("Histogram of Sepal Length")
    
    
    ggplotly(p, config = list(scrollZoom = TRUE, displayModeBar = TRUE))
    
  })
  
  
  
  # Histogram of Sepal Width data ----
  # with requested number of bins
  output$sepalWidthPlot <- renderPlotly({
    
    x    <- iris$Sepal.Width
    bins <- seq(min(x), max(x), length.out = input$bins2 + 1)
    
    p <- ggplot(data = iris, aes(x = Sepal.Width)) + 
      geom_histogram(breaks = bins, fill = input$sepalWidthColor ,color="orange") +
      xlab("Sepal Width") + 
      ylab("Count") +
      ggtitle("Histogram of Sepal Width")
    
    ggplotly(p, config = list(scrollZoom = TRUE, displayModeBar = TRUE))
    
    
  })
  
  # Histogram of Petal Length data ----
  # with requested number of bins
  output$petalLengthPlot <- renderPlotly({
    
    x    <- iris$Petal.Length
    bins <- seq(min(x), max(x), length.out = input$bins3 + 1)
    
    p <- ggplot(data = iris, aes(x = Petal.Length)) + 
      geom_histogram(breaks = bins, fill = input$petalLengthColor ,color="orange") +
      xlab("Petal Length") + 
      ylab("Count") +
      ggtitle("Histogram of Petal Length")
    
    ggplotly(p, config = list(scrollZoom = TRUE, displayModeBar = TRUE))
    
  })
  
  # Histogram of Petal Width data ----
  # with requested number of bins
  output$petalWidthPlot <- renderPlotly({
    
    x    <- iris$Petal.Width
    bins <- seq(min(x), max(x), length.out = input$bins4 + 1)
    
    p <- ggplot(data = iris, aes(x = Petal.Width)) + 
      geom_histogram(breaks = bins, fill = input$petalWidthColor ,color="orange") +
      xlab("Petal Width") + 
      ylab("Count") +
      ggtitle("Histogram of Petal Width")
    
    ggplotly(p, config = list(scrollZoom = TRUE, displayModeBar = TRUE))
    
  })
}

shinyApp(ui = ui, server = server)

