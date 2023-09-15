library(shiny)
library(ggplot2)
library(plotly)
library(shinyWidgets)


# Define UI for app that draws a scatter plot----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Scatter Plot of Iris Data"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select box for the X variable ----
      selectInput(inputId = "x_var",
                  label = "X variable:",
                  choices = names(iris),
                  selected = "Sepal.Length"),
      
      # Input: Select box for the Y variable ----
      selectInput(inputId = "y_var",
                  label = "Y variable:",
                  choices = names(iris),
                  selected = "Sepal.Width"),
      
      checkboxGroupInput(inputId = "species",
                         label = "Select species to display:",
                         choices = c("setosa", "versicolor", "virginica"),
                         selected = c("setosa")),
      
      
      selectInput(inputId = "setosa_color",
                  label = "Color for Setosa:",
                  choices = c("Red", "Green", "Blue", "Black", "Orange", "Pink"),
                  selected = "Red"),
      
      selectInput(inputId = "versicolor_color",
                  label = "Color for Versicolor:",
                  choices = c("Red", "Green", "Blue", "Black", "Orange", "Pink"),
                  selected = "Green"),
      
      selectInput(inputId = "virginica_color",
                  label = "Color for Virginica:",
                  choices = c("Red", "Green", "Blue", "Black", "Orange", "Pink"),
                  selected = "Blue"),
      
      
      # Input: Slider for the size of the scatter plot points ----
      sliderInput(inputId = "setosa_size",
                  label = "Size of setosa plot points:",
                  min = 1,
                  max = 10,
                  value = 5),
      
      # Input: Select box for the shape of the scatter plot points ----
      pickerInput(inputId = "setosa_shape",
                  label = "Shape of setosa plot points:",
                  choices = list(
                    "Circle" = 19,
                    "Square" = 15,
                    "Triangle" = 17,
                    "Hexagon" = 23
                  ),
                  selected = 19),
      
      # Input: Slider for the size of the scatter plot points ----
      sliderInput(inputId = "versicolor_size",
                  label = "Size of versicolor plot points:",
                  min = 1,
                  max = 10,
                  value = 5),
      
      # Input: Select box for the shape of the scatter plot points ----
      pickerInput(inputId = "versicolor_shape",
                  label = "Shape of versicolor plot points:",
                  choices = list(
                    "Circle" = 19,
                    "Square" = 15,
                    "Triangle" = 17,
                    "Hexagon" = 23
                  ),
                  selected = 19),
      
      # Input: Slider for the size of the scatter plot points ----
      sliderInput(inputId = "virginica_size",
                  label = "Size of virginica plot points:",
                  min = 1,
                  max = 10,
                  value = 5),
      
      # Input: Select box for the shape of the scatter plot points ----
      pickerInput(inputId = "virginica_shape",
                  label = "Shape of virginica plot points:",
                  choices = list(
                    "Circle" = 19,
                    "Square" = 15,
                    "Triangle" = 17,
                    "Hexagon" = 23
                  ),
                  selected = 19)
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Scatter plot ----
      plotlyOutput(outputId = "scatterPlot", width = "100%", height = "500px")
    )
  )
)

# Define server logic required to draw a scatter plot----
server <- function(input, output) {
  
  
  output$scatterPlot <- renderPlotly({
    
    # Filter the data by the selected species
    filtered_iris <- iris[iris$Species %in% input$species, ]
    
    ggplot(data = filtered_iris, aes(x = !!as.symbol(input$x_var), y = !!as.symbol(input$y_var))) + 
      geom_point(data = filtered_iris[filtered_iris$Species == "setosa", ], size = input$setosa_size, shape = as.integer(input$setosa_shape), color = input$setosa_color) +
      geom_point(data = filtered_iris[filtered_iris$Species == "versicolor", ], size = input$versicolor_size, shape = as.integer(input$versicolor_shape), color = input$versicolor_color) +
      geom_point(data = filtered_iris[filtered_iris$Species == "virginica", ], size = input$virginica_size, shape = as.integer(input$virginica_shape), color = input$virginica_color) +
      xlab(input$x_var) + 
      ylab(input$y_var) + 
      ggtitle("Scatter Plot of Iris Data")
    
  })
  
  
  
  
}


# Run the app ----
shinyApp(ui = ui, server = server)
