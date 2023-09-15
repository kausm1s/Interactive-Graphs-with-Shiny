library(shiny)
library(ggplot2)
library(plotly)

# Define UI for app that displays violin or box plots for separate species in a single plot
ui <- fluidPage(
  titlePanel("Violin or Box Plot of Iris Data"),
  sidebarLayout(
    sidebarPanel(
      
      # Input: Select box for the Y variable ----
      selectInput(inputId = "y_var",
                  label = "Y variable:",
                  choices = names(iris),
                  selected = "Sepal.Width"),
      
      selectInput(inputId = "plot_type",
                   label = "Select plot type:",
                   choices = c("Violin Plot", "Box Plot"),
                   selected = "Violin Plot")
    ),
    mainPanel(
      plotlyOutput(outputId = "iris_plot", width = "100%", height = "500px")
    )
  )
)

# Define server logic for app that displays violin or box plots for separate species in a single plot
server <- function(input, output) {
  output$iris_plot <- renderPlotly({
    
    # Create plot based on selected plot type
    if (input$plot_type == "Violin Plot") {
      ggplot(iris, aes(x = Species, y = !!as.symbol(input$y_var), fill = Species)) +
        geom_violin(trim = FALSE) +
        labs(x = "Species", y = input$y_var, title = "Violin Plot of Iris Data")
    } else {
      ggplot(iris, aes(x = Species, y = !!as.symbol(input$y_var), fill = Species)) +
        geom_boxplot() +
        labs(x = "Species", y = input$y_var, title = "Box Plot of Iris Data")
    }
  })
}


# Run the app
shinyApp(ui = ui, server = server)
