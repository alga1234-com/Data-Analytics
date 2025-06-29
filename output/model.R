library(shiny)

# Load model and data
model <- readRDS("model.rds")
data <- read.csv("data.csv")

ui <- fluidPage(
  titlePanel("Housing Price Prediction App"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("neigh", "Select Neighborhood:", 
                  choices = unique(data$Neighborhood)),
      br(),
      helpText("This app uses a trained model to predict housing prices.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Table", DT::dataTableOutput("table")),
        tabPanel("Plot", plotOutput("pricePlot"))
      )
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    subset(data, Neighborhood == input$neigh)
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(filtered_data())
  })
  
  output$pricePlot <- renderPlot({
    df <- filtered_data()
    plot(df$Liv_Area, df$Sale_Price,
         main = paste("Sale Price vs Living Area -", input$neigh),
         xlab = "Living Area (sq ft)",
         ylab = "Sale Price",
         pch = 19, col = "darkblue")
    points(df$Liv_Area, df$Predicted, col = "red", pch = 4)
    legend("topleft", legend = c("Actual", "Predicted"), 
           col = c("darkblue", "red"), pch = c(19, 4))
  })
}

shinyApp(ui = ui, server = server)
