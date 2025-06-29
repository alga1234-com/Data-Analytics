library(shiny)
library(dplyr)

# Define the UI
ui <- fluidPage(
  titlePanel("Search Combined Housing Data"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("min_price", "Minimum Sale Price:", value = 100000),
      numericInput("max_price", "Maximum Sale Price:", value = 300000),
      
      numericInput("min_area", "Minimum Living Area (sq ft):", value = 500),
      numericInput("max_area", "Maximum Living Area (sq ft):", value = 2000),
      
      numericInput("bedrooms", "Total Bedrooms (optional):", value = NA),
      
      textInput("neighborhood", "Neighborhood (optional):", value = ""),
      
      actionButton("search", "Search")
    ),
    
    mainPanel(
      h4("Filtered Results"),
      dataTableOutput("results")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  filtered_data <- eventReactive(input$search, {
    data <- combined_data %>%
      filter(Sale_Price >= input$min_price,
             Sale_Price <= input$max_price,
             Liv_Area >= input$min_area,
             Liv_Area <= input$max_area)
    
    if (!is.na(input$bedrooms)) {
      data <- data %>% filter(total_bedrooms == input$bedrooms)
    }
    
    if (input$neighborhood != "") {
      data <- data %>% filter(grepl(input$neighborhood, Neighborhood, ignore.case = TRUE))
    }
    
    data
  })
  
  output$results <- renderDataTable({
    filtered_data()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
