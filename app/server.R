# server.R

library(shiny)
library(ggplot2)
library(dplyr)

shinyServer(function(input, output, session) {
  
  # Summary statistics table
  output$summaryTable <- renderTable({
    combined_data %>%
      group_by(Source) %>%
      summarize(
        Count = n(),
        AvgPrice = mean(SalePrice, na.rm = TRUE),
        MedianPrice = median(SalePrice, na.rm = TRUE)
      )
  })
  
  # Sale Price by Neighborhood bar plot
  output$priceByNeighborhoodPlot <- renderPlot({
    combined_data_summary <- combined_data %>%
      group_by(Neighborhood, Source) %>%
      summarise(SalePrice = mean(SalePrice, na.rm = TRUE), .groups = "drop")
    
    ggplot(combined_data_summary, aes(x = reorder(Neighborhood, -SalePrice), y = SalePrice, fill = Source)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(title = "Average Sale Price by Neighborhood", x = "Neighborhood", y = "Average Sale Price") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_fill_manual(values = c("Ames" = "steelblue", "California" = "darkorange"))
  })
  
  # Sale Price vs Living Area scatterplot
  output$salePriceVsLivArea <- renderPlot({
    ggplot(combined_data, aes(x = LivArea, y = SalePrice, color = Source)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", color = "red") +
      labs(title = "Sale Price vs Living Area", x = "Living Area (sq ft)", y = "Sale Price")
  })
  
  # Sale Price by Bedroom Count boxplot
  output$salePriceByBedrooms <- renderPlot({
    ggplot(combined_data, aes(x = as.factor(TotalBedrooms), y = SalePrice)) +
      geom_boxplot(fill = "green") +
      labs(title = "Sale Price by Bedroom Count", x = "Total Bedrooms", y = "Sale Price")
  })
  
  # Living Area by Neighborhood boxplot
  output$livAreaByNeighborhood <- renderPlot({
    ggplot(combined_data, aes(x = Neighborhood, y = LivArea)) +
      geom_boxplot(fill = "wheat") +
      labs(title = "Living Area by Neighborhood", x = "Neighborhood", y = "Living Area (sq ft)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Top 20 most expensive houses plot
  output$top20ExpensivePlot <- renderPlot({
    ggplot(combined_data_sample, aes(x = LivArea, y = SalePrice, color = Source)) +
      geom_point(size = 3) +
      labs(
        title = "Top 20 Most Expensive Houses by Living Area",
        subtitle = "Comparison of Ames and California Housing Markets",
        x = "Living Area (sq ft)",
        y = "Sale Price (USD)",
        color = "Data Source"
      ) +
      theme_minimal()
  })
  
  # Linear model summary
  output$lmSummary <- renderPrint({
    summary(lm_model)
  })
  
  # Linear model metrics
  output$lmMetrics <- renderPrint({
    cat("Linear Regression Metrics:\n")
    print(lm_metrics)
  })
  
  # XGBoost model metrics
  output$xgbMetrics <- renderPrint({
    cat("XGBoost Metrics:\n")
    print(xgb_metrics)
  })
  
  # XGBoost feature importance table
  output$xgbImportance <- renderTable({
    xgb_importance
  })
  
  # Residual plot for linear model
  output$residualPlot <- renderPlot({
    residuals <- resid(lm_model)
    fitted <- fitted(lm_model)
    ggplot(data.frame(Fitted = fitted, Residuals = residuals), aes(x = Fitted, y = Residuals)) +
      geom_point(alpha = 0.5) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(title = "Residuals vs Fitted Values (Linear Model)", x = "Fitted Values", y = "Residuals") +
      theme_minimal()
  })
})
