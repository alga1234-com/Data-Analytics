sidebarLayout(
  sidebarPanel(
    helpText("Explore housing price data and predictive models."),
    br(),
    h4("Model Performance"),
    verbatimTextOutput("lmMetrics"),
    verbatimTextOutput("xgbMetrics"),
    br(),
    h4("Feature Importance (XGBoost)"),
    tableOutput("xgbImportance")
  ),
  
  mainPanel(  # <- You must also define the corresponding main panel
    tabsetPanel(
      tabPanel("📊 Data Summary", verbatimTextOutput("summary")),
      tabPanel("🏘️ Sale Price by Neighborhood", plotOutput("barPlot")),
      tabPanel("📈 Sale Price vs Living Area", plotOutput("scatterPlot")),
      tabPanel("📦 Boxplot by Bedrooms", plotOutput("boxPlot")),
      tabPanel("📐 Model Metrics", tableOutput("modelResults")),
      tabPanel("🔥 Feature Importance (XGBoost)", tableOutput("xgbImp")),
      tabPanel("🔗 Correlation Matrix", plotOutput("corPlot")),
      tabPanel("💰 Top Expensive Houses", plotOutput("top20ExpensivePlot"))
    )
  )
)
