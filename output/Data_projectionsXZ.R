setwd("C:/Users/kaggwa/Desktop/projects/Data-Analysi-main")

list.files()

# Step 1: Install and Load Required Packages
install.packages(c("dplyr", "caret", "xgboost", "Matrix", "data.table"))
install.packages("tidyverse")
install.packages("ggplot2", dependencies = TRUE)
install.packages("RColorBrewer")
install.packages("randomForest")
install.packages("caTools")
library(caTools)
library(randomForest)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(caret)
library(xgboost)
library(Matrix)
library(data.table)

#AUnified Data Integration and Cleaning
library(dplyr)

# 1. Read each CSV
AmesHousing       <- read.csv("data/AmesHousing.csv", stringsAsFactors = FALSE)
Californiahousing <- read.csv("data/Californiahousing.csv", stringsAsFactors = FALSE)

# 2. Find common columns
common_cols <- intersect(names(AmesHousing), names(Californiahousing))

# 3. Subset and tag
Ames_common <- AmesHousing[, common_cols] %>%
  mutate(Source = "Ames")

California_common <- Californiahousing[, common_cols] %>%
  mutate(Source = "California")

# 4. Bind them
combined_data <- bind_rows(Ames_common, California_common)

#View Structure and Summary
# View the structure of the full combined data
str(combined_data)

# Basic summary statistics
summary(combined_data)

#Group count by sourcename:
combined_data %>%
  group_by(Source) %>%
  summarize(count = n()) %>%
  print()
#Now read combined_data

setwd("C:/Users/kaggwa/Desktop/projects/Data-Analysi-main")
combined_data <- read.csv("data/combined_data.csv", stringsAsFactors = FALSE)


#To pens the data frames in the RStudio viewer
# Basic summary

------------------------------------------------------------------
  
  #DATA CLEANING AND PREPROCESSING
  ------------------------------------------------------------------
  # Ensure Neighborhood is a factor
  combined_data$Neighborhood <- as.factor(combined_data$Neighborhood)

--------------------------------------------------------------------------------
  #DATA EXPLORATION (EDA)
  --------------------------------------------------------------------------------
  # Get the list of known neighborhoods from training data
  known_neighs <- unique(combined_data$Neighborhood)


#Reading

summary(combined_data)
colnames(combined_data)


write.csv(combined_data, "C:/Users/kaggwa/Desktop/projects/Data-Analysi-main/data/combined_data_summary.csv", row.names = FALSE)

head(combined_data_summary)
unique(combined_data_summary$Source)

#Step 3 VISUALIZE DATA
#Plot Average Sale Price by Neighborhood
install.packages("ggplot2")  # Install ggplot2 (if not already installed)
library(ggplot2)             # Load ggplot2 package
library(dplyr)

combined_data_summary<- read.csv("data/combined_data_summary.csv", stringsAsFactors = FALSE)

combined_data_summary <- combined_data %>%
  group_by(Neighborhood, Source) %>%
  summarise(
    SalePrice = mean(SalePrice, na.rm = TRUE),
    .groups = "drop"
  )


ggplot(combined_data_summary, aes(x = reorder(Neighborhood, -SalePrice), y = SalePrice, fill = Source)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.9) +
  labs(title = "SalePrice by Neighborhood",
       x = "Neighborhood", y = "SalePrice") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = c("Ames" = "steelblue", "California" = "darkorange"))

#Scutter plot to find relationship of SalePrice by LivArea

ggplot(combined_data, aes(x = LivArea, y = SalePrice)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Sale Price vs Living Area", x = "Living Area (sq ft)", y = "Sale Price")

#Box Plot for SalePrice by Bedrooms
ggplot(combined_data, aes(x = as.factor(TotalBedrooms), y = SalePrice)) +
  geom_boxplot(fill = "green") +
  labs(title = "Sale Price by Bedroom Count", x = "Total Bedrooms", y = "Sale Price")

#Living Area by Neighborhood

ggplot(combined_data, aes(x = Neighborhood, y = LivArea)) +
  geom_boxplot(fill = "wheat") +
  labs(title = "Living Area by Neighborhood", x = "Neighborhood", y = "Living Area (sq ft)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#A Bar plot plot comparing price vs. predicted sale prices 

library(dplyr)

combined_data_sample <- combined_data %>%
  as_tibble() %>%
  arrange(desc(SalePrice)) %>%
  slice(1:20)


library(ggplot2)

ggplot(combined_data_sample, aes(x = LivArea, y = SalePrice, color = Source)) +
  geom_point(size = 3) +
  labs(
    title = "Top 20 Most Expensive Houses by Living Area",
    subtitle = "Comparison of Ames and California Housing Markets",
    x = "Living Area (sq ft)",
    y = "Sale Price (USD)",
    color = "Data Source",
    caption = "Data Source: AmesHousing.csv and Californiahousing.csv"
  ) +
  theme_minimal()




write.csv(cor_data, "C:/Users/kaggwa/Desktop/projects/Data-Analysi-main/data/correlation_matrix.csv", row.names = TRUE)

#Model Building, Fitting & Testing in R
#Step 1: Data Preparation

library(dplyr)
library(caret)
objects()


#Define Data to be predicted 

install.packages("caTools")   # Run this once
library(caTools)              # Then load the package
## 5. Linear Regression Modeling
# Split data
install.packages("caret")
library(caret)

set.seed(123)
train_idx <- createDataPartition(combined_data$Sale_Price, p = 0.7, list = FALSE)

#create train_data
library(caret)
set.seed(123)

# Create training indices
train_idx <- createDataPartition(combined_data$Sale_Price, p = 0.7, list = FALSE)

# Create training and test sets
train_data <- combined_data[train_idx, ]
test_data <- combined_data[-train_idx, ]

# Linear model
lm_model <- lm(Sale_Price ~ Liv_Area + Source, data = train_data)
summary(lm_model)


# Predict and evaluate
# make sure caret is loaded
library(caret)      
library(caret)

# Predict on test data
predictions <- predict(lm_model, newdata = test_data)

# Actual values
actuals <- test_data$Sale_Price

# Calculate evaluation metrics
eval_metrics <- postResample(pred = predictions, obs = actuals)

print(eval_metrics)

# Convert to data frame for saving
eval_df <- as.data.frame(t(eval_metrics))
write.csv(eval_df, "C:/Users/kaggwa/Desktop/projects/Data-Analysi-main/data/model_evaluation.csv", row.names = TRUE)

#Now i want to Automatically captures interactions between nonlinearities, and categorical splits
#for Better performance in real-world data
#Load required packages
library(xgboost)
library(caret)

# Create design matrix (remove intercept column)
train_matrix <- model.matrix(Sale_Price ~ Liv_Area + Total_bedrooms + Source, data = train_data)[, -1]

# Extract target variable
train_label <- train_data$Sale_Price


# Train the XGBoost model
xgb_model <- xgboost(
  data = train_matrix,
  label = train_label,
  nrounds = 100,
  objective = "reg:squarederror",
  verbose = 0
)

#Testing the Model
# Create test matrix
test_matrix <- model.matrix(Sale_Price ~ Liv_Area + Total_bedrooms + Source, data = test_data)[, -1]

# Predict
xgb_predictions <- predict(xgb_model, newdata = test_matrix)

# Evaluate performance (e.g., RMSE)

xgb_eval <- postResample(pred = xgb_predictions, obs = test_data$Sale_Price)

# Shows RMSE, R², MAE
print(xgb_eval)  

# I now want to compare xgboost with feature importance
library(xgboost)
library(Matrix)
feature_names <- colnames(train_matrix)

# Plot feature importance without using the pipe operator
importance_matrix <- xgb.importance(feature_names = feature_names, model = xgb_model)


# Shows the top 5 most important features
print(importance_matrix)
write.csv(importance_matrix, 
          "C:/Users/kaggwa/Desktop/projects/Data-Analysi-main/data/xgb_feature_importance.csv", 
          row.names = FALSE)


#Visualize Feature Relationships
library(ggplot2)

library(ggplot2)
names(combined_data)

library(ggplot2)

ggplot(combined_data, aes(x = Liv_Area, y = Sale_Price)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "blue") +
  labs(title = "Sale Price vs Living Area",
       x = "Living Area (sq ft)", y = "Sale Price")



# Load necessary libraries
library(ggplot2)
library(reshape2)
library(corrplot)

# Step 1: Load the dataset
data <- read.csv("C:/Users/kaggwa/Desktop/projects/Data-Analysi-main/data/model_predictions.csv")


# Step 2: Check structure and clean column names if necessary
str(data)
colnames(data)

# Step 3: Select numeric columns for correlation analysis
# You may need to adjust these names based on your file content
numeric_data <- data %>% 
  select_if(is.numeric) %>% 
  na.omit()  # remove rows with missing values

# Step 4: Compute correlation matrix
cor_matrix <- cor(numeric_data)

# Step 5: Plot correlation matrix
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.cex = 0.8,
         title = "Correlation Matrix", mar = c(0,0,1,0))

#Correlation Heat map between Var1 and Va2
# Compute correlation matrix (if not done)
cor_matrix <- cor(numeric_data, use = "complete.obs")

# Melt the correlation matrix into long format
library(tibble)


melted_cor <- as.data.frame(cor_matrix) %>%
  rownames_to_column(var = "Var1") %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "value")


library(caret)
set.seed(123)

#Target variable is named SalePrice (case sensitive)
split_index <- createDataPartition(combined_data$Sale_Price, p = 0.8, list = FALSE)

train_data <- combined_data[split_index, ]
test_data <- combined_data[-split_index, ]

# Now fit the model
lm_model <- lm(Sale_Price ~ ., data = train_data)

# Step 3: Predict on test data
predictions <- predict(lm_model, newdata = test_data)

# Step 4: View model summary and performance
# Extract coefficients table from lm_model summary
coef_table <- summary(lm_model)$coefficients

# Convert to data frame
coef_df <- as.data.frame(coef_table)

# Now save to CSV
write.csv(coef_df, file = "C:/Users/kaggwa/Desktop/projects/Data-Analysi-main/output/lm_model_coefficients.csv", row.names = TRUE)

# Actual values
actual <- test_data$Sale_Price

# Create results data frame
results <- data.frame(ID = 1:nrow(test_data),
                      Actual = actual,
                      Predicted = predictions)


write.csv(results, "C:/Users/kaggwa/Desktop/projects/Data-Analysi-main/data/prediction_results.csv", row.names = FALSE)

library(xgboost)

# Step 1: Create feature matrix (exclude target variable)
X <- model.matrix(Sale_Price ~ . -1, data = train_data)

# Step 2: Extract target vector
y <- train_data$Sale_Price

# Step 3: Create DMatrix for training
dtrain <- xgb.DMatrix(data = X, label = y)

# Step 4: Train XGBoost model
xgb_model <- xgboost(data = dtrain,
                     nrounds = 100,
                     objective = "reg:squarederror",
                     verbose = 0)

#Save the model

# Save the XGBoost model with timestamp
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
model_path <- paste0("C:/Users/kaggwa/Desktop/projects/Data-Analysi-main/data/xgboost_model_", timestamp, ".model")

xgb.save(xgb_model, model_path)

