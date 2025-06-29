# global.R
#Loads and preprocesses data
#AND Performs modeling (Linear Regression and XGBoost)
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(xgboost)
library(Matrix)
library(data.table)
library(randomForest)
library(caTools)
library(corrplot)
library(reshape2)
library(tibble)

# Set working directory
setwd("C:/Users/kaggwa/Desktop/projects/Data-Analysi-main")

# Load datasets
AmesHousing <- read.csv("data/AmesHousing.csv", stringsAsFactors = FALSE)
Californiahousing <- read.csv("data/Californiahousing.csv", stringsAsFactors = FALSE)

# Harmonize common columns and combine datasets
common_cols <- intersect(names(AmesHousing), names(Californiahousing))
Ames_common <- AmesHousing[, common_cols] %>% mutate(Source = "Ames")
California_common <- Californiahousing[, common_cols] %>% mutate(Source = "California")
combined_data <- bind_rows(Ames_common, California_common)

# Ensure factor for categorical
combined_data$Source <- as.factor(combined_data$Source)
combined_data$Neighborhood <- as.factor(combined_data$Neighborhood)

# Data split for modeling
set.seed(123)
train_index <- createDataPartition(combined_data$SalePrice, p = 0.7, list = FALSE)
train_data <- combined_data[train_index, ]
test_data <- combined_data[-train_index, ]

# Linear Regression Model
lm_model <- lm(SalePrice ~ LivArea + Source, data = train_data)
lm_predictions <- predict(lm_model, newdata = test_data)
lm_metrics <- postResample(pred = lm_predictions, obs = test_data$SalePrice)

# XGBoost Model preparation
train_matrix <- model.matrix(SalePrice ~ LivArea + TotalBedrooms + Source, data = train_data)[, -1]
test_matrix  <- model.matrix(SalePrice ~ LivArea + TotalBedrooms + Source, data = test_data)[, -1]
train_label <- train_data$SalePrice
test_label  <- test_data$SalePrice

# Train XGBoost
xgb_model <- xgboost(data = train_matrix,
                     label = train_label,
                     nrounds = 100,
                     objective = "reg:squarederror",
                     verbose = 0)

# XGBoost predictions and metrics
xgb_predictions <- predict(xgb_model, newdata = test_matrix)
xgb_metrics <- postResample(pred = xgb_predictions, obs = test_label)

# Feature importance for XGBoost
feature_names <- colnames(train_matrix)
xgb_importance <- xgb.importance(feature_names = feature_names, model = xgb_model)

# Top 20 expensive houses sample for plotting
combined_data_sample <- combined_data %>%
  as_tibble() %>%
  arrange(desc(SalePrice)) %>%
  slice(1:20)
