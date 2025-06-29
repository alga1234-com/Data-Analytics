setwd("C:/Users/kaggwa/Documents/r-project2025/class-project")

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

# Load required packages

# 1. Read both datasetslibrary(janitor)
#Step 1: View Column Names
names(AmesHousing)
names(Californiahousing)

library(dplyr)

# Rename relevant columns (example — adapt as needed)
install.packages("dplyr")
library(dplyr)

# Ames Dataset
library(dplyr)
Ames <- AmesHousing %>%
  select(Sale_Price, Liv_Area, total_bedrooms, Neighborhood) %>%
  mutate(Source = "Ames")

# California Dataset
library(dplyr)
California <- Californiahousing %>%
  select(Sale_Price, Liv_Area, total_bedrooms, Neighborhood) %>%
  mutate(Source = "California")

#Combine both datasets:
combined_data <- bind_rows(Ames, California)

#Summarize by Neighborhood and Source
combined_summary <- combined_data %>%
  group_by(Neighborhood, Source) %>%
  summarise(Sale_Price = mean(Sale_Price, na.rm = TRUE), .groups = "drop")


#View Structure and Summary
# View the structure of the full combined data
str(combined_data)

# Basic summary statistics
summary(combined_data)

View(combined_data)

#View Grouped Summary Table
# Show structure and content of the grouped summary
head(combined_data)
colnames(combined_data)
str(combined_summary)
head(combined_summary)
table(combined_data$sourcename)

combined_data %>%
  group_by(sourcename) %>%
  summarize(count = n()) %>%
  print()

#Save Combined dataset as CSV_dataset

write.csv(combined_data, "combined_data.csv", row.names = FALSE)


#To pens the data frames in the RStudio viewer
# Basic summary

------------------------------------------------------------------
  
  #DATA CLEANING AND PREPROCESSING
  ------------------------------------------------------------------
  # Ensure Neighborhood is a factor
  combined_data$Neighborhood <- as.factor(combined_data$Neighborhood)

#Checking for missing values in the Combined_data
sum(is.na(combined_data$Liv_Area))

colnames(combined_data)

summary(combined_data)

nrow(combined_data)
nrow(combined_data %>% filter(!is.na(Liv_Area)))

sum(is.na(combined_data$Liv_Area))


# Filter out rows with missing Liv_Area, keeping only complete cases:
  
combined_data_clean <- combined_data %>% filter(!is.na(Liv_Area))

#Impute missing Liv_Area values, for example with median:

median_liv_area <- median(combined_data$Liv_Area, na.rm = TRUE)
combined_data <- combined_data %>%
  mutate(Liv_Area = ifelse(is.na(Liv_Area), median_liv_area, Liv_Area))

# Step 2: Remove rows with unseen Neighborhoods
common_neighs <- intersect(unique(combined_data$Neighborhood), unique(combined_data$Neighborhood))
combined_data <- combined_data %>% filter(Neighborhood %in% common_neighs)

# Step 3: Confirm there are no issues left
summary(combined_data)
colSums(is.na(combined_data))  
# Should all be 0

--------------------------------------------------------------------------------
  #DATA Data Validation and EXPLORATION (EDA)
  --------------------------------------------------------------------------------
  # Get the list of known neighborhoods from training data
  known_neighs <- unique(combined_data$Neighborhood)


#Reading

summary(combined_data)
colnames(combined_data)


#Step 3 VISUALIZE DATA
#Plot BAr graph of verage Sale Price by Neighborhood
install.packages("ggplot2")  
library(ggplot2)             

ggplot(combined_summary, aes(x = reorder(Neighborhood, -SalePrice), y = SalePrice, fill = Source)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.9) +
  labs(title = "SalePrice by Neighborhood",
       x = "Neighborhood", y = "SalePrice") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = c("Ames" = "steelblue", "California" = "darkorange"))

colnames(combined_summary)

View(combined_summary)

#Scutter plot to find relationship of SalePrice by LivArea

ggplot(combined_data, aes(x = LivArea, y = SalePrice)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Sale Price vs Living Area", x = "Living Area (sq ft)", y = "Sale Price")


ggplot(combined_data, aes(x = reorder(Neighborhood, -Sale_Price), 
                             y = Sale_Price, 
                             fill = Source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Sale Price by Neighborhood and Source",
       x = "Neighborhood", y = "Average Sale Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = c("Ames" = "steelblue", "California" = "darkorange"))

#Box Plot for SalePrice by Bedrooms
ggplot(combined_data, aes(x = as.factor(total_bedrooms), y = Sale_Price)) +
  geom_boxplot(fill = "green") +
  labs(title = "Sale Price by Bedroom Count", 
       x = "Total Bedrooms", 
       y = "Sale Price") +
  theme_minimal()

install.packages("httpgd")


#Living Area by Neighborhood
#This is generated to understanding housing market patterns in the data set.

ggplot(combined_data, aes(x = Neighborhood, y = Liv_Area)) +
  geom_boxplot(fill = "wheat") +
  labs(title = "Living Area by Neighborhood", x = "Neighborhood", y = "Living Area (sq ft)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(ggcorrplot)
library(dplyr)

# Select numeric columns
numeric_data <- combined_data %>%
  select(where(is.numeric))

# Compute correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")

# Plot heatmap
# Load necessary libraries
library("ggplot2")

# Select numeric columns
numeric_data <- combined_data %>%
  select(Sale_Price, Liv_Area, total_bedrooms)

# Compute correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")

# Plot heatmap
install.packages("ggcorrplot")   
library(ggcorrplot)             

library(corrplot)

# Select only Var1 and Var2
selected_vars <- combined_data %>% select(Var1, Var2)

selected_vars <- combined_data %>% select(Sale_Price, Liv_Area)


# Compute correlation matrix
cor_submatrix <- cor(selected_vars, use = "complete.obs")


library(dplyr)
library(corrplot)

# Select the three variables
selected_vars <- combined_data %>% select(Sale_Price, Liv_Area, total_bedrooms)

# Compute correlation matrix
cor_submatrix <- cor(selected_vars, use = "complete.obs")

# Plot correlation matrix (3x3)
selected_vars <- combined_data %>% select(Sale_Price, Liv_Area, total_bedrooms)
cor_submatrix <- cor(selected_vars, use = "complete.obs")

addCoef.cex = 1.2

corrplot(cor_submatrix,
         method = "color",
         col = colorRampPalette(c("blue", "white", "red"))(200),
         addCoef.col = "black",
         addCoef.cex = 1.2,  # CORRECTED
         tl.cex = 1.2,
         title = "Correlation between Sale Price, Living Area, and Total Bedrooms",
         mar = c(0, 0, 2, 0))

library(dplyr)

combined_data %>%
  group_by(Source) %>%
  summarise(
    count = n(),
    avg_price = mean(Sale_Price, na.rm = TRUE),
    avg_liv_area = mean(Liv_Area, na.rm = TRUE),
    avg_bedrooms = mean(total_bedrooms, na.rm = TRUE),
    median_price = median(Sale_Price, na.rm = TRUE)
  )


# Sample: take top 20 rows for clarity (optional, to avoid overcrowding)

combined_data <- results %>% slice(1:20)


# Convert to long format for grouped bar plot
plot_combined_data_long <- plot_combined_dataa %>%
  select(ID, SalePrice, PredictedPrice) %>%
  pivot_longer(cols = c(SalePrice, PredictedPrice), names_to = "Type", values_to = "Price")

# Plot
ggplot(plot_data_long, aes(x = reorder(ID, -Price), y = Price, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Actual vs Predicted Sale Prices", x = "House ID", y = "Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("SalePrice" = "steelblue", "PredictedPrice" = "orange"))


#Correlation Matrix (Optional Advanced)
cor_data <- combined_data %>%
  select(SalePrice, LivArea, Total_bedrooms) %>%  # Correct column name
  cor(use = "complete.obs")  # Compute correlation for selected columns

print(cor_data)  # Display correlation matrix


#Model Building, Fitting & Testing in R
#Step 1: Data Preparation

library(dplyr)
library(caret)
objects()


#Define Data to be predicted 

install.packages("caTools")   # Run this once
library(caTools)              # Then load the package


i# Split data into training and test sets
set.seed(123)
library(caret)
train_index <- createDataPartition(combined_data$Sale_Price, p = 0.8, list = FALSE)
train_data <- combined_data[train_index, ]
test_data <- combined_data[-train_index, ]

# Fit linear regression
lm_model <- lm(Sale_Price ~ ., data = train_data)

# Summary of the model
summary(lm_model)

# Predict on test set
lm_preds <- predict(lm_model, newdata = test_data)

# Evaluate performance
postResample(lm_preds, test_data$Sale_Price)

# the -1 removes intercept column

# Train the XGBoost model
library(xgboost)

model <- xgboost(data = train_matrix, 
                 label = train_data$SalePrice, 
                 nrounds = 100, 
                 objective = "reg:squarederror")


#Testing the Model
# Convert test data to numeric matrix
test_matrix <- model.matrix(SalePrice ~ . - 1, data = test_data)

# Make predictions
predictions <- predict(model, newdata = test_matrix)

# Evaluate performance (e.g., RMSE)
actual <- test_data$SalePrice
rmse <- sqrt(mean((predictions - actual)^2))
print(paste("RMSE:", rmse))

# Plot feature importance without using the pipe operator
importance <- xgb.importance(model = model)
xgb.plot.importance(importance, top_n = 10)

# Shows the top 5 most important features
head(importance, 5)  

#Visualize Feature Relationships
library(ggplot2)

library(ggplot2)
names(combined_data)

ggplot(combined_data, aes(x = LivArea, y = SalePrice)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "blue") +
  labs(title = "SalePrice vs LivArea",
       x = "LivArea", y = "SalePrice")


# Load necessary libraries
library(ggplot2)
library(reshape2)
library(corrplot)

# Step 1: Load the dataset
data <- read.csv("model_predictions.csv")

# Step 2: Select only numeric columns
numeric_data <- data[sapply(data, is.numeric)]

# Step 3: Compute the correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")


# Step 4: Plot using ggplot2
# Melt the correlation matrix
melted_cor <- melt(cor_matrix)

# Assume 'data' has a target variable called 'SalePrice'
set.seed(123)
library(caret)

# Merge based on 'Neighborhood'
combined_data <- merge(ames_data, housing_data, by = "Neighborhood", all = TRUE)

names(ames_data)
names(housing_data)

# Optional: Clean or transform if needed
# e.g., Remove missing values
combined_data <- na.omit(combined_data)

# Check if 'SalePrice' exists
names(df)

# Split into training and testing sets
split <- createDataPartition(data$SalePrice, p = 0.8, list = FALSE)
train_data <- data[split, ]
test_data <- data[-split, ]

# Fit a linear regression model
model_lm <- lm(SalePrice ~ ., data = train_data)

# Get R-squared on training data
r_squared <- summary(model_lm)$r.squared
print(paste("R-squared (Training Data):", round(r_squared, 4)))

#Correlation Heat map between Var1 and Va2
ggplot(data = melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1)) +
  coord_fixed()

# Optional: Plot using corrplot for an alternative style
corrplot(cor_matrix, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45, addCoef.col = "black")


# Export predictions
results <- data.frame(ID = 1:nrow(test_data),
                      Actual = actual,
                      Predicted = predictions)

write.csv(results, "model_predictions.csv", row.names = FALSE)

# Export feature importance
write.csv(importance, "feature_importance.csv", row.names = FALSE)

#Save the model with a timestamp
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
xgb.save(model, paste0("xgboost_model_", timestamp, ".model"))

save.image(file = "learning_model.RData")

saveRDS(model, file = "learning_model.rds")



