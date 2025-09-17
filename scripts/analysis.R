# Load libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(caret)
library(randomForest)
library(glmnet)
library(Metrics) 
library(corrplot)

# Load data
data <- read.csv("Project/final_data.csv")


### 1.Data Cleaning & Preparation

glimpse(data)
summary(data)

# Count missing values per column
colSums(is.na(data))

# Covert data type
data$Year <- as.integer(data$Year)
data$State <- as.factor(data$State)

# Check for duplicates
sum(duplicated(data))


### 2. Exploratory Data Analysis
## 2.1. Summary statistics
# Generate full descriptive statistics
summary_stats <- data %>%
  select(where(is.numeric)) %>%
  psych::describe() %>%
  as.data.frame() %>%
  mutate(across(everything(), ~ round(.x, 2)))
print(summary_stats)

## 2.2. Visualize distributions of key variables
# Histogram of Credit Card Debt per Capita
ggplot(data, aes(x = Credit_Debt)) +
  geom_histogram(binwidth = 200, fill = "orange", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Credit Card Debt Per Capita", x = "Credit Card Debt", y = "Count")

# Comment: The histogram shows credit card debt per capita is roughly normally distributed but 
# slightly right-skewed. Most states have debt around 2000–4000 USD per capita.


# Boxplot of Credit Card Debt by Year
ggplot(data, aes(x = factor(Year), y = Credit_Debt)) +
  geom_boxplot(fill = "purple") +
  theme_minimal() +
  labs(title = "Credit Card Debt Per Capita by Year", x = "Year", y = "Credit Card Debt")
# Comment: The boxplot over years reveals trends and variability: debt decreased during 
# 2010–2015 (likely post-recession effects), then started rising again, with higher medians 
# and wider spread in recent years like 2022 and 2023.

## 2.3 Apply log transformation to skewed variables - Population & Income
data <- data %>%
  mutate(
    log_Population = log(Population),
    log_Income = log(Income)
  )

# Visual skewness check for Population and Income using histograms + density
plot_skew <- function(df, var) {
  p1 <- ggplot(df, aes(x = .data[[var]])) +
    geom_histogram(aes(y = ..density..), fill = "skyblue", color = "black", bins = 30) +
    geom_density(color = "red", size = 1) +
    labs(title = paste(var, "(Before Log)")) +
    theme_minimal()
  
  p2 <- ggplot(df, aes(x = log(.data[[var]]))) +
    geom_histogram(aes(y = ..density..), fill = "orange", color = "black", bins = 30) +
    geom_density(color = "red", size = 1) +
    labs(title = paste(var, "(After Log)")) +
    theme_minimal()
  
  p1 | p2
}

# Show plots for Population and Income
plot_skew(data, "Population") / plot_skew(data, "Income")

## 2.4. Scatterplots to check relationships
# Get all numeric predictors except the response variable
predictors <- data %>%
  select(where(is.numeric), -Credit_Debt, -Population, -Income) %>%
  colnames()

# Loop and create scatterplots
plots <- map(predictors, function(var) {
  ggplot(data, aes_string(x = var, y = "Credit_Debt")) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = TRUE, color = "blue") +
    theme_minimal() +
    labs(
      title = paste("CreditCardDebt vs", var),
      x = var,
      y = "Credit Card Debt"
    )+
    theme(
      plot.title = element_text(size = 10, face = "bold"), 
      axis.title.x = element_text(size = 8),                 
      axis.title.y = element_text(size = 8)                 
    )
})

# Show all plots in one display
wrap_plots(plots, ncol = 2)


## 2.5. Check outliers
# Select numeric variables except Year
num_vars <- data %>% 
  select(where(is.numeric), -Year) %>% 
  names()

plot_outliers <- list()
for (var in num_vars) {
  p <- ggplot(data, aes(x = "", y = .data[[var]])) +
    geom_boxplot(fill = "coral") +
    labs(title = paste("Boxplot of", var), x = NULL, y = var) +
    theme_minimal()
  plot_outliers[[var]] <- p
}
plot_outliers

wrap_plots(plot_outliers, ncol = 2)

# Boxplots for selected key variables
selected_vars <- c("Credit_Debt", "Unemployment_Rate", "log_Income")

plot_selected_outliers <- list()
for (var in selected_vars) {
  p <- ggplot(data, aes(x = "", y = .data[[var]])) +
    geom_boxplot(fill = "coral") +
    labs(title = paste("Boxplot of", var), x = NULL, y = var) +
    theme_minimal()
  plot_selected_outliers[[var]] <- p
}

wrap_plots(plot_selected_outliers, ncol = 3)


# Comment: 
# Credit_Debt: Fairly symmetric distribution, but a few high outliers around 5,000+.

# Population: Extremely right-skewed with many high outliers; most states have 
# smaller populations, but a few states have bigger populations. May need a transformation.

# Unemployment_Rate: Slight right skew with clear outliers.

# CPI: Very tight range, low variability, which makes sense since CPI is a 
# national-level measure and changes gradually.

# Percent_Bachelors: Slight right skew, with some states having significantly 
# higher education levels than the majority.

# Income: Right skew with extreme high outliers; some states have much higher 
# median incomes. May need a transformation.


### 3. Check for Multicollinearity
## 3.1 Correlation matrix for numeric predictors
num_data <- data %>% 
  select(where(is.numeric)) %>%
  select(-Year) 

cor_matrix <- cor(num_data, use = "complete.obs")

# Plot correlation heatmap
corrplot(cor_matrix, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45, number.cex = 0.7)

## 3.2 Variance Inflation Factor (VIF)
# Build a model with all predictors except the target variable
vif_model <- lm(Credit_Debt ~ log_Population + Unemployment_Rate + CPI +
                  Percent_Bachelors + log_Income, data = data)
vif_values <- car::vif(vif_model)

# Show VIF values
vif_df <- data.frame(
  Variable = names(vif_values),
  VIF = as.numeric(vif_values),
  row.names = NULL
)
print(vif_df)

# Comment: 
# All the VIFs < 5, indicating that multicollinearity is not a concern in this dataset.
# However, skewness in Population and Income suggests transformations are needed 
# before regression to improve model assumptions. 
# CPI shows little variation and may have limited predictive power, but will be 
# retained for now.


### 4. Model Selection & Training
##  Train/Test Split
# Filter train year - 2022 & test year - 2023
train_data <- filter(data, Year == 2022)
test_data  <- filter(data, Year == 2023)

# Target variables
y_train <- train_data$Credit_Debt
y_test  <- test_data$Credit_Debt

# ----- 1. Multiple Linear Regression -----
mlr_model <- lm(Credit_Debt ~ log_Population + Unemployment_Rate + CPI +
                  Percent_Bachelors + log_Income, data = train_data)
summary(mlr_model)
mlr_pred <- predict(mlr_model, newdata = test_data)
mlr_rmse <- rmse(y_test, mlr_pred)
mlr_r2 <- cor(y_test, mlr_pred)^2

# ----- 2. Lasso Regression -----
set.seed(123)

# Matrix form for glmnet (Lasso needs this)
x_train <- model.matrix(Credit_Debt ~ log_Population + Unemployment_Rate + CPI +
                          Percent_Bachelors + log_Income, data = train_data)[, -1]
x_test  <- model.matrix(Credit_Debt ~ log_Population + Unemployment_Rate + CPI +
                          Percent_Bachelors + log_Income, data = test_data)[, -1]

lasso_model <- cv.glmnet(x_train, y_train, alpha = 1)
summary(lasso_model)
lasso_pred <- predict(lasso_model, s = lasso_model$lambda.min, newx = x_test)
lasso_rmse <- rmse(y_test, lasso_pred)
lasso_r2 <- cor(y_test, lasso_pred)^2

# Coefficients for Lasso
coef(lasso_model, s = lasso_model$lambda.min)

### Comment: 
# Based on the Lasso model, it kept Unemployment_Rate, Percent_Bachelors, and log_Income, 
# while removing Population and CPI entirely. This suggests those dropped variables 
# either add noise or are redundant with the others.

# ----- 3. Random Forest -----
set.seed(123)
rf_model <- randomForest(Credit_Debt ~ log_Population + Unemployment_Rate + CPI +
                           Percent_Bachelors + log_Income,
                         data = train_data, ntree = 500, importance = TRUE)
summary(rf_model)
rf_pred <- predict(rf_model, newdata = test_data)
rf_rmse <- rmse(y_test, rf_pred)
rf_r2 <- cor(y_test, rf_pred)^2

# Variable Importance
varImpPlot(rf_model)


# ----- Model Comparison -----
results <- data.frame(
  Model = c("Multiple Linear Regression", "Lasso Regression", "Random Forest"),
  RMSE = c(mlr_rmse, lasso_rmse, rf_rmse),
  R2 = c(mlr_r2, lasso_r2, rf_r2)
)

print(results)


