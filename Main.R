# Load libraries
library(tidyverse)
library(forecast)
library(corrplot)
library(dplyr)

# Load data (replace "Superstore.csv" with your actual file path)
data <- read.csv("Superstore.csv")

# Check for missing values (adjust as needed)
summary(data)

# Encode non-numeric columns for correlation matrix
non_numeric_cols <- sapply(data, function(x) !is.numeric(x))
encoded_df <- data
for (col in names(encoded_df)[non_numeric_cols]) {
  encoded_df[[col]] <- as.numeric(factor(encoded_df[[col]]))
}

# Check for columns with zero standard deviation
constant_columns <- sapply(encoded_df, function(x) sd(x, na.rm = TRUE) == 0)
encoded_df <- encoded_df[, !constant_columns]

encoded_matrix <- data.matrix(encoded_df)
correlation_matrix <- cor(encoded_matrix)
correlation_matrix <- round(correlation_matrix, 3)
print(correlation_matrix)
par(mar = c(1,1,1,1)) # Adjust margin
corrplot(correlation_matrix, method = "color", addCoef.col = "black", number.cex = 0.5)

# Create a date column if Order Date is not already a date format
data$Order.Date <- as.Date(data$Order.Date, format = "%m/%d/%y")

# Feature engineering (consider creating additional features)
data$Month <- format(data$Order.Date, "%m")

# Define a function to remove outliers without normalizing
remove_outliers <- function(dataframe) {
  Q1 <- quantile(dataframe$Sales, 0.05, na.rm = TRUE)
  Q3 <- quantile(dataframe$Sales, 0.95, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5* IQR
  upper_bound <- Q3 + 1.5* IQR
  dataframe <- dataframe[dataframe$Sales >= lower_bound & dataframe$Sales <= upper_bound, ]
  return(dataframe)
}

# Call the function on 'Superstore'
data <- remove_outliers(data)
summary(data)

# Function to plot sales for a given subcategory
plot_sales <- function(subcategory) {
  sub_data <- filter(data, Sub.Category == subcategory)
  ggplot(sub_data, aes(x = Order.Date, y = Sales)) +
    geom_line() +
    labs(title = paste("Sales of", subcategory), x = "Order Date", y = "Sales") +
    theme_minimal()
}

# Create separate plots for each subcategory
plots <- lapply(unique(data$Sub.Category), plot_sales)
for (plot in plots) {
  print(plot)
}

# Create a list to store forecasts for each subcategory
forecast_list <- list()

# Loop through each unique subcategory
for (subcategory in unique(data$Sub.Category)) {
  # Filter data for the current subcategory
  sub_data <- filter(data, Sub.Category == subcategory)
  
  # Split data into training (Jan to Nov) and testing (Dec)
  train_data <- sub_data %>% filter(format(Order.Date, "%m") %in% c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11"))
  test_data <- sub_data %>% filter(format(Order.Date, "%m") == "12")
  
  # Time series analysis of Sales using training data
  sales_ts <- ts(train_data$Sales, start = c(year(min(train_data$Order.Date)), month(min(train_data$Order.Date))), frequency = 12)
  
  # Fit ARIMA model
  arima_model <- auto.arima(sales_ts)
  
  # Forecast sales for December
  forecast_arima <- forecast(arima_model, h = nrow(test_data))
  
  # Store the forecast in the list
  forecast_list[[subcategory]] <- list(forecast = forecast_arima, actual = test_data$Sales, test_dates = test_data$Order.Date)
}

# Create a data frame to store forecasts for all subcategories
forecast_df <- data.frame(
  Subcategory = character(),
  Date = as.Date(character()),
  Forecast = numeric(),
  stringsAsFactors = FALSE
)

# Populate the data frame with forecast values for all subcategories
for (i in seq_along(forecast_list)) {
  subcategory <- names(forecast_list)[i]
  forecast_data <- forecast_list[[subcategory]]$forecast
  
  # Generate dates for the forecast period (December)
  forecast_dates <- forecast_list[[subcategory]]$test_dates
  
  # Combine subcategory, dates, and forecasted sales into a data frame
  subcategory_forecast <- data.frame(
    Subcategory = rep(subcategory, length(forecast_dates)),
    Date = forecast_dates,
    Forecast = forecast_data$mean
  )
  
  # Append to the main forecast data frame
  forecast_df <- bind_rows(forecast_df, subcategory_forecast)
}

# Print the forecast table
view(forecast_df)

# Create a function to calculate performance metrics
calculate_metrics <- function(actual, forecast) {
  n <- min(length(actual), length(forecast))
  actual <- actual[1:n]
  forecast <- forecast[1:n]
  
  errors <- actual - forecast
  mae <- mean(abs(errors))
  mse <- mean(errors^2)
  rmse <- sqrt(mse)
  mape <- mean(abs(errors / actual)) * 100
  return(c(MAE = mae, RMSE = rmse, MAPE = mape))
}

# Initialize empty data frame to store metrics
performance_metrics <- data.frame(
  Subcategory = character(),
  MAE = numeric(),
  RMSE = numeric(),
  MAPE = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each subcategory
for (subcategory in unique(data$Sub.Category)) {
  actual_sales <- forecast_list[[subcategory]]$actual
  forecast_sales <- forecast_list[[subcategory]]$forecast$mean
  
  # Calculate performance metrics
  metrics <- calculate_metrics(actual_sales, forecast_sales)
  
  # Create a data frame for the metrics of the current subcategory
  metrics_df <- data.frame(
    Subcategory = subcategory,
    MAE = metrics["MAE"],
    RMSE = metrics["RMSE"],
    MAPE = metrics["MAPE"]
  )
  
  # Append the metrics to the main data frame
  performance_metrics <- rbind(performance_metrics, metrics_df)
}

# Print the performance metrics
view(performance_metrics)

# Plot actual vs forecasted data
plot_actual_vs_forecast <- function(subcategory) {
  sub_data <- filter(data, Sub.Category == subcategory)
  train_data <- sub_data %>% filter(format(Order.Date, "%m") %in% c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11"))
  test_data <- sub_data %>% filter(format(Order.Date, "%m") == "12")
  
  forecast_data <- forecast_list[[subcategory]]$forecast
  forecast_dates <- forecast_list[[subcategory]]$test_dates
  
  ggplot() +
    geom_line(data = train_data, aes(x = Order.Date, y = Sales, color = "Actual")) +
    geom_line(data = test_data, aes(x = Order.Date, y = Sales, color = "Forecast")) +
    labs(title = paste("Actual vs Forecasted Sales of", subcategory), x = "Date", y = "Sales") +
    theme_minimal() +
    scale_color_manual(name = "Legend", values = c("Actual" = "blue", "Forecast" = "green"))
}

# Create and print plots for each subcategory
actual_vs_forecast_plots <- lapply(unique(data$Sub.Category), plot_actual_vs_forecast)
for (plot in actual_vs_forecast_plots) {
  print(plot)
}
