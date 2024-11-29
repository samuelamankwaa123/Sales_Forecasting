install.packages("dplyr")
install.packages("skimr")
install.packages("cli")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("forecast")
install.packages("timetk")
install.packages("writexl")
install.packages("forecast")
#packageVersion("cli")
#update.packages(ask = FALSE)
#search()

#.libPaths()
#install.packages("rbibutils", dependencies = TRUE)
#install.packages("Rcpp", dependencies = TRUE)
#packageVersion("Rcpp")

#options(timeout = 600)  # Increase timeout to 600 seconds
#memory.limit(size = 8000)  # Set limit to 8 GB

#memory.limit()


#update.packages(ask = FALSE)

# Load necessary librarieslibrary(tidyverse)    # For data manipulation and visualization

library(lubridate)
library(ggplot2)
library(lubridate)    # For date and time manipulation
library(forecast)     # For forecasting models
library(timetk)       # For time-series visualization and manipulation
library(skimr)        # For detailed data summaries
library(dplyr)
library(writexl)

# Load datasets

train_data <- read.csv("C:/Users/Kasutaja/documents/TRAIN.csv")
test_data <- read.csv("C:/Users/Kasutaja/documents/TEST_FINAL.csv")
train_data
test_data
# 1. Data Overview
print("Train Data Overview:")
glimpse(train_data)

print("Test Data Overview:")
glimpse(test_data)

# Check for missing values
print("Missing Values in Train Data:")
sapply(train_data, function(x) sum(is.na(x)))

print("Missing Values in Test Data:")
sapply(test_data, function(x) sum(is.na(x)))

# 2. Parse Dates and Basic Preprocessing
train_data <- train_data %>%
  mutate(date = as.Date(Date, format = "%Y-%m-%d")) %>%
  arrange(date)

test_data <- test_data %>%
  mutate(date = as.Date(Date, format = "%Y-%m-%d")) %>%
  arrange(date)

# Summary Statistics
print("Summary of Train Data:")
skim(train_data)
#colnames(train_data)

# 3. Visualizing Sales Trends
# Plot the overall sales trend in the training data
ggplot(train_data, aes(x = date, y = Sales)) +
  geom_line(color = "steelblue") +
  labs(title = "Sales Trend Over Time", x = "Date", y = "Sales") +
  theme_minimal()
#train_data$date <- as.Date(train_data$date, format = "%Y-%m-%d")
train_data$Sales <- as.numeric(train_data$Sales)


# Decompose Time Series (if applicable)
sales_ts <- ts(train_data$Sales, frequency = 12, start = c(year(min(train_data$date)), month(min(train_data$date))))
sales_decomp <- decompose(sales_ts)

plot(sales_decomp) +
  ggtitle("Decomposition of Sales Time Series") +
  theme_minimal()



# 4. Forecasting Models
# Convert training data into a time-series object
#sales_ts <- ts(train_data$Sales, frequency = 12, start = c(year(min(train_data$date)), month(min(train_data$date))))

# Fit ARIMA model
arima_model <- auto.arima(sales_ts)
summary(arima_model)

# Forecast Sales
forecasted_sales <- forecast(arima_model, h = 12)  # Forecast next 12 months
forecasted_sales
plot(forecasted_sales) +
  labs(title = "ARIMA Forecast of Sales", x = "Time", y = "Sales") +
  theme_minimal()



# 6. Seasonal Trend Analysis
# Seasonal plot
ggseasonplot(sales_ts, year.labels = TRUE, year.labels.left = TRUE) +
  labs(title = "Seasonal Plot of Sales", x = "Month", y = "Sales") +
  theme_minimal()

# 7. Exporting Results
# Save forecasted results
forecasted_results <- as.data.frame(forecasted_sales)
write.csv(forecasted_results, "C:/Users/Kasutaja/Downloads/forecasted_sales.csv")

# End of Script
