library(forecast)
library(TSA)
library(tseries)
library(tidyverse)

## 5. Consider the well-known time series data “co2” (monthly carbon dioxide readings through 11 years in Alert, Canada).

### 5.a. Fit a deterministic regression model in terms of months and time. Are the regression coefficients significant? What is the adjusted R-squared? (Note that the month variable should be treated as categorical and transformed into 11 dummy variables.)

data <- read.csv("C:/Git_Code/Some-practice/TSA HW07.co2.csv")
data$month <- factor(data$month, levels = unique(data$month))
model <- lm(co2_level ~ time_trend + month, data = data)
summary(model)
cat("Adjusted R-squared:", summary(model)$adj.r.squared)

### 5.b. Identify, estimate the SARIMA model for the co2 level.

co2_ts <- ts(data$co2_level, start = c(1994, 1), frequency = 12)
plot(co2_ts, main = "CO2 Levels Over Time", ylab = "CO2 Level", xlab = "index t")
sarima_model <- auto.arima(co2_ts, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
summary(sarima_model)
checkresiduals(sarima_model)


### 5.c. Compare the two models above, what do you observe?

calculate_mape <- function(actual, predicted) {
  return(mean(abs((actual - predicted) / actual)) * 100)
}

deterministic_predictions <- as.numeric(predict(model, newdata = data))
sarima_predictions <- as.numeric(fitted(sarima_model))

mape_deterministic <- calculate_mape(data$co2_level, deterministic_predictions)
mape_sarima <- calculate_mape(data$co2_level, sarima_predictions)

cat("MAPE for Deterministic Regression:", mape_deterministic, "%\n")
cat("MAPE for SARIMA Model:", mape_sarima, "%\n")

