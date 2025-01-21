library(forecast)
library(TSA)
library(tseries)

## 2. Recall the dataset “robot” firstly introduced in TSA HW06.

### 2.a. Use IMA(1, 1) to forecast five values ahead and calculate the 95% confidence intervals.

robot_data <- read.csv("C:/Git_Code/Some-practice/TSA HW06.robot.csv")
robot_ts <- ts(robot_data$robot)
robot_ima <- Arima(robot_ts, order = c(0, 1, 1))
forecast_result_ima <- forecast(robot_ima, h = 5)
print(forecast_result_ima)

### 2.b. Display the actual values, the five forecasts and the 95% confidence intervals of the five forecasts, all in

plot(forecast_result_ima, main = "Forecast with 95% Confidence Intervals of IMA(1,1)", xlab = "t (index)", ylab = "Values")

### 2.c. Use ARMA(1, 1) to forecast five values ahead and calculate the 95% confidence intervals. Compare the results with those in (a), what do you observe?

robot_arma <- Arima(robot_ts, order = c(1, 0, 1))
forecast_result_arma <- forecast(robot_arma, h = 5)
print(forecast_result_arma)
plot(forecast_result_arma, main = "Forecast with 95% Confidence Intervals of ARMA(1,1)", xlab = "t (index)", ylab = "Values")

## 3. The dataset “boardings” contains the monthly number of passengers who boarded light rail trains and buses in Denver, Colorado, from August 2000 to March 2006.

### 3.a. Plot the time series and tell your observation if there exists seasonality and if the series is stationary. 

boardings_data <- read.csv("C:/Git_Code/Some-practice/TSA HW08.boardings.csv")
boardings_ts <- ts(boardings_data$log_boardings, start = c(2000, 8), frequency = 12)
plot(boardings_ts, main = "Time Series of Boardings", xlab = "t (index)", ylab = "Log Boardings")
decomposed_boardings_ts <- decompose(boardings_ts)
plot(decomposed_boardings_ts)

### 3.b. Plot the sample ACF and see what are the significant lags?

acf(as.vector(boardings_ts), main = "Sample ACF of Time Series of Boardings")

### 3.c. Fit the data with ARMA(0, 3) × (1,0)_12, evaluate if the estimated coefficients \{ \hat{\theta}_1, \hat{\theta}_2, \hat{\theta}_3, \hat{\phi}_{12} \} are significant. 

model <- Arima(boardings_ts, order = c(0, 0, 3), seasonal = c(1, 0, 0))
summary(model)

coefficients <- coef(model)
standard_errors <- sqrt(diag(model$var.coef))
t_values <- coefficients / standard_errors

results <- data.frame(
  Coefficient = coefficients,
  `Standard Error` = standard_errors,
  `t-value` = t_values
)

print(results)

## 4. The monthly airline passengers, first investigated by Box and Jenkins in 1976, is considered as the classic time series dataset (see “TSA HW08.airpass.csv”).

### 4.a. Plot the time series in its original scale and the log-transformed scale. Do you think making the logtransformation is appropriate?

airpass_data <- read.csv("C:/Git_Code/Some-practice/TSA HW08.airpass.csv")
airpass_ts <- ts(airpass_data$airpass, start = c(1976,1) , frequency = 12)
log_airpass_ts <- log(airpass_ts)

plot(airpass_ts, main = "Time Series of Airpass", xlab = "t (index)", ylab = "Passengers")
plot(log_airpass_ts, main = "Time Series of Airpass(log)", xlab = "t (index)", ylab = "Passengers(log)")

### 4.b. Make the first-order difference over the “log-transformed” data. What do you observe?

diff_log_airpass_ts <- diff(log_airpass_ts)
plot(diff_log_airpass_ts, main = "Time Series of Airpass(log and diff)", xlab = "t (index)", ylab = "Passengers(log and diff)")

### 4.c. Make a seasonal difference of the resulted series in (b), what do you observe?

seasonality_diff_log_airpass_ts <- diff(diff_log_airpass_ts, lag = 12)
plot(seasonality_diff_log_airpass_ts, main = "Time Series of Airpass(log and diff and seasonality)", xlab = "t (index)", ylab = "Passengers(log and diff and seasonality)")

### 4.d. Plot the sample ACF of the resulted series in (c), explain what you see.

acf(as.vector(diff_log_airpass_ts), main = "Sample ACF of Time Series of Airpass(log and diff and seasonality)")
acf(as.vector(seasonality_diff_log_airpass_ts), main = "Sample ACF of Time Series of Airpass(log and diff and seasonality)")

### 4.e. Fit an ARIMA(0,1,1) × (0,1,1)_12 model to the log-transformed series. Diagnose the residuals of this model, including the sample ACF and the normality test

airpass_sarima <- Arima(log_airpass_ts, order = c(0, 1, 1), seasonal = c(0, 1, 1))
summary(airpass_sarima)
residuals <- residuals(airpass_sarima)
plot(residuals, main = "Residuals Time Series", ylab = "Residuals", xlab = "Time")
acf(residuals, main = "ACF of Residuals")
qqnorm(residuals)
qqline(residuals)
acf(residuals, main = "Sample ACF of Residuals")


### 4.f. Make forecasts for “two” years based on the model in (e). The confidence intervals shall be included.

forecast_periods <- 24
forecasts <- forecast(airpass_sarima, h = forecast_periods)
print(forecasts)
plot(forecasts, main = "Two-Year Forecast with Confidence Intervals",
     xlab = "Time", ylab = "Log(Air Passengers)")
forecast_original_scale <- exp(forecasts$mean)
forecast_ci_original_scale <- exp(forecasts$lower)
forecast_original <- cbind(forecast_original_scale, forecast_ci_original_scale)
print(forecast_original)

print(forecasts$)


airpass_sarima2 <- Arima(seasonality_diff_log_airpass_ts, order = c(0, 1, 1), seasonal = c(0, 1, 1))
forecasts <- forecast(airpass_sarima2, h = 24)
plot(forecasts, main = "Two-Year Forecast with Confidence Intervals",
     xlab = "Time", ylab = "Log(Air Passengers)")

airpass_sarima3 <- Arima(airpass_ts, order = c(0, 1, 1), seasonal = c(0, 1, 1))
forecasts <- forecast(airpass_sarima3, h = 24)
plot(forecasts, main = "Two-Year Forecast with Confidence Intervals",
     xlab = "Time", ylab = "Log(Air Passengers)")

