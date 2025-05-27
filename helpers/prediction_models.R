# Helper functions for prediction models

# Naive model (last value continues)
naive_model <- function(data, horizon = 14) {
  if(is.null(data) || nrow(data) < 5) return(NULL)

  # Get last observed value
  last_value <- tail(data$Close, 1)
  last_date <- tail(data$date, 1)

  # Create prediction dataframe
  prediction_dates <- seq(as.Date(last_date) + 1, by = "day", length.out = horizon)

  # Filter out weekends if needed
  is_weekday <- !weekdays(prediction_dates) %in% c("Saturday", "Sunday")
  prediction_dates <- prediction_dates[is_weekday]

  predictions <- data.frame(
    date = prediction_dates,
    prediction = rep(last_value, length(prediction_dates)),
    # Simple confidence interval (fixed percentage)
    lower_ci = rep(last_value * 0.95, length(prediction_dates)),
    upper_ci = rep(last_value * 1.05, length(prediction_dates))
  )

  # Calculate model accuracy
  # For naive model, use mean absolute percentage error on a validation set
  # This is placeholder logic - implement actual validation
  accuracy <- data.frame(
    Metric = c("MAPE", "RMSE"),
    Value = c(5.0, 10.0)  # Placeholder values
  )

  return(list(
    predictions = predictions,
    accuracy = accuracy
  ))
}

# ARIMA model
arima_model <- function(data, horizon = 14) {
  if(is.null(data) || nrow(data) < 30) return(NULL)

  # Create time series
  ts_data <- ts(data$Close, frequency = 1)

  # Fit ARIMA model
  model <- tryCatch({
    auto.arima(ts_data)
  }, error = function(e) {
    NULL
  })

  if(is.null(model)) return(NULL)

  # Forecast
  forecast_result <- forecast(model, h = horizon)

  # Create prediction dataframe
  last_date <- tail(data$date, 1)
  prediction_dates <- seq(as.Date(last_date) + 1, by = "day", length.out = horizon)

  # Filter out weekends if needed
  is_weekday <- !weekdays(prediction_dates) %in% c("Saturday", "Sunday")
  prediction_dates <- prediction_dates[is_weekday]

  predictions <- data.frame(
    date = prediction_dates,
    prediction = as.numeric(forecast_result$mean)[1:length(prediction_dates)],
    lower_ci = as.numeric(forecast_result$lower[, 2])[1:length(prediction_dates)],
    upper_ci = as.numeric(forecast_result$upper[, 2])[1:length(prediction_dates)]
  )

  # Calculate model accuracy
  accuracy <- data.frame(
    Metric = c("MAPE", "RMSE"),
    Value = c(
      accuracy(forecast_result)[, "MAPE"][1],
      accuracy(forecast_result)[, "RMSE"][1]
    )
  )

  return(list(
    predictions = predictions,
    accuracy = accuracy
  ))
}

# Random Forest model on lagged features
random_forest_model <- function(data, horizon = 14) {
  if(is.null(data) || nrow(data) < 30) return(NULL)

  # This is a placeholder for a random forest model
  # In a real implementation, you would:
  # 1. Create lagged features
  # 2. Split data into train/test
  # 3. Train a random forest model
  # 4. Make predictions

  # For this example, we'll just create dummy predictions

  # Create prediction dataframe
  last_date <- tail(data$date, 1)
  last_value <- tail(data$Close, 1)
  prediction_dates <- seq(as.Date(last_date) + 1, by = "day", length.out = horizon)

  # Filter out weekends if needed
  is_weekday <- !weekdays(prediction_dates) %in% c("Saturday", "Sunday")
  prediction_dates <- prediction_dates[is_weekday]

  # Create random walk predictions with trend
  set.seed(123)
  random_changes <- cumsum(rnorm(length(prediction_dates), mean = 0.001, sd = 0.01))
  predictions <- last_value * (1 + random_changes)

  prediction_df <- data.frame(
    date = prediction_dates,
    prediction = predictions,
    lower_ci = predictions * 0.95,
    upper_ci = predictions * 1.05
  )

  # Placeholder accuracy metrics
  accuracy <- data.frame(
    Metric = c("MAPE", "RMSE"),
    Value = c(3.5, 7.2)  # Placeholder values
  )

  return(list(
    predictions = prediction_df,
    accuracy = accuracy
  ))
}

# XGBoost model on lagged features
xgboost_model <- function(data, horizon = 14) {
  if(is.null(data) || nrow(data) < 30) return(NULL)

  # This is a placeholder for an XGBoost model
  # In a real implementation, you would:
  # 1. Create lagged features
  # 2. Split data into train/test
  # 3. Train an XGBoost model
  # 4. Make predictions

  # For this example, we'll just create dummy predictions

  # Create prediction dataframe
  last_date <- tail(data$date, 1)
  last_value <- tail(data$Close, 1)
  prediction_dates <- seq(as.Date(last_date) + 1, by = "day", length.out = horizon)

  # Filter out weekends if needed
  is_weekday <- !weekdays(prediction_dates) %in% c("Saturday", "Sunday")
  prediction_dates <- prediction_dates[is_weekday]

  # Create slightly upward trend predictions
  set.seed(456)
  random_changes <- cumsum(rnorm(length(prediction_dates), mean = 0.002, sd = 0.008))
  predictions <- last_value * (1 + random_changes)

  prediction_df <- data.frame(
    date = prediction_dates,
    prediction = predictions,
    lower_ci = predictions * 0.93,
    upper_ci = predictions * 1.07
  )

  # Placeholder accuracy metrics
  accuracy <- data.frame(
    Metric = c("MAPE", "RMSE"),
    Value = c(2.8, 6.5)  # Placeholder values
  )

  return(list(
    predictions = prediction_df,
    accuracy = accuracy
  ))
}
