predictions_server <- function(input, output, session) {
  # Reactive values to store model results
  model_results <- reactiveValues(
    predictions = NULL,
    accuracy = NULL,
    model_type = NULL
  )

  #Those functions assume data is provided as data.frame()

  #General function to split time sereis
  split_time_series <- function(data, train_prop = 0.8) {
    n <- nrow(data)
    train_size <- floor(n * train_prop)

    list(
      train = data[1:train_size, ],
      test = data[(train_size + 1):n, ]
    )
  }

  #General function to plot data from market with data estimated
  plot_forecast <- function(data, forecast, data_label = "Dane giełdowe", forecast_label = "Predykcja") {
  df_data <- tibble(
    time = data$time,
    value = data$price,
    type = data_label
  )

  df_forecast <- tibble(
    #Changing time to account for possible xgboost compatibility problems
    time = seq(from = max(data$time) + 1, length.out = length(forecast$mean), by = 1),
    value = as.numeric(forecast$mean),
    lower = as.numeric(forecast$lower[, 2]),
    upper = as.numeric(forecast$upper[, 2]),
    type = forecast_label
  )

    df_all <- bind_rows(df_data, df_forecast)

    ggplot(df_all, aes(x = time, y = value, color = type)) +
      geom_line(size = 1) +
      geom_ribbon(
        data = df_forecast,
        aes(ymin = lower, ymax = upper, fill = type),
        alpha = 0.2, color = NA
      ) +
      scale_color_manual(values = setNames(c("black", "red"), c(data_label, forecast_label))) +
      scale_fill_manual(values = setNames("red", forecast_label)) +
      labs(
        title = "Wykres predykcji",
        x = "Czas", y = "Wartość", color = "Legenda", fill = "Przedział ufności"
      ) +
      theme_minimal()
  }

  generate_lagged_df <- function(data, lags = 1:5) {
    stopifnot(all(c("time", "price") %in% colnames(data)))
    df <- data
    for (lag in lags) {
      df[[paste0("lag", lag)]] <- dplyr::lag(df$price, n = lag)
    }
    df <- tidyr::drop_na(df)
    return(df)
  }

  #Function to predict, plot and describe accuracy with naive models
  predict_with_naive <- function(data, horizon) {
    time <- data$time
    price <- data$price
    prediction <- naive(price, h = horizon)

    sp_data <- split_time_series(data)
    train <- sp_data$train
    test <- sp_data$test
    acc_model <- naive(train, h = length(test))
    acc <- accuracy(acc_model, test)

    plot_pred <- plot_forecast(
      data = tibble(time = data$time, price = data$price),
      forecast = prediction,
      forecast_label = "Predykcja modelu naiwnego"
    )

    return(list(
      prediction = prediction,
      accuracy = acc,
      plot = plot_pred
    ))
  }

  #Function to predict, plot and describe accuracy with ARIMA models
  predict_with_arima <- function(data, horizon) {
    price <- data$price

    model <- auto.arima(price)
    prediction <- forecast(model, horizon)

    sp_data <- split_time_series(data)
    train <- sp_data$train$price
    test <- sp_data$test$price
    acc_model <- auto.arima(train)
    acc_prediction <- forecast(acc_model, h = length(test))
    acc <- accuracy(acc_prediction, test)

    plot_pred <- plot_forecast(
      data = data,
      forecast = prediction,
      forecast_label = "Predykcja modelu ARIMA"
    )

    return(list(
      prediction = prediction,
      accuracy = acc,
      plot = plot_pred
    ))
  }

  #Function to predict, plot and describe accuracy with Random Forest
  predict_with_random_forest <- function(data, horizon) {
    df_lagged <- generate_lagged_df(data, 1:horizon)

    split <- split_time_series(df_lagged)
    train <- split$train
    test <- split$test

    formula_rf <- as.formula(paste("price ~", paste0("lag", 1:horizon, collapse = " + ")))
    model <- randomForest(formula_rf, data = train, ntree=100)

    pred_test <- predict(model, newdata = test)
    acc <- accuracy(pred_test, test$price)

    latest <- df_lagged[nrow(df_lagged), paste0("lag", 1:horizon)]
    future_preds <- numeric(horizon)

    full_model <- randomForest(formula_rf, data = data, ntree=100)

    for (i in 1:horizon) {
      pred <- predict(full_model, newdata = as.data.frame(t(latest)))
      future_preds[i] <- pred
      latest <- c(tail(as.numeric(latest), -1), pred)
    }

    fc <- list(
      mean = ts(future_preds, start = 1, frequency = 1),
      lower = matrix(NA, nrow = horizon, ncol = 2),
      upper = matrix(NA, nrow = horizon, ncol = 2)
    )
    class(fc) <- "forecast"

    plot_obj <- plot_forecast(data, fc, forecast_label = "Predykcja Random Forest")

    return(list(
      prediction = fc,
      accuracy = acc,
      plot = plot_obj
    ))
  }

  #Function to predict, plot and describe accuracy with XGBoost
  predict_with_xgboost <- function(data,horizon){
    df_lagged <- generate_lagged_df(data, 1:horizon)

    split <- split_time_series(df_lagged)
    train <- split$train
    test <- split$test

    train_matrix <- as.matrix(train[, paste0("lag", 1:horizon)])
    test_matrix <- as.matrix(test[, paste0("lag", 1:horizon)])

    #Let's take 100 trees for simplicity
    model <- xgboost(data = train_matrix, label = train$price, nrounds = 100, objective = "reg:squarederror", verbose = 0)
    pred_test <- predict(model, newdata = test_matrix)
    acc <- accuracy(pred_test, test$price)

    latest <- df_lagged[nrow(df_lagged), paste0("lag", 1:horizon)]
    future_preds <- numeric(horizon)

    full_matrix <- as.matrix(df_lagged[, paste0("lag", 1:horizon)])
    full_model <- xgboost(data = full_matrix, label = data$price, nrounds = 100, objective = "reg:squarederror", verbose = 0)

    for (i in 1:horizon){
      pred <- predict(full_model, newdata = as.matrix(t(latest)))
      future_preds[i] <- pred
      latest <- c(tail(as.numeric(latest), -1), pred)
    }

    fc <- list(
      mean = ts(future_preds, start = 1, frequency = 1),
      lower = matrix(NA, nrow = horizon, ncol = 2),
      upper = matrix(NA, nrow = horizon, ncol = 2)
    )
    class(fc) <- "forecast"

    plot_obj <- plot_forecast(data, fc, forecast_label = "Predykcja XGBoost")

    return(list(
      prediction = fc,
      accuracy = acc,
      plot = plot_obj
    ))

  }

  #TODO connect functions with the rest of server + UI

  # Get data for selected instrument
  selected_instrument_data <- reactive({
    req(input$market)
    req(input$date_range)

    # Get data from Yahoo Finance
    # This is a placeholder - implement actual data retrieval
    data <- data.frame()

    return(data)
  })

  # Run prediction model when button is clicked
  observeEvent(input$run_model, {
    req(input$market)
    req(input$predictor)
    req(input$pred_horizon)

    # Show a progress notification
    withProgress(message = "Budowanie modelu predykcyjnego...", {

      # Get data
      data <- selected_instrument_data()

      # Run appropriate model based on selection
      if(input$predictor == "naive") {
        # Naive model (last value prediction)
        # This is a placeholder - implement actual model
      } else if(input$predictor == "ARIMA") {
        # ARIMA model
        # This is a placeholder - implement actual model
      } else if(input$predictor == "random_forest") {
        # Random forest model
        # This is a placeholder - implement actual model
      } else if(input$predictor == "xgb_boost") {
        # XGBoost model
        # This is a placeholder - implement actual model
      }

      # Store results in reactive values
      model_results$predictions <- data.frame() # Replace with actual predictions
      model_results$accuracy <- data.frame() # Replace with actual accuracy metrics
      model_results$model_type <- input$predictor
    })
  })

  # Plot outputs
  output$prediction_plot <- renderPlotly({
    req(model_results$predictions)

    # Create prediction plot
    plot_ly() %>%
      layout(title = paste0("Predykcja dla ", names(which(input$market == unlist(input$market)))))
  })

  output$prediction_table <- renderPlotly({
    req(model_results$predictions)

    # Create table of prediction values
    plot_ly() %>%
      layout(title = "Wartości predykcji")
  })

  # Table output
  output$prediction_accuracy_table <- renderDT({
    req(model_results$accuracy)

    datatable(model_results$accuracy)
  })
}
