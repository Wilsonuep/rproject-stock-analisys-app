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
    train_price <- train$price
    test_price <- test$price
    acc_model <- naive(train_price, h = length(test_price))
    acc <- accuracy(acc_model, test_price)[, c("RMSE", "MAE", "MAPE")]
    

    plot_pred <- plot_forecast(
      data = tibble(time = data$time, price = data$price),
      forecast = prediction,
      forecast_label = "Predykcja modelu naiwnego"
    )
    pred <- prediction
    pred_df <- data.frame(
      time = time(pred$mean),                    # oś czasu
      mean = as.numeric(pred$mean),              # predykcja punktowa
      lower80 = as.numeric(pred$lower[,1]),      # dolny 80%
      lower95 = as.numeric(pred$lower[,2]),      # dolny 95%
      upper80 = as.numeric(pred$upper[,1]),      # górny 80%
      upper95 = as.numeric(pred$upper[,2])       # górny 95%
    )
    colnames(pred_df) <- c("Okres", "Średnia", "Dolny 80%", "Dolny 95%", "Górny 80%", "Górny 95%")
    pred_df$Okres <- NULL
    
    return(list(
      prediction = pred_df,
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
    acc <- accuracy(acc_prediction, test)[, c("RMSE", "MAE", "MAPE")]

    plot_pred <- plot_forecast(
      data = data,
      forecast = prediction,
      forecast_label = "Predykcja modelu ARIMA"
    )
    pred <- prediction
    pred_df <- data.frame(
      time = time(pred$mean),                    # oś czasu
      mean = as.numeric(pred$mean),              # predykcja punktowa
      lower80 = as.numeric(pred$lower[,1]),      # dolny 80%
      lower95 = as.numeric(pred$lower[,2]),      # dolny 95%
      upper80 = as.numeric(pred$upper[,1]),      # górny 80%
      upper95 = as.numeric(pred$upper[,2])       # górny 95%
    )
    colnames(pred_df) <- c("Okres", "Średnia", "Dolny 80%", "Dolny 95%", "Górny 80%", "Górny 95%")
    pred_df$Okres <- NULL

    return(list(
      prediction = pred_df,
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
    full_model <- randomForest(formula_rf, data = df_lagged, ntree=100)
    
    for (i in 1:horizon) {
      pred <- predict(full_model, newdata = latest)
      future_preds[i] <- pred
      latest <- setNames(as.data.frame(t(c(tail(as.numeric(latest), -1), pred))), paste0("lag", 1:horizon))
    }
    
    
    fc <- list(
      mean = ts(future_preds, start = 1, frequency = 1),
      lower = matrix(NA, nrow = horizon, ncol = 2),
      upper = matrix(NA, nrow = horizon, ncol = 2)
    )
    class(fc) <- "forecast"
    
    fc_df <- data.frame(
      time = 1:horizon,
      mean = as.numeric(fc$mean)
    )
    colnames(fc_df) <- c("Okres", "Średnia")
    fc_df$Okres <- NULL
    

    plot_obj <- plot_forecast(data, fc, forecast_label = "Predykcja Random Forest")

    return(list(
      prediction = fc_df,
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
    # Obliczanie metryk dokładności
    errors <- test$price - pred_test
    rmse <- sqrt(mean(errors^2))
    mae <- mean(abs(errors))
    mape <- mean(abs(errors / test$price)) * 100

    acc <- data.frame(
      RMSE = rmse,
      MAE = mae,
      MAPE = mape
    )

    latest <- df_lagged[nrow(df_lagged), paste0("lag", 1:horizon)]
    future_preds <- numeric(horizon)

    full_matrix <- as.matrix(df_lagged[, paste0("lag", 1:horizon)])
    label1 <- df_lagged$price
    full_model <- xgboost(data = full_matrix, label = label1, nrounds = 100, objective = "reg:squarederror", verbose = 0)
    for (i in 1:horizon){
      pred <- predict(full_model, newdata = as.matrix(latest))
      future_preds[i] <- pred
      latest <- setNames(as.data.frame(t(c(tail(as.numeric(latest), -1), pred))), paste0("lag", 1:horizon))
      
    }

    fc <- list(
      mean = ts(future_preds, start = 1, frequency = 1),
      lower = matrix(NA, nrow = horizon, ncol = 2),
      upper = matrix(NA, nrow = horizon, ncol = 2)
    )
    class(fc) <- "forecast"
    fc_df <- data.frame(
      time = 1:horizon,
      mean = as.numeric(fc$mean)
    )
    colnames(fc_df) <- c("Okres", "Średnia")
    fc_df$Okres <- NULL

    plot_obj <- plot_forecast(data, fc, forecast_label = "Predykcja XGBoost")

    return(list(
      prediction = fc_df,
      accuracy = acc,
      plot = plot_obj
    ))

  }

  # ----------------------------------------
  # Functions to UI connection
  # ----------------------------------------

  # Get data for selected instrument
  selected_instrument_data <- reactive({
    req(input$market)
    req(input$date_range)
    market <- input$market
    from_date <- input$date_range[1]
    to_date <- input$date_range[2]
    data <- getSymbols(market, src = "yahoo",
                        from = from_date,
                        to = to_date,
                        auto.assign = FALSE)
    df <- data.frame(
      time = index(data),
      price = as.numeric(Cl(data))
    )
    return(df)
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
      horizon <- input$pred_horizon

      # Run appropriate model based on selection
      if(input$predictor == "naive") {
        model <- predict_with_naive(data, horizon)
      } else if(input$predictor == "ARIMA") {
        model <- predict_with_arima(data, horizon)
      } else if(input$predictor == "random_forest") {
        model <- predict_with_random_forest(data, horizon)
      } else if(input$predictor == "xgb_boost") {
        model <- predict_with_xgboost(data, horizon)
      }


      # Store results in reactive values
      model_results$predictions <- model$prediction
      model_results$accuracy <- model$accuracy
      model_results$plot <- model$plot
      model_results$model_type <- input$predictor

      showNotification(paste0("Zbudowano model predykcyjny", input$predictor), type = "message", duration = 3)
    })
  })

  # Plot outputs
  output$prediction_plot <- renderPlotly({
    req(model_results$predictions)
    ggplotly(model_results$plot)
  })


  # Table output
  output$prediction_accuracy_table <- renderDT({
    req(model_results$accuracy)
    acc <- model_results$accuracy
    if (nrow(acc) > 1){
      rownames(acc) <- c("Zestaw treningowy", "Zestaw testowy") 
    }
    else{
      rownames(acc) <- c("Zestaw testowy")
    }
    datatable(acc)
  })
  
  output$prediction_table <- renderDT({
    req(model_results$accuracy)
    pred <- model_results$predictions
    datatable(pred)
  })
}
