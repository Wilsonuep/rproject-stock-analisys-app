predictions_server <- function(input, output, session) {
  # Reactive values to store model results
  model_results <- reactiveValues(
    predictions = NULL,
    accuracy = NULL,
    model_type = NULL
  )

  #General function to split time sereis
  split_time_series <- function(data, train_prop = 0.8) {
    n <- length(data)
    train_size <- floor(n * train_prop)
    list(
      train = window(data, end = time(data)[train_size]),
      test = window(data, start = time(data)[train_size + 1])
    )
  }

  #General function to plot data from market with data estimated
  plot_forecast <- function(data, forecast, data_label = "Dane giełdowe", forecast_label = "Predykcja") {
    data_time <- time(data)
    data_values <- as.numeric(data)

    df_data <- tibble(
      time = data_time,
      value = data_values,
      type = data_label
    )

    fc_time <- time(forecast$mean)
    fc_values <- as.numeric(forecast$mean)
    fc_lower <- as.numeric(forecast$lower[,2])  # 95% CI
    fc_upper <- as.numeric(forecast$upper[,2])

    df_forecast <- tibble(
      time = fc_time,
      value = fc_values,
      lower = fc_lower,
      upper = fc_upper,
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

  #Function to predict, plot and describe accuracy naive models
  predict_with_naive <- function(data, horizon) {
    prediction <- naive(data, h = horizon)

    sp_data <- split_time_series(data)
    train <- sp_data$train
    test <- sp_data$test
    acc_model <- naive(train, h = length(test))
    acc <- accuracy(acc_model, test)

    plot_obj <- plot_forecast(data, prediction, forecast_label = "Predykcja modelu naiwnego")

    return(list(
      prediction = prediction,
      accuracy = acc,
      plot = plot_obj
    ))
  }

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
