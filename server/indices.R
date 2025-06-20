indices_server <- function(input, output, session) {
  # Get normalized indices data
  indices_data <- reactive({
    req(input$Indices)
    req(input$date_range)
    req(input$indices_norm_method)
    
    # Get data from Yahoo Finance
    start_date <- input$date_range[1]
    end_date <- input$date_range[2]
    
    # Try to load data
    tryCatch({
      data <- load_market_data(input$Indices, start_date, end_date)
      return(data)
    }, error = function(e) {
      warning(paste("Error in indices_data:", e$message))
      return(NULL)
    })
    
  })
  
  
  
  
  # Performance metrics for selected period
  indices_performance <- reactive({
    req(input$Indices)
    req(input$indices_perf_period)
    
    
    # Define the period to analyze
    period_days <- switch(input$indices_perf_period,
                          "1w" = 7,
                          "1m" = 30,
                          "3m" = 90,
                          "1y" = 365,
                          "YTD" = as.numeric(difftime(Sys.Date(), as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01")), units = "days")),
                          "All" = Inf)
    # Get data for the period
    if(period_days == Inf) {
      start_date <- input$date_range[1]
      end_date <- input$date_range[2]
      data_period <- get_close_data(input$Indices, start_date, end_date)
    } else {
      start_date <- Sys.Date() - period_days
      end_date <- input$date_range[2]
      data <- get_close_data(input$Indices, start_date, end_date)
      data_period <- data[paste0(start_date, "/"), ]
    }
    
    if(nrow(data_period) < 2) return(NULL)
    
    # Calculate performance metrics
    first_prices <- data_period[1,]
    last_prices <- data_period[nrow(data_period),]
    
    performance <- data.frame(
      Asset = gsub("\\^", "", gsub("=F", "", colnames(data_period))),
      Start_Price = as.numeric(first_prices),
      End_Price = as.numeric(last_prices),
      Total_Return = (as.numeric(last_prices) / as.numeric(first_prices) - 1) * 100,
      Annualized_Return = ((as.numeric(last_prices) / as.numeric(first_prices)) ^
                             (365 / as.numeric(difftime(index(data_period)[nrow(data_period)],
                                                        index(data_period)[1],
                                                        units = "days"))) - 1) * 100,
      Volatility = apply(diff(log(data_period)) * 100, 2, sd, na.rm = TRUE) * sqrt(252)
    )
    

    colnames(performance) <- c("Symbol", "Cena początkowa", "Cena końcowa", "Całkowity zwrot", "Roczny zwrot", "Zmienność")
    return(performance)
  })
  
  
  # Calculate largest changes for indicies
  indices_changes2 <- reactive({
    data <- indices_data()
    
    if(is.null(data) || nrow(data) == 0) {
      return(NULL)
    }
    
    # Calculate changes by symbol
    changes <- data.frame()
    symbols <- unique(data$symbol)
    
    for(symbol in symbols) {
      symbol_data <- data[data$symbol == symbol, ]
      if(nrow(symbol_data) >= 2) {
        # Order by date to ensure correct calculation
        symbol_data <- symbol_data[order(symbol_data$date), ]
        
        first_price <- symbol_data$Close[1]
        last_price <- tail(symbol_data$Close, 1)
        
        if(!is.na(first_price) && !is.na(last_price) && first_price != 0) {
          change_pct <- ((last_price / first_price) - 1) * 100
          
          # Get display name for the symbol
          display_name <- get_display_name(symbol)
          
          changes <- rbind(changes, data.frame(
            symbol = symbol,
            display_name = display_name,
            change_pct = change_pct
          ))
        }
      }
    }
    
    return(changes)
  })
  
  
  # InfoBox outputs
  output$biggest_index_gain <- renderInfoBox({
    # Find index with biggest gain
    changes <- indices_changes2()
    biggest_gain_index <- which.max((changes$change_pct))
    biggest_gain <- changes[biggest_gain_index, ]
    formatted_pct <- paste0(
      ifelse(biggest_gain$change_pct >= 0, "+", ""),
      round(biggest_gain$change_pct, 2),
      "%"
    )
    
    infoBox(
      "Największy zysk",
      biggest_gain$display_name,
      formatted_pct, # Replace with actual data
      icon = icon("arrow-up"),
      color = "green"
    )
  })
  
  output$biggest_index_lose <- renderInfoBox({
    # Find index with biggest loss
    changes <- indices_changes2()
    biggest_loss_index <- which.min((changes$change_pct))
    biggest_loss <- changes[biggest_loss_index, ]
    formatted_pct <- paste0(
      ifelse(biggest_loss$change_pct >= 0, "+", ""),
      round(biggest_loss$change_pct, 2),
      "%"
    )
    if (biggest_loss$change_pct > 0) {
      infoBox(
        "Najmniejszy zysk",
        biggest_loss$display_name,
        formatted_pct, # Replace with actual data
        icon = icon("arrow-up"),
        color = "green"
      )
    }
    else{
      infoBox(
        "Największa strata",
        biggest_loss$display_name,
        formatted_pct, # Replace with actual data
        icon = icon("arrow-down"),
        color = "red"
      )
    }
  })
  
  # Plot outputs
  output$index_market_performance_plot <- renderPlotly({
    data <- indices_data()
    
    if(is.null(data) || nrow(data) == 0) {
      return(plot_ly() %>%
               layout(title = "Brak danych dla indeksów",
                      xaxis = list(title = ""),
                      yaxis = list(title = "")))
    }
    
    # Try to create normalized data and plot
    tryCatch({
      # Normalize data for comparison
      normalized_data <- normalize_data(data, input$indices_norm_method)
      
      if(is.null(normalized_data) || nrow(normalized_data) == 0) {
        return(plot_ly() %>%
                 layout(title = "Błąd podczas normalizacji danych indeksów",
                        xaxis = list(title = ""),
                        yaxis = list(title = "")))
      }
      
      # Create performance comparison plot
      yaxis_title <- switch(input$indices_norm_method,
                            percent_change = "Zmiana [%]",
                            z_score = "Z-Score",
                            min_max = "Min-Max Normalizacja")
      
      p <- plot_ly() %>%
        layout(title = "Porównanie indeksów",
               xaxis = list(title = "Data"),
               yaxis = list(title = yaxis_title),
               hovermode = "closest")
      
      # Add a line for each symbol
      for(sym in unique(normalized_data$symbol)) {
        sym_data <- normalized_data[normalized_data$symbol == sym, ]
        sym_data <- sym_data[order(sym_data$date), ]
        
        # Get display name
        display_name <- get_display_name(sym)
        
        p <- p %>% add_trace(
          x = sym_data$date,
          y = sym_data$normalized,
          type = 'scatter',
          mode = 'lines',
          name = display_name
        )
      }
      
      return(p)
    }, error = function(e) {
      return(plot_ly() %>%
               layout(title = paste("Błąd wykresu:", e$message),
                      xaxis = list(title = ""),
                      yaxis = list(title = "")))
    })
  })
  
  output$index_risk_return_plot <- renderPlotly({
    data <- get_close_data(input$Indices, input$date_range[1], input$date_range[2])
    if(is.null(data) || ncol(data) < 2)
      return(plotly_empty(type = "scatter", mode = "markers"))
    
    # Calculate returns and volatility
    daily_returns <- diff(log(data)) * 100
    mean_returns <- apply(daily_returns, 2, mean, na.rm = TRUE) * 252  # Annualized
    volatility <- apply(daily_returns, 2, sd, na.rm = TRUE) * sqrt(252)  # Annualized
    
    # Create data frame for plot
    risk_return_df <- data.frame(
      Asset = gsub("\\^", "", gsub("=F", "", colnames(daily_returns))),
      Return = mean_returns,
      Risk = volatility
    )
    
    # Create plot
    p <- plot_ly(risk_return_df, x = ~Risk, y = ~Return,
                 type = "scatter", mode = "markers+text",
                 marker = list(size = 10, opacity = 0.7),
                 text = ~Asset,
                 textposition = "top center") %>%
      layout(title = "Analiza ryzyka i zwrotu",
             xaxis = list(title = "Ryzyko (zmienność w ujęciu rocznym %)"),
             yaxis = list(title = "Zwrot (% w ujęciu rocznym)"))
    
    return(p)
  })
  
  
  # Table output
  output$index_performance_table <- renderDT({
    datatable(indices_performance(), options = list(
      pageLength = 15,
      scrollX = TRUE
    )) %>%
      formatRound(columns = c("Cena początkowa", "Cena końcowa"), digits = 2) %>%
      formatRound(columns = c("Całkowity zwrot", "Roczny zwrot", "Zmienność"), digits = 2) %>%
      formatStyle(
        'Całkowity zwrot',
        backgroundColor = styleInterval(c(0), c('rgba(255,0,0,0.3)', 'rgba(0,255,0,0.3)'))
      )
  })
}
