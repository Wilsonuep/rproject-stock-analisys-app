# Helper functions for data processing

# Function to load market data from Yahoo Finance
load_market_data <- function(symbols, start_date, end_date) {
  if(length(symbols) == 0) return(NULL)

  all_data <- list()

  for(symbol in symbols) {
    tryCatch({
      data <- getSymbols(symbol, src = "yahoo",
                        from = start_date,
                        to = end_date,
                        auto.assign = FALSE)
      # Convert to data frame and add symbol
      data_df <- data.frame(date = index(data), coredata(data))
      data_df$symbol <- symbol

      all_data[[symbol]] <- data_df
    }, error = function(e) {
      warning(paste("Error loading data for", symbol, ":", e$message))
    })
  }

  # Combine all data
  if(length(all_data) > 0) {
    result <- do.call(rbind, all_data)
    return(result)
  } else {
    return(NULL)
  }
}

# Function to normalize data using different methods
normalize_data <- function(data, method = "percent_change", baseline_date = NULL) {
  if(is.null(data)) return(NULL)

  # Split data by symbol
  symbols <- unique(data$symbol)
  result <- list()

  for(sym in symbols) {
    sym_data <- data[data$symbol == sym, ]

    if(method == "percent_change") {
      # Calculate percent change from first value
      first_value <- sym_data$Close[1]
      sym_data$normalized <- (sym_data$Close / first_value - 1) * 100
    } else if(method == "z_score") {
      # Z-score normalization
      mean_val <- mean(sym_data$Close)
      sd_val <- sd(sym_data$Close)
      sym_data$normalized <- (sym_data$Close - mean_val) / sd_val
    } else if(method == "min_max") {
      # Min-max normalization
      min_val <- min(sym_data$Close)
      max_val <- max(sym_data$Close)
      sym_data$normalized <- (sym_data$Close - min_val) / (max_val - min_val)
    }

    result[[sym]] <- sym_data
  }

  # Combine all normalized data
  if(length(result) > 0) {
    return(do.call(rbind, result))
  } else {
    return(NULL)
  }
}

# Calculate performance metrics for specified period
calculate_performance <- function(data, period = "1m") {
  if(is.null(data)) return(NULL)

  # Determine date cutoff based on period
  today <- Sys.Date()
  cutoff_date <- switch(period,
                       "1d" = today - 1,
                       "1w" = today - 7,
                       "1m" = today - 30,
                       "3m" = today - 90,
                       "1y" = today - 365,
                       today - 30)  # Default to 1 month

  # Calculate metrics for each symbol
  symbols <- unique(data$symbol)
  results <- data.frame()

  for(sym in symbols) {
    sym_data <- data[data$symbol == sym, ]

    # Get recent data based on cutoff
    recent_data <- sym_data[sym_data$date >= cutoff_date, ]

    if(nrow(recent_data) > 0) {
      # Calculate metrics
      first_price <- recent_data$Close[1]
      last_price <- recent_data$Close[nrow(recent_data)]
      return_pct <- (last_price / first_price - 1) * 100

      # Calculate volatility (standard deviation of daily returns)
      daily_returns <- diff(log(recent_data$Close))
      volatility <- sd(daily_returns, na.rm = TRUE) * sqrt(252) * 100  # Annualized volatility

      # Create row for results
      result_row <- data.frame(
        Symbol = sym,
        Start_Price = first_price,
        End_Price = last_price,
        Return_Pct = return_pct,
        Volatility = volatility,
        Sharpe = ifelse(volatility > 0, return_pct / volatility, 0)
      )

      results <- rbind(results, result_row)
    }
  }

  return(results)
}
