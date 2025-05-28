# Helper functions for data processing

# Function to load market data from Yahoo Finance
load_market_data <- function(symbols, start_date, end_date) {
  if(length(symbols) == 0) return(NULL)

  all_data <- list()
  standard_cols <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")

  for(symbol in symbols) {
    tryCatch({
      # Get data from Yahoo Finance
      data <- getSymbols(symbol, src = "yahoo",
                        from = start_date,
                        to = end_date,
                        auto.assign = FALSE)

      # Handle missing values right after fetching
      # Apply na.locf first (carry forward values)
      data <- na.locf(data, na.rm = FALSE)
      # Then handle any remaining NAs at the beginning
      data <- na.locf(data, fromLast = TRUE)

      # If there are still any NAs, use linear interpolation
      if(any(is.na(data))) {
        data <- na.approx(data, na.rm = FALSE)
      }

      # If there are STILL any NAs (at edges), fill with nearest value
      if(any(is.na(data))) {
        data <- na.fill(data, "extend")
      }

      # Convert to data frame and standardize columns
      data_df <- data.frame(date = index(data))

      # Get the actual column names from the data
      actual_cols <- colnames(data)

      # Create standardized columns
      for(std_col in standard_cols) {
        col_pattern <- paste0("\\.", std_col, "$")
        matching_cols <- grep(col_pattern, actual_cols, value = TRUE)

        if(length(matching_cols) > 0) {
          data_df[[std_col]] <- as.numeric(data[, matching_cols[1]])
        } else {
          # If column doesn't exist, add NA
          data_df[[std_col]] <- NA
        }
      }

      # Add symbol column
      data_df$symbol <- symbol

      all_data[[symbol]] <- data_df

    }, error = function(e) {
      warning(paste("Error loading data for", symbol, ":", e$message))
    })
  }

  # Check if we have any data before combining
  if(length(all_data) == 0) {
    return(NULL)
  }

  # Ensure all data frames have the same structure before combining
  result <- data.frame() # Empty data frame to start

  for(df in all_data) {
    # Make sure we have data
    if(nrow(df) > 0) {
      # If result is empty, use first dataframe as template
      if(nrow(result) == 0) {
        result <- df
      } else {
        # Make sure columns match before rbinding
        if(length(setdiff(colnames(result), colnames(df))) == 0 &&
           length(setdiff(colnames(df), colnames(result))) == 0) {
          result <- rbind(result, df)
        } else {
          warning(paste("Skipping data with mismatched columns for", df$symbol[1]))
        }
      }
    }
  }

  return(result)
}

# Function to normalize data using different methods
normalize_data <- function(data, method = "percent_change", baseline_date = NULL) {
  if(is.null(data) || nrow(data) == 0) return(NULL)

  # Make sure we have the Close column
  if(!"Close" %in% colnames(data)) {
    warning("Missing Close column in data. Cannot normalize.")
    return(data)
  }

  # Split data by symbol
  symbols <- unique(data$symbol)
  result <- list()

  for(sym in symbols) {
    sym_data <- data[data$symbol == sym, ]

    # Skip if we don't have enough data
    if(nrow(sym_data) < 2) {
      warning(paste("Not enough data to normalize for symbol:", sym))
      next
    }

    # Make sure data is ordered by date
    sym_data <- sym_data[order(sym_data$date), ]

    # Handle any NAs in Close
    if(any(is.na(sym_data$Close))) {
      sym_data$Close <- na.approx(sym_data$Close, na.rm = FALSE)
      sym_data$Close <- na.locf(sym_data$Close)
      sym_data$Close <- na.locf(sym_data$Close, fromLast = TRUE)
    }

    if(method == "percent_change") {
      # Calculate percent change from first value
      first_value <- sym_data$Close[1]
      if(!is.na(first_value) && first_value != 0) {
        sym_data$normalized <- (sym_data$Close / first_value - 1) * 100
      } else {
        sym_data$normalized <- 0
      }
    } else if(method == "z_score") {
      # Z-score normalization
      mean_val <- mean(sym_data$Close, na.rm = TRUE)
      sd_val <- sd(sym_data$Close, na.rm = TRUE)
      if(!is.na(sd_val) && sd_val != 0) {
        sym_data$normalized <- (sym_data$Close - mean_val) / sd_val
      } else {
        sym_data$normalized <- 0
      }
    } else if(method == "min_max") {
      # Min-max normalization
      min_val <- min(sym_data$Close, na.rm = TRUE)
      max_val <- max(sym_data$Close, na.rm = TRUE)
      if(!is.na(min_val) && !is.na(max_val) && (max_val - min_val) != 0) {
        sym_data$normalized <- (sym_data$Close - min_val) / (max_val - min_val)
      } else {
        sym_data$normalized <- 0
      }
    }

    result[[sym]] <- sym_data
  }

  # Combine all normalized data
  if(length(result) > 0) {
    # Use data.table::rbindlist for more robust binding or ensure all have same structure
    combined <- do.call(rbind, lapply(result, function(x) {
      # Ensure all data frames have the same columns
      if(!"normalized" %in% colnames(x)) {
        x$normalized <- 0
      }
      return(x)
    }))
    return(combined)
  } else {
    return(NULL)
  }
}

# Calculate performance metrics for specified period
calculate_performance <- function(data, period = "1m") {
  if(is.null(data) || nrow(data) == 0) return(NULL)

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

    if(nrow(recent_data) > 1) {  # Need at least 2 points for return calculation
      # Calculate metrics
      first_price <- recent_data$Close[1]
      last_price <- recent_data$Close[nrow(recent_data)]

      # Check for valid prices
      if(!is.na(first_price) && !is.na(last_price) && first_price > 0) {
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
          Volatility = ifelse(is.na(volatility), 0, volatility),
          Sharpe = ifelse(volatility > 0, return_pct / volatility, 0)
        )

        results <- rbind(results, result_row)
      }
    }
  }

  return(results)
}

# Function to convert symbol code to display name
get_display_name <- function(symbol) {
  # Define mapping of symbols to display names
  symbol_mapping <- c(
    # Indices
    "^GSPC" = "S&P 500",
    "^GDAXI" = "DAX",
    "^FTSE" = "FTSE",
    "^FCHI" = "CAC 40",
    "^STOXX50E" = "EURO STOXX 50",

    # Stocks
    "AAPL" = "Apple",
    "NVDA" = "Nvidia",
    "MSFT" = "Microsoft",
    "GOOG" = "Google",
    "META" = "Meta",

    # Currencies
    "EURUSD=X" = "Euro -> Dolar",
    "GBPUSD=X" = "Funt -> Dolar",
    "EURGBP=X" = "Euro -> Funt",
    "EURPLN=X" = "Euro -> Złoty",
    "USDPLN=X" = "Dolar -> Złoty",
    "GBPPLN=X" = "Funt -> Złoty"
  )

  # Return the display name if found, otherwise return the symbol itself
  if(symbol %in% names(symbol_mapping)) {
    return(symbol_mapping[symbol])
  } else {
    return(symbol)
  }
}
