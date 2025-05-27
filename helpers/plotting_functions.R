# Helper functions for creating plots

# Function to create market performance comparison plot
create_performance_plot <- function(data, title = "Market Performance") {
  if(is.null(data) || nrow(data) == 0) {
    return(plot_ly() %>%
             layout(title = paste(title, "- No data available"),
                    showlegend = FALSE))
  }

  # Create a plot for each symbol
  p <- plot_ly() %>%
    layout(title = title,
           xaxis = list(title = "Date"),
           yaxis = list(title = "Normalized Value"),
           hovermode = "closest")

  # Add a line for each symbol
  for(sym in unique(data$symbol)) {
    sym_data <- data[data$symbol == sym, ]

    # Get display name (convert symbol code to readable name)
    display_name <- sym

    p <- p %>% add_trace(
      x = sym_data$date,
      y = sym_data$normalized,
      type = 'scatter',
      mode = 'lines',
      name = display_name
    )
  }

  return(p)
}

# Function to create risk-return scatter plot
create_risk_return_plot <- function(performance_data, title = "Risk vs. Return") {
  if(is.null(performance_data) || nrow(performance_data) == 0) {
    return(plot_ly() %>%
             layout(title = paste(title, "- No data available"),
                    showlegend = FALSE))
  }

  p <- plot_ly() %>%
    add_trace(
      x = performance_data$Volatility,
      y = performance_data$Return_Pct,
      type = 'scatter',
      mode = 'markers+text',
      text = performance_data$Symbol,
      textposition = 'top center',
      marker = list(
        size = 10,
        opacity = 0.8
      )
    ) %>%
    layout(
      title = title,
      xaxis = list(title = "Risk (Volatility %)"),
      yaxis = list(title = "Return (%)"),
      hovermode = "closest"
    )

  return(p)
}

# Function to create prediction plot
create_prediction_plot <- function(historical_data, prediction_data, title = "Model Prediction") {
  if(is.null(historical_data) || is.null(prediction_data)) {
    return(plot_ly() %>%
             layout(title = paste(title, "- No data available"),
                    showlegend = FALSE))
  }

  p <- plot_ly() %>%
    # Add historical data
    add_trace(
      x = historical_data$date,
      y = historical_data$Close,
      type = 'scatter',
      mode = 'lines',
      name = 'Historical Data',
      line = list(color = 'blue')
    ) %>%
    # Add prediction data
    add_trace(
      x = prediction_data$date,
      y = prediction_data$prediction,
      type = 'scatter',
      mode = 'lines',
      name = 'Prediction',
      line = list(color = 'red', dash = 'dash')
    ) %>%
    # Add prediction interval if available
    add_ribbons(
      x = prediction_data$date,
      ymin = prediction_data$lower_ci,
      ymax = prediction_data$upper_ci,
      name = '95% Confidence Interval',
      line = list(color = 'rgba(0,0,0,0)'),
      fillcolor = 'rgba(255,0,0,0.2)',
      showlegend = TRUE
    ) %>%
    layout(
      title = title,
      xaxis = list(title = "Date"),
      yaxis = list(title = "Value"),
      hovermode = "closest"
    )

  return(p)
}
