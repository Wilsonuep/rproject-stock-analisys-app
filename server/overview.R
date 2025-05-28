overview_server <- function(input, output, session) {
  # Reactive functions for data retrieval and processing
  indices_data <- reactive({
    # Validate input
    if(is.null(input$Indices) || length(input$Indices) == 0) {
      return(NULL)
    }
    if(is.null(input$date_range) || length(input$date_range) != 2) {
      return(NULL)
    }

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

  stocks_data <- reactive({
    # Validate input
    if(is.null(input$Stocks) || length(input$Stocks) == 0) {
      return(NULL)
    }
    if(is.null(input$date_range) || length(input$date_range) != 2) {
      return(NULL)
    }

    # Get data from Yahoo Finance
    start_date <- input$date_range[1]
    end_date <- input$date_range[2]

    # Try to load data
    tryCatch({
      data <- load_market_data(input$Stocks, start_date, end_date)
      return(data)
    }, error = function(e) {
      warning(paste("Error in stocks_data:", e$message))
      return(NULL)
    })
  })

  currencies_data <- reactive({
    # Validate input
    if(is.null(input$Currencies) || length(input$Currencies) == 0) {
      return(NULL)
    }
    if(is.null(input$date_range) || length(input$date_range) != 2) {
      return(NULL)
    }

    # Get data from Yahoo Finance
    start_date <- input$date_range[1]
    end_date <- input$date_range[2]

    # Try to load data
    tryCatch({
      data <- load_market_data(input$Currencies, start_date, end_date)
      return(data)
    }, error = function(e) {
      warning(paste("Error in currencies_data:", e$message))
      return(NULL)
    })
  })

  # Calculate largest changes for each type of financial instrument
  indices_changes <- reactive({
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

  stocks_changes <- reactive({
    data <- stocks_data()

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

  currencies_changes <- reactive({
    data <- currencies_data()

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

  # Biggest change info boxes
  output$biggest_index_change <- renderInfoBox({
    changes <- indices_changes()

    if(is.null(changes) || nrow(changes) == 0) {
      return(infoBox(
        "Indeksy",
        "Brak danych",
        icon = icon("chart-line"),
        color = "blue"
      ))
    }

    # Find the one with the largest absolute change
    largest_change_index <- which.max(abs(changes$change_pct))
    largest_change <- changes[largest_change_index, ]

    # Determine color based on positive or negative
    box_color <- ifelse(largest_change$change_pct >= 0, "green", "red")
    box_icon <- ifelse(largest_change$change_pct >= 0, "arrow-up", "arrow-down")

    # Format the percentage with sign
    formatted_pct <- paste0(
      ifelse(largest_change$change_pct >= 0, "+", ""),
      round(largest_change$change_pct, 2),
      "%"
    )

    infoBox(
      paste("Indeks:", largest_change$display_name),
      formatted_pct,
      icon = icon(box_icon),
      color = box_color
    )
  })

  output$biggest_stock_change <- renderInfoBox({
    changes <- stocks_changes()

    if(is.null(changes) || nrow(changes) == 0) {
      return(infoBox(
        "Akcje",
        "Brak danych",
        icon = icon("chart-line"),
        color = "blue"
      ))
    }

    # Find the one with the largest absolute change
    largest_change_index <- which.max(abs(changes$change_pct))
    largest_change <- changes[largest_change_index, ]

    # Determine color based on positive or negative
    box_color <- ifelse(largest_change$change_pct >= 0, "green", "red")
    box_icon <- ifelse(largest_change$change_pct >= 0, "arrow-up", "arrow-down")

    # Format the percentage with sign
    formatted_pct <- paste0(
      ifelse(largest_change$change_pct >= 0, "+", ""),
      round(largest_change$change_pct, 2),
      "%"
    )

    infoBox(
      paste("Akcje:", largest_change$display_name),
      formatted_pct,
      icon = icon(box_icon),
      color = box_color
    )
  })

  output$biggest_currency_change <- renderInfoBox({
    changes <- currencies_changes()

    if(is.null(changes) || nrow(changes) == 0) {
      return(infoBox(
        "Waluty",
        "Brak danych",
        icon = icon("chart-line"),
        color = "blue"
      ))
    }

    # Find the one with the largest absolute change
    largest_change_index <- which.max(abs(changes$change_pct))
    largest_change <- changes[largest_change_index, ]

    # Determine color based on positive or negative
    box_color <- ifelse(largest_change$change_pct >= 0, "green", "red")
    box_icon <- ifelse(largest_change$change_pct >= 0, "arrow-up", "arrow-down")

    # Format the percentage with sign
    formatted_pct <- paste0(
      ifelse(largest_change$change_pct >= 0, "+", ""),
      round(largest_change$change_pct, 2),
      "%"
    )

    infoBox(
      paste("Waluty:", largest_change$display_name),
      formatted_pct,
      icon = icon(box_icon),
      color = box_color
    )
  })

  # Overview plots
  output$overview_index_plot <- renderPlotly({
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
      normalized_data <- normalize_data(data, method = "percent_change")

      if(is.null(normalized_data) || nrow(normalized_data) == 0) {
        return(plot_ly() %>%
                 layout(title = "Błąd podczas normalizacji danych indeksów",
                        xaxis = list(title = ""),
                        yaxis = list(title = "")))
      }

      # Create performance comparison plot
      p <- plot_ly() %>%
        layout(title = "Porównanie wyników indeksów",
               xaxis = list(title = "Data"),
               yaxis = list(title = "Zmiana [%]"),
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

  output$overview_stock_plot <- renderPlotly({
    data <- stocks_data()

    if(is.null(data) || nrow(data) == 0) {
      return(plot_ly() %>%
               layout(title = "Brak danych dla akcji",
                      xaxis = list(title = ""),
                      yaxis = list(title = "")))
    }

    # Try to create normalized data and plot
    tryCatch({
      # Normalize data for comparison
      normalized_data <- normalize_data(data, method = "percent_change")

      if(is.null(normalized_data) || nrow(normalized_data) == 0) {
        return(plot_ly() %>%
                 layout(title = "Błąd podczas normalizacji danych akcji",
                        xaxis = list(title = ""),
                        yaxis = list(title = "")))
      }

      # Create performance comparison plot
      p <- plot_ly() %>%
        layout(title = "Porównanie wyników akcji",
               xaxis = list(title = "Data"),
               yaxis = list(title = "Zmiana [%]"),
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

  output$overview_currency_plot <- renderPlotly({
    data <- currencies_data()

    if(is.null(data) || nrow(data) == 0) {
      return(plot_ly() %>%
               layout(title = "Brak danych dla walut",
                      xaxis = list(title = ""),
                      yaxis = list(title = "")))
    }

    # Try to create normalized data and plot
    tryCatch({
      # Normalize data for comparison
      normalized_data <- normalize_data(data, method = "percent_change")

      if(is.null(normalized_data) || nrow(normalized_data) == 0) {
        return(plot_ly() %>%
                 layout(title = "Błąd podczas normalizacji danych walut",
                        xaxis = list(title = ""),
                        yaxis = list(title = "")))
      }

      # Create performance comparison plot
      p <- plot_ly() %>%
        layout(title = "Porównanie wyników walut",
               xaxis = list(title = "Data"),
               yaxis = list(title = "Zmiana [%]"),
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
}
