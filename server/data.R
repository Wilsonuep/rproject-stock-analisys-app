data_server <- function(input, output, session) {
  # Reactive functions for data retrieval
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

  currency_data <- reactive({
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

  # Table outputs
  output$index_table <- renderDT({
    data <- indices_data()
    if (!is.null(data)) {
      colnames(data) <- c("Data", "Otwarcie", "Najwyższa", "Najniższa", "Zamknięcie", "Wolumen", "Skorygowana", "Symbol")
    }
    datatable(data)
  })

  output$stock_table <- renderDT({
    data <- stocks_data()
    if (!is.null(data)) {
      colnames(data) <- c("Data", "Otwarcie", "Najwyższa", "Najniższa", "Zamknięcie", "Wolumen", "Skorygowana", "Symbol")
    }
    datatable(data)
  })

  output$currency_table <- renderDT({
    data <- currency_data()
    if (!is.null(data)) {
      colnames(data) <- c("Data", "Otwarcie", "Najwyższa", "Najniższa", "Zamknięcie", "Wolumen", "Skorygowana", "Symbol")
    }
    datatable(data)
  })
}
