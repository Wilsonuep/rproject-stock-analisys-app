source("server/overview.R")
source("server/data.R")
source("server/indices.R")
source("server/stocks.R")
source("server/currency.R")
source("server/predictions.R")

server <- function(input, output, session) {
  # Call each module's server function
  overview_server(input, output, session)
  data_server(input, output, session)
  indices_server(input, output, session)
  stocks_server(input, output, session)
  currency_server(input, output, session)
  predictions_server(input, output, session)

  # Global reactive values and observers
  observeEvent(input$refresh_data, {
    # Data refresh logic
  })
}
