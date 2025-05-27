sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Overview", tabName = "overview", icon = icon("pie-chart")),
    menuItem("Eksplorator danych", tabName = "data", icon = icon("database")),
    menuItem("Giełda indeksów", tabName = "indices_performance", icon = icon("line-chart")),
    menuItem("Giełda akcji", tabName = "stock_performance", icon = icon("area-chart")),
    menuItem("Giełda walut", tabName = "curr_performance", icon = icon("money")),
    menuItem("Model predykcji", tabName = "predictions", icon = icon("chart-line")),
    menuItem("About", tabName = "about", icon = icon("info-circle"))
  ),
  br(),
  dateRangeInput("date_range", "Wybierz zakres:",
                 start = "2019-01-01",
                 end = Sys.Date(),
                 format = "yyyy-mm-dd"),

  h4("Wybór instrumentów finansowych"),
  selectInput(
    inputId = "Indices",
    label = "Wybierz indeksy:",
    choices = c("S&P 500" = "^GSPC", "DAX" = "^GDAXI", "FTSE" = "^FTSE",
                "CAC 40" = "^FCHI", "EURO STOXX 50" = "^STOXX50E"),
    multiple = TRUE
  ),
  selectInput(
    inputId = "Stocks",
    label = "Wybierz akcje:",
    choices = c("Apple" = "AAPL", "Nvidia" = "NVDA", "Microsoft" = "MSFT",
                "Google" = "GOOG", "Meta" = "META"),
    multiple = TRUE
  ),
  selectInput(
    inputId = "Currencies",
    label = "Wybierz waluty:",
    choices = c("Euro -> Dolar" = "EURUSD=X", "Funt -> Dolar" = "GBPUSD=X", "Euro -> Funt" = "EURGBP=X",
                "Euro -> Złoty" = "EURPLN=X", "Dolar -> Złoty" = "USDPLN=X", "Funt -> Złoty" = "GBPPLN=X"),
    multiple = TRUE
  ),
  br(),
  actionButton("refresh_data", "Odświeżanie danych", icon = icon("sync"))
)
