stock_tab <- tabItem(tabName = "stock_performance",
  h2("Analiza wyników akcji"),
  fluidRow(
    box(title = "Porównanie wyników akcji", status = "primary", width = 12,
        selectInput("stocks_norm_method", "Metoda normalizacji:",
                    choices = c("Zmiana procentowa" = "percent_change",
                                "Z-Score" = "z_score",
                                "Min-Max" = "min_max")),
        plotlyOutput("stocks_market_performance_plot", height = 500)
    )
  ),
  fluidRow(
    infoBoxOutput("biggest_stock_gain", width = 6),
    infoBoxOutput("biggest_stock_lose", width = 6),
  ),
  fluidRow(
    box(title = "Wyniki akcji", width = 6, status = "info",
        selectInput("stocks_perf_period", " Długość porównania:",
                    choices = c("1-Tydzień" = "1w",
                                "1-Miesiąc" = "1m", "3-Miesiąc" = "3m",
                                "1-rok" = "1y")),
        DTOutput("stocks_performance_table")
    ),
    box(title = "Analiza ryzyka i zwrotu", width = 6, status = "warning",
        plotlyOutput("stocks_risk_return_plot", height = 300)
    )
  )
)
