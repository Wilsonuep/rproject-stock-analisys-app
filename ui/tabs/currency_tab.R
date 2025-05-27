currency_tab <- tabItem(tabName = "curr_performance",
  h2("Analiza wyników walut"),
  fluidRow(
    box(title = "Porównanie wyników walut", status = "primary", width = 12,
        selectInput("curr_norm_method", "Metoda normalizacji:",
                    choices = c("Zmiana procentowa" = "percent_change",
                                "Z-Score" = "z_score",
                                "Min-Max" = "min_max")),
        plotlyOutput("curr_market_performance_plot", height = 500)
    )
  ),
  fluidRow(
    infoBoxOutput("biggest_curr_gain", width = 6),
    infoBoxOutput("biggest_curr_lose", width = 6),
  ),
  fluidRow(
    box(title = "Wyniki walutowe", width = 6, status = "info",
        selectInput("curr_perf_period", " Długość porównania:",
                    choices = c("1-Dzień" = "1d", "1-Tydzień" = "1w",
                                "1-Miesiąc" = "1m", "3-Miesiąc" = "3m",
                                "1-rok" = "1y")),
        DTOutput("curr_performance_table")
    ),
    box(title = "Risk Analysis", width = 6, status = "warning",
        plotlyOutput("curr_risk_return_plot", height = 300)
    )
  )
)
