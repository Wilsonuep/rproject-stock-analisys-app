indices_tab <- tabItem(tabName = "indices_performance",
  h2("Analiza wyników indeksów"),
  fluidRow(
    box(title = "Porównanie wyników indeksów", status = "primary", width = 12,
        selectInput("indices_norm_method", "Metoda normalizacji:",
                    choices = c("Zmiana procentowa" = "percent_change",
                                "Z-Score" = "z_score",
                                "Min-Max" = "min_max")),
        plotlyOutput("index_market_performance_plot", height = 500)
    )
  ),
  fluidRow(
    infoBoxOutput("biggest_index_gain", width = 6),
    infoBoxOutput("biggest_index_lose", width = 6),
  ),
  fluidRow(
    box(title = "Wyniki indeksów", width = 6, status = "info",
        selectInput("indices_perf_period", " Długość porównania:",
                    choices = c("1-Tydzień" = "1w",
                                "1-Miesiąc" = "1m",
                                "3-Miesiąc" = "3m",
                                "1-rok" = "1y")),
        DTOutput("index_performance_table")
    ),
    box(title = "Analiza ryzyka i zwrotu", width = 6, status = "warning",
        plotlyOutput("index_risk_return_plot", height = 300)
    )
  )
)
