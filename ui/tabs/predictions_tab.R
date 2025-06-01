predictions_tab <- tabItem(tabName = "predictions",
  h2("Model predykcji"),
  fluidRow(
    box(title = "Konfiguracja", width = 12, status = "primary",
        fluidRow(
          column(4, selectInput(
            inputId = "market",
            label = "Wybierz instrument finansowy",
            choices = list(
              "Indeks" = c("S&P 500" = "^GSPC", "DAX" = "^GDAXI", "FTSE" = "^FTSE",
                          "CAC 40" = "^FCHI", "EURO STOXX 50" = "^STOXX50E"),
              "Akcje" = c("Apple" = "AAPL", "Nvidia" = "NVDA", "Microsoft" = "MSFT",
                          "Google" = "GOOG", "Meta" = "META"),
              "Waluta" = c("Euro -> Dolar" = "EURUSD=X", "Funt -> Dolar" = "GBPUSD=X",
                          "Euro -> Funt" = "EURGBP=X", "Euro -> Złoty" = "EURPLN=X",
                          "Dolar -> Złoty" = "USDPLN=X", "Funt -> Złoty" = "GBPPLN=X")
            ),
            selected = NULL,
            multiple = FALSE
          )),
          column(4, selectInput("predictor", "Wybierz model:",
                                choices = c("Model naiwny" = "naive",
                                            "ARIMA" = "ARIMA",
                                            "Las losowy na opóźnieniach" = "random_forest",
                                            "XGBoost na opóźnieniach" = "xgb_boost"))),
          column(4, sliderInput("pred_horizon", "Okres prognozowania w wybranych dla interwału jednostkach:",
                                min = 7, max = 60, value = 14))
        ),
        actionButton("run_model", "Zbuduj model predykcyjny", icon = icon("gears"))
    )
  ),

  fluidRow(
    box(title = "Wyniki modelu", width = 12, status = "info",
        plotlyOutput("prediction_plot", height = 400)
    )
  ),

  fluidRow(
    box(title = "Dokładność modelu", width = 6, status = "success",
        DTOutput("prediction_accuracy_table")
    ),
    box(title = "Wyniki predykcji", width = 6, status = "info",
        plotlyOutput("prediction_table", height = 300)
    )
  )
)
