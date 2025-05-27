data_tab <- tabItem(tabName = "data",
  h2("Eksplorator danych"),
  fluidRow(
    box(title = "Widok danych", width = 12, status = "primary",
        tabsetPanel(
          tabPanel("Indeksy", DTOutput("index_table")),
          tabPanel("Akcje", DTOutput("stock_table")),
          tabPanel("Waluty", DTOutput("currency_table"))
        )
    )
  )
)
