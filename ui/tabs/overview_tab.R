overview_tab <- tabItem(tabName = "overview",
                        h2("Giełdowy Dashboard"),
                        fluidRow(
                          box(title = "Największe zmiany", status = "info", solidHeader = FALSE,
                              collapsible = FALSE, width = 12,
                              fluidRow(
                                infoBoxOutput("biggest_index_change", width = 4),
                                infoBoxOutput("biggest_stock_change", width = 4),
                                infoBoxOutput("biggest_currency_change", width = 4)
                              ))
                        ),

                        fluidRow(
                          box(title = "Wyniki - Giełda Indeksów", status = "primary", solidHeader = TRUE,
                              collapsible = FALSE, width = 12,
                              plotlyOutput("overview_index_plot", height = 300))
                        ),
                        fluidRow(
                          box(title = "Wyniki - Giełda Akcji", status = "primary", solidHeader = TRUE,
                              collapsible = TRUE, width = 12,
                              plotlyOutput("overview_stock_plot", height = 300))
                        ),
                        fluidRow(
                          box(title = "Wyniki - Giełda Walut", status = "primary", solidHeader = TRUE,
                              collapsible = TRUE, width = 12,
                              plotlyOutput("overview_currency_plot", height = 300))
                        )
)
