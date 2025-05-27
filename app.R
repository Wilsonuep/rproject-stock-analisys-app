library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(plotly) 
library(quantmod)
library(eurostat)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(corrplot)
library(scales)
library(PerformanceAnalytics)
library(zoo)
library(xts)

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "Aplikacja analizująca instrumenty finansowe"),
    
    dashboardSidebar(
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
    ),
    dashboardBody(
      shinyjs::useShinyjs(),
      
      tabItems(
        # Overview
        tabItem(tabName = "overview",
                h2("Giełdowy Dashboard"),
                fluidRow(
                  #TO-DO: Napisać funkcję znajdującą największą zmianę między wynikami i wypluwa % zmiane z nazwą
                  infoBoxOutput("biggest_index_change", width = 4), 
                  infoBoxOutput("biggest_stock_change", width = 4),
                  infoBoxOutput("biggest_currency_change", width = 4)
                ),
                
                fluidRow(
                  box(title = "Wyniki - Giełda Indeksów", status = "primary", solidHeader = TRUE, collapsible = FALSE, width = 12,
                      plotlyOutput("overview_index_plot", height = 200))
                  ),
                fluidRow(
                  box(title = "Wyniki - Giełda Akcji", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                      plotlyOutput("overview_stock_plot", height = 200))
                ),
                fluidRow(
                  box(title = "Wyniki - Giełda Walut", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                      plotlyOutput("overview_relationship_plot", height = 200))
                )
        ),
        
        # Eksplorator danych
        tabItem(tabName = "data",
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
        ),
        
        # Giełda indeksów
        tabItem(tabName = "indices_performance",
                h2("Analiza wyników indeksów"),
                fluidRow(
                  box(title = "Porównanie wyników indeksów", status = "primary", width = 12,
                      selectInput("norm_method", "Metoda normalizacji:", 
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
                      selectInput("perf_period", " Długość porównania:", 
                                  choices = c("1-Dzień" = "1d", "1-Tydzień" = "1w", "1-Miesiąc" = "1m", "3-Miesiąc" = "3m", "1-rok" = "1y")), #Definicja długości
                      DTOutput("index_performance_table")
                  ),
                  box(title = "Risk Analysis", width = 6, status = "warning",
                      plotlyOutput("index_risk_return_plot", height = 300)
                  )
                )
        ),
        # Giełda akcji
        tabItem(tabName = "stock_performance",
                h2("Analiza wyników ackji"),
                fluidRow(
                  box(title = "Porównanie wyników akcji", status = "primary", width = 12,
                      selectInput("norm_method", "Metoda normalizacji:", 
                                  choices = c("Zmiana procentowa" = "percent_change", 
                                              "Z-Score" = "z_score", 
                                              "Min-Max" = "min_max")),
                      plotlyOutput("stock_market_performance_plot", height = 500)
                  )
                ),
                fluidRow(
                  infoBoxOutput("biggest_stock_gain", width = 6), 
                  infoBoxOutput("biggest_stock_lose", width = 6),
                ),
                fluidRow(
                  box(title = "Wyniki akcji", width = 6, status = "info",
                      selectInput("perf_period", " Długość porównania:", 
                                  choices = c("1-Dzień" = "1d", "1-Tydzień" = "1w", "1-Miesiąc" = "1m", "3-Miesiąc" = "3m", "1-rok" = "1y")), #Definicja długości
                      DTOutput("stock_performance_table")
                  ),
                  box(title = "Risk Analysis", width = 6, status = "warning",
                      plotlyOutput("stock_risk_return_plot", height = 300)
                  )
                )
        ),
        # Giełda walut
        tabItem(tabName = "curr_performance",
                h2("Analiza wyników walut"),
                fluidRow(
                  box(title = "Porównanie wyników walut", status = "primary", width = 12,
                      selectInput("norm_method", "Metoda normalizacji:", 
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
                      selectInput("perf_period", " Długość porównania:", 
                                  choices = c("1-Dzień" = "1d", "1-Tydzień" = "1w", "1-Miesiąc" = "1m", "3-Miesiąc" = "3m", "1-rok" = "1y")), #Definicja długości
                      DTOutput("curr_performance_table")
                  ),
                  box(title = "Risk Analysis", width = 6, status = "warning",
                      plotlyOutput("curr_risk_return_plot", height = 300)
                  )
                )
        ),
        
        # Model predykcji
        tabItem(tabName = "predictions",
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
                            "Waluta" = c("Euro -> Dolar" = "EURUSD=X", "Funt -> Dolar" = "GBPUSD=X", "Euro -> Funt" = "EURGBP=X",
                                         "Euro -> Złoty" = "EURPLN=X", "Dolar -> Złoty" = "USDPLN=X", "Funt -> Złoty" = "GBPPLN=X")
                          ),
                          selected = NULL,
                          multiple = FALSE
                          )
                        ),
                        column(4, selectInput("predicotr", "Wybierz model:", 
                                              choices = c("Model naiwny" = "naive", 
                                                          "ARIMA" = "ARIMA",
                                                          "Las losowy na opóźnieniach" = "random_forest",
                                                          "XGBoost na opóźnieniach" = "xgb_boost"))),
                        column(4, sliderInput("pred_horizon", "Okres prognozowania w dniach:", 
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
        ),
        
        # About Tab
        tabItem(tabName = "about",
                h2("O autorach"),
                box(width = 12,
                    h3("Aplikacja powstała na zaliczenia przedmiotu: Podstawy programowania z R"),
                    p("Aplikacja służy do analizy wybranych insturmentów finansowych, monitorowania ich wartości oraz 
                    predykcji przyszłych za pomocą różnych modeli predykcji. (\n)
                      Modele zostały dobrane na podstawie dokładności w predykcji na podstawie jednej cechy oraz niskich wymagań założeń"),
                    p("Autorzy"),
                    tags$ul(
                      tags$li("Piotr Wilma (124832)"),
                      tags$li("Mikołaj Kaczmarek (124942)"),
                      tags$li("Wiktor Pietrzyński (125134)"),
                      tags$li("Łukasz Dębski (126759)"),
                      tags$li("Marek Halber (122814)"),
                    ),
                    h4("Źródło danych: Yahoo Finance (via quantmod package)"),
                )
        )
      )
    )
)

# Jakiś default wygenerowany serwer skrypt poniżej
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
