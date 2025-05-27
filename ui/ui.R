# Source all tab files
source("ui/tabs/overview_tab.R")
source("ui/tabs/data_tab.R")
source("ui/tabs/indices_tab.R")
source("ui/tabs/stock_tab.R")
source("ui/tabs/currency_tab.R")
source("ui/tabs/predictions_tab.R")
source("ui/tabs/about_tab.R")

# Create the UI
ui <- dashboardPage(
  dashboardHeader(title = "Aplikacja analizująca instrumenty finansowe"),
  sidebar,  # From sidebar.R
  dashboardBody(
    shinyjs::useShinyjs(),
    tabItems(
      overview_tab,
      data_tab,
      indices_tab,
      stock_tab,
      currency_tab,
      predictions_tab,
      about_tab
    )
  )
)
