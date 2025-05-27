# Load global variables and functions
source("global.R")

# Source UI components
source("ui/sidebar.R")
source("ui/ui.R")

# Source server logic
source("server/server.R")

# Run the application
shinyApp(ui = ui, server = server)
