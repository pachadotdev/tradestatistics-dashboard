# Read credentials from file excluded in .gitignore
readRenviron("/tradestatistics/credentials.txt")

ui <- tradestatisticsdashboard::app_ui()
server <- tradestatisticsdashboard::app_server
