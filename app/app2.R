library(shinydashboard)
library(tidyverse)

#ui stuff

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Overview", tabName = "dashboard"),
        menuItem("Findings", tabName = "findings"),
        menuItem("Data View", tabName = "data_view")
        
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "dashboard",
                
        ),
        
        tabItem(tabName = "findings",
                
        ),
        
        tabItem(tabName = "data_view",
                
        )
    )
)


# UI
ui <- dashboardPage(
    skin = "blue",
    dashboardHeader(title = "PHS Dashboard"),
    sidebar,
    body
)

server <- function(input, output, session) {
    
    
}

shinyApp(ui, server)