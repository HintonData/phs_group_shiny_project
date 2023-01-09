library(shinydashboard)
library(tidyverse)

#ui stuff

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Overview", tabName = "overview"),
        menuItem("Findings", tabName = "findings"),
        menuItem("Data View", tabName = "data_view")
        
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "overview",
                h2("Overview"),
                p(
                    "Since the start of the pandemic, NHS Scotland has had to adjust and adapt to unforeseen and changing circumstances on a continual basis to meet the ever changing demand and challenges facing it. While the pandemic has been an unforeseen pressure, one predictable pressure is the winter period and effect it has on unscheduled care and subsequent capacity and demand on hospital services in the NHS."
                ),
                
                p(
                    "Hospital services make up a considerable part of the whole healthcare system and as such draw much of the attention in terms of the statistics in the public domain."
                ),
                
                p(
                    "We would like you to create a dashboard using R-Shiny and PHS open data on secondary care to investigate what has been happening from 2020 onwards in the acute hospital sector. We would like you to use the data and visualisations in your dashboard to tell the story of and weave your narrative of the pandemic and its impact on the secondary care sector in NHS Scotland."
                ),
                
                p(
                    "More specifically, we would like to consider the impact that winter may have on health care, primarily the hospital (acute care) sector and try to answer both of the following questions;"
                ),
                
                p(b("1. To what extent are the 'winter crises' reported by the media real?"),
                p(b("2. How has the Covid-19 pandemic affected provision of acute care in Scotland?"))
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