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
                fluidRow(
                    column(12, "Since the start of the pandemic, NHS Scotland has had to adjust and adapt to unforeseen and changing circumstances on a continual basis to meet the ever changing demand and challenges facing it. While the pandemic has been an unforeseen pressure, one predictable pressure is the winter period and effect it has on unscheduled care and subsequent capacity and demand on hospital services in the NHS.", tags$br(), tags$br(),

"Hospital services make up a considerable part of the whole healthcare system and as such draw much of the attention in terms of the statistics in the public domain.", tags$br(), tags$br(),

"We would like you to create a dashboard using R-Shiny and PHS open data on secondary care to investigate what has been happening from 2020 onwards in the acute hospital sector. We would like you to use the data and visualisations in your dashboard to tell the story of and weave your narrative of the pandemic and its impact on the secondary care sector in NHS Scotland.", tags$br(), tags$br(),

"More specifically, we would like to consider the impact that winter may have on health care, primarily the hospital (acute care) sector and try to answer both of the following questions;", tags$br(), tags$br(),

"1. To what extent are the 'winter crises' reported by the media real?", tags$br(),
"2. How has the Covid-19 pandemic affected provision of acute care in Scotland?"), tags$br()
                )
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