library(shiny)
library(shinydashboard)
library(leaflet)

#ui stuff

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Overview", tabName = "overview"),
        menuItem("Findings", tabName = "findings"),
        menuItem("Data View", tabName = "data_view"),
        menuItem("Conclusions", tabName = "conclusion")
        
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "overview",
                h1("Overview"),
                p(
                    "As said previously, we have set out to answer two questions;"
                ),
                
                p(
                    "1. To what extent are the 'winter crises' reported by the media real?
"
                ),

p(
    "2. How has the Covid-19 pandemic affected provision of acute care in Scotland?"
),

p(
    "In this presentation, we will show how Covid has impacted the NHS, specifically in terms of hospital services. We will also present our analysis on the so-called ‘winter crises’ mentioned previously. Using data from Public Health Scotland, we will show our key insights on the temporal, demographic and geographic angles, as well as general trends, and give you our conclusions."
)),


tabItem(tabName = "findings",
        
        sidebarLayout(
            sidebarPanel(width = 3,
                         fluidRow(column(12,
                                         
                                         selectInput(inputId = "findings_hb_input",
                                                     label = "Health Board",
                                                     choices = hb_choices_basic)
                                         
                         )
                         ),
                         
                         fluidRow(column(12,leafletOutput("findings_minimap")))
            ),
            
            mainPanel(width = 9,
                      tabBox(id = "findings_tabs",
                             width = 12,
                             
                             tabPanel("Demographics - Age/Sex",
                                      fluidRow(column(12, 
                                                      plotOutput("demo_graph_gender"),
                                                      plotOutput("demo_graph_age")
                                      ))
                             ),
                             
                             tabPanel("Demographics - Deprivation",
                                      fluidRow(column(12, 
                                                      plotOutput("demo_prop_depriv")
                                      ))
                             ),
                             
                             tabPanel("A&E",
                                      plotOutput("ae_attendance")
                             ),
                             
                             tabPanel("Seasonal",
                                      fluidRow(column(12,plotOutput("bed_occupancy"))),
                                      fluidRow(column(12,plotOutput("stay_length")))
                             ),
                             
                             tabPanel("Covid Impacts - KPIs",
                                      fluidRow(column(12,
                                                      plotOutput("wait_overlay")
                                      )),
                                      fluidRow(column(6,
                                                      plotOutput("wait_targets")),
                                               column(6, 
                                                      plotOutput("occupancy_kpi"))),
                                      
                             ),
                             
                             tabPanel("Covid Impacts - Demographics",
                                      
                                      fluidRow(
                                          column(6, plotOutput("demo_graph_gender_prop")    
                                          ),
                                          column(6, plotOutput("demo_prop_change")
                                          )
                                      ),
                                      
                                      fluidRow(
                                          column(6, plotOutput("demo_prop_depriv_cov")),
                                          column(6, plotOutput("impact_pop_demos"))
                                      )),
                             
                             tabPanel("2020-",
                                      plotOutput("covid_graph")
                                      
                             )
                             
                             
                      )
                      
            )
        )),

tabItem(tabName = "data_view",
        (tabBox(id = "data_view_tabs",
                width = 16,
                tabPanel("Demographics",
                         dataTableOutput("demo_table")),
                
                tabPanel("A&E",
                         dataTableOutput("ae_table")),
                
                tabPanel("Covid Impacts",
                         dataTableOutput("covid_table")),
                tabPanel("Beds",
                         dataTableOutput("beds_table")))
        )
),

tabItem(tabName = "conclusion",
        h1("Conclusion"),
        p(
            "Winter spike:"
        ),
        
        p(
            "The winter spike is very real, and places great strain on the NHS system via more inpatients who generally stay for longer, driving up bed occupancy"
        ),
        
        p(
            "Covid Impact:"
        ),
        
        p(
            "Covid has had an impact across the whole of the National Health Service, with many more patients in hospitals leading to serious challenges in achieving wait targets. Covid itself has had an outsized effect on the elderly population"
            
        )
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