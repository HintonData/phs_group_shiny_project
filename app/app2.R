library(shinydashboard)
library(tidyverse)
library(here)

as_demo_clean <- read_csv(here("data/clean_data/as_demo_clean.csv"))
hb_names <- read_csv(here("data/clean_data/hb_simple.csv"))
ae_activity <- read_csv(here("data/clean_data/ae_activity_clean.csv"))

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
                h1("Overview"),
                p(
                    "Since the start of the pandemic, NHS Scotland has had to adjust and adapt to unforeseen and changing circumstances on a continual basis to meet the ever changing demand and challenges facing it. While the pandemic has been an unforeseen pressure, one predictable pressure is the winter period and effect it has on unscheduled care and subsequent capacity and demand on hospital services in the NHS."
                ),
                
                p(
                    "Hospital services make up a considerable part of the whole healthcare system and as such draw much of the attention in terms of the statistics in the public domain."
                ),
                
                p(
                    "We have created a dashboard using R-Shiny and data from Public Health Scotland on secondary care to investigate what has been happening from 2020 onwards in the acute hospital sector. Using visualisations we will show our narrative of how the NHS coped during the pandemic."
                ),
                
                p(
                    "We have focused on the impact of winter on health care, mainly the hospital (acute care) sector and specifically these questions:"
                ),
                
                p("1. To what extent are the 'winter crises' reported by the media real?"),
                p("2. How has the Covid-19 pandemic affected provision of acute care in Scotland?")),
                
                tabItem(tabName = "findings",
                        
                        sidebarLayout(
                            sidebarPanel(width = 3,
                                         fluidRow(selectInput(inputId = "findings_hb_input",
                                                              label = "Health Board",
                                                              choices = sort(unique(hb_names$hb_name)))
                                         ),
                                         
                                         fluidRow()
                            ),
                            
                            mainPanel(
                                tabBox(id = "findings_tabs",
                                       width = 9,
                                       tabPanel("Demographics",
                                                plotOutput("demo_graph")),
                                       tabPanel("A&E",
                                                plotOutput("ae_attendance")))
                )),
                
                tabItem(tabName = "data_view",
                        
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

server <- function(input, output, session) {
    
    output$demo_graph <- renderPlot({
        
        demo_totals <- as_demo_clean %>% 
            mutate(
                #remove Year from Quarter column
                q = str_sub(quarter, -2, -1),
                #create a factor
                q = factor(
                    q,
                    levels = c("Q1", "Q2", "Q3", "Q4"),
                    ordered = TRUE
                ),
                year = yq(quarter),
                year = year(year)
            )
        
        demo_totals %>% 
            filter(hb_name == input$findings_hb_input) %>% 
            group_by(quarter) %>% 
            summarise(total_age_sex = sum(episodes)) %>% 
            ggplot(aes(x = quarter, y = total_age_sex, group = 1)) +
            geom_line(colour = "steelblue") +
            geom_point(colour = "steelblue") +
            geom_vline(xintercept = '2020Q1', color = 'black', linetype="dotted")+
            geom_text(aes(x='2020Q1', label="\nCovid", y=1500000), colour="red", angle=90, text=element_text(size=11)) +
            geom_text(aes(x='2020Q1', label="Pre-Covid\n", y=1500000), colour="blue", angle=90, text=element_text(size=11))+
            theme_classic() +
            scale_y_continuous(labels = scales::comma, 
                               expand = c(0, 0),
                               limits = c(0, NA)) +
            labs(title = "Nationwide Hospital Attendances (Scotland)",
                 y = "Hospital Attendances") +
            theme(axis.title.x = element_blank(),
                  axis.text.x = element_text(angle = 45, hjust = 1),
                  legend.position = "none")
        
        
        
    })
    
    output$ae_attendance <- renderPlot({
        
        if (input$findings_hb_input != "All"){
            
            ae_activity %>% 
                filter(hb_name == input$findings_hb_input) %>% 
                group_by(hb_name, year, month) %>% 
                summarise(number_of_attendances_aggregate = sum(number_of_attendances_aggregate), .groups = "drop") %>% 
                ggplot(aes(x = month, y = number_of_attendances_aggregate)) +
                geom_point(aes(group = hb_name)) +
                geom_line(aes(group = hb_name)) +
                theme(legend.position = "bottom", legend.title = element_blank()) +
                ggtitle(case_when(
                    input$findings_hb_input == "All" ~ "NHS Scotland Aggregate A&E Attendance per Month",
                    TRUE ~ paste0("NHS ", input$findings_hb_input, " Aggregate A&E Attendance per Month"))) +
                scale_y_continuous(labels = scales::label_comma()) +
                guides(fill = guide_legend(nrow = 5, ncol= 4, byrow = TRUE)) +
                facet_wrap(~year, scales = "free_x")
        }
        
        else {
            ae_activity %>% 
                group_by(year, month) %>% 
                summarise(number_of_attendances_aggregate = sum(number_of_attendances_aggregate), .groups = "drop") %>% 
                ggplot(aes(x = month, y = number_of_attendances_aggregate)) +
                geom_point(aes(group = 1)) +
                geom_line(aes(group = 1)) +
                theme(legend.position = "bottom", legend.title = element_blank()) +
                ggtitle("NHS Scotland Aggregate A&E Attendance per Month") +
                scale_y_continuous(labels = scales::label_comma()) +
                guides(fill = guide_legend(nrow = 5, ncol= 4, byrow = TRUE)) +
                facet_wrap(~year, scales = "free_x")
        }
        
    })
}

shinyApp(ui, server)