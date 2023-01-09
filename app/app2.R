library(shinydashboard)
library(tidyverse)
library(here)
library(leaflet)
library(lubridate)

#map test
library(sf)
library(plotly)

health_board_map <- st_read(dsn = here("data/clean_data/"),
                            layer = "health_board_map_simple")

as_demo_clean <- read_csv(here("data/clean_data/as_demo_clean.csv"))
hb_names <- read_csv(here("data/clean_data/hb_simple.csv"))
ae_activity <- read_csv(here("data/clean_data/ae_activity_clean.csv"))
covid_impacts <- read_csv(here("data/clean_data/covid_impacts_clean.csv"))

hb_choices <- sort(append(unique(hb_names$hb_name), "All"))

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
                                         fluidRow(column(12,selectInput(inputId = "findings_hb_input",
                                                              label = "Health Board",
                                                              choices = hb_choices))
                                         ),
                                         
                                         fluidRow(column(12,plotlyOutput("hb_map", height = "300px")))
                            ),
                            
                            mainPanel(
                                tabBox(id = "findings_tabs",
                                       width = 12,
                                      
                                       tabPanel("Demographics",
                                                fluidRow(column(12, 
                                                                plotOutput("demo_graph")
                                                        )),
                                                fluidRow(column(6,
                                                            plotOutput("demo_graph_gender_prop")    
                                                        ),
                                                        column(6,plotOutput("demo_prop_change")
                                                               ))
                                                ),
                                       
                                       tabPanel("A&E",
                                                plotOutput("ae_attendance")),
                                       
                                       tabPanel("Covid Impacts",
                                                plotOutput("covid_graph"))
                                       
                                       )
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
    
    demo_clean <- reactive({
        if (input$findings_hb_input != "All"){
            as_demo_clean %>% filter(hb_name == input$findings_hb_input)
        }
        else
        {
            as_demo_clean
        }
    })
    
    output$hb_map <- renderPlotly({
        
        p <- health_board_map %>%
            ggplot(aes(text = hb_name)) +
            geom_sf(fill = "gray90", col = "gray40", size = 0.1) +
            geom_sf(data = health_board_map,
                    fill = "orange", size = 0.1, colour = "white") +
            theme_void()
        
        if(input$findings_hb_input != "All"){
            p <- health_board_map %>%
                ggplot(aes(text = hb_name)) +
                geom_sf(fill = "gray90", col = "gray40", size = 0.1) +
                geom_sf(data = health_board_map %>% filter(hb_name == input$findings_hb_input),
                        fill = "steelblue", size = 0.1, colour = "white") +
                theme_void()
        }
        
        ggplotly(p,
                 tooltip = "text")
    })
    
    output$demo_graph <- renderPlot({
        
        demo_totals <- demo_clean() %>% 
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
            group_by(quarter) %>% 
            summarise(total_age_sex = sum(episodes)) %>% 
            ggplot(aes(x = quarter, y = total_age_sex, group = 1)) +
            geom_line(colour = "steelblue") +
            geom_point(colour = "steelblue") +
            theme_classic() +
            scale_y_continuous(labels = scales::comma, 
                               expand = c(0, 0),
                               limits = c(0, NA)) +
            labs(title = "Nationwide Hospital Attendances (Scotland)",
                 y = "Hospital Attendances") +
            theme(axis.title.x = element_blank(),
                  axis.text.x = element_text(angle = 45, hjust = 1),
                  legend.position = "none")+
            ggtitle(case_when(
                input$findings_hb_input == "All" ~ "Nationwide Hospital Attendances (Scotland)",
                TRUE ~ paste0("Hospital Attendances (NHS ", input$findings_hb_input, ")")))
        
    })
    
    output$demo_graph_gender_prop <- renderPlot({
        
        total_sex <- demo_clean() %>% 
            group_by(is_covid_year) %>% 
            summarise(total_episodes = sum(episodes))
        
        demo_clean() %>% 
            group_by(is_covid_year, sex) %>% 
            summarise(sum_episodes = sum(episodes)) %>% 
            left_join(total_sex, "is_covid_year") %>% 
            mutate(proportion = sum_episodes / total_episodes,
                   is_covid_year = factor(is_covid_year, c("Pre_Covid", "Covid"))) %>% 
            ggplot(aes(x = is_covid_year, y = proportion, fill = sex,
                       label = scales::percent(proportion, accuracy = 0.1))) +
            geom_col() +
            #adds label to each column
            geom_label(position = position_stack(vjust = 0.5), fill = "white") +
            scale_fill_brewer(palette = "Dark2")+
            labs(subtitle = "Pre-covid vs During Covid",
                 fill = "Sex")+
            #fix aesthetics (removing background etc (theme_void() doesn't remove everything correctly))
            theme(panel.background = element_blank(),
                  panel.grid = element_blank(),
                  axis.line = element_blank(),
                  axis.text.y = element_blank(),
                  axis.title = element_blank(),
                  axis.ticks = element_blank(),
                  axis.text.x = element_text(face = "bold", 
                                             size = 11, 
                                             colour = "black"))+
        ggtitle(case_when(
            input$findings_hb_input == "All" ~ "Nationwide Proportion of attendances by sex (Scotland)",
            TRUE ~ paste0("Proportion of attendances by sex (NHS ", input$findings_hb_input, ")")))
    })
    
    output$demo_prop_change <- renderPlot({
        total_pre_covid <- demo_clean() %>% 
            group_by(is_covid_year) %>% 
            summarise(total_episodes = sum(episodes))
        
        change_in_age <- demo_clean() %>% 
            group_by(is_covid_year, sex, age) %>%
            summarise(sum_episodes = sum(episodes)) %>%
            left_join(total_pre_covid, by = "is_covid_year") %>%
            mutate(
                sex = factor(sex, c("Male", "Female")),
                prop_age_group = if_else(
                    is_covid_year == "Pre_Covid",
                    sum_episodes / total_episodes,
                    sum_episodes / total_episodes
                )
            ) %>%
            select(-sum_episodes, -total_episodes) %>%
            pivot_wider(names_from = is_covid_year, values_from = prop_age_group) %>%
            mutate(diff = Covid - Pre_Covid,
                   is_positive = if_else(diff > 0, TRUE, FALSE)) %>%
            select(-Covid, -Pre_Covid)
        
        #graph
        ggplot(change_in_age, aes(x = age, y = diff, fill = is_positive)) +
            geom_col() +
            geom_hline(yintercept = 0) +
            geom_text(aes(label = scales::percent(diff, accuracy = 0.01), y = diff + 0.0015 * sign(diff))) +
            scale_fill_brewer(palette = "Set1")+
            labs(y = "Change (%)",
                 title = "Change in demographic proportions: Pre-Covid vs Covid")+
            facet_wrap(~ sex, ncol = 1)+
            theme_minimal()+
            theme(legend.position = "none",
                  axis.text.x = element_text(angle = 45, hjust = 1, size = 9, face = "bold"),
                  axis.title.x = element_blank(),
                  axis.line.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.line.y = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.title.y = element_blank(),
                  panel.background = element_blank(),
                  panel.grid = element_blank(),
                  strip.text.x = element_text(
                      size = 12, color = "black", face = "bold"
                  ),
                  strip.placement = "inside")+
            ggtitle(case_when(
                input$findings_hb_input == "All" ~ "Change in demographic proportions: Pre-Covid vs Covid",
                TRUE ~ paste0("Change in demographic proportions: Pre-Covid vs Covid (NHS ", input$findings_hb_input, ") ")))
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
    
    output$covid_graph <- renderPlot({
        
        covid_impacts %>%
            mutate(week_ending = ymd(week_ending)) %>% 
            group_by(week_ending) %>% 
            filter(sex == "All") %>% 
            summarise(sum_admissions = sum(number_admissions)) %>% 
            ggplot() +
            aes(x = week_ending, y = sum_admissions) +
            geom_line()
    })
    
    output$findings_minimap <- renderLeaflet({
        
        leaflet() %>% 
            addTiles()
    })
}

shinyApp(ui, server)