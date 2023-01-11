library(shinydashboard)
library(tidyverse)
library(here)
library(leaflet)
library(lubridate)
library(sf)
library(plotly)

health_board_map <- st_read(dsn = here("data/clean_data/"),
                            layer = "health_board_map_simple") %>% 
                    st_transform("+proj=longlat +datum=WGS84")

as_demo_clean <- read_csv(here("data/clean_data/as_demo_clean.csv"))
hb_names <- read_csv(here("data/clean_data/hb_simple.csv"))
hb_names_basic <- read_csv(here("data/clean_data/hb_basic.csv"))
ae_activity <- read_csv(here("data/clean_data/ae_activity_clean.csv"))
covid_impacts <- read_csv(here("data/clean_data/covid_impacts_clean.csv"))
beds <- read_csv(here("data/clean_data/beds_clean.csv"))

hb_choices <- sort(append(unique(hb_names$hb_name), "All"))
hb_choices_basic <- sort(append(unique(hb_names_basic$hb_name), "All"))

wt_basic <- read_csv(here("data/clean_data/waiting_times_basic.csv"))

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
                                                plotOutput("covid_graph")),
                                       
                                       tabPanel("Beds",
                                                fluidRow(plotOutput("bed_occupancy")),
                                                fluidRow(plotOutput("stay_length"))),
                                       
                                       tabPanel("KPI",
                                                fluidRow(column(12,
                                                                plotOutput("wait_overlay")
                                                                )),
                                                fluidRow(column(6,
                                                                "...."),
                                                         column(6, plotOutput("wait_targets")))
                                                )
                                       
                                       )
                ))),
                
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

# reactive data ------------------------------------------------------   
    demo_clean <- reactive({
        if (input$findings_hb_input != "All"){
            as_demo_clean %>% filter(hb_name == input$findings_hb_input)
        }
        else
        {
            as_demo_clean
        }
    })
    
    ae_reactive <- reactive({
        
        if (input$findings_hb_input != "All"){
            
            ae_activity %>% filter(hb_name == input$findings_hb_input)
        }
        else
        {
            ae_activity
        }
    })
    
    wt_reactive <- reactive({
        
        if (input$findings_hb_input != "All"){
            
            wt_basic %>% filter(hb_name == input$findings_hb_input)
        }
        else
        {
            wt_basic
        }
    })
    
    covid_clean <- reactive({
        if (input$findings_hb_input != "All"){
            covid_impacts %>% filter(hb_name == input$findings_hb_input)
        }
        else
        {
            covid_impacts
        }
    })
    
    beds_reactive <- reactive({
        
        if (input$findings_hb_input != "All"){
            beds %>% filter(hb_name == input$findings_hb_input)
        }
        
        else
        {
            beds
        }
    })
  
# map ---------------------------------------------------------------  
    map_reactive <- reactive({
        
        leaflet(options = leafletOptions(zoomControl = FALSE,
                                         attributionControl = FALSE,
                                         dragging = FALSE,
                                         tap = FALSE,
                                         touchZoom = FALSE,
                                         doubleClickZoom = FALSE,
                                         keyboard = FALSE)) %>% 
            addTiles()
        
    })
    
    output$findings_minimap <- renderLeaflet({
        
        if(input$findings_hb_input != "All"){
            
            map_reactive() %>% 
                addPolygons(data = health_board_map %>% 
                                filter(hb_name == input$findings_hb_input))
        }
        
        else{
            
            map_reactive() %>% 
                addPolygons(data = health_board_map)
            
        }})

# demo graphs ----------------------------------------------------------------   
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
            labs(y = "Change (%)")+
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
    

# a&e graphs -----------------------------------------------------------------    
    output$ae_attendance <- renderPlot({
        
        if (input$findings_hb_input != "All"){
            
            ae_reactive() %>% 
                group_by(hb_name, year, month) %>% 
                summarise(number_of_attendances_aggregate = sum(number_of_attendances_aggregate), .groups = "drop") %>% 
                ggplot(aes(x = month, y = number_of_attendances_aggregate)) +
                geom_point(aes(group = hb_name), colour = "steelblue") +
                geom_line(aes(group = hb_name), colour = "steelblue") +
                theme(legend.position = "bottom", legend.title = element_blank()) +
                ggtitle(paste0("Aggregate A&E Attendance per Month (NHS ", input$findings_hb_input, ") ")) +
                scale_y_continuous(labels = scales::label_comma()) +
                guides(fill = guide_legend(nrow = 5, ncol= 4, byrow = TRUE)) +
                facet_wrap(~year, scales = "free_x")+
                labs(y = "Number of attendences aggregate")
        }
        
        else {
            ae_reactive() %>% 
                group_by(year, month) %>% 
                summarise(number_of_attendances_aggregate = sum(number_of_attendances_aggregate), .groups = "drop") %>% 
                ggplot(aes(x = month, y = number_of_attendances_aggregate)) +
                geom_point(aes(group = 1), colour = "steelblue") +
                geom_line(aes(group = 1), colour = "steelblue") +
                theme(legend.position = "bottom", legend.title = element_blank()) +
                ggtitle("Nationwide Aggregate A&E Attendance per Month (Scotland)") +
                scale_y_continuous(labels = scales::label_comma()) +
                guides(fill = guide_legend(nrow = 5, ncol= 4, byrow = TRUE)) +
                facet_wrap(~year, scales = "free_x")+
                labs(y = "Number of attendences aggregate")
        }
        
    })

# covid graphs -------------------------------------------------------------
    output$covid_graph <- renderPlot({
        
        covid_clean() %>%
            mutate(week_ending = ymd(week_ending)) %>% 
            group_by(week_ending) %>% 
            filter(sex == "All") %>% 
            summarise(sum_admissions = sum(number_admissions)) %>% 
            ggplot() +
            aes(x = week_ending, y = sum_admissions) +
            geom_line(colour = "steelblue")+
            ggtitle(case_when(
                input$findings_hb_input == "All" ~ "Number of Covid Hostpial Admissions",
                TRUE ~ paste0("Number of Covid Hostpial Admissions (NHS ", input$findings_hb_input, ") ")))+
            labs(y = "Number of Admissions",
                 x = "Week Ending")
    })
    
# beds graphs --------------------------------------------------------------
    
    ws_set_highlights <- function(df,col_name){
      
      myenc <- enquo(col_name)

      df %>% 
        mutate(diff = case_when(
          yq == "2017Q4" ~ round(((lead(!!myenc) - lag(!!myenc, 1)) / lag(!!myenc, 1)), 4) * 100,
          yq == "2018Q2" ~ round(((lead(!!myenc) - lag(!!myenc, 1)) / lag(!!myenc, 1)), 4) * 100,
          yq == "2018Q4" ~ round(((lead(!!myenc) - lag(!!myenc, 1)) / lag(!!myenc, 1)), 4) * 100,
          yq == "2019Q2" ~ round(((lead(!!myenc) - lag(!!myenc, 1)) / lag(!!myenc, 1)), 4) * 100,
          yq == "2019Q4" ~ round(((lead(!!myenc) - lag(!!myenc, 1)) / lag(!!myenc, 1)), 4) * 100,
          yq == "2020Q2" ~ round(((lead(!!myenc) - lag(!!myenc, 1)) / lag(!!myenc, 1)), 4) * 100,
          yq == "2020Q4" ~ round(((lead(!!myenc) - lag(!!myenc, 1)) / lag(!!myenc, 1)), 4) * 100,
          yq == "2021Q2" ~ round(((lead(!!myenc) - lag(!!myenc, 1)) / lag(!!myenc, 1)), 4) * 100,
          yq == "2021Q4" ~ round(((lead(!!myenc) - lag(!!myenc, 1)) / lag(!!myenc)), 4) * 100,
          TRUE ~ NA_real_))
    }
    
    
    output$bed_occupancy <- renderPlot({
        
        beds_reactive() %>% 
        group_by(yq) %>% 
        summarise(all_staffed_beddays = sum(all_staffed_beddays),
                  total_occupied_beddays = sum(total_occupied_beddays),
                  empty_beds = sum(all_staffed_beddays - total_occupied_beddays)) %>% 
        ws_set_highlights(total_occupied_beddays) %>% 
        ggplot(aes(x = yq)) +
        geom_point(aes(y = all_staffed_beddays, group = 1)) +
        geom_line(aes(y = all_staffed_beddays, group = 1, colour = all_staffed_beddays)) +
        geom_point(aes(y = total_occupied_beddays, group = 1)) +
        geom_line(aes(y = total_occupied_beddays, colour = total_occupied_beddays, group = 1)) +
        geom_rect(aes(xmin = "2017Q3", xmax = "2018Q1", ymin = -Inf, ymax = Inf), alpha = 0.01) +
        geom_text(aes(y = max(total_occupied_beddays * 0.75), x = yq, label = diff)) +
        geom_vline(xintercept = "2017Q3", linetype = "dashed") +
        geom_vline(xintercept = "2018Q1", linetype = "dashed") +
        geom_rect(aes(xmin = "2018Q3", xmax = "2019Q1", ymin = -Inf, ymax = Inf), alpha = 0.01) +
        geom_vline(xintercept = "2018Q3", linetype = "dashed") +
        geom_vline(xintercept = "2019Q1", linetype = "dashed") +
        geom_rect(aes(xmin = "2019Q3", xmax = "2020Q1", ymin = -Inf, ymax = Inf), alpha = 0.01) +
        geom_vline(xintercept = "2019Q3", linetype = "dashed") +
        geom_vline(xintercept = "2020Q1", linetype = "dashed") +
        geom_rect(aes(xmin = "2020Q3", xmax = "2021Q1", ymin = -Inf, ymax = Inf), alpha = 0.01) +
        geom_vline(xintercept = "2020Q3", linetype = "dashed") +
        geom_vline(xintercept = "2021Q1", linetype = "dashed") +
        geom_rect(aes(xmin = "2021Q3", xmax = "2022Q1", ymin = -Inf, ymax = Inf), alpha = 0.01) +
        geom_vline(xintercept = "2021Q3", linetype = "dashed") +
        geom_vline(xintercept = "2022Q1", linetype = "dashed") +
        scale_y_continuous(labels = scales::label_comma()) +
        labs(y = "Number of Bed Days",
             x = "Year, Quarter") +
        theme(axis.text.x = element_text(angle = 90))
        
    })
    
    output$stay_length <- renderPlot({
        
        as_demo_clean %>% 
            dplyr::rename(yq = quarter) %>% 
        filter(admission_type == "All Inpatients") %>% 
        group_by(yq, is_covid_year) %>% 
        summarise(episodes = sum(episodes),
                  average_length_of_episode = round(mean(average_length_of_episode, na.rm = TRUE), 2),
                  average_length_of_stay = round(mean(average_length_of_stay, na.rm = TRUE), 2), .groups = "drop") %>% 
        ws_set_highlights(average_length_of_stay) %>% 
        ggplot(aes(x = yq, y = average_length_of_stay)) +
        geom_line(aes(group = 1)) +
        geom_text(aes(y = 6.5, label = diff)) +
        geom_rect(aes(xmin = "2017Q3", xmax = "2018Q1", ymin = -Inf, ymax = Inf), alpha = 0.01) +
        geom_vline(xintercept = "2017Q3", linetype = "dashed") +
        geom_vline(xintercept = "2018Q1", linetype = "dashed") +
        geom_rect(aes(xmin = "2018Q3", xmax = "2019Q1", ymin = -Inf, ymax = Inf), alpha = 0.01) +
        geom_vline(xintercept = "2018Q3", linetype = "dashed") +
        geom_vline(xintercept = "2019Q1", linetype = "dashed") +
        geom_rect(aes(xmin = "2019Q3", xmax = "2020Q1", ymin = -Inf, ymax = Inf), alpha = 0.01) +
        geom_vline(xintercept = "2019Q3", linetype = "dashed") +
        geom_vline(xintercept = "2020Q1", linetype = "dashed") +
        geom_rect(aes(xmin = "2020Q3", xmax = "2021Q1", ymin = -Inf, ymax = Inf), alpha = 0.01) +
        geom_vline(xintercept = "2020Q3", linetype = "dashed") +
        geom_vline(xintercept = "2021Q1", linetype = "dashed") +
        geom_rect(aes(xmin = "2021Q3", xmax = "2022Q1", ymin = -Inf, ymax = Inf), alpha = 0.01) +
        geom_vline(xintercept = "2021Q3", linetype = "dashed") +
        geom_vline(xintercept = "2022Q1", linetype = "dashed") +
        theme(axis.text.x = element_text(angle = 90)) 
        
        })
    
# wait times graphs ------------------------------------------------------
    
    output$wait_overlay <- renderPlot({
    
        ae_reactive() %>% 
        group_by(hb_name, year, month) %>% 
        summarise(number_of_attendances_aggregate = sum(number_of_attendances_aggregate),
                  number_meeting_target_aggregate = sum(number_meeting_target_aggregate),
                  attendance_greater8hrs = sum(attendance_greater8hrs),
                  attendance_greater12hrs = sum(attendance_greater12hrs),
                  .groups = "drop") %>% 
        ggplot(aes(x = month, y = number_meeting_target_aggregate)) +
        geom_col(aes(y = number_of_attendances_aggregate), fill = "red") +
        geom_col(fill = "green") +
        theme(legend.position = "bottom", legend.title = element_blank()) +
        ggtitle(case_when(
            input$findings_hb_input == "All" ~ "NHS Scotland Aggregate A&E Attendance vs Wait Target, per Month",
            TRUE ~ paste0("NHS ", input$findings_hb_input, " Aggregate A&E Attendance vs Wait Target, per Month"))) +
        scale_y_continuous(labels = scales::label_comma()) +
        guides(fill = guide_legend(nrow = 5, ncol= 4, byrow = TRUE)) +
        facet_wrap(~year, scales = "free_x")
        
        })
    
    output$wait_targets <- renderPlot({
        
        wt_targets <- wt_reactive() %>%
            group_by(is_covid_year) %>%
            summarise(
                attendance_sum = sum(total_attendances),
                wait_target = sum(wait_lt_4hrs)
            ) %>%
            pivot_longer(wait_target, names_to = "wait_time", values_to = "value") %>%
            mutate(proportion = value / attendance_sum) %>%
            mutate(target = ifelse(round(proportion * 100, 2) > 90, TRUE, FALSE),
                   x_label = ifelse(is_covid_year == TRUE, "Covid", "Pre Covid")) %>% 
            mutate(Colour = ifelse(proportion > 0.9, "green", "red"))
        
        
        ggplot(wt_targets) +
            geom_bar(aes(
                x = x_label,
                y = proportion,
                col = Colour),
                stat = "identity",
                position = "dodge"
            ) +
            geom_label(aes(y = proportion / 2, x = x_label, label = paste0("",round(proportion * 100, 2), "%")))+
            coord_flip() +
            scale_color_identity()+
            theme(
                legend.position = "none",
                axis.title.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.ticks.x = element_blank(),
                axis.text.x = element_blank(),
                axis.title.x = element_blank(),
                panel.background = element_blank(),
                panel.grid = element_blank(),
                axis.text.y = element_text(
                    size = 9,
                    color = "black",
                    face = "bold"
                )
            )+
            ggtitle(case_when(
                input$findings_hb_input == "All" ~ "Nationwide Proportion of A&E attendences \nmeeting target of < 4hrs(Scotland)",
                TRUE ~ paste0("NHS ", input$findings_hb_input, " Proportion of A&E attendences \nmeeting target of < 4hrs")))
    })
    
#page 3 (Datatables) -------------------------------------------------
    
    output$demo_table <- renderDataTable(
        
        as_demo_clean,
        options = list(pageLength = 10)
        
    )
    
    output$ae_table <- renderDataTable(
        
        ae_activity,
        options = list(pageLength = 10)
        
    )
    
    output$covid_table <- renderDataTable(
        
        covid_impacts,
        options = list(pageLength = 10)
        
    )
    
    output$beds_table <- renderDataTable(
        
        beds,
        options = list(pageLength = 10)
    )
    
}

shinyApp(ui, server)

