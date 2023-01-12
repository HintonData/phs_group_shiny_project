library(shinydashboard)
library(tidyverse)
library(leaflet)
library(lubridate)
library(sf)


server <- function(input, output, session) {
    
    # reactive data ------------------------------------------------------   
    demo_clean <- reactive({
        
        
        if (input$findings_hb_input != "All"){
            
            as_demo_clean %>% filter(hb_name == input$findings_hb_input,
                                     admission_type == "All Inpatients")
        }
        else
        {
            as_demo_clean %>% filter(admission_type == "All Inpatients")
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
    
    depriv_reactive <- reactive({
        
        
        if (input$findings_hb_input != "All"){
            
        demo_deprivation_clean %>% 
            filter(hb_name == input$findings_hb_input,
                   admission_type == "All Inpatients",
                   simd != "NA")
        }
        
        else
        {
            demo_deprivation_clean
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
    output$demo_graph_gender <- renderPlot({
        
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
            group_by(quarter, sex) %>% 
            summarise(total_age_sex = sum(episodes)) %>% 
            ggplot(aes(x = quarter, y = total_age_sex)) +
            geom_line(aes(colour = sex, group = sex)) +
            geom_point(aes(colour = sex)) +
            theme_classic() +
            scale_y_continuous(labels = scales::comma) +
            labs(title = "Nationwide Hospital Attendances (Scotland)",
                 y = "Hospital Attendances") +
            ggtitle(case_when(
                input$findings_hb_input == "All" ~ "Nationwide Hospital Attendances, Sex (Scotland)",
                TRUE ~ paste0("Hospital Attendances, Sex (NHS ", input$findings_hb_input, ")"))) +
            theme_classic() +
            theme(axis.text.x = element_text(angle = 45,
                                             hjust = 1,
                                             size = 12),
                  axis.title.x = element_blank(),
                  axis.text.y = element_text(size = 12),
                  legend.position = "top",
                  legend.title = element_blank(),
                  legend.key.width = unit(2, "cm"),
                  legend.key.height = unit(1, "cm"),
                  legend.text = element_text(size = 12))
        
    })
    
    output$demo_graph_age <- renderPlot({
        
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
            group_by(quarter, age) %>% 
            summarise(total_age_sex = sum(episodes)) %>% 
            ggplot(aes(x = quarter, y = total_age_sex)) +
            geom_line(aes(colour = age, group = age)) +
            geom_point(aes(colour = age)) +
            scale_y_continuous(labels = scales::comma) +
            labs(title = "Nationwide Hospital Attendances (Scotland)",
                 y = "Hospital Attendances") +
            ggtitle(case_when(
                input$findings_hb_input == "All" ~ "Nationwide Hospital Attendances, Age Group (Scotland)",
                TRUE ~ paste0("Hospital Attendances, Age Group (NHS ", input$findings_hb_input, ")"))) +
            theme_classic() +
            theme(axis.text.x = element_text(angle = 45,
                                             hjust = 1,
                                             size = 12),
                  axis.title.x = element_blank(),
                  axis.text.y = element_text(size = 12),
                  legend.position = "bottom",
                  legend.title = element_blank(),
                  legend.key.size = unit(1, "cm"),
                  legend.text = element_text(size = 12))
        
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
            ggplot(aes(x = is_covid_year, y = proportion, fill = sex)) +
            geom_col() +
            #adds label to each column
            geom_label(aes(label = scales::percent(proportion, accuracy = 0.1)), 
                       position = position_stack(), vjust = 0) +
            geom_label(aes(label = scales::percent(proportion, accuracy = 0.1)), 
                       position = position_stack(), vjust = 3, fill = "white") +
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
                input$findings_hb_input == "All" ~ "Nationwide Proportion of Attendances by Sex (Scotland)",
                TRUE ~ paste0("Proportion of attendances by Sex (NHS ", input$findings_hb_input, ")")))
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
    
    # deprivation graphs -------------------------------------------------------
    
    output$demo_prop_depriv <- renderPlot({
        
        depriv_reactive() %>% 
        group_by(simd, quarter) %>% 
        summarise(total = sum(episodes)) %>% 
        ggplot(aes(x = quarter, y = total)) +
        geom_line(aes(colour = simd, group = simd))
        
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
                labs(y = "Number of attendences",
                     x = "\nMonth") +
                theme_classic() +
                theme(axis.text = element_text(size = 12),
                      strip.text = element_text(size = 12))
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
                labs(y = "Number of attendences",
                     x = "\nMonth") +
                theme_classic() +
                theme(axis.text = element_text(size = 12),
                      strip.text = element_text(size = 12))
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
                input$findings_hb_input == "All" ~ "Weekly Hospital Admissions, 2020 onwards (NHS Scotland)",
                TRUE ~ paste0("Weekly Hospital Admissions, 2020 onwards (NHS ", input$findings_hb_input, ") ")))+
            labs(y = "Number of Admissions",
                 x = "Week Ending") +
            theme_classic() +
            theme(axis.text = element_text(size = 12),
                  axis.title.x = element_blank())
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
                TRUE ~ NA_real_)) %>% 
            mutate(label = ifelse(is.na(diff) == FALSE, paste0("<-  ", diff,"%  ->"), NA_character_))
    }
    
    
    output$bed_occupancy <- renderPlot({
        
        beds_reactive() %>% 
            group_by(yq) %>% 
            summarise(all_staffed_beddays = sum(all_staffed_beddays),
                      total_occupied_beddays = sum(total_occupied_beddays),
                      empty_beds = sum(all_staffed_beddays - total_occupied_beddays)) %>% 
            ws_set_highlights(total_occupied_beddays) %>% 
            ggplot(aes(x = yq)) +
            #       geom_point(aes(y = all_staffed_beddays, group = 1)) +
            #       geom_line(aes(y = all_staffed_beddays, group = 1, colour = "Staffed Bed Days")) +
            geom_point(aes(y = total_occupied_beddays), colour = "steelblue") +
            geom_line(aes(y = total_occupied_beddays, group = 1), colour = "steelblue") +
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
            geom_label(aes(y = max(total_occupied_beddays * 0.75), x = yq, label = label)) +
            scale_y_continuous(labels = scales::label_comma()) +
            labs(y = "Number of Bed Days\n",
                 x = "\nYear, Quarter",
                 title = "Occupied Bed Days per Quarter, Highlighting Half Year") +
            theme_classic() +
            theme(axis.text.x = element_text(angle = 45,
                                             hjust = 1,
                                             size = 12),
                  axis.text.y = element_text(size = 12),
                  legend.position = "hidden",
                  legend.title = element_blank(),
                  legend.key.width = unit(2, "cm"),
                  legend.key.height = unit(1, "cm"),
                  legend.text = element_text(size = 12))
        
    })
    
    output$stay_length <- renderPlot({
        
        demo_clean() %>% 
            dplyr::rename(yq = quarter) %>% 
            filter(admission_type == "All Inpatients") %>% 
            group_by(yq, is_covid_year) %>% 
            summarise(episodes = sum(episodes),
                      average_length_of_episode = round(mean(average_length_of_episode, na.rm = TRUE), 2),
                      average_length_of_stay = round(mean(average_length_of_stay, na.rm = TRUE), 2), .groups = "drop") %>% 
            ws_set_highlights(average_length_of_stay) %>% 
            ggplot(aes(x = yq, y = average_length_of_stay)) +
            geom_point(colour = "steelblue") +
            geom_line(aes(group = 1), colour = "steelblue") +
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
            geom_label(aes(y = mean(average_length_of_stay), x = yq, label = label)) +
            labs(y = "Length of Stay (days)\n",
                 x = "\nYear, Quarter",
                 title = "Length of In-Patient Stay, Highlighting Half-Year") +
            theme_classic() +
            theme(axis.text.x = element_text(angle = 45,
                                             hjust = 1,
                                             size = 12),
                  axis.text.y = element_text(size = 12),
                  legend.position = "hidden",
                  legend.title = element_blank(),
                  legend.key.width = unit(2, "cm"),
                  legend.key.height = unit(1, "cm"),
                  legend.text = element_text(size = 12))
        
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
            geom_col(aes(y = number_of_attendances_aggregate, fill = "2")) +
            geom_col(aes(fill = "1")) +
             ggtitle(case_when(
                 input$findings_hb_input == "All" ~ "NHS Scotland Aggregate A&E Attendance vs Wait Target, per Month",
                 TRUE ~ paste0("NHS ", input$findings_hb_input, " Aggregate A&E Attendance vs Wait Target, per Month"))) +
             scale_y_continuous(labels = scales::label_comma()) +
             scale_fill_manual(labels = c("Met Target", "Target Missed"), values = c("#4DAF4A", "#E41A1D")) +
             facet_wrap(~year, scales = "free_x") +
             labs(y = "No. Attendances",
                  x = "\nMonth") +
            theme_classic() +
            theme(axis.text = element_text(size = 12),
                  strip.text = element_text(size = 12),
                  legend.position = "top",
                  legend.title = element_blank(),
                  legend.key.size = unit(0.8, "cm"),
                  legend.text = element_text(size = 12))
        
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
            mutate(Colour = ifelse(proportion > 0.9, "#4DAF4A", "#E41A1D"))
        
        ggplot(wt_targets) +
            geom_bar(aes(
                x = x_label,
                y = proportion,
                fill = Colour),
                stat = "identity"
            ) +
            geom_label(aes(y = proportion / 2, x = x_label, label = paste0("",round(proportion * 100, 2), "%")))+
            coord_flip() +
            scale_fill_identity()+
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
                    size = 11,
                    color = "black",
                    face = "bold"
                )
            )+
            ggtitle(case_when(
                input$findings_hb_input == "All" ~ "Nationwide Proportion of A&E attendences \nmeeting target of < 4hrs(Scotland)",
                TRUE ~ paste0("NHS ", input$findings_hb_input, " Proportion of A&E attendences \nmeeting target of < 4hrs")))
    })
    
    output$occupancy_kpi <- renderPlot({
        
        beds_reactive() %>% 
        mutate(is_covid = (year >= 2020)) %>% 
            group_by(year) %>% 
            summarise(occupancy_pct = mean(percentage_occupancy, na.rm = TRUE), is_covid) %>% 
            distinct() %>% 
            ggplot(aes(x = year, y = occupancy_pct, fill = is_covid)) +
            geom_col() +
            geom_label(aes(y = occupancy_pct, label = paste0(round(occupancy_pct, 2), "%")), fill = "white") +
            scale_x_continuous(breaks = c(2017:2022)) +
            scale_fill_manual(labels = c("Pre-Covid", "Covid"), values = c("#4DAF4A", "#E41A1D")) +
            labs(y = "Average Occupancy (%)\n",
                 x = "\nYear",
                 title = "Average Bed Occupancy per Year") +
            theme_classic() +
            theme(axis.text.x = element_text(size = 12),
                  axis.text.y = element_text(size = 12),
                  legend.position = "right",
                  legend.title = element_blank(),
                  legend.key.size = unit(1, "cm"),
                  legend.text = element_text(size = 12))
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
