library(shiny)
library(plotly)

ui <- fluidPage(
    
    fluidRow(selectInput(inputId = "ae_att_hb_input",
                         label = "Health Board",
                         choices = sort(append(
                             iconv(unique(ae_activity$hb_name), "latin1", "ASCII", sub=" "),
                             "All")))
    ),
    
    fluidRow(plotOutput("ae_attendance")
    ),
    
    fluidRow(selectInput(inputId = "beds_specialty_input",
                         label = "Specialty",
                         choices = sort(unique(beds$specialty_name)))
    ),
    
    fluidRow(plotlyOutput("beds")
    ),
    
    fluidRow(plotlyOutput("beds2")
    )
)

server <- function(input, output) {
    
    output$ae_attendance <- renderPlot({
        
        if (input$ae_att_hb_input != "All"){
            
            ae_activity %>% 
                filter(hb_name == input$ae_att_hb_input) %>% 
                group_by(hb_name, year, month) %>% 
                summarise(number_of_attendances_aggregate = sum(number_of_attendances_aggregate), .groups = "drop") %>% 
                ggplot(aes(x = month, y = number_of_attendances_aggregate)) +
                geom_point(aes(group = hb_name)) +
                geom_line(aes(group = hb_name)) +
                theme(legend.position = "bottom", legend.title = element_blank()) +
                ggtitle(case_when(
                    input$ae_att_hb_input == "All" ~ "NHS Scotland Aggregate A&E Attendance per Month",
                    TRUE ~ paste0("NHS ", input$ae_att_hb_input, " Aggregate A&E Attendance per Month"))) +
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
    
    output$beds <- renderPlotly({
        
        ggplotly(beds %>% 
                     filter(specialty_name == case_when(
                         input$beds_specialty_input == "All" ~ specialty_name,
                         TRUE ~ input$beds_specialty_input)) %>% 
                     group_by(year, quarter, specialty_name) %>% 
                     summarise(all_staffed_beddays = sum(all_staffed_beddays),
                               total_occupied_beddays = sum(total_occupied_beddays),
                               empty_beds = sum(all_staffed_beddays - total_occupied_beddays)) %>% 
                     ggplot(aes(x = quarter)) +
                     geom_point(aes(y = total_occupied_beddays, colour = year)) +
                     geom_line(aes(y = total_occupied_beddays, colour = year, group = year)) +
                     scale_y_continuous(labels = scales::label_comma()) +
                     theme(legend.position = "bottom"))
        
    })
    
    output$beds2 <- renderPlotly({
        
        ggplotly(beds %>%
        group_by(yq, specialty_name) %>% 
        summarise(all_staffed_beddays = sum(all_staffed_beddays),
                  total_occupied_beddays = sum(total_occupied_beddays),
                  percentage_occupancy = mean(percentage_occupancy, na.rm = TRUE)) %>% 
        ggplot(aes(x = yq)) +
        geom_point(aes(y = percentage_occupancy, group = specialty_name, colour = specialty_name)) +
        geom_line(aes(y = percentage_occupancy, group = specialty_name, colour = specialty_name)) +
        geom_rect(aes(xmin = "2017Q3", xmax = "2018Q1", ymin = min(percentage_occupancy), ymax = Inf), alpha = 0.005) +
        geom_rect(aes(xmin = "2018Q3", xmax = "2019Q1", ymin = min(percentage_occupancy), ymax = Inf), alpha = 0.005) +
        geom_rect(aes(xmin = "2019Q3", xmax = "2020Q1", ymin = min(percentage_occupancy), ymax = Inf), alpha = 0.005) +
        geom_rect(aes(xmin = "2020Q3", xmax = "2021Q1", ymin = min(percentage_occupancy), ymax = Inf), alpha = 0.005) +
        geom_rect(aes(xmin = "2021Q3", xmax = "2022Q1", ymin = min(percentage_occupancy), ymax = Inf), alpha = 0.005) +
        scale_y_continuous(labels = scales::label_comma()) +
        labs(y = "Occupancy %",
             x = "Year, Quarter") +
        theme(axis.text.x = element_text(angle = 90),
              legend.position = "hidden"))
    })
    
}

shinyApp(ui, server)
