### this is a shiny web app for binning date time data and taking summary statistics
### 
###
###         Created by:         Coy McNew           2020-08-11
###         Last edited by:     Coy McNew           2020-08-11

library(shiny)
library(shinyTime)
library(DT)
library(tidyverse)
library(lubridate)
library(openxlsx)

ui <- shinyUI(fluidPage(
    titlePanel("Binning and Summary Stats Tool"),
    tabsetPanel(
        tabPanel("Data Input",
                 titlePanel("Input Data & Selections"),
                 sidebarLayout(
                     sidebarPanel(
                         fileInput('file_input', 'Upload Data xlsx:',
                                   accept=c('.xlsx')),
                         
                         # added interface for uploading data from
                         # http://shiny.rstudio.com/gallery/file-upload.html
                         tags$br(),
                         selectInput('sheet_name', 'Sheet Name:', ""),
                         selectInput('date_time_col', 'Date Time Column:', ""),
                         selectInput('value_col', 'Value Column:', ""),
                         numericInput('bin_min', 'Bin Width (minutes):', 15, 1, 2400, step = 1),
                         dateInput('start_date', 'Start Date:'),
                         timeInput('start_time', 'Start Time:'),
                         actionButton('calculate', 'GO!', icon("calculator"))
                         
                     ),
                     mainPanel(
                         dataTableOutput('table')
                         # verbatimTextOutput('debug')
                     )
                 )
        )
    )
))

server <- shinyServer(function(input, output, session) {
    values <- reactiveValues(df_data = NULL, file_path = NULL, df_out = NULL)
    
    observeEvent(input$file_input, {
        values$file_path <- input$file_input$datapath
        sheets <- getSheetNames(input$file_input$datapath)
        updateSelectInput(session, inputId = 'sheet_name', label = 'Sheet Name:',
                          choices = sheets, select = sheets[1])
        values$df_out = NULL
    })
    observeEvent(input$sheet_name, {
        if(input$sheet_name != "") {
            values$df_data <- read.xlsx(values$file_path, sheet = input$sheet_name)
            updateSelectInput(session, inputId = 'date_time_col', label = 'Date Time Column:',
                              choices = names(values$df_data), select = names(values$df_data)[1])
            updateSelectInput(session, inputId = 'value_col', label = 'Value Column:',
                              choices = names(values$df_data), selected = names(values$df_data)[2])
        }
    })
    observeEvent(input$date_time_col, {
        updateDateInput(session, inputId = 'start_date', label = 'Start Date:',
                        value = tryCatch(
                            {
                                values$df_data %>% 
                                    pull(!!sym(input$date_time_col)) %>% 
                                    min() %>% convertToDateTime() %>%
                                    date()
                            },
                            error = function(cond) {
                                print("start_date error")
                                return(NA)
                            },
                            warnining = function(cond) {
                                print("start_date warning")
                                return(NA)
                            }
                            
                        )
        )
        updateTimeInput(session, inputId = 'start_time', label = 'Start Time:',
                        value = tryCatch(
                            {
                                values$df_data %>% 
                                    pull(!!sym(input$date_time_col)) %>% 
                                    min() %>% convertToDateTime() %>%
                                    floor_date("15 minutes")
                            },
                            error = function(cond) {
                                print("start_time error")
                                return(NULL)
                            },
                            warnining = function(cond) {
                                print("start_time warning")
                                return(NULL)
                            }
                            
                        ))
    })
    observeEvent(input$calculate, {
        temp <- values$df_data %>%
            select(!!sym(input$date_time_col), !!sym(input$value_col)) %>%
            # #fix date time
            mutate_at(vars(!!sym(input$date_time_col)), convertToDateTime) %>%
            #add na row at start_date_time_selection for binning
            bind_rows(
                tibble(
                    !!sym(input$date_time_col) := ymd_hms(
                        paste0(
                            as.character(input$start_date), " ", hour(input$start_time), ":", minute(input$start_time), ":", second(input$start_time)
                        ), tz = tz(input$start_time)
                    ), 
                    !!sym(input$value_col) := NA)
            ) %>% 
            arrange(!!sym(input$date_time_col)) %>%
            #bin data
            mutate(start_time = cut(!!sym(input$date_time_col), breaks = paste(input$bin_min, "mins"))) %>%
            #convert to date time
            mutate_at(vars(start_time), ymd_hms) %>%
            mutate(end_time = start_time + minutes(input$bin_min)) %>%
            #generate stats grouped by bin start time
            group_by(start_time) %>%
            mutate(max_value = max(!!sym(input$value_col), na.rm = TRUE), min_value = min(!!sym(input$value_col), na.rm = TRUE)) %>%
            ungroup() %>%
            #label rows as max or min
            mutate(
               is_max = ifelse(!!sym(input$value_col)==max_value, TRUE, FALSE),
               is_min = ifelse(!!sym(input$value_col)==min_value, TRUE, FALSE)
            )
        values$df_out <- left_join(
            temp %>%
                filter(is_max) %>%
                select(start_time, end_time, max_value, time_of_max_value = !!sym(input$date_time_col)),
            temp %>%
                filter(is_min) %>%
                select(start_time, end_time, min_value, time_of_min_value = !!sym(input$date_time_col)),
            by = c("start_time", "end_time")
        ) %>%
            mutate_at(vars(start_time, end_time, time_of_max_value, time_of_min_value), as.character)
    })
    output$table <- renderDataTable(
        if(is.null(values$df_out)) {
            values$df_data
        } else {
            values$df_out
        },
        rownames = FALSE, extensions = 'Buttons',
        options = list(
            dom = 'lBfrtip',
            buttons = c('csv', 'excel', 'pdf'),
            columnDefs = list(list(className = 'dt-center')),
            lengthMenu = c(10, 20, 30),
            pageLength = 15),
        server = FALSE
    )
    # output$debug <- renderPrint(
    # )
})

# Run the application 
shinyApp(ui = ui, server = server)
