# app.R
# Interactive Time Series Data Visualization with Points, Error Bars, Equal Discrete Dates, and Internal Legend

if (!require(shiny)) install.packages("shiny")
if (!require(tidyverse)) install.packages("tidyverse") 
if (!require(plotly)) install.packages("plotly")        

library(shiny)
library(tidyverse)
library(plotly)

ui <- fluidPage(
    titlePanel("Interactive Time Series Data Visualization"),
    
    sidebarLayout(
        sidebarPanel(
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            tags$hr(),
            checkboxInput("header", "Header", TRUE),
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                         selected = ","),
            radioButtons("quote", "Quote",
                         choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
                         selected = '"'),
            
            tags$hr(),
            uiOutput("dateColumnSelect"),
            
            selectInput("dateFormat", "Select Data's Date Format:",
                        choices = c("YYYY-MM-DD" = "%Y-%m-%d",
                                    "MM/DD/YYYY" = "%m/%d/%Y",
                                    "DD-MM-YYYY" = "%d-%m-%Y",
                                    "YYYY/MM/DD" = "%Y/%m/%d",
                                    "Year Only (YYYY)" = "year_only"),
                        selected = "%Y-%m-%d"),
            
            uiOutput("valueColumnSelect"),
            uiOutput("categoryColumnSelect"),
            
            radioButtons("plot_option", "Plot by:",
                         choices = c("No Faceting / Color by Category" = "color",
                                     "Facet by Category" = "facet",
                                     "No Category" = "none"),
                         selected = "none"),
            
            tags$hr(),
            radioButtons("error_type", "Error Bars:",
                         choices = c("None" = "none",
                                     "Standard Error (SE)" = "se",
                                     "Standard Deviation (SD)" = "sd"),
                         selected = "se"),
            
            tags$hr(),
            textInput("xlab_text", "X-axis Label (optional):", value = ""),
            textInput("ylab_text", "Y-axis Label (optional):", value = ""),
            
            tags$hr(),
            uiOutput("dateRangeFilter")
        ),
        
        mainPanel(
            plotlyOutput("timeSeriesPlot", height = "600px")
        )
    )
)

server <- function(input, output, session) {
    
    data_input <- reactive({
        req(input$file1)
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df)
    })
    
    output$dateColumnSelect <- renderUI({
        df <- data_input()
        date_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x) || is.numeric(x))]
        selectInput("dateCol", "Select Date Column:", choices = date_cols)
    })
    
    output$valueColumnSelect <- renderUI({
        df <- data_input()
        numeric_cols <- names(df)[sapply(df, is.numeric)]
        selectInput("valueCols", "Select Value Column(s):", choices = numeric_cols, multiple = TRUE)
    })
    
    output$categoryColumnSelect <- renderUI({
        df <- data_input()
        non_numeric_cols <- names(df)[sapply(df, function(x) !is.numeric(x))]
        if (!is.null(input$dateCol)) {
            non_numeric_cols <- non_numeric_cols[non_numeric_cols != input$dateCol]
        }
        selectInput("categoryCol", "Select Categorical Variable:", choices = non_numeric_cols, selected = NULL)
    })
    
    plot_data <- reactive({
        df <- data_input()
        req(input$dateCol)
        req(input$valueCols)
        req(input$dateFormat)
        
        df_plot <- df %>% rename(Date = !!sym(input$dateCol))
        
        if (input$dateFormat == "year_only") {
            df_plot$Date <- as.Date(paste0(df_plot$Date, "-01-01"), format = "%Y-%m-%d")
        } else {
            df_plot$Date <- as.Date(df_plot$Date, format = input$dateFormat)
        }
        
        cols_to_select <- c("Date", input$valueCols)
        if (!is.null(input$categoryCol) && input$categoryCol != "" && input$plot_option != "none") {
            cols_to_select <- c(cols_to_select, input$categoryCol)
        }
        
        df_plot <- df_plot[, cols_to_select, drop = FALSE] %>% filter(!is.na(Date))
        
        if (!is.null(input$categoryCol) && input$categoryCol != "" && input$plot_option != "none") {
            df_plot <- df_plot %>% pivot_longer(cols = -c(Date, !!sym(input$categoryCol)), names_to = "Variable", values_to = "Value")
            group_vars <- c("Date", "Variable", input$categoryCol)
        } else {
            df_plot <- df_plot %>% pivot_longer(cols = -Date, names_to = "Variable", values_to = "Value")
            group_vars <- c("Date", "Variable")
        }
        
        df_plot <- df_plot %>%
            group_by(across(all_of(group_vars))) %>%
            summarize(
                Mean = mean(Value, na.rm = TRUE),
                SD = sd(Value, na.rm = TRUE),
                N = n(),
                SE = SD / sqrt(N),
                .groups = 'drop'
            ) %>%
            mutate(
                SD = ifelse(is.na(SD), 0, SD),
                SE = ifelse(is.na(SE), 0, SE)
            ) %>%
            arrange(Date)
        
        if (!is.null(input$dateRange)) {
            df_plot <- df_plot %>% filter(Date >= input$dateRange[1] & Date <= input$dateRange[2])
        }
        
        return(df_plot)
    })
    
    output$dateRangeFilter <- renderUI({
        df_plot_for_dates <- plot_data()
        if (nrow(df_plot_for_dates) > 0 && !any(is.na(df_plot_for_dates$Date))) {
            min_date <- min(df_plot_for_dates$Date, na.rm = TRUE)
            max_date <- max(df_plot_for_dates$Date, na.rm = TRUE)
            dateRangeInput("dateRange", "Filter by Date Range:",
                           start = min_date, end = max_date,
                           min = min_date, max = max_date)
        } else {
            dateRangeInput("dateRange", "Filter by Date Range:",
                           start = Sys.Date(), end = Sys.Date(),
                           min = Sys.Date(), max = Sys.Date(), disabled = TRUE)
        }
    })
    
    output$timeSeriesPlot <- renderPlotly({
        df_plot <- plot_data()
        req(nrow(df_plot) > 0)
        req(length(input$valueCols) > 0)
        
        plot_title <- if (length(input$valueCols) == 1) {
            paste("Time Series of", input$valueCols)
        } else {
            "Time Series of Selected Variables"
        }
        
        x_label <- if (input$xlab_text != "") input$xlab_text else NULL
        y_label <- if (input$ylab_text != "") input$ylab_text else NULL
        
        sorted_dates <- sort(unique(df_plot$Date))
        fmt <- if (input$dateFormat == "year_only") "%Y" else input$dateFormat
        ordered_date_strings <- unique(format(sorted_dates, fmt))
        
        df_plot <- df_plot %>%
            mutate(
                Error = case_when(
                    input$error_type == "se" ~ SE,
                    input$error_type == "sd" ~ SD,
                    TRUE ~ 0
                ),
                ymin = Mean - Error,
                ymax = Mean + Error,
                FormattedDate = format(Date, fmt),
                DateFactor = factor(FormattedDate, levels = ordered_date_strings)
            )
        
        p <- ggplot(df_plot, aes(x = DateFactor, y = Mean)) +
            geom_line(size = 0.8) +
            geom_point(size = 2) +
            labs(title = plot_title, x = x_label, y = y_label) +
            theme_minimal() +
            theme(
                plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
                axis.title = element_text(size = 14),
                axis.text = element_text(size = 12),
                axis.text.x = element_text(angle = 45, hjust = 1),
                
                # In-chart legend configuration
                legend.position = c(0.05, 0.95),        # Top-left inside position
                legend.justification = c("left", "top"), # Anchor alignment
                legend.background = element_rect(fill = alpha("white", 0.7), color = "grey80"), # Translucent background box
                
                panel.grid.major.x = element_line(color = "grey80", linetype = "dotted")
            ) +
            scale_x_discrete(drop = FALSE)
        
        if (input$error_type != "none") {
            p <- p + geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2)
        }
        
        if (!is.null(input$categoryCol) && input$categoryCol != "") {
            if (input$plot_option == "facet") {
                p <- p + facet_wrap(as.formula(paste("~", input$categoryCol)), scales = "free_y") +
                    aes(color = Variable, group = Variable)
            } else if (input$plot_option == "color") {
                if (length(input$valueCols) == 1) {
                    p <- p + aes(color = !!sym(input$categoryCol), group = !!sym(input$categoryCol)) +
                        labs(color = input$categoryCol)
                } else {
                    p <- p + aes(color = interaction(Variable, !!sym(input$categoryCol)),
                                 group = interaction(Variable, !!sym(input$categoryCol))) +
                        labs(color = paste("Variable &", input$categoryCol))
                }
            } else {
                p <- p + aes(color = Variable, group = Variable)
            }
        } else {
            p <- p + aes(color = Variable, group = Variable)
        }
        
        ggplotly(p) %>% layout(hovermode = "x unified")
    })
}

shinyApp(ui = ui, server = server)