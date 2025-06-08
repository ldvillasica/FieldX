# app.R for Descriptive Analysis

# Load necessary packages
library(shiny)
library(dplyr)       # For data manipulation
library(ggplot2)     # For plotting
library(DT)          # For interactive data tables
library(psych)       # For more comprehensive descriptive statistics (e.g., describe())
# No longer strictly need qqplotr if not doing QQ plots, but no harm in keeping it if already installed.
# library(qqplotr) # Keeping it commented out as it's not strictly needed for this request

# Define the User Interface (UI)
ui <- fluidPage(
    titlePanel("Interactive Data Explorer: Descriptive Analysis"),
    
    sidebarLayout(
        sidebarPanel(
            h3("1. Upload Your Data"),
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            tags$hr(),
            checkboxInput("header", "Header", TRUE),
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     `Double Quote` = "\"",
                                     `Single Quote` = "'"),
                         selected = "\""),
            tags$hr(),
            h3("2. Select Variables for Analysis"),
            uiOutput("variableSelectors"), # Dynamic UI for variable selection
            hr(),
            h3("3. Plot Customization"),
            # Input for bin width for histograms, only shown if selected var is numeric
            conditionalPanel(
                condition = "input.main_tabs == 'Univariate Plots' && input.selected_variable_univariate != 'None' && output.isNumericSelectedVar", # Use output to check type
                sliderInput("binwidth_slider", "Number of Bins (Histogram/Density):", min = 5, max = 100, value = 30)
            )
            
        ),
        
        mainPanel(
            tabsetPanel(
                id = "main_tabs",
                tabPanel("Data Table",
                         h4("Uploaded Data:"),
                         DT::dataTableOutput("contents")),
                tabPanel("Summary Statistics",
                         h4("Descriptive Statistics:"),
                         uiOutput("summaryStatsUI")),
                tabPanel("Univariate Plots",
                         h4("Univariate Plots:"),
                         plotOutput("univariatePlot", height = "500px"))
            )
        )
    )
)

# Define the Server Logic
server <- function(input, output, session) {
    
    # Reactive value to store the uploaded data
    data_upload <- reactive({
        req(input$file1) # Require a file to be uploaded
        
        df <- tryCatch(
            {
                read.csv(input$file1$datapath,
                         header = input$header,
                         sep = input$sep,
                         quote = input$quote,
                         stringsAsFactors = TRUE) # Keep strings as factors initially
            },
            error = function(e) {
                stop(safeError(e)) # Stop app if error in reading file
            }
        )
        return(df)
    })
    
    # Display the uploaded data table
    output$contents <- DT::renderDataTable({
        DT::datatable(data_upload(), options = list(pageLength = 10))
    })
    
    # Dynamic UI for variable selection (after data is loaded)
    output$variableSelectors <- renderUI({
        df <- data_upload()
        if (is.null(df)) return(NULL)
        
        var_choices <- c("None", names(df))
        tagList(
            selectInput("selected_variable_univariate", "Select Primary Variable:",
                        choices = var_choices, selected = "None"),
            selectInput("selected_grouping_variable", "Select Grouping Variable (Optional for Box/Bar Plots):",
                        choices = c("None", names(df)), selected = "None")
        )
    })
    
    # Reactive to check if selected_variable_univariate is numeric
    output$isNumericSelectedVar <- reactive({
        df <- data_upload()
        selected_var <- input$selected_variable_univariate
        !is.null(df) && selected_var != "None" && is.numeric(df[[selected_var]])
    })
    outputOptions(output, "isNumericSelectedVar", suspendWhenHidden = FALSE)
    
    
    # Generate Summary Statistics UI
    output$summaryStatsUI <- renderUI({
        df <- data_upload()
        if (is.null(df)) return(p("Please upload data to view summary statistics."))
        
        summary_outputs <- lapply(names(df), function(col_name) {
            col_data <- df[[col_name]]
            if (is.numeric(col_data)) {
                desc_df <- psych::describe(col_data, na.rm = TRUE) %>%
                    as.data.frame() %>%
                    select(n, mean, sd, min, max, median, mad, se) %>%
                    t() %>% as.data.frame()
                colnames(desc_df) <- col_name
                desc_df <- tibble(Statistic = rownames(desc_df), Value = desc_df[[col_name]])
                
                list(
                    h5(strong(paste("Numerical Variable:", col_name))),
                    renderTable(desc_df, rownames = FALSE, caption = paste("Summary for", col_name), caption.placement = "top")
                )
            } else if (is.factor(col_data) || is.character(col_data)) {
                freq_table <- as.data.frame(table(col_data))
                colnames(freq_table) <- c("Category", "Count")
                freq_table <- freq_table %>%
                    mutate(Proportion = Count / sum(Count))
                
                list(
                    h5(strong(paste("Categorical Variable:", col_name))),
                    renderTable(freq_table, rownames = FALSE, caption = paste("Frequencies for", col_name), caption.placement = "top")
                )
            } else {
                NULL
            }
        })
        tagList(summary_outputs)
    })
    
    
    # Generate Univariate Plot
    output$univariatePlot <- renderPlot({
        df <- data_upload()
        selected_var <- input$selected_variable_univariate
        grouping_var <- input$selected_grouping_variable
        
        if (is.null(df) || selected_var == "None" || !(selected_var %in% names(df))) {
            return(NULL)
        }
        
        col_data <- df[[selected_var]]
        plot_title <- paste(selected_var, "Distribution")
        
        p <- NULL
        
        if (is.numeric(col_data)) {
            # If a grouping variable is selected and is categorical, draw grouped box plot.
            # Otherwise, draw Histogram & Density Plot by default.
            if (grouping_var != "None" && grouping_var %in% names(df) && (is.factor(df[[grouping_var]]) || is.character(df[[grouping_var]]))) {
                # Grouped Box Plot
                p <- ggplot(df, aes_string(x = grouping_var, y = selected_var, fill = grouping_var)) +
                    geom_boxplot() +
                    labs(title = paste("Box Plot of", selected_var, "by", grouping_var),
                         x = grouping_var, y = selected_var) +
                    theme_minimal() +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1))
            } else {
                # Histogram & Density Plot
                bins <- input$binwidth_slider # Use the slider input for bins
                p <- ggplot(df, aes_string(x = selected_var)) +
                    geom_histogram(aes(y = after_stat(density)), bins = bins, fill = "skyblue", color = "black") +
                    geom_density(color = "red", linewidth = 1) +
                    labs(title = plot_title, x = selected_var, y = "Density") +
                    theme_minimal()
            }
        } else if (is.factor(col_data) || is.character(col_data)) {
            # For categorical, we'll draw a bar chart.
            # If a grouping variable is also selected and is categorical, draw grouped bar chart.
            if (grouping_var != "None" && grouping_var %in% names(df) && (is.factor(df[[grouping_var]]) || is.character(df[[grouping_var]]))) {
                # Grouped Bar Chart
                df[[selected_var]] <- as.factor(df[[selected_var]]) # Ensure factors
                df[[grouping_var]] <- as.factor(df[[grouping_var]])
                
                p <- ggplot(df, aes_string(x = selected_var, fill = grouping_var)) +
                    geom_bar(position = "dodge", color = "black") +
                    labs(title = paste("Grouped Bar Chart of", selected_var, "by", grouping_var),
                         x = selected_var, y = "Count") +
                    theme_minimal() +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1))
            } else {
                # Simple Bar Chart
                p <- ggplot(df, aes_string(x = selected_var)) +
                    geom_bar(fill = "lightgreen", color = "black") +
                    labs(title = paste("Bar Chart of", selected_var), x = selected_var, y = "Count") +
                    theme_minimal() +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1))
            }
        }
        print(p)
    })
}

# Run the application
shinyApp(ui = ui, server = server)