# app.R for Descriptive Analysis with shinydashboard and Kruskal-Wallis

# Load necessary packages
library(shiny)
library(shinydashboard) # For creating dashboards
library(dplyr)          # For data manipulation
library(ggplot2)        # For plotting
library(DT)             # For interactive data tables
library(psych)          # For more comprehensive descriptive statistics (e.g., describe(), skew, kurtosi)
library(tidyr)          # For pivot_longer if needed for more complex summary tables

# Define the User Interface (UI)
ui <- dashboardPage(
    dashboardHeader(title = "Interactive Data Explorer"),
    
    dashboardSidebar(
        sidebarMenu(
            id = "main_tabs_sidebar", # Add an ID to the sidebar menu for conditional panel
            menuItem("Data Upload", tabName = "data_upload", icon = icon("upload")),
            menuItem("Data Table", tabName = "data_table", icon = icon("table")),
            menuItem("Summary Statistics", tabName = "summary_stats", icon = icon("calculator")),
            menuItem("Univariate Plots", tabName = "univariate_plots", icon = icon("chart-bar")),
            menuItem("Group Comparisons", tabName = "group_comparisons", icon = icon("equals")) # New menu item
        ),
        
        # Common controls for data upload and variable selection
        # These will appear on all tabs since they are in the sidebar
        tags$hr(),
        h4("1. Upload Your Data"),
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
        h4("2. Select Variables for Analysis"),
        uiOutput("variableSelectors"), # Dynamic UI for variable selection
        hr(),
        h4("3. Plot Customization"),
        # Input for bin width for histograms, only shown if selected var is numeric
        conditionalPanel(
            condition = "input.main_tabs_sidebar == 'univariate_plots' && input.selected_variable_univariate != 'None' && output.isNumericSelectedVar",
            sliderInput("binwidth_slider", "Number of Bins (Histogram/Density):", min = 5, max = 100, value = 30)
        ),
        
        # Specific controls for Group Comparisons tab
        conditionalPanel(
            condition = "input.main_tabs_sidebar == 'group_comparisons'",
            hr(),
            h4("4. Group Comparison Settings"),
            uiOutput("groupComparisonVariableSelectors") # Dedicated selectors for this tab
        )
    ),
    
    dashboardBody(
        tabItems(
            tabItem(tabName = "data_upload",
                    h2("Upload Your Dataset"),
                    fluidRow(
                        box(title = "Instructions", status = "primary", solidHeader = TRUE, width = 12,
                            p("Please upload your CSV file using the controls in the sidebar."),
                            p("Ensure you set the correct header, separator, and quote options.")
                        )
                    )
            ),
            tabItem(tabName = "data_table",
                    h2("Uploaded Data Table"),
                    fluidRow(
                        box(title = "Data Preview", status = "info", solidHeader = TRUE, width = 12,
                            DT::dataTableOutput("contents")
                        )
                    )
            ),
            tabItem(tabName = "summary_stats",
                    h2("Descriptive Statistics"),
                    uiOutput("summaryStatsUI") # This will render the boxes for each variable
            ),
            tabItem(tabName = "univariate_plots",
                    h2("Univariate Plots"),
                    fluidRow(
                        box(title = "Selected Variable Plot", status = "success", solidHeader = TRUE, width = 12,
                            plotOutput("univariatePlot", height = "500px")
                        )
                    )
            ),
            tabItem(tabName = "group_comparisons", # New tab content
                    h2("Group Comparisons"),
                    fluidRow(
                        box(title = "Kruskal-Wallis Test", status = "primary", solidHeader = TRUE, width = 12,
                            p("Performs a Kruskal-Wallis rank sum test to compare distributions across groups."),
                            p("Requires a numeric dependent variable and a categorical grouping variable with at least 3 unique categories."),
                            hr(),
                            verbatimTextOutput("kruskalWallisOutput")
                        )
                    )
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
    
    # Dynamic UI for variable selection (used across multiple tabs for general selection)
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
    
    # Dynamic UI for group comparison specific variable selection
    output$groupComparisonVariableSelectors <- renderUI({
        df <- data_upload()
        if (is.null(df)) return(NULL)
        
        numeric_vars <- names(df)[sapply(df, is.numeric)]
        categorical_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
        
        tagList(
            selectInput("kw_dependent_var", "Select Numeric Dependent Variable (Kruskal-Wallis):",
                        choices = c("None", numeric_vars), selected = "None"),
            selectInput("kw_grouping_var", "Select Categorical Grouping Variable (Kruskal-Wallis):",
                        choices = c("None", categorical_vars), selected = "None")
        )
    })
    
    
    # Reactive to check if selected_variable_univariate is numeric
    output$isNumericSelectedVar <- reactive({
        df <- data_upload()
        selected_var <- input$selected_variable_univariate
        !is.null(df) && selected_var != "None" && is.numeric(df[[selected_var]])
    })
    outputOptions(output, "isNumericSelectedVar", suspendWhenHidden = FALSE)
    
    
    # Generate Summary Statistics UI using boxes for better arrangement
    output$summaryStatsUI <- renderUI({
        df <- data_upload()
        if (is.null(df)) {
            return(
                fluidRow(
                    box(title = "No Data", status = "warning", solidHeader = TRUE, width = 12,
                        p("Please upload data to view summary statistics.")
                    )
                )
            )
        }
        
        summary_boxes <- lapply(names(df), function(col_name) {
            col_data <- df[[col_name]]
            
            if (is.numeric(col_data)) {
                # Get the descriptive statistics from psych::describe
                desc_output <- psych::describe(col_data, na.rm = TRUE)
                
                # Manually create the desired tibble (data frame) from specific elements of desc_output
                desc_df <- tibble(
                    Statistic = c("n", "Mean", "SD", "Median", "Min", "Max", "Skew", "Kurtosis", "SE"), # Added Skew, Kurtosis
                    Value = c(
                        desc_output$n, 
                        desc_output$mean, 
                        desc_output$sd, 
                        desc_output$median, # Ensure median is included
                        desc_output$min, 
                        desc_output$max, 
                        desc_output$skew,    # psych::describe includes skew
                        desc_output$kurtosis, # psych::describe includes kurtosis
                        desc_output$se
                    )
                )
                
                box(
                    title = paste("Numerical Variable:", col_name),
                    status = "info",
                    solidHeader = TRUE,
                    width = 6, # Adjust width to control how many boxes per row (e.g., 6 for two per row)
                    renderTable(desc_df, rownames = FALSE, caption = paste("Summary for", col_name), caption.placement = "top")
                )
            } else if (is.factor(col_data) || is.character(col_data)) {
                freq_table <- as.data.frame(table(col_data))
                colnames(freq_table) <- c("Category", "Count")
                freq_table <- freq_table %>%
                    mutate(Proportion = Count / sum(Count)) %>%
                    arrange(desc(Count)) # Order by count for better readability
                
                box(
                    title = paste("Categorical Variable:", col_name),
                    status = "warning",
                    solidHeader = TRUE,
                    width = 6, # Adjust width
                    renderTable(freq_table, rownames = FALSE, caption = paste("Frequencies for", col_name), caption.placement = "top")
                )
            } else {
                NULL # Do not create a box for other data types
            }
        })
        
        fluidRow(summary_boxes) # Wrap all generated boxes in a fluidRow
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
    
    # Kruskal-Wallis Test Output
    output$kruskalWallisOutput <- renderPrint({
        req(input$kw_dependent_var, input$kw_grouping_var)
        
        df <- data_upload()
        dep_var_name <- input$kw_dependent_var
        group_var_name <- input$kw_grouping_var
        
        if (is.null(df) || dep_var_name == "None" || group_var_name == "None" ||
            !(dep_var_name %in% names(df)) || !(group_var_name %in% names(df))) {
            cat("Please select a numeric dependent variable and a categorical grouping variable to perform the Kruskal-Wallis test.\n")
            return()
        }
        
        dep_var_data <- df[[dep_var_name]]
        group_var_data <- df[[group_var_name]]
        
        # Ensure grouping variable is a factor and has at least 3 levels (required for KW)
        group_var_data <- as.factor(group_var_data)
        
        if (!is.numeric(dep_var_data)) {
            cat("Error: Dependent variable must be numeric.\n")
            return()
        }
        
        if (nlevels(group_var_data) < 2) { # Kruskal-Wallis requires at least 2 groups
            cat("Error: Grouping variable must have at least 2 unique categories for comparison.\n")
            return()
        }
        
        if (nlevels(group_var_data) < 3 && input$main_tabs_sidebar == 'group_comparisons') {
            cat("Kruskal-Wallis test is typically used for comparing 3 or more groups.\n")
            cat("For 2 groups, consider a Wilcoxon Rank Sum Test (Mann-Whitney U test).\n\n")
        }
        
        # Remove rows with NAs in either variable for the test
        test_data <- data.frame(dep = dep_var_data, group = group_var_data) %>%
            na.omit()
        
        if (nrow(test_data) == 0) {
            cat("No complete cases found for the selected variables. Check for missing values.\n")
            return()
        }
        
        # Perform Kruskal-Wallis test
        tryCatch({
            formula_str <- paste(dep_var_name, "~", group_var_name)
            kw_result <- kruskal.test(as.formula(formula_str), data = df)
            
            cat(paste("Kruskal-Wallis Rank Sum Test for", dep_var_name, "by", group_var_name, "\n\n"))
            print(kw_result)
            
            # Add group means/medians for context
            cat("\n\nDescriptive Statistics by Group:\n")
            if (is.numeric(dep_var_data)) {
                df_summary <- df %>%
                    group_by(across(all_of(group_var_name))) %>%
                    summarise(
                        Count = n(),
                        Mean = mean(!!sym(dep_var_name), na.rm = TRUE),
                        SD = sd(!!sym(dep_var_name), na.rm = TRUE),
                        Median = median(!!sym(dep_var_name), na.rm = TRUE)
                    )
                print(as.data.frame(df_summary)) # Print as data frame for cleaner output in verbatimTextOutput
            }
            
        }, error = function(e) {
            cat("An error occurred during the Kruskal-Wallis test:\n")
            cat(e$message, "\n")
        })
    })
}

# Run the application
shinyApp(ui = ui, server = server)