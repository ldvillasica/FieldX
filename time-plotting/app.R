# app.R
# This R Shiny application allows users to upload a CSV file, select
# a date column and multiple value columns, and visualize the time series data
# interactively as line graphs using plotly. Users can also filter the data by a date range,
# and now, facet the plots by a selected categorical variable.

# Install necessary packages if not already installed
if (!require(shiny)) install.packages("shiny")
if (!require(tidyverse)) install.packages("tidyverse") # For data manipulation and ggplot2
if (!require(plotly)) install.packages("plotly")        # For interactive plots

# Load the packages
library(shiny)
library(tidyverse)
library(plotly)

# Define the User Interface (UI)
ui <- fluidPage(
    # Application title
    titlePanel("Interactive Time Series Data Visualization"),
    
    # Sidebar layout with input controls
    sidebarLayout(
        # Sidebar panel for user inputs
        sidebarPanel(
            # File input to allow users to upload their CSV data
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line for visual separation
            tags$hr(),
            
            # Checkbox to specify if the CSV has a header row
            checkboxInput("header", "Header", TRUE),
            
            # Radio buttons to select the separator used in the CSV file
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Radio buttons to select the quoting style in the CSV file
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Horizontal line for visual separation
            tags$hr(),
            
            # Dropdown to select the column to be used as the date variable
            uiOutput("dateColumnSelect"),
            
            # Dropdown to select the date format for parsing (this will also drive tick labels)
            selectInput("dateFormat", "Select Data's Date Format:",
                        choices = c("YYYY-MM-DD" = "%Y-%m-%d",
                                    "MM/DD/YYYY" = "%m/%d/%Y",
                                    "DD-MM-YYYY" = "%d-%m-%Y",
                                    "YYYY/MM/DD" = "%Y/%m/%d",
                                    "Year Only (YYYY)" = "year_only"),
                        selected = "%Y-%m-%d"),
            
            # Dropdown to select one or more columns containing the time series values
            # 'multiple = TRUE' allows selection of multiple variables
            uiOutput("valueColumnSelect"),
            
            # Dropdown to select a categorical variable for faceting or coloring
            uiOutput("categoryColumnSelect"),
            
            # Radio buttons to choose between faceting, coloring, or neither
            radioButtons("plot_option", "Plot by:",
                         choices = c("No Faceting / Color by Category" = "color",
                                     "Facet by Category" = "facet",
                                     "No Category" = "none"),
                         selected = "none"),
            
            # Horizontal line for visual separation
            tags$hr(),
            
            # Text inputs for X and Y axis labels
            textInput("xlab_text", "X-axis Label (optional):", value = ""),
            textInput("ylab_text", "Y-axis Label (optional):", value = ""),
            
            # Select input for X-axis major break interval
            selectInput("x_axis_major_breaks", "X-axis Major Tick Interval:",
                        choices = c("Auto" = "auto",
                                    "1 Day" = "1 day",
                                    "1 Week" = "1 week",
                                    "1 Month" = "1 month",
                                    "3 Months" = "3 months",
                                    "6 Months" = "6 months",
                                    "1 Year" = "1 year",
                                    "2 Years" = "2 years",
                                    "5 Years" = "5 years"),
                        selected = "auto"),
            
            # Horizontal line for visual separation
            tags$hr(),
            
            # Date range input for filtering the time series data
            uiOutput("dateRangeFilter")
        ),
        
        # Main panel for displaying the plot
        mainPanel(
            # Output for the interactive time series plot
            plotlyOutput("timeSeriesPlot", height = "600px")
        )
    )
)

# Define the Server logic
server <- function(input, output, session) {
    
    # Reactive expression to read the uploaded CSV file
    # This expression re-runs whenever 'file1', 'header', 'sep', or 'quote' inputs change
    data_input <- reactive({
        req(input$file1) # Require that a file has been uploaded
        
        # Read the CSV file based on user inputs
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df)
    })
    
    # Render UI for selecting the date column
    # This UI element is created dynamically once the data is loaded
    output$dateColumnSelect <- renderUI({
        df <- data_input()
        # Get column names that could potentially be dates (character, factor, or numeric for years)
        date_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x) || is.numeric(x))]
        selectInput("dateCol", "Select Date Column:", choices = date_cols)
    })
    
    # Render UI for selecting the value column(s)
    # This UI element is created dynamically once the data is loaded
    output$valueColumnSelect <- renderUI({
        df <- data_input()
        # Get column names that are numeric (integer or double)
        numeric_cols <- names(df)[sapply(df, is.numeric)]
        # Allow multiple selections for value columns
        selectInput("valueCols", "Select Value Column(s):", choices = numeric_cols, multiple = TRUE)
    })
    
    # Render UI for selecting the category column (for faceting or coloring)
    output$categoryColumnSelect <- renderUI({
        df <- data_input()
        # Get column names that are not numeric (potential categorical variables)
        non_numeric_cols <- names(df)[sapply(df, function(x) !is.numeric(x))]
        
        # Filter out the selected date column to avoid self-selection
        if (!is.null(input$dateCol)) {
            non_numeric_cols <- non_numeric_cols[non_numeric_cols != input$dateCol]
        }
        selectInput("categoryCol", "Select Categorical Variable:", choices = non_numeric_cols, selected = NULL)
    })
    
    # Reactive expression to prepare the data for plotting
    # This includes parsing dates, pivoting data for multiple variables, filtering, and handling missing selections
    plot_data <- reactive({
        df <- data_input()
        req(input$dateCol)    # Ensure date column is selected
        req(input$valueCols)  # Ensure at least one value column is selected
        req(input$dateFormat) # Ensure date format is selected
        
        # Rename selected date column to 'Date' for consistent plotting
        df_plot <- df %>%
            rename(Date = !!sym(input$dateCol))
        
        # Parse the 'Date' column based on the selected format
        if (input$dateFormat == "year_only") {
            # For 'Year Only', construct a date string as `%Y-01-01` and then parse
            df_plot$Date <- as.Date(paste0(df_plot$Date, "-01-01"), format = "%Y-%m-%d")
        } else {
            # For other formats, use the specified format directly
            df_plot$Date <- as.Date(df_plot$Date, format = input$dateFormat)
        }
        
        # Select columns: Date, chosen value columns, and the category column if selected and a valid plot option
        cols_to_select <- c("Date", input$valueCols)
        # Only include category column if it's selected AND we're using it for plotting
        if (!is.null(input$categoryCol) && input$categoryCol != "" && input$plot_option != "none") {
            cols_to_select <- c(cols_to_select, input$categoryCol)
        }
        
        # Using base R's bracket notation for column selection to avoid dplyr::select issues
        # This is more robust against potential dplyr versioning/conflict problems
        df_plot <- df_plot[, cols_to_select, drop = FALSE] %>%
            filter(!is.na(Date)) # Filter out rows where Date parsing failed
        
        # Pivot the data from wide to long format for plotting multiple variables
        # If a category column is selected, ensure it's kept in the pivot
        if (!is.null(input$categoryCol) && input$categoryCol != "" && input$plot_option != "none") {
            df_plot <- df_plot %>%
                pivot_longer(cols = -c(Date, !!sym(input$categoryCol)), names_to = "Variable", values_to = "Value")
        } else {
            df_plot <- df_plot %>%
                pivot_longer(cols = -Date, names_to = "Variable", values_to = "Value")
        }
        
        # Aggregate data to ensure one value per date, variable (and category) for smooth lines
        if (!is.null(input$categoryCol) && input$categoryCol != "" && input$plot_option != "none") {
            df_plot <- df_plot %>%
                group_by(Date, Variable, !!sym(input$categoryCol)) %>%
                summarize(Value = mean(Value, na.rm = TRUE), .groups = 'drop')
        } else {
            df_plot <- df_plot %>%
                group_by(Date, Variable) %>%
                summarize(Value = mean(Value, na.rm = TRUE), .groups = 'drop')
        }
        
        df_plot <- df_plot %>%
            arrange(Date) # Ensure data is ordered by date for continuous lines
        
        # Apply date range filter if it exists
        if (!is.null(input$dateRange)) {
            df_plot <- df_plot %>%
                filter(Date >= input$dateRange[1] & Date <= input$dateRange[2])
        }
        
        return(df_plot)
    })
    
    # Render UI for the date range filter
    # This is dynamically created based on the min/max dates available in the data
    output$dateRangeFilter <- renderUI({
        df_plot_for_dates <- plot_data() # Use a temporary reactive to get overall date range before faceting/filtering
        # Check if df_plot is not empty and contains valid dates after pivoting
        if (nrow(df_plot_for_dates) > 0 && !any(is.na(df_plot_for_dates$Date))) {
            min_date <- min(df_plot_for_dates$Date, na.rm = TRUE)
            max_date <- max(df_plot_for_dates$Date, na.rm = TRUE)
            dateRangeInput("dateRange", "Filter by Date Range:",
                           start = min_date,
                           end = max_date,
                           min = min_date,
                           max = max_date)
        } else {
            # If no valid dates, show a disabled input or message
            dateRangeInput("dateRange", "Filter by Date Range:",
                           start = Sys.Date(),
                           end = Sys.Date(),
                           min = Sys.Date(),
                           max = Sys.Date(),
                           disabled = TRUE)
        }
    })
    
    
    # Render the interactive time series plot using plotly
    output$timeSeriesPlot <- renderPlotly({
        df_plot <- plot_data() # Get the prepared data
        
        # Check if the plot_data is available and has at least one row
        req(nrow(df_plot) > 0)
        req(length(input$valueCols) > 0) # Ensure at least one value column is selected for plotting
        
        # Construct the title dynamically based on selected variables
        plot_title <- if (length(input$valueCols) == 1) {
            paste("Time Series of", input$valueCols)
        } else {
            "Time Series of Selected Variables"
        }
        
        # Determine x-axis label - now NULL if user input is empty
        x_label <- if (input$xlab_text != "") {
            input$xlab_text
        } else {
            NULL # No default label if user doesn't provide one
        }
        
        # Determine y-axis label - now NULL if user input is empty
        y_label <- if (input$ylab_text != "") {
            input$ylab_text
        } else {
            NULL # No default label if user doesn't provide one
        }
        
        # Determine the x-axis tick label format based on input$dateFormat
        # Handle "year_only" specifically
        x_tick_label_format <- if (input$dateFormat == "year_only") {
            "%Y" # Just year for the tick labels if data was year-only
        } else {
            input$dateFormat # Use the same format as the input data
        }
        
        # Determine major date breaks based on user input
        major_breaks_arg <- if (input$x_axis_major_breaks == "auto") {
            waiver() # ggplot2's default auto-breaks
        } else {
            input$x_axis_major_breaks # User-defined interval
        }
        
        # Base ggplot object for line graphs
        p <- ggplot(df_plot, aes(x = Date, y = Value)) +
            geom_line(size = 0.8) +
            labs(title = plot_title,
                 x = x_label, # Use dynamic x-label (can be NULL)
                 y = y_label) + # Use dynamic y-label (can be NULL)
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
                  axis.title = element_text(size = 14),
                  axis.text = element_text(size = 12),
                  legend.position = "bottom",
                  panel.grid.major.x = element_line(color = "grey80", linetype = "dotted"),
                  panel.grid.minor.x = element_blank()
            ) +
            scale_x_date(date_breaks = major_breaks_arg,
                         date_labels = x_tick_label_format,
                         minor_breaks = "1 month")
        
        
        # Apply plotting options based on user selection
        if (!is.null(input$categoryCol) && input$categoryCol != "") {
            if (input$plot_option == "facet") {
                p <- p + facet_wrap(as.formula(paste("~", input$categoryCol)), scales = "free_y") +
                    aes(color = Variable) # Color by Variable within facets
            } else if (input$plot_option == "color") {
                # Logic for legend simplification based on number of value columns:
                if (length(input$valueCols) == 1) {
                    # If only one value column, color directly by the category variable
                    p <- p + aes(color = !!sym(input$categoryCol)) +
                        labs(color = input$categoryCol) # Set legend title to category name
                } else {
                    # If multiple value columns, revert to coloring by combination for clarity
                    p <- p + aes(color = interaction(Variable, !!sym(input$categoryCol))) +
                        labs(color = paste("Variable &", input$categoryCol))
                }
            } else { # plot_option == "none"
                p <- p + aes(color = Variable) # Default to coloring by Variable
            }
        } else { # No category column selected or plot_option is "none"
            p <- p + aes(color = Variable) # Default to coloring by Variable
        }
        
        # Convert ggplot to an interactive plotly object
        ggplotly(p) %>%
            layout(hovermode = "x unified") # Improve hover experience
    })
}

# Run the Shiny application
shinyApp(ui = ui, server = server)