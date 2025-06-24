# app.R
# This R Shiny application allows users to upload a CSV file, select
# a date column and multiple value columns, and visualize the time series data
# interactively as line graphs using plotly. Users can also filter the data by a date range.

# Install necessary packages if not already installed
if (!require(shiny)) install.packages("shiny")
if (!require(tidyverse)) install.packages("tidyverse") # For data manipulation and ggplot2
if (!require(plotly)) install.packages("plotly")       # For interactive plots

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
            
            # Dropdown to select the date format
            selectInput("dateFormat", "Select Date Format:",
                        choices = c("YYYY-MM-DD" = "%Y-%m-%d",
                                    "MM/DD/YYYY" = "%m/%d/%Y",
                                    "DD-MM-YYYY" = "%d-%m-%Y",
                                    "YYYY/MM/DD" = "%Y/%m/%d",
                                    "Year Only (YYYY)" = "year_only"),
                        selected = "%Y-%m-%d"),
            
            # Dropdown to select one or more columns containing the time series values
            # 'multiple = TRUE' allows selection of multiple variables
            uiOutput("valueColumnSelect"),
            
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
    
    # Reactive expression to prepare the data for plotting
    # This includes parsing dates, pivoting data for multiple variables, filtering, and handling missing selections
    plot_data <- reactive({
        df <- data_input()
        req(input$dateCol)  # Ensure date column is selected
        req(input$valueCols) # Ensure at least one value column is selected
        req(input$dateFormat) # Ensure date format is selected
        
        # Rename selected date column to 'Date' for consistent plotting
        df_plot <- df %>%
            rename(Date = !!sym(input$dateCol))
        
        # Parse the 'Date' column based on the selected format
        if (input$dateFormat == "year_only") {
            # For 'Year Only', construct a date string asYYYY-01-01 and then parse
            df_plot$Date <- as.Date(paste0(df_plot$Date, "-01-01"), format = "%Y-%m-%d")
        } else {
            # For other formats, use the specified format directly
            df_plot$Date <- as.Date(df_plot$Date, format = input$dateFormat)
        }
        
        # Select only the Date column and the chosen value columns
        # Then pivot the data from wide to long format for plotting multiple variables
        df_plot <- df_plot %>%
            select(Date, one_of(input$valueCols)) %>%
            filter(!is.na(Date)) %>% # Filter out rows where Date parsing failed
            # Pivot data to long format for plotting multiple variables
            pivot_longer(cols = -Date, names_to = "Variable", values_to = "Value") %>%
            # Aggregate data to ensure one value per date and variable for smooth lines
            group_by(Date, Variable) %>%
            summarize(Value = mean(Value, na.rm = TRUE), .groups = 'drop') %>% # Calculate mean, drop grouping
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
        df_plot <- plot_data()
        # Check if df_plot is not empty and contains valid dates after pivoting
        if (nrow(df_plot) > 0 && !any(is.na(df_plot$Date))) {
            min_date <- min(df_plot$Date, na.rm = TRUE)
            max_date <- max(df_plot$Date, na.rm = TRUE)
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
        
        # Create the ggplot object for line graphs with multiple variables
        p <- ggplot(df_plot, aes(x = Date, y = Value, color = Variable)) + # Map 'Variable' to color for multiple lines
            geom_line(size = 0.8) + # Only line graph, removed geom_point
            labs(title = plot_title,
                 x = "Year",
                 y = "Score") + # Y-axis label becomes "Value" as it represents multiple variables
            theme_minimal() + # Minimal theme for clean look
            theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
                  axis.title = element_text(size = 14),
                  axis.text = element_text(size = 12),
                  legend.position = "bottom") + # Position legend at the bottom for clarity
            # Explicitly set x-axis breaks to show yearly labels
            scale_x_date(date_breaks = "1 year", date_labels = "%Y")
        
        
        # Convert ggplot to an interactive plotly object
        ggplotly(p) %>%
            layout(hovermode = "x unified") # Improve hover experience
    })
}

# Run the Shiny application
shinyApp(ui = ui, server = server)