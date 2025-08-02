# app.R

library(shiny)
library(ggplot2)
library(dplyr)
library(readr) # For read_csv
library(rlang) # For !!sym() to handle column names with spaces
library(tidyr) # For pivot_longer to reshape data

# Define UI for application that draws a boxplot with facets
ui <- fluidPage(
    # Add a meta tag for responsive design
    tags$head(
        tags$meta(name="viewport", content="width=device-width, initial-scale=1.0"),
        # Custom CSS for better aesthetics and responsiveness
        tags$style(HTML("
      body {
        font-family: 'Inter', sans-serif;
        background-color: #f0f2f5;
        color: #333;
        padding: 20px;
      }
      .container-fluid {
        max-width: 1200px;
        margin: auto;
        background-color: #ffffff;
        border-radius: 12px;
        box-shadow: 0 4px 20px rgba(0, 0, 0, 0.08);
        padding: 30px;
      }
      h1 {
        color: #2c3e50;
        text-align: center;
        margin-bottom: 30px;
        font-weight: 700;
      }
      .well {
        background-color: #ecf0f1;
        border: 1px solid #bdc3c7;
        border-radius: 8px;
        padding: 20px;
        margin-bottom: 20px;
      }
      .form-group label {
        font-weight: 600;
        margin-bottom: 8px;
        display: block;
      }
      .form-control {
        border-radius: 6px;
        border: 1px solid #ced4da;
        padding: 10px 15px;
        box-shadow: inset 0 1px 2px rgba(0,0,0,.075);
      }
      .btn-primary {
        background-color: #3498db;
        border-color: #3498db;
        border-radius: 8px;
        padding: 10px 20px;
        font-weight: 600;
        transition: background-color 0.3s ease, border-color 0.3s ease;
      }
      .btn-primary:hover {
        background-color: #2980b9;
        border-color: #2980b9;
      }
      .shiny-plot-output {
        border: 1px solid #e0e0e0;
        border-radius: 8px;
        overflow: hidden;
        background-color: #fdfdfd;
      }
      .col-sm-4, .col-sm-8 {
        padding: 15px; /* Add padding for better spacing on smaller screens */
      }
      @media (max-width: 768px) {
        .col-sm-4, .col-sm-8 {
          width: 100%;
          float: none;
        }
        .container-fluid {
          padding: 15px;
        }
      }
    "))
    ),
    
    titlePanel("Faceted Boxplot Generator for Multiple Response Variables"),
    
    sidebarLayout(
        sidebarPanel(
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            tags$hr(),
            uiOutput("categorical_var_selector"), # Renamed for clarity
            uiOutput("response_vars_selector"),   # New selector for multiple response vars
            tags$hr(),
            helpText("Upload a CSV file. Select one 'Categorical Variable' (for X-axis grouping within facets) and multiple 'Response Variables' (each will get its own facet).")
        ),
        
        mainPanel(
            plotOutput("boxplot_plot", height = "600px")
        )
    )
)

# Define server logic required to draw a boxplot
server <- function(input, output, session) {
    
    # Reactive value to store the uploaded data
    data_upload <- reactiveVal(NULL)
    
    # Observe file input and read data
    observeEvent(input$file1, {
        req(input$file1) # Ensure a file is uploaded
        tryCatch({
            df <- read_csv(input$file1$datapath)
            data_upload(df)
        },
        error = function(e) {
            # Show a notification if file reading fails
            showNotification(paste("Error reading file:", e$message), type = "error")
            data_upload(NULL) # Clear data on error
        })
    })
    
    # Render UI for categorical variable selection (formerly 'parameter_selector')
    output$categorical_var_selector <- renderUI({
        df <- data_upload()
        if (is.null(df)) {
            return(NULL)
        }
        # Exclude columns that are likely numeric and would be response variables
        numeric_cols <- names(df)[sapply(df, is.numeric)]
        categorical_cols <- setdiff(names(df), numeric_cols)
        
        selectInput("cat_var_col", "Select Categorical Variable (for X-axis):",
                    choices = c("None", categorical_cols), # Allow 'None' for no categorical grouping
                    selected = "None")
    })
    
    # Render UI for multiple response variables selection (new)
    output$response_vars_selector <- renderUI({
        df <- data_upload()
        if (is.null(df)) {
            return(NULL)
        }
        # Suggest numeric columns as response variables
        numeric_cols <- names(df)[sapply(df, is.numeric)]
        selectInput("response_vars", "Select Response Variables (for Boxplots/Facets):",
                    choices = numeric_cols,
                    multiple = TRUE,
                    selected = numeric_cols[1]) # Select the first numeric column by default
    })
    
    # Reactive expression for the plot data, filtered and prepared
    plot_data <- reactive({
        df <- data_upload()
        req(df, input$response_vars) # Ensure data and at least one response var is selected
        
        # Validate selected response variables
        if (length(input$response_vars) == 0) {
            showNotification("Please select at least one Response Variable.", type = "warning")
            return(NULL)
        }
        if (!all(input$response_vars %in% names(df))) {
            showNotification("One or more selected Response Variables not found in data. Please re-select.", type = "warning")
            return(NULL)
        }
        
        # Validate categorical variable if selected
        if (input$cat_var_col != "None" && !(input$cat_var_col %in% names(df))) {
            showNotification("Selected Categorical Variable not found in data. Please re-select.", type = "warning")
            return(NULL)
        }
        
        # Reshape data from wide to long format using pivot_longer
        # The 'names_to' column will hold the original column names (e.g., "Aggregate_Stability_Index")
        # The 'values_to' column will hold the numeric values for plotting
        df_long <- df %>%
            pivot_longer(
                cols = all_of(input$response_vars), # Columns to pivot
                names_to = "Variable",              # New column for original variable names
                values_to = "Value"                 # New column for the values
            )
        
        # Filter out rows with NA in the Value column (from pivoted data)
        df_long <- df_long %>% filter(!is.na(Value))
        if (nrow(df_long) == 0) {
            showNotification("No valid data points remaining after filtering for missing values in Response Variables.", type = "warning")
            return(NULL)
        }
        
        # Convert 'Variable' to factor for faceting
        df_long$Variable <- as.factor(df_long$Variable)
        
        # Convert categorical variable to factor if selected
        if (input$cat_var_col != "None" && input$cat_var_col %in% names(df_long)) {
            df_long[[input$cat_var_col]] <- as.factor(df_long[[input$cat_var_col]])
            # Filter out NAs in categorical variable if it's used for grouping
            df_long <- df_long %>% filter(!is.na(!!sym(input$cat_var_col)))
        }
        
        df_long
    })
    
    # Render the boxplot
    output$boxplot_plot <- renderPlot({
        df_plot <- plot_data()
        
        # Use validate to provide user-friendly messages for missing/invalid inputs
        validate(
            need(!is.null(df_plot), "Please upload a CSV file and select at least one Response Variable."),
            need(nrow(df_plot) > 0, "No data available to plot after filtering. Check your data and column selections."),
            need(length(input$response_vars) > 0, "Please select at least one Response Variable.")
        )
        
        # Create the base boxplot
        p <- ggplot(df_plot, aes(y = Value))
        
        # Determine x-axis aesthetic based on categorical variable selection
        if (input$cat_var_col != "None" && input$cat_var_col %in% names(df_plot)) {
            p <- p + geom_boxplot(aes(x = !!sym(input$cat_var_col), fill = !!sym(input$cat_var_col)), outlier.shape = NA) +
                geom_jitter(aes(x = !!sym(input$cat_var_col)), width = 0.2, alpha = 0.3, color = "darkblue", size = 1) +
                labs(x = input$cat_var_col, fill = input$cat_var_col)
        } else {
            # If no categorical variable, use a dummy x-axis for a single boxplot per facet
            p <- p + geom_boxplot(aes(x = factor(1)), outlier.shape = NA, fill = "#3498db") +
                geom_jitter(aes(x = factor(1)), width = 0.2, alpha = 0.3, color = "darkblue", size = 1) +
                labs(x = NULL)
        }
        
        # Apply faceting by the new 'Variable' column (which holds the names of the response variables)
        p <- p + facet_wrap(~ Variable, scales = "free_y", ncol = 3) +
            labs(
                title = paste("Boxplots for Selected Response Variables"),
                y = "Value"
            ) +
            theme_minimal() +
            theme(
                plot.title = element_text(hjust = 0.5, size = 18, face = "bold", margin = margin(b = 15)),
                axis.title.y = element_text(size = 14, margin = margin(r = 10)),
                axis.text.y = element_text(size = 10),
                # Only show x-axis text if a categorical variable is selected
                axis.text.x = if (input$cat_var_col != "None") element_text(size = 10, angle = 45, hjust = 1) else element_blank(),
                axis.ticks.x = if (input$cat_var_col != "None") element_line() else element_blank(),
                strip.text = element_text(size = 12, face = "bold", color = "white"),
                strip.background = element_rect(fill = "#2c3e50", color = NA), # Dark background for facet titles
                panel.spacing = unit(1.5, "lines"), # Space between facets
                panel.grid.major.x = element_blank(), # Remove vertical grid lines
                panel.grid.minor = element_blank(),
                legend.position = "bottom",
                legend.title = element_text(size = 12, face = "bold"),
                legend.text = element_text(size = 10)
            )
        
        p
    })
}

# Run the application
shinyApp(ui = ui, server = server)