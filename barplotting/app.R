# app.R
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr) # For pivot_longer
library(DT)    # For data table preview

# --- User Interface (UI) ---
ui <- fluidPage(
    # Application title
    titlePanel("Yearly Multi-Variable Bar Graph Visualization with Data Upload"),
    
    # Sidebar with input controls
    sidebarLayout(
        sidebarPanel(
            # Input for file upload
            fileInput("file_upload",
                      "Upload Your Data (CSV File)",
                      accept = c(".csv", ".txt") # Accept CSV and text files
            ),
            tags$hr(), # Horizontal rule for separation
            
            # Placeholder for year selection (will be dynamically updated)
            uiOutput("year_selector"),
            
            # Placeholder for variable selection (will be dynamically updated)
            uiOutput("variable_selector"),
            
            helpText("Note: Upload a CSV file. Ensure it has a 'Year' column and other numerical variables you wish to plot.")
        ),
        
        # Main panel for displaying outputs
        mainPanel(
            plotOutput("barPlot"), # Output for the bar graph
            DT::dataTableOutput("uploaded_data_preview") # Optional: Preview of the uploaded data
        )
    )
)

# --- Server Logic ---
server <- function(input, output, session) {
    
    # Reactive value to store the uploaded data
    uploaded_data <- reactiveVal(NULL)
    
    # Observe the file input and read the CSV
    observeEvent(input$file_upload, {
        req(input$file_upload) # Ensure a file is uploaded
        
        tryCatch({
            # Read the CSV file
            df_uploaded <- read.csv(input$file_upload$datapath, header = TRUE, stringsAsFactors = FALSE)
            
            # Basic validation: Check if 'Year' column exists
            if (!"Year" %in% names(df_uploaded)) {
                stop("Uploaded file must contain a 'Year' column.")
            }
            
            # Update the reactive value with the uploaded data
            uploaded_data(df_uploaded)
            
        }, error = function(e) {
            # Show an error message if something goes wrong
            showNotification(paste("Error reading file:", e$message), type = "error")
            uploaded_data(NULL) # Reset data if error occurs
        })
    })
    
    # Dynamically render the year selection UI based on uploaded_data
    output$year_selector <- renderUI({
        df_current <- uploaded_data()
        if (!is.null(df_current) && "Year" %in% names(df_current)) {
            years_in_data <- sort(unique(df_current$Year))
            checkboxGroupInput(
                inputId = "selected_years",
                label = "Select Year(s):",
                choices = years_in_data,
                selected = years_in_data # Select all years by default
            )
        } else {
            # Show a message if no data or 'Year' column is missing
            p("Upload a CSV file with a 'Year' column to select years.")
        }
    })
    
    # Dynamically render the variable selection UI based on uploaded_data
    output$variable_selector <- renderUI({
        df_current <- uploaded_data()
        if (!is.null(df_current)) {
            # Identify numeric columns (excluding 'Year')
            available_vars <- names(df_current)[sapply(df_current, is.numeric) & names(df_current) != "Year"]
            
            if (length(available_vars) > 0) {
                checkboxGroupInput(
                    inputId = "selected_variables",
                    label = "Select Variable(s) to Display:",
                    choices = available_vars,
                    selected = available_vars[1] # Select the first numeric variable by default
                )
            } else {
                p("No numeric variables found in the uploaded data (apart from 'Year').")
            }
        } else {
            p("Upload a CSV file to select variables.")
        }
    })
    
    # Reactive expression to filter data based on user selections
    filtered_data <- reactive({
        df_to_plot <- uploaded_data() # Get the current uploaded data
        req(df_to_plot, input$selected_years, input$selected_variables) # Ensure data and selections exist
        
        # Filter by selected years
        data_filtered_by_year <- df_to_plot %>%
            filter(Year %in% input$selected_years)
        
        # Combine 'Year' with selected variables for subsetting
        cols_to_keep <- c("Year", input$selected_variables)
        
        # Ensure all selected columns actually exist in the dataframe
        # This helps prevent errors if for some reason a selected variable isn't in the data
        existing_cols_to_keep <- intersect(cols_to_keep, names(data_filtered_by_year))
        
        # Subset the data using base R indexing instead of dplyr::select
        # drop=FALSE ensures it remains a data.frame even if only one column is selected
        data_for_pivot <- data_filtered_by_year[, existing_cols_to_keep, drop = FALSE]
        
        # Then pivot the data from wide to long format for ggplot
        data_long <- data_for_pivot %>%
            pivot_longer(
                cols = -Year,          # All columns except 'Year'
                names_to = "Variable", # New column for variable names
                values_to = "Value"    # New column for variable values
            )
        return(data_long)
    })
    
    # Render the bar plot
    output$barPlot <- renderPlot({
        plot_data <- filtered_data()
        
        # Check if there is data to plot
        if (nrow(plot_data) == 0) {
            return(ggplot() +
                       geom_text(aes(x=0.5, y=0.5, label="No data selected for plotting. Please select years and variables."), size=6, color = "gray50") +
                       theme_void())
        }
        
        # Create the bar graph using ggplot2
        ggplot(plot_data, aes(x = as.factor(Year), y = Value, fill = Variable)) +
            geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
            labs(
                title = "Yearly Variable Comparison",
                x = "Year",
                y = "Value",
                fill = "Variable"
            ) +
            theme_minimal() +
            theme(
                plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
                axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
                axis.text.y = element_text(size = 12),
                axis.title = element_text(size = 14),
                legend.title = element_text(size = 14),
                legend.text = element_text(size = 12),
                panel.grid.major.x = element_blank(), # Remove vertical grid lines
                panel.grid.minor.x = element_blank()  # Remove minor vertical grid lines
            ) +
            scale_fill_brewer(palette = "Set2") # Use a color-blind friendly palette
    })
    
    # Optional: Preview of the uploaded data
    output$uploaded_data_preview <- DT::renderDataTable({
        df_current <- uploaded_data()
        if (!is.null(df_current)) {
            DT::datatable(df_current, options = list(pageLength = 5, scrollX = TRUE))
        }
    })
}

# Run the application
shinyApp(ui = ui, server = server)