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
            
            # Input for custom plot title
            textInput("plot_title_input", "Custom Plot Title:", value = "Yearly Variable Comparison"),
            tags$hr(),
            
            # Placeholder for year selection (will be dynamically updated)
            uiOutput("year_selector"),
            actionButton("select_all_years", "Select All Years"),
            actionButton("deselect_all_years", "Deselect All Years"),
            tags$hr(),
            
            # Placeholder for variable selection (will be dynamically updated)
            uiOutput("variable_selector"),
            actionButton("select_all_variables", "Select All Variables"),
            actionButton("deselect_all_variables", "Deselect All Variables"),
            tags$hr(),
            
            # New: Placeholder for categorical variable selection (for faceting)
            uiOutput("categorical_variable_selector"),
            tags$hr(),
            
            # Download buttons
            downloadButton("download_plot", "Download Plot (.png)"),
            downloadButton("download_data", "Download Filtered Data (.csv)"),
            
            helpText("Note: Upload a CSV file. Ensure it has a 'Year' column and other numerical variables you wish to plot. For faceting, include a categorical 'Remarks' column or similar.")
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
    
    # Observe "Select All Years" button
    observeEvent(input$select_all_years, {
        df_current <- uploaded_data()
        if (!is.null(df_current) && "Year" %in% names(df_current)) {
            years_in_data <- sort(unique(df_current$Year))
            updateCheckboxGroupInput(session, "selected_years", selected = years_in_data)
        }
    })
    
    # Observe "Deselect All Years" button
    observeEvent(input$deselect_all_years, {
        updateCheckboxGroupInput(session, "selected_years", selected = character(0))
    })
    
    # Dynamically render the variable selection UI based on uploaded_data
    output$variable_selector <- renderUI({
        df_current <- uploaded_data()
        if (!is.null(df_current)) {
            # Identify numeric columns (excluding 'Year' and any identified categorical columns)
            # We'll refine this to exclude potentially used categorical column later if needed,
            # but for now, it's just 'Year'
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
    
    # Observe "Select All Variables" button
    observeEvent(input$select_all_variables, {
        df_current <- uploaded_data()
        if (!is.null(df_current)) {
            available_vars <- names(df_current)[sapply(df_current, is.numeric) & names(df_current) != "Year"]
            updateCheckboxGroupInput(session, "selected_variables", selected = available_vars)
        }
    })
    
    # Observe "Deselect All Variables" button
    observeEvent(input$deselect_all_variables, {
        updateCheckboxGroupInput(session, "selected_variables", selected = character(0))
    })
    
    # New: Dynamically render the categorical variable selection UI
    output$categorical_variable_selector <- renderUI({
        df_current <- uploaded_data()
        if (!is.null(df_current)) {
            # Identify character or factor columns (excluding 'Year' if it's somehow not numeric)
            # You might want to refine this to exclude IDs or other non-relevant columns
            categorical_cols <- names(df_current)[sapply(df_current, function(x) is.character(x) | is.factor(x))]
            
            if (length(categorical_cols) > 0) {
                selectInput(
                    inputId = "facet_variable",
                    label = "Select Categorical Variable for Separation (e.g., Remarks):",
                    choices = c("None", categorical_cols), # Add "None" option
                    selected = "None"
                )
            } else {
                p("No suitable categorical variables found for separation.")
            }
        } else {
            p("Upload a CSV file to select categorical variables.")
        }
    })
    
    
    # Reactive expression to filter data based on user selections
    filtered_data <- reactive({
        df_to_plot <- uploaded_data() # Get the current uploaded data
        req(df_to_plot, input$selected_years, input$selected_variables) # Ensure data and selections exist
        
        # Filter by selected years
        data_filtered_by_year <- df_to_plot %>%
            filter(Year %in% input$selected_years)
        
        # Prepare columns for pivoting, including the selected facet variable if any
        cols_to_keep <- c("Year", input$selected_variables)
        if (input$facet_variable != "None" && input$facet_variable %in% names(data_filtered_by_year)) {
            cols_to_keep <- c(cols_to_keep, input$facet_variable)
        }
        
        # Ensure all selected columns actually exist in the dataframe
        existing_cols_to_keep <- intersect(cols_to_keep, names(data_filtered_by_year))
        
        # Subset the data
        data_for_pivot <- data_filtered_by_year[, existing_cols_to_keep, drop = FALSE]
        
        # Then pivot the data from wide to long format for ggplot
        # Ensure we don't pivot the facet variable
        pivot_cols <- setdiff(existing_cols_to_keep, c("Year", input$facet_variable))
        
        data_long <- data_for_pivot %>%
            pivot_longer(
                cols = all_of(pivot_cols), # Only pivot the numerical variables
                names_to = "Variable",    # New column for variable names
                values_to = "Value"       # New column for variable values
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
        p <- ggplot(plot_data, aes(x = as.factor(Year), y = Value, fill = Variable)) +
            geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
            labs(
                title = input$plot_title_input,
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
        
        # New: Add faceting if a categorical variable is selected
        if (input$facet_variable != "None" && input$facet_variable %in% names(plot_data)) {
            p <- p + facet_wrap(as.formula(paste("~", input$facet_variable)), scales = "free_y")
        }
        
        return(p)
    })
    
    # Download plot handler
    output$download_plot <- downloadHandler(
        filename = function() {
            paste0(gsub(" ", "_", input$plot_title_input), "_", Sys.Date(), ".png")
        },
        content = function(file) {
            plot_width <- 10
            plot_height <- 7
            dpi_val <- 300 # dots per inch for higher resolution
            
            # To capture the plot with faceting, we need to explicitly pass the plot object
            # from renderPlot which is stored in the output$barPlot
            ggsave(file, plot = output$barPlot(), width = plot_width, height = plot_height, units = "in", dpi = dpi_val)
        }
    )
    
    # Download data handler
    output$download_data <- downloadHandler(
        filename = function() {
            paste0("filtered_data_", Sys.Date(), ".csv")
        },
        content = function(file) {
            write.csv(filtered_data(), file, row.names = FALSE)
        }
    )
    
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