# app.R
library(shiny)
library(shinydashboard) # For dashboard layout
library(ggplot2)
library(dplyr)
library(tidyr) # For pivot_longer
library(DT)    # For data table preview

# --- User Interface (UI) ---
ui <- dashboardPage(
    # Dashboard Header: Application title
    dashboardHeader(title = "Data Trend Dashboard"),
    
    # Dashboard Sidebar: Input controls
    dashboardSidebar(
        sidebarMenu(
            menuItem("Data Visualization", tabName = "dashboard", icon = icon("chart-bar")),
            menuItem("Raw Data", tabName = "raw_data", icon = icon("table"))
        ),
        tags$hr(), # Horizontal rule for separation
        
        # Input for file upload
        fileInput("file_upload",
                  "Upload Your Data (CSV File)",
                  accept = c(".csv", ".txt") # Accept CSV and text files
        ),
        tags$hr(),
        
        # Input for custom plot title
        textInput("plot_title_input", "Custom Plot Title:", value = "Yearly Variable Comparison"),
        tags$hr(),
        
        # Plot Type Selector: NEW
        radioButtons("plot_type", "Select Plot Type:",
                     choices = c("Bar Plot", "Histogram", "Density Plot", "Line Graph"), # Added Line Graph
                     selected = "Bar Plot"),
        tags$hr(),
        
        # Placeholder for year selection (will be dynamically updated)
        uiOutput("year_selector"),
        actionButton("select_all_years", "Select All Years"),
        actionButton("deselect_all_years", "Deselect All Years"),
        tags$hr(),
        
        # Placeholder for numerical variable selection (will be dynamically updated)
        uiOutput("variable_selector"),
        actionButton("select_all_variables", "Select All Variables"),
        actionButton("deselect_all_variables", "Deselect All Variables"),
        tags$hr(),
        
        # Placeholder for categorical variable selection (for faceting)
        uiOutput("categorical_variable_selector"),
        tags$hr(),
        
        # Download buttons
        downloadButton("download_plot", "Download Plot (.png)"),
        downloadButton("download_data", "Download Filtered Data (.csv)"),
        
        helpText("Note: Upload a CSV file. Ensure it has a 'Year' column and other numerical variables you wish to plot. For faceting, include a categorical column like 'Remarks'.")
    ),
    
    # Dashboard Body: Main content area
    dashboardBody(
        tabItems(
            # First tab content: Dashboard for plot and controls
            tabItem(tabName = "dashboard",
                    fluidRow(
                        box(title = "Trend Visualization", status = "primary", solidHeader = TRUE, width = 12,
                            plotOutput("barPlot", height = "500px")
                        )
                    )
            ),
            
            # Second tab content: Raw data preview
            tabItem(tabName = "raw_data",
                    fluidRow(
                        box(title = "Uploaded Data Preview", status = "info", solidHeader = TRUE, width = 12,
                            DT::dataTableOutput("uploaded_data_preview")
                        )
                    )
            )
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
            showNotification(paste("Error reading file:", e$message), type = "error", duration = 5)
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
    
    # Dynamically render the numerical variable selection UI based on uploaded_data
    output$variable_selector <- renderUI({
        df_current <- uploaded_data()
        if (!is.null(df_current)) {
            current_facet_var <- input$facet_variable
            if (is.null(current_facet_var)) current_facet_var <- "None"
            
            available_vars <- names(df_current)[sapply(df_current, is.numeric) & names(df_current) != "Year" & names(df_current) != current_facet_var]
            
            if (length(available_vars) > 0) {
                checkboxGroupInput(
                    inputId = "selected_variables",
                    label = "Select Variable(s) to Display:",
                    choices = available_vars,
                    selected = available_vars[1]
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
            current_facet_var <- input$facet_variable
            if (is.null(current_facet_var)) current_facet_var <- "None"
            available_vars <- names(df_current)[sapply(df_current, is.numeric) & names(df_current) != "Year" & names(df_current) != current_facet_var]
            updateCheckboxGroupInput(session, "selected_variables", selected = available_vars)
        }
    })
    
    # Observe "Deselect All Variables" button
    observeEvent(input$deselect_all_variables, {
        updateCheckboxGroupInput(session, "selected_variables", selected = character(0))
    })
    
    # Dynamically render the categorical variable selection UI for faceting
    output$categorical_variable_selector <- renderUI({
        df_current <- uploaded_data()
        if (!is.null(df_current)) {
            categorical_cols <- names(df_current)[sapply(df_current, function(x) is.character(x) | is.factor(x))]
            
            default_selection <- "None"
            if ("Remarks" %in% categorical_cols) {
                default_selection <- "Remarks"
            } else if (length(categorical_cols) > 0) {
                default_selection <- categorical_cols[1]
            }
            
            if (length(categorical_cols) > 0) {
                selectInput(
                    inputId = "facet_variable",
                    label = "Select Categorical Variable for Separation (e.g., Remarks):",
                    choices = c("None", categorical_cols),
                    selected = default_selection
                )
            } else {
                p("No suitable categorical variables found for separation.")
            }
        } else {
            p("Upload a CSV file to select categorical variables.")
        }
    })
    
    # Reactive expression to filter and pivot data based on user selections
    filtered_data <- reactive({
        df_to_plot <- uploaded_data()
        req(df_to_plot, input$selected_years, input$selected_variables)
        
        data_filtered_by_year <- df_to_plot %>%
            filter(Year %in% input$selected_years)
        
        cols_to_keep <- c("Year", input$selected_variables)
        
        if (input$facet_variable != "None" && input$facet_variable %in% names(data_filtered_by_year)) {
            cols_to_keep <- c(cols_to_keep, input$facet_variable)
        }
        
        existing_cols_to_keep <- intersect(cols_to_keep, names(data_filtered_by_year))
        data_for_pivot <- data_filtered_by_year[, existing_cols_to_keep, drop = FALSE]
        pivot_cols <- setdiff(existing_cols_to_keep, c("Year", input$facet_variable))
        
        data_long <- data_for_pivot %>%
            pivot_longer(
                cols = all_of(pivot_cols),
                names_to = "Variable",
                values_to = "Value"
            ) %>%
            mutate(across(starts_with(input$facet_variable), as.factor))
        
        return(data_long)
    })
    
    # Render the plot based on selected type
    output$barPlot <- renderPlot({
        plot_data <- filtered_data()
        
        if (nrow(plot_data) == 0) {
            return(ggplot() +
                       geom_text(aes(x=0.5, y=0.5, label="No data selected for plotting. Please adjust your selections."), size=6, color = "gray50") +
                       theme_void())
        }
        
        # Base plot common elements
        p <- ggplot(plot_data) +
            labs(
                title = input$plot_title_input,
                fill = "Subject/Variable" # Consistent legend title
            ) +
            theme_minimal() +
            theme(
                plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
                axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
                axis.text.y = element_text(size = 12),
                axis.title = element_text(size = 14),
                legend.title = element_text(size = 14),
                legend.text = element_text(size = 12),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank()
            ) +
            scale_fill_brewer(palette = "Set2")
        
        # Apply specific geom based on plot type
        if (input$plot_type == "Bar Plot") {
            p <- p + aes(x = as.factor(Year), y = Value, fill = Variable) +
                geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
                labs(x = "Year", y = "Score")
        } else if (input$plot_type == "Histogram") {
            p <- p + aes(x = Value, fill = Variable) +
                geom_histogram(bins = 30, aes(y = after_stat(density)), alpha = 0.7, position = "identity") +
                labs(x = "Score", y = "Density")
        } else if (input$plot_type == "Density Plot") {
            p <- p + aes(x = Value, fill = Variable) +
                geom_density(alpha = 0.7, position = "identity") +
                labs(x = "Score", y = "Density")
        } else if (input$plot_type == "Line Graph") { # NEW: Line Graph
            p <- p + aes(x = as.factor(Year), y = Value, color = Variable, group = Variable) +
                geom_line(linewidth = 1) + # Use size for line thickness
                geom_point(size = 3, shape = 21, fill = "white") + # Add points for clarity
                labs(x = "Year", y = "Score") +
                scale_color_brewer(palette = "Set2") # Use color for lines
        }
        
        
        # Apply faceting if selected, adjusted for different plot types
        if (input$facet_variable != "None" && input$facet_variable %in% names(plot_data)) {
            if (input$plot_type == "Bar Plot" || input$plot_type == "Line Graph") { # Bar and Line graphs can use facet_wrap for a single categorical variable
                p <- p + facet_wrap(as.formula(paste("~", input$facet_variable)), scales = "free_y", ncol = 2)
            } else { # For Histogram and Density Plot, which are better with facet_grid for Variable comparison
                p <- p + facet_grid(as.formula(paste(input$facet_variable, "~ Variable")), scales = "free_y")
            }
        } else {
            # If no explicit facet variable
            if (input$plot_type == "Histogram" || input$plot_type == "Density Plot") {
                # For histograms and density, if multiple subjects, still facet by Variable for clarity
                if (length(unique(plot_data$Variable)) > 1) {
                    p <- p + facet_wrap(~ Variable, scales = "free_y", ncol = 2)
                }
            }
            # For Bar Plot and Line Graph with no facet_variable, we don't need additional faceting here
            # as 'fill=Variable' (bar) or 'color=Variable' (line) handles differentiation.
        }
        
        return(p)
    })
    
    # Download plot handler
    output$download_plot <- downloadHandler(
        filename = function() {
            paste0(gsub(" ", "_", input$plot_title_input), "_", Sys.Date(), ".png")
        },
        content = function(file) {
            plot_width <- 12
            plot_height <- 8
            dpi_val <- 300
            
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
    
    # Preview of the uploaded data
    output$uploaded_data_preview <- DT::renderDataTable({
        df_current <- uploaded_data()
        if (!is.null(df_current)) {
            DT::datatable(df_current, options = list(pageLength = 10, scrollX = TRUE))
        } else {
            data.frame(Message = "Upload a CSV file to see a preview.")
        }
    })
}

# Run the application
shinyApp(ui = ui, server = server)