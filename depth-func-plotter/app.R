# app.R

# Load necessary libraries
library(shiny)
library(aqp)
library(Hmisc)
library(lattice)
library(MASS)
library(stringr)
library(DT)
library(colourpicker)
library(dplyr)
library(tidyr)
library(zoo)

# --- UI (User Interface) ---
ui <- fluidPage(
    titlePanel("Depth Function Plotter"),
    
    sidebarLayout(
        sidebarPanel(
            h3("1. Data Input"),
            fileInput("file1", "Upload Horizon Data (CSV)",
                      multiple = FALSE,
                      accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
            checkboxInput("header", "Header", TRUE),
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                         selected = ","),
            tags$hr(),
            h4("2. Column Selection"),
            uiOutput("id_col_select"),
            uiOutput("top_col_select"),
            uiOutput("bottom_col_select"),
            uiOutput("grouping_col_select"),
            uiOutput("property_col_select"), # NOW ALLOWS MULTIPLE SELECTION
            actionButton("process_and_plot_btn", "Generate Plot", icon = icon("chart-line")),
            
            tags$hr(),
            h3("3. Plot Settings"),
            numericInput("slab_width", "Slab Interval (cm):", value = 3, min = 1, step = 1),
            colourInput("line_color", "Median Line Color (for single variable, or 1st variable if multiple):", value = "RoyalBlue"),
            colourInput("ribbon_color", "Ribbon Fill Color (applied to all variables):", value = "RoyalBlue"), # Re-added for the "shadow"
            sliderInput("line_size", "Line Size:", min = 0.5, max = 3, value = 2, step = 0.25),
            sliderInput("ribbon_alpha", "Ribbon Transparency:", min = 0, max = 1, value = 0.25, step = 0.05),
            
            h4("Axis Limits & Layout"),
            numericInput("plot_xlim_min", "X-axis Min (across all variables):", value = 0), # Important for comparing properties on same scale
            numericInput("plot_xlim_max", "X-axis Max (across all variables):", value = 250), # Important for comparing properties on same scale
            numericInput("plot_ylim_min", "Y-axis Min Depth (e.g., -2):", value = -2),
            numericInput("plot_ylim_max", "Y-axis Max Depth (e.g., 100):", value = 100),
            numericInput("facet_cols", "Number of Facet Columns (e.g., 5):", value = 5, min = 1, step = 1),
            sliderInput("y_tick_number", "Y-axis Tick Number:", min = 2, max = 20, value = 10, step = 1),
            
            h4("Plot Dimensions"),
            numericInput("plot_height", "Plot Height (px):", value = 600, min = 300, step = 50),
            numericInput("plot_width", "Plot Width (px):", value = 800, min = 400, step = 50)
        ),
        mainPanel(
            tabsetPanel(
                id = "main_tabs",
                tabPanel("Data Preview & Summary",
                         h3("Uploaded Data Preview"),
                         DT::dataTableOutput("data_preview"),
                         tags$hr(),
                         h3("SoilProfileCollection Summary"),
                         verbatimTextOutput("spc_summary")
                ),
                tabPanel("Slab Data Table",
                         h3("Slabbed Data (after aqp::slab)"),
                         DT::dataTableOutput("slab_data_table")
                ),
                tabPanel("Depth Function Plot",
                         value = "depth_plot_tab",
                         uiOutput("plot_title"), # Dynamic plot title
                         plotOutput("slab_plot")
                )
            )
        )
    )
)

# --- Server Logic ---
server <- function(input, output, session) {
    
    # Reactive value to hold the raw data.frame from CSV upload
    r_uploaded_raw_data <- reactiveVal(NULL)
    # Reactive value for storing the SoilProfileCollection object
    r_spc <- reactiveVal(NULL)
    # Reactive value for storing slab_data
    r_slab_data <- reactiveVal(NULL)
    
    # For good practice, stop app when browser tab is closed
    session$onSessionEnded(stopApp)
    
    # --- 1. Data Upload ---
    observeEvent(input$file1, {
        req(input$file1)
        tryCatch({
            df <- read.csv(input$file1$datapath,
                           header = input$header,
                           sep = input$sep,
                           stringsAsFactors = FALSE)
            r_uploaded_raw_data(df)
            showNotification("CSV file loaded. Select columns below.", type = "message")
        }, error = function(e) {
            showNotification(paste("Error reading CSV:", e$message), type = "error")
            r_uploaded_raw_data(NULL)
        })
    })
    
    # --- 2. Reactive UI for Column Selection ---
    output$id_col_select <- renderUI({
        df <- r_uploaded_raw_data()
        if (is.null(df)) return(NULL)
        cols <- names(df)
        selected_val <- if("FC" %in% cols) "FC" else (if(length(cols) > 0) cols[1] else NULL)
        selectInput("id_col", "Profile ID Column:", choices = cols, selected = selected_val)
    })
    
    output$top_col_select <- renderUI({
        df <- r_uploaded_raw_data()
        if (is.null(df)) return(NULL)
        cols <- names(df)
        selected_val <- if("top" %in% cols) "top" else (if(length(cols) > 1) cols[2] else NULL)
        selectInput("top_col", "Top Depth Column:", choices = cols, selected = selected_val)
    })
    
    output$bottom_col_select <- renderUI({
        df <- r_uploaded_raw_data()
        if (is.null(df)) return(NULL)
        cols <- names(df)
        selected_val <- if("bottom" %in% cols) "bottom" else (if(length(cols) > 2) cols[3] else NULL)
        selectInput("bottom_col", "Bottom Depth Column:", choices = cols, selected = selected_val)
    })
    
    output$grouping_col_select <- renderUI({
        df <- r_uploaded_raw_data()
        if (is.null(df)) return(NULL)
        all_cols <- names(df)
        selected_val <- if("Field.Code" %in% all_cols) "Field.Code" else (if("FC" %in% all_cols) "FC" else (if(length(all_cols) > 0) all_cols[1] else NULL))
        selectInput("grouping_col", "Grouping/Faceting Column (e.g., FC):",
                    choices = all_cols,
                    selected = selected_val)
    })
    
    output$property_col_select <- renderUI({
        df <- r_uploaded_raw_data()
        if (is.null(df)) return(NULL)
        numeric_cols <- names(df)[sapply(df, is.numeric)]
        selected_val <- if("OC" %in% numeric_cols) "OC" else (if(length(numeric_cols) > 0) numeric_cols[1] else NULL)
        # KEY CHANGE: Allow multiple selection
        selectInput("property_col", "Property Column(s) (e.g., OC, Clay):",
                    choices = numeric_cols,
                    selected = selected_val,
                    multiple = TRUE)
    })
    
    # --- Dynamically set the plot title ---
    output$plot_title <- renderUI({
        req(input$property_col)
        title_text <- if (length(input$property_col) > 1) {
            paste(paste(input$property_col, collapse = ", "), "Depth Functions (Median with IQR)")
        } else {
            paste(input$property_col, "Depth Function (Median with IQR)")
        }
        h3(title_text)
    })
    
    
    # --- 3. Process Data and Generate Plot ---
    observeEvent(input$process_and_plot_btn, {
        req(r_uploaded_raw_data(), input$id_col, input$top_col, input$bottom_col,
            input$grouping_col, input$property_col, input$slab_width)
        
        df_raw <- r_uploaded_raw_data()
        id_col <- input$id_col
        top_col <- input$top_col
        bottom_col <- input$bottom_col
        grouping_col <- input$grouping_col
        property_cols <- input$property_col # Now a vector of column names
        
        # --- Input Validation ---
        required_cols <- c(id_col, top_col, bottom_col, grouping_col, property_cols)
        if (!all(required_cols %in% names(df_raw))) {
            missing_cols <- setdiff(required_cols, names(df_raw))
            showNotification(paste("Error: Missing required columns in data:", paste(missing_cols, collapse = ", ")), type = "error", duration = NULL)
            return()
        }
        
        # Ensure depth columns are numeric
        if (!is.numeric(df_raw[[top_col]])) {
            showNotification(paste0("Error: Top depth column '", top_col, "' is not numeric."), type = "error", duration = NULL)
            return()
        }
        if (!is.numeric(df_raw[[bottom_col]])) {
            showNotification(paste0("Error: Bottom depth column '", bottom_col, "' is not numeric."), type = "error", duration = NULL)
            return()
        }
        # Ensure property columns are numeric
        if (!all(sapply(df_raw[property_cols], is.numeric))) {
            non_numeric_props <- property_cols[!sapply(df_raw[property_cols], is.numeric)]
            showNotification(paste0("Error: Property column(s) '", paste(non_numeric_props, collapse = ", "), "' are not numeric."), type = "error", duration = NULL)
            return()
        }
        
        # --- Create SoilProfileCollection (SPC) ---
        spc <- NULL
        tryCatch({
            df_for_spc <- df_raw
            df_for_spc[[grouping_col]] <- factor(df_for_spc[[grouping_col]])
            
            aqp::depths(df_for_spc) <- as.formula(paste0(id_col, " ~ ", top_col, " + ", bottom_col))
            spc <- df_for_spc
            r_spc(spc)
            
            logic_issues <- aqp::checkHzDepthLogic(spc)
            if (nrow(logic_issues) > 0) {
                showNotification(paste0("Warning: Found ", nrow(logic_issues), " horizon depth logic issues. Check 'Data Preview & Summary' tab."), type = "warning", duration = 8)
            } else {
                showNotification("SoilProfileCollection created successfully. Generating plot...", type = "message")
            }
            
        }, error = function(e) {
            showNotification(paste("Error creating SoilProfileCollection:", e$message), type = "error", duration = NULL)
            r_spc(NULL)
            return()
        })
        
        req(r_spc())
        
        # --- Slab Calculation ---
        slab_data_aqp <- NULL
        tryCatch({
            # Dynamically construct formula for multiple properties
            formula_props_str <- paste(property_cols, collapse = " + ")
            formula_slab <- as.formula(paste0(grouping_col, " ~ ", formula_props_str))
            
            slab_data_aqp <- aqp::slab(r_spc(), fm = formula_slab,
                                       slab.structure = input$slab_width,
                                       slab.fun = quantile,
                                       probs = c(0.25, 0.50, 0.75),
                                       na.rm = TRUE)
            
            r_slab_data(slab_data_aqp)
            
        }, error = function(e) {
            showNotification(paste("Error during slab calculation:", e$message), type = "error", duration = NULL)
            r_slab_data(NULL)
            return()
        })
        
        req(r_slab_data(), nrow(r_slab_data()) > 0)
        
        # --- Plotting with lattice::xyplot ---
        output$slab_plot <- renderPlot({
            oc1 <- r_slab_data()
            
            # Ensure 'variable' column is a factor, respecting the order of selection if desired
            if ("variable" %in% names(oc1) && length(property_cols) > 1) {
                oc1$variable <- factor(oc1$variable, levels = property_cols)
            }
            
            # Construct dynamic X-axis label
            xlab_label <- paste0(paste(property_cols, collapse = ", "), " (", input$slab_width, " cm slabs)")
            
            # Determine line colors for multiple variables
            line_colors <- if (length(property_cols) > 1) {
                # If multiple, let lattice choose default colors for distinct lines
                # Or you can define a custom palette: e.g., RColorBrewer::brewer.pal(length(property_cols), "Set1")
                trellis.par.get("superpose.line")$col[1:length(property_cols)] # Use default lattice colors
            } else {
                input$line_color # Use user-selected color for single variable
            }
            
            # Create the lattice plot
            p_lattice <- xyplot(top ~ X50. | oc1[[grouping_col]], # Faceting is correctly specified here
                                data = oc1,
                                # KEY FIX: groups should be 'variable' (symbol) for multiple lines per facet
                                groups = variable,
                                ylab = 'Depth (cm)',
                                xlab = xlab_label,
                                lower = oc1$X25.,
                                upper = oc1$X75.,
                                ylim = c(input$plot_ylim_max, input$plot_ylim_min),
                                xlim = c(input$plot_xlim_min, input$plot_xlim_max),
                                panel = panel.depth_function,
                                alpha = input$ribbon_alpha,
                                sync.colors = TRUE, # Syncs colors between lines and ribbon for each group
                                par.settings = list(
                                    superpose.line = list(col = line_colors, lwd = input$line_size),
                                    superpose.fill = list(col = input$ribbon_color, alpha = input$ribbon_alpha)
                                ),
                                layout = c(input$facet_cols, 1),
                                strip = strip.custom(bg = grey(0.8)),
                                scales = list(y = list(tick.number = input$y_tick_number),
                                              alternating = 3,
                                              relation = 'free' # Keep free X-axis scale for each facet
                                ),
                                # KEY CHANGE: Add a legend for multiple variables
                                key = list(
                                    # Use levels(oc1$variable) for legend text
                                    text = list(levels(oc1$variable)),
                                    lines = list(col = line_colors, lwd = input$line_size),
                                    columns = min(length(property_cols), 3), # Limit legend columns for readability
                                    title = "Property",
                                    cex.title = 1
                                )
            )
            print(p_lattice)
        }, height = function() input$plot_height, width = function() input$plot_width)
        
        updateTabsetPanel(session, "main_tabs", selected = "depth_plot_tab")
    })
    
    # --- Data Preview and SPC Summary Outputs ---
    output$data_preview <- DT::renderDataTable({
        df <- r_uploaded_raw_data()
        if (is.null(df)) return(NULL)
        DT::datatable(df, options = list(pageLength = 10))
    })
    
    output$spc_summary <- renderPrint({
        spc <- r_spc()
        if (is.null(spc)) {
            "No SoilProfileCollection loaded yet. Please upload data and click 'Generate Plot'."
        } else {
            summary_output <- list(
                "SoilProfileCollection Summary:",
                paste("Number of profiles:", length(spc)),
                paste("Horizon Count:", nrow(aqp::horizons(spc))),
                paste("Profile ID Column (internal):", aqp::idname(spc)),
                paste("Depth Columns (internal):", aqp::depths(spc)[1], "-", aqp::depths(spc)[2]),
                "\nHorizon Data Column Classes:",
                sapply(aqp::horizons(spc), class),
                "\nHorizon Depth Logic Check (results if any warnings):"
            )
            
            logic_issues <- aqp::checkHzDepthLogic(spc)
            if (nrow(logic_issues) > 0) {
                summary_output[[length(summary_output) + 1]] <- logic_issues
            } else {
                summary_output[[length(summary_output) + 1]] <- "No issues found with horizon depth logic."
            }
            
            return(summary_output)
        }
    })
    
    # Output for slab_data table (FIXED with explicit column selection for variable)
    output$slab_data_table <- DT::renderDataTable({
        slab_df <- r_slab_data()
        if (is.null(slab_df)) return(NULL)
        
        # Define common columns
        common_cols <- c("top", "bottom", "X25.", "X50.", "X75.", "contributing_fraction")
        
        # Conditionally add 'variable' column if it exists in slab_df
        cols_to_select <- if ("variable" %in% names(slab_df)) {
            c("variable", common_cols)
        } else {
            common_cols
        }
        
        # Perform select and rename
        slab_df_for_display <- slab_df %>%
            dplyr::select(dplyr::all_of(cols_to_select)) %>% # Use dplyr::select to be explicit
            dplyr::rename( # Use dplyr::rename for clarity
                `25th Percentile` = X25.,
                `Median` = X50.,
                `75th Percentile` = X75.
            ) %>%
            dplyr::mutate(dplyr::across(c(top, `25th Percentile`, `Median`, `75th Percentile`, contributing_fraction), ~ round(.x, 2)))
        
        DT::datatable(slab_df_for_display, options = list(pageLength = 10))
    })
}

# Run the application
shinyApp(ui = ui, server = server)