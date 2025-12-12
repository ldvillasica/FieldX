# app.R

# Load necessary libraries
library(shiny)
library(aqp)
library(lattice)
library(Hmisc)
library(MASS)
library(DT)
library(colourpicker)
library(dplyr) 

# --- UI (User Interface) ---
ui <- fluidPage(
    titlePanel("AQP Paper Style Depth Plotter (Site-Specific Visualization)"),
    
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
            uiOutput("property_col_select"), 
            
            tags$hr(),
            h4("3. Site/Group Filtering"),
            uiOutput("group_col_select"),  # NEW: Select the column for grouping
            uiOutput("group_filter_select"), # NEW: Select the specific group value to plot
            
            actionButton("process_and_plot_btn", "Generate Plot", icon = icon("chart-line")),
            
            tags$hr(),
            h3("4. Paper Style Settings"),
            
            # --- STYLE CONTROLS ---
            checkboxInput("show_points", "Show Raw Data Points (Open Circles)", value = TRUE),
            checkboxInput("show_fraction", "Show Contributing Fraction (%)", value = TRUE),
            checkboxInput("log_scale", "Logarithmic X-axis (e.g. for Carbon)", value = FALSE),
            
            numericInput("jitter_amount", "Jitter Amount:", value = 0.5, min = 0, step = 0.1),
            numericInput("point_size", "Point Size (cex):", value = 0.8, min = 0.1, step = 0.1),
            
            h4("Slab & Interval Settings"),
            numericInput("slab_width", "Slab Interval (cm):", value = 1, min = 1, step = 1),
            # Controls for flexible quantiles
            numericInput("lower_quantile", "Lower Quantile (%) for shading (e.g., 30):", value = 30, min = 1, max = 49, step = 1),
            numericInput("upper_quantile", "Upper Quantile (%) for shading (e.g., 70):", value = 70, min = 51, max = 99, step = 1),
            p(em("Note: The solid line is always the 50th percentile (Median).")),
            
            h4("Axis Limits (Y-axis only)"),
            numericInput("plot_ylim_min", "Y-axis Min Depth:", value = -5),
            numericInput("plot_ylim_max", "Y-axis Max Depth:", value = 150),
            
            h4("Dimensions"),
            numericInput("plot_height", "Height (px):", value = 700),
            numericInput("plot_width", "Width (px):", value = 600) 
        ),
        mainPanel(
            tabsetPanel(
                id = "main_tabs",
                tabPanel("Data Preview", DT::dataTableOutput("data_preview")),
                tabPanel("Paper Style Plot",
                         value = "depth_plot_tab",
                         uiOutput("plot_title"),
                         plotOutput("slab_plot")
                )
            )
        )
    )
)

# --- Server Logic ---
server <- function(input, output, session) {
    
    r_uploaded_raw_data <- reactiveVal(NULL)
    r_spc <- reactiveVal(NULL)
    r_slab_data <- reactiveVal(NULL)
    
    session$onSessionEnded(stopApp)
    
    # 1. Data Upload (omitted for brevity, assume same)
    observeEvent(input$file1, {
        req(input$file1)
        tryCatch({
            df <- read.csv(input$file1$datapath, header = input$header, sep = input$sep, stringsAsFactors = FALSE)
            r_uploaded_raw_data(df)
        }, error = function(e) showNotification(paste("Error reading CSV:", e$message), type = "error"))
    })
    
    # 2. Dynamic UI for Columns
    output$id_col_select <- renderUI({
        df <- r_uploaded_raw_data(); if (is.null(df)) return(NULL)
        selectInput("id_col", "Profile ID Column:", choices = names(df))
    })
    output$top_col_select <- renderUI({
        df <- r_uploaded_raw_data(); if (is.null(df)) return(NULL)
        selectInput("top_col", "Top Depth Column:", choices = names(df), selected = names(df)[2])
    })
    output$bottom_col_select <- renderUI({
        df <- r_uploaded_raw_data(); if (is.null(df)) return(NULL)
        selectInput("bottom_col", "Bottom Depth Column:", choices = names(df), selected = names(df)[3])
    })
    output$property_col_select <- renderUI({
        df <- r_uploaded_raw_data(); if (is.null(df)) return(NULL)
        nums <- names(df)[sapply(df, is.numeric)]
        selectInput("property_col", "Property Column(s):", choices = nums, multiple = TRUE)
    })
    
    # NEW: Select Grouping Column
    output$group_col_select <- renderUI({
        df <- r_uploaded_raw_data(); if (is.null(df)) return(NULL)
        selectInput("group_col", "Select Group/Site ID Column:", choices = names(df))
    })
    
    # NEW: Select Group Value to Filter
    output$group_filter_select <- renderUI({
        df <- r_uploaded_raw_data(); 
        group_col <- input$group_col
        if (is.null(df) || is.null(group_col) || group_col == "") return(NULL)
        
        # Get unique values from the selected column
        choices <- unique(df[[group_col]])
        
        selectInput("group_val", "Select Group Value to Visualize:", choices = choices, selected = choices[1])
    })
    
    output$plot_title <- renderUI({
        req(input$property_col, input$group_val)
        h3(paste("Aggregated Depth Function for Site:", input$group_val))
    })
    
    # 3. Process Data
    observeEvent(input$process_and_plot_btn, {
        req(r_uploaded_raw_data(), input$id_col, input$top_col, input$bottom_col, input$property_col, input$lower_quantile, input$upper_quantile, input$group_col, input$group_val)
        
        # Validation for quantile values
        if (input$lower_quantile >= 50 || input$upper_quantile <= 50 || input$lower_quantile >= input$upper_quantile) {
            showNotification("Quantile limits must be < 50% and > 50% respectively, and lower must be less than upper.", type = "error")
            return()
        }
        
        df_raw <- r_uploaded_raw_data()
        id <- input$id_col
        top <- input$top_col
        bot <- input$bottom_col
        props <- input$property_col
        group_col <- input$group_col
        group_val <- input$group_val
        
        if (!all(c(id, top, bot, props, group_col) %in% names(df_raw))) {
            showNotification("Missing selected columns.", type = "error")
            return()
        }
        
        tryCatch({
            # Data Cleaning
            df_clean <- df_raw[!is.na(df_raw[[id]]) & df_raw[[id]] != "", ]
            df_clean[[id]] <- as.character(df_clean[[id]])
            
            # NEW: Filter data by selected group value
            df_filtered <- df_clean[df_clean[[group_col]] == group_val, ]
            
            if(nrow(df_filtered) == 0) {
                showNotification(paste("No data found for group:", group_val), type = "warning")
                return()
            }
            
            # Create SPC using the filtered data
            aqp::depths(df_filtered) <- as.formula(paste0(id, " ~ ", top, " + ", bot))
            
            # Site Data Table (Crucial for the aqp logic to work)
            profile_ids <- unique(profile_id(df_filtered))
            # Since we are filtering *before* SPC creation, we can assign a dummy group 
            # or simply rely on the aggregation over the remaining profiles.
            site_df <- data.frame(
                profile_id = profile_ids,
                aggregated_group = "filtered_site_data"
            )
            names(site_df)[1] <- id 
            aqp::site(df_filtered) <- site_df
            
            r_spc(df_filtered)
            
            # Slab Calculation
            withProgress(message = paste('Calculating aggregates for', group_val, '...'), value = 0.5, {
                # Aggregate over the single 'filtered_site_data' group (which contains all filtered profiles)
                fm <- as.formula(paste0("aggregated_group", " ~ ", paste(props, collapse = " + ")))
                
                # Use user-defined probabilities
                lower_p <- input$lower_quantile / 100
                upper_p <- input$upper_quantile / 100
                my_probs <- c(lower_p, 0.50, upper_p)
                
                slab_res <- aqp::slab(df_filtered, fm = fm, slab.structure = input$slab_width, 
                                      slab.fun = function(x) quantile(x, probs = my_probs, na.rm = TRUE))
                
                # --- DYNAMIC SAFE COLUMN RENAMING ---
                current_names <- names(slab_res)
                name_map <- c()
                
                lower_name_parts <- c(paste0("X", round(my_probs[1] * 100), "."), paste0(round(my_probs[1] * 100), "%"))
                for (n in lower_name_parts) { if (n %in% current_names) name_map[n] <- "p.qlow" }
                
                median_name_parts <- c("X50.", "50%")
                for (n in median_name_parts) { if (n %in% current_names) name_map[n] <- "p.qmed" }
                
                upper_name_parts <- c(paste0("X", round(my_probs[3] * 100), "."), paste0(round(my_probs[3] * 100), "%"))
                for (n in upper_name_parts) { if (n %in% current_names) name_map[n] <- "p.qupp" }
                
                if(length(name_map) > 0) {
                    for (old_name in names(name_map)) {
                        idx <- which(current_names == old_name)
                        current_names[idx] <- name_map[old_name]
                    }
                    names(slab_res) <- current_names
                } else {
                    showNotification("Warning: Failed to rename quantile columns. Plotting may fail.", type = "warning")
                }
                
                r_slab_data(slab_res)
            })
            
        }, error = function(e) {
            showNotification(paste("Error processing data:", e$message), type = "error")
        })
        
        updateTabsetPanel(session, "main_tabs", selected = "depth_plot_tab")
    })
    
    # 4. Render Plot
    output$slab_plot <- renderPlot({
        req(r_slab_data(), r_spc(), input$property_col)
        
        slab_df <- r_slab_data()
        spc_raw <- aqp::horizons(r_spc())
        
        required_cols <- c("p.qlow", "p.qmed", "p.qupp")
        if (!all(required_cols %in% names(slab_df))) {
            validate(paste("Error: Required quantile columns (", paste(required_cols, collapse=", "), ") are missing. Check quantile inputs and data.", sep=""))
        }
        
        if ("variable" %in% names(slab_df) && length(input$property_col) > 1) {
            slab_df$variable <- factor(slab_df$variable, levels = input$property_col)
        }
        
        # --- CUSTOM PANEL FUNCTION (omitted for brevity, assume same) ---
        paper_panel <- function(x, y, groups, subscripts, 
                                raw_data, top_c, bot_c, prop_cols, 
                                show_pts, show_frac, jit_amt, pt_size, ...) {
            
            panel_slab <- slab_df[subscripts, ]
            panel_slab <- panel_slab[order(panel_slab$top), ]
            
            panel.grid(h = -1, v = -1, lty = 3, col = "gray")
            
            if(nrow(panel_slab) > 0) {
                
                # 1. SHADING (Lower to Upper Quantile)
                poly_x <- c(panel_slab$p.qlow, rev(panel_slab$p.qupp))
                poly_y <- c(panel_slab$top, rev(panel_slab$top))
                panel.polygon(poly_x, poly_y, col = "royalblue", border = NA, alpha = 0.25)
                
                # 2. QUANTILE LINES (Lower and Upper)
                panel.lines(x = panel_slab$p.qlow, y = panel_slab$top, type = "S", col = "black", lwd = 1, lty = 1)
                panel.lines(x = panel_slab$p.qupp, y = panel_slab$top, type = "S", col = "black", lwd = 1, lty = 1)
                
                # 3. MEDIAN LINE (50th percentile)
                panel.lines(x = panel_slab$p.qmed, y = panel_slab$top, type = "S", col = "blue4", lwd = 2, lty = 1)
            }
            
            # Raw Data Points
            if (show_pts) {
                p_col_name <- if("variable" %in% names(panel_slab)) as.character(panel_slab$variable[1]) else input$property_col[1]
                if (p_col_name %in% names(raw_data)) {
                    # Filter raw data for the current property only
                    raw_sub <- raw_data[!is.na(raw_data[[p_col_name]]), ]
                    y_mid <- (raw_sub[[input$top_col]] + raw_sub[[input$bottom_col]]) / 2
                    panel.xyplot(x = raw_sub[[p_col_name]], y = y_mid,
                                 type = "p", pch = 1, col = "black", cex = pt_size,
                                 jitter.x = TRUE, factor = jit_amt)
                }
            }
            
            # Contributing Fraction Text (omitted for brevity, assume same)
            if (show_frac && nrow(panel_slab) > 0) {
                depth_range <- range(panel_slab$top, na.rm=TRUE)
                step <- 20
                targets <- seq(from=0, to=max(depth_range, na.rm=TRUE) + step, by=step)
                for(t in targets) {
                    row_idx <- which.min(abs(panel_slab$top - t))
                    if(length(row_idx) > 0) {
                        d <- panel_slab$top[row_idx]
                        frac <- panel_slab$contributing_fraction[row_idx]
                        if (!is.na(frac) && d < input$plot_ylim_max && d >= input$plot_ylim_min) {
                            txt <- paste0(round(frac * 100), "%")
                            x_pos <- current.panel.limits()$xlim[2] 
                            panel.text(x = x_pos, y = d, labels = txt, adj = c(1.1, 0.5), cex = 0.8, col = "black")
                        }
                    }
                }
            }
        }
        
        # --- Lattice Formula and Layout ---
        use_formula <- if(length(input$property_col) > 1) {
            as.formula("top ~ p.qmed | variable")
        } else {
            as.formula("top ~ p.qmed")
        }
        
        # --- CUSTOM PREPANEL WITH FIXED +10 UNIT PADDING ---
        custom_prepanel <- function(x, y, subscripts, ...) {
            panel_slab <- slab_df[subscripts, ]
            
            # Define core data range based on quantiles (p.qlow and p.qupp)
            min_x <- min(panel_slab$p.qlow, na.rm = TRUE)
            max_x <- max(panel_slab$p.qupp, na.rm = TRUE)
            
            # Include raw data points in the max/min calculation if visible
            if (input$show_points) {
                p_col_name <- if("variable" %in% names(panel_slab)) as.character(panel_slab$variable[1]) else input$property_col[1]
                raw_sub <- spc_raw[[p_col_name]]
                if (!is.null(raw_sub)) {
                    min_x <- min(min_x, raw_sub, na.rm = TRUE)
                    max_x <- max(max_x, raw_sub, na.rm = TRUE)
                }
            }
            
            # --- FIXED UNIT PADDING ---
            padding_unit <- 10
            
            xmin_padded <- min_x - padding_unit
            xmax_padded <- max_x + padding_unit
            
            # Ensure non-negative limit for non-log plots if min_x is near zero
            if (min_x >= 0 && xmin_padded < 0) {
                xmin_padded <- 0
            }
            
            list(
                xlim = c(xmin_padded, xmax_padded),
                ylim = range(y, finite = TRUE)
            )
        }
        
        
        # scale_set uses the custom prepanel for X-axis
        scale_set <- list(
            y = list(alternating = 3, tick.number = 10, relation = "free"), 
            x = list(relation = "free", prepanel = custom_prepanel)
        )
        if (input$log_scale) {
            scale_set$x$log <- 10
        }
        
        # Calculate dynamic layout
        n_panels <- length(input$property_col)
        n_cols <- min(n_panels, 5) 
        n_rows <- ceiling(n_panels / n_cols)
        plot_layout <- c(n_cols, n_rows)
        
        # --- FINAL XYPLOT CALL ---
        xyplot(use_formula, 
               data = slab_df,
               ylim = c(input$plot_ylim_max, input$plot_ylim_min), 
               ylab = "Depth (cm)",
               xlab = "Median Bounded by Lower and Upper Quantiles",
               panel = paper_panel,
               # Pass user settings to panel
               raw_data = spc_raw,
               top_c = input$top_col,
               bot_c = input$bottom_col,
               prop_cols = input$property_col,
               show_pts = input$show_points,
               show_frac = input$show_fraction,
               jit_amt = input$jitter_amount,
               pt_size = input$point_size,
               
               # Standard Lattice controls
               scales = scale_set,
               strip = strip.custom(bg = grey(0.9)),
               layout = plot_layout,
               as.table = TRUE
        )
    }, height = function() input$plot_height, width = function() input$plot_width)
    
    # 5. Data Preview
    output$data_preview <- DT::renderDataTable({
        req(r_uploaded_raw_data())
        DT::datatable(r_uploaded_raw_data(), options = list(pageLength = 5))
    })
}

# Run
shinyApp(ui, server)