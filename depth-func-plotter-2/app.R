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
    titlePanel("AQP Paper Style Depth Plotter (All Sites Combined)"),
    
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
            actionButton("process_and_plot_btn", "Generate Plot", icon = icon("chart-line")),
            
            tags$hr(),
            h3("3. Paper Style Settings"),
            
            # --- STYLE CONTROLS ---
            checkboxInput("show_points", "Show Raw Data Points (Open Circles)", value = TRUE),
            checkboxInput("show_fraction", "Show Contributing Fraction (%)", value = TRUE),
            checkboxInput("log_scale", "Logarithmic X-axis (e.g. for Carbon)", value = FALSE),
            
            numericInput("jitter_amount", "Jitter Amount:", value = 0.5, min = 0, step = 0.1),
            numericInput("point_size", "Point Size (cex):", value = 0.8, min = 0.1, step = 0.1),
            
            h4("Slab & Interval Settings"),
            numericInput("slab_width", "Slab Interval (cm):", value = 1, min = 1, step = 1),
            p(em("Note: Lines represent the 25th, 50th (Median), and 75th percentiles.")),
            
            h4("Axis Limits"),
            numericInput("plot_xlim_min", "X-axis Min:", value = 0), 
            numericInput("plot_xlim_max", "X-axis Max:", value = 60), 
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
    
    # 1. Data Upload
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
    
    output$plot_title <- renderUI({
        req(input$property_col)
        h3(paste("Aggregated Depth Function:", paste(input$property_col, collapse=", ")))
    })
    
    # 3. Process Data
    observeEvent(input$process_and_plot_btn, {
        req(r_uploaded_raw_data(), input$id_col, input$top_col, input$bottom_col, input$property_col)
        
        df_raw <- r_uploaded_raw_data()
        id <- input$id_col
        top <- input$top_col
        bot <- input$bottom_col
        props <- input$property_col
        
        if (!all(c(id, top, bot, props) %in% names(df_raw))) {
            showNotification("Missing selected columns.", type = "error")
            return()
        }
        
        tryCatch({
            # Clean Data
            df_clean <- df_raw[!is.na(df_raw[[id]]) & df_raw[[id]] != "", ]
            df_clean[[id]] <- as.character(df_clean[[id]])
            
            # --- FIX: Define Grouping Variable but DO NOT put it in df_clean yet ---
            grouping_var_name <- "all_groups"
            grouping_val <- "All Sites"
            
            # Create SPC
            aqp::depths(df_clean) <- as.formula(paste0(id, " ~ ", top, " + ", bot))
            
            # --- FIX: Create a CLEAN Site Data Table and assign the dummy group ---
            profile_ids <- unique(profile_id(df_clean))
            
            # Create new site_df containing ONLY the ID and the new group column
            site_df <- data.frame(
                profile_id = profile_ids,
                all_groups = grouping_val
            )
            names(site_df)[1] <- id # Rename the ID column to match user input
            
            aqp::site(df_clean) <- site_df
            
            r_spc(df_clean)
            
            # Slab Calculation
            withProgress(message = 'Calculating aggregates...', value = 0.5, {
                # Formula uses the site variable "all_groups"
                fm <- as.formula(paste0(grouping_var_name, " ~ ", paste(props, collapse = " + ")))
                
                my_probs <- c(0.25, 0.50, 0.75)
                
                slab_res <- aqp::slab(df_clean, fm = fm, slab.structure = input$slab_width, 
                                      slab.fun = function(x) quantile(x, probs = my_probs, na.rm = TRUE))
                
                # --- Fix Column Names (X25. -> p.q25) ---
                if ("X25." %in% names(slab_res)) slab_res <- rename(slab_res, p.q25 = X25.)
                if ("X50." %in% names(slab_res)) slab_res <- rename(slab_res, p.q50 = X50.)
                if ("X75." %in% names(slab_res)) slab_res <- rename(slab_res, p.q75 = X75.)
                
                if ("25%" %in% names(slab_res)) slab_res <- rename(slab_res, p.q25 = `25%`)
                if ("50%" %in% names(slab_res)) slab_res <- rename(slab_res, p.q50 = `50%`)
                if ("75%" %in% names(slab_res)) slab_res <- rename(slab_res, p.q75 = `75%`)
                
                r_slab_data(slab_res)
            })
            
        }, error = function(e) {
            showNotification(paste("Error processing data:", e$message), type = "error")
        })
        
        updateTabsetPanel(session, "main_tabs", selected = "depth_plot_tab")
    })
    
    # 4. Render Plot
    output$slab_plot <- renderPlot({
        req(r_slab_data(), r_spc())
        
        slab_df <- r_slab_data()
        spc_raw <- aqp::horizons(r_spc())
        
        # --- ERROR CHECK ---
        if (!"p.q50" %in% names(slab_df)) {
            validate(paste("Error: Quantiles missing. Cols:", paste(names(slab_df), collapse=", ")))
        }
        
        # Ensure variable factor order
        if ("variable" %in% names(slab_df) && length(input$property_col) > 1) {
            slab_df$variable <- factor(slab_df$variable, levels = input$property_col)
        }
        
        # --- CUSTOM PANEL FUNCTION ---
        paper_panel <- function(x, y, groups, subscripts, 
                                raw_data, top_c, bot_c, prop_cols, 
                                show_pts, show_frac, jit_amt, pt_size, ...) {
            
            panel_slab <- slab_df[subscripts, ]
            panel_slab <- panel_slab[order(panel_slab$top), ]
            
            panel.grid(h = -1, v = -1, lty = 3, col = "gray")
            
            if(nrow(panel_slab) > 0) {
                panel.lines(x = panel_slab$p.q25, y = panel_slab$top, type = "S", col = "black", lwd = 1, lty = 1)
                panel.lines(x = panel_slab$p.q75, y = panel_slab$top, type = "S", col = "black", lwd = 1, lty = 1)
                panel.lines(x = panel_slab$p.q50, y = panel_slab$top, type = "S", col = "black", lwd = 2, lty = 1)
            }
            
            if (show_pts) {
                raw_sub <- raw_data 
                
                if (nrow(raw_sub) > 0) {
                    y_mid <- (raw_sub[[input$top_col]] + raw_sub[[input$bottom_col]]) / 2
                    p_col_name <- if("variable" %in% names(panel_slab)) as.character(panel_slab$variable[1]) else input$property_col[1]
                    
                    if (p_col_name %in% names(raw_sub)) {
                        panel.xyplot(x = raw_sub[[p_col_name]], y = y_mid,
                                     type = "p", pch = 1, col = "black", cex = pt_size,
                                     jitter.x = TRUE, factor = jit_amt)
                    }
                }
            }
            
            if (show_frac && nrow(panel_slab) > 0) {
                depth_range <- range(panel_slab$top, na.rm=TRUE)
                step <- 20 
                targets <- seq(from=0, to=max(depth_range), by=step)
                
                for(t in targets) {
                    row_idx <- which.min(abs(panel_slab$top - t))
                    if(length(row_idx) > 0) {
                        d <- panel_slab$top[row_idx]
                        frac <- panel_slab$contributing_fraction[row_idx]
                        if (!is.na(frac)) {
                            txt <- paste0(round(frac * 100), "%")
                            x_pos <- current.panel.limits()$xlim[2] 
                            panel.text(x = x_pos, y = d, labels = txt, adj = c(1.1, 0.5), cex = 0.8, col = "black")
                        }
                    }
                }
            }
        }
        
        # --- Lattice Settings ---
        use_formula <- as.formula("top ~ p.q50 | all_groups")
        
        scale_set <- list(y = list(alternating = 3, tick.number = 10), 
                          x = list(relation = "free"))
        if (input$log_scale) {
            scale_set$x$log <- 10
        }
        
        xyplot(use_formula, 
               data = slab_df,
               ylim = c(input$plot_ylim_max, input$plot_ylim_min), 
               xlim = c(input$plot_xlim_min, input$plot_xlim_max),
               ylab = "Depth (cm)",
               xlab = paste(input$property_col, collapse=", "),
               panel = paper_panel,
               raw_data = spc_raw,
               top_c = input$top_col,
               bot_c = input$bottom_col,
               prop_cols = input$property_col,
               show_pts = input$show_points,
               show_frac = input$show_fraction,
               jit_amt = input$jitter_amount,
               pt_size = input$point_size,
               scales = scale_set,
               strip = strip.custom(bg = grey(0.9)),
               layout = c(1, 1), 
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