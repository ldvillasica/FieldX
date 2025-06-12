# app.R

# Load necessary libraries
library(shiny)
library(shinydashboard)
library(readr)     # For flexible CSV reading (read_delim)
library(dplyr)     # For data manipulation
library(ggplot2)   # For static plot downloads (e.g., variogram, map images)
library(sf)        # For spatial vector data (modern R spatial)
library(sp)        # Older spatial classes, used by gstat
library(gstat)     # Core geostatistics package for variogram, kriging, IDW
library(raster)    # For creating raster grids from spatial data
library(leaflet)   # For interactive maps
library(viridis)   # For aesthetically pleasing color palettes
library(DT)        # For interactive data tables
library(tidyr)     # For data reshaping (not strictly used here, but good general utility)

# --- UI (User Interface) ---
ui <- dashboardPage(
    dashboardHeader(title = "Spatial Data Interpolator"),
    dashboardSidebar(
        sidebarMenu(
            id = "sidebarMenu", # Add ID for updating tabs
            menuItem("Upload Data", tabName = "upload_data", icon = icon("upload")),
            menuItem("Spatial Analysis", tabName = "spatial_analysis", icon = icon("globe"))
        )
    ),
    dashboardBody(
        tabItems(
            # Tab 1: Data Upload
            tabItem(tabName = "upload_data",
                    h2("Upload Your Spatial Data"),
                    fluidRow(
                        box(
                            title = "Data Input", status = "primary", solidHeader = TRUE,
                            width = 6,
                            fileInput("file1", "Choose CSV File (must contain X, Y coordinates)",
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
                                                     "Double Quote" = '"',
                                                     "Single Quote" = "'"),
                                         selected = '"'),
                            actionButton("load_data_btn", "Load Data & Go to Analysis")
                        ),
                        box(
                            title = "Data Preview", status = "info", solidHeader = TRUE,
                            width = 6,
                            DTOutput("contents")
                        )
                    )
            ),
            
            # Tab 2: Spatial Analysis
            tabItem(tabName = "spatial_analysis",
                    h2("Spatial Interpolation and Visualization"),
                    fluidRow(
                        box(
                            title = "Analysis Controls", status = "primary", solidHeader = TRUE,
                            width = 4,
                            uiOutput("x_coord_select"),
                            uiOutput("y_coord_select"),
                            # CRS Selection for Input Data
                            selectInput("input_crs_epsg", "Input Data CRS (EPSG Code):",
                                        choices = c("WGS84 (Lat/Lon) - EPSG:4326" = 4326,
                                                    "UTM Zone 51N (Philippines) - EPSG:32651" = 32651,
                                                    "Other (Manual Entry)" = ""),
                                        selected = 4326), # Default for GPS data
                            conditionalPanel(
                                condition = "input.input_crs_epsg == ''",
                                numericInput("manual_input_crs", "Enter Custom Input CRS EPSG Code:", value = NA, min = 1)
                            ),
                            # Analysis CRS Selection
                            numericInput("analysis_crs_epsg", "Analysis CRS (Projected, for Geostatistics) - EPSG Code:",
                                         value = 32651, min = 1), # Default to UTM 51N for Butuan City area
                            helpText("Geostatistical analysis (variogram, Kriging) works best on projected coordinates (e.g., UTM). Data will be transformed internally."),
                            uiOutput("variable_select"),
                            numericInput("grid_res", "Interpolation Grid Resolution (units of Analysis CRS, e.g., 5 for 5m x 5m)", value = 5, min = 1),
                            selectInput("interp_method", "Interpolation Method:",
                                        choices = c("Ordinary Kriging" = "kriging",
                                                    "Inverse Distance Weighting (IDW)" = "idw"),
                                        selected = "kriging"),
                            conditionalPanel(
                                condition = "input.interp_method == 'idw'",
                                sliderInput("idw_power", "IDW Power:", min = 0.1, max = 5, value = 2, step = 0.1)
                            ),
                            conditionalPanel(
                                condition = "input.interp_method == 'kriging'",
                                selectInput("variogram_model", "Variogram Model:",
                                            choices = c("Spherical" = "Sph",
                                                        "Exponential" = "Exp",
                                                        "Gaussian" = "Gau"),
                                            selected = "Sph"),
                                helpText("Variogram parameters will be automatically fitted. A good fit is crucial for Kriging.")
                            ),
                            actionButton("run_interpolation", "Run Interpolation & Generate Maps")
                        ),
                        box(
                            title = "Variogram (for Kriging)", status = "success", solidHeader = TRUE,
                            width = 8,
                            plotOutput("variogram_plot", height = "300px"),
                            verbatimTextOutput("variogram_summary")
                        )
                    ),
                    fluidRow(
                        box(
                            title = "Interpolated Map", status = "info", solidHeader = TRUE,
                            width = 6,
                            leafletOutput("interpolated_map", height = "500px"),
                            downloadButton("download_interpolated_map", "Download Interpolated Map (PNG)"),
                            downloadButton("download_interpolated_data", "Download Interpolated Data (CSV)")
                        ),
                        box(
                            title = "Prediction Variance Map (Kriging Only)", status = "warning", solidHeader = TRUE,
                            width = 6,
                            leafletOutput("variance_map", height = "500px"),
                            downloadButton("download_variance_map", "Download Variance Map (PNG)")
                        )
                    )
            )
        )
    )
)

# --- Server Logic ---
server <- function(input, output, session) {
    
    # Reactive expression to read uploaded data
    raw_data <- reactive({
        req(input$file1) # Ensure a file is uploaded
        
        inFile <- input$file1
        df <- read_delim(inFile$datapath,
                         col_names = input$header,
                         delim = input$sep, # Correct argument for custom separators
                         quote = input$quote,
                         show_col_types = FALSE) # Suppress column specification messages
        df
    })
    
    # Output: Data preview table
    output$contents <- renderDT({
        datatable(raw_data(), options = list(pageLength = 10))
    })
    
    # Dynamic UI for X coordinate selection (suggesting Longitude)
    output$x_coord_select <- renderUI({
        df <- raw_data()
        req(df)
        coords_cols <- names(df)[sapply(df, is.numeric)]
        selectInput("x_coord_col", "Select X (Longitude) Coordinate Column:",
                    choices = coords_cols,
                    selected = grep("lon|long|x_coord", coords_cols, ignore.case = TRUE, value = TRUE)[1])
    })
    
    # Dynamic UI for Y coordinate selection (suggesting Latitude)
    output$y_coord_select <- renderUI({
        df <- raw_data()
        req(df)
        coords_cols <- names(df)[sapply(df, is.numeric)]
        selectInput("y_coord_col", "Select Y (Latitude) Coordinate Column:",
                    choices = coords_cols,
                    selected = grep("lat|y_coord", coords_cols, ignore.case = TRUE, value = TRUE)[1])
    })
    
    # Dynamic UI for variable selection (all numeric columns excluding coordinates)
    output$variable_select <- renderUI({
        df <- raw_data()
        req(df, input$x_coord_col, input$y_coord_col)
        numeric_cols <- names(df)[sapply(df, is.numeric)]
        # Exclude selected coordinate columns from the variable list
        variable_cols <- numeric_cols[!numeric_cols %in% c(input$x_coord_col, input$y_coord_col)]
        if (length(variable_cols) == 0) {
            helpText("No suitable numeric variables found for interpolation.")
        } else {
            selectInput("interp_var", "Select Variable to Interpolate:",
                        choices = variable_cols,
                        selected = grep("yield|ph|om|nitrogen", variable_cols, ignore.case = TRUE, value = TRUE)[1])
        }
    })
    
    # Observe button click to switch tab
    observeEvent(input$load_data_btn, {
        updateTabItems(session, "sidebarMenu", selected = "spatial_analysis")
    })
    
    # Reactive: Convert data to spatial points (sf and sp objects) with CRS transformation
    spatial_data <- eventReactive(input$run_interpolation, {
        req(input$interp_var, input$x_coord_col, input$y_coord_col, raw_data())
        
        df <- raw_data()
        
        # Determine input CRS from user selection
        input_crs_code <- if (input$input_crs_epsg == "") {
            req(input$manual_input_crs)
            as.numeric(input$manual_input_crs)
        } else {
            as.numeric(input$input_crs_epsg)
        }
        
        # Validate coordinate columns exist in the data
        if (!all(c(input$x_coord_col, input$y_coord_col) %in% names(df))) {
            stop("Selected X or Y coordinate column not found in data. Please check data columns.")
        }
        # Validate the variable to interpolate is numeric
        if (!is.numeric(df[[input$interp_var]])) {
            stop(paste0("Selected variable '", input$interp_var, "' is not numeric. Please select a numeric variable."))
        }
        
        # Create sf object with specified input CRS
        sf_points <- tryCatch({
            st_as_sf(df, coords = c(input$x_coord_col, input$y_coord_col), crs = input_crs_code)
        }, error = function(e) {
            stop(paste("Error creating spatial points (st_as_sf): ", e$message, ". Check X/Y columns or Input CRS.", sep=""))
        })
        
        # --- DEBUGGING: Check points after initial sf conversion ---
        print("sf_points after initial conversion:")
        print(head(sf_points))
        print(summary(st_coordinates(sf_points)))
        print(paste("Number of points:", nrow(sf_points)))
        print(st_crs(sf_points)) # Print CRS after initial conversion
        # --- END DEBUGGING ---
        
        # Validate analysis CRS
        analysis_crs_code <- as.numeric(input$analysis_crs_epsg)
        if (is.na(analysis_crs_code) || analysis_crs_code <= 0) {
            stop("Invalid Analysis CRS EPSG code. Please enter a positive integer.")
        }
        
        # Transform to the analysis CRS (projected) for geostatistical calculations
        if (input_crs_code != analysis_crs_code) {
            sf_points <- tryCatch({
                st_transform(sf_points, crs = analysis_crs_code)
            }, error = function(e) {
                # Catch and re-throw the error with a more specific message if transformation fails
                stop(paste("CRS Transformation Error (st_transform): ", e$message, ". Please check EPSG codes or ensure 'sf' package has PROJ data.", sep=""))
            })
            showNotification(paste("Transformed data from CRS:", input_crs_code, "to CRS:", analysis_crs_code), type = "message", duration = 5)
        }
        
        # Ensure sf_points is not empty after transformation
        if (nrow(sf_points) == 0) {
            stop("No points remain after CRS transformation. Check input coordinates and CRS definitions.")
        }
        
        # --- DEBUGGING: Check points after transformation ---
        print("sf_points after transformation:")
        print(head(sf_points))
        print(summary(st_coordinates(sf_points)))
        print(paste("Number of points after transformation:", nrow(sf_points)))
        print(st_crs(sf_points)) # Print CRS after transformation
        # --- END DEBUGGING ---
        
        
        # Convert to sp object (needed by gstat functions)
        sp_points <- as(sf_points, "Spatial")
        sp_points
    })
    
    # Reactive: Create an empty grid for interpolation (in the Analysis CRS)
    interpolation_grid <- eventReactive(input$run_interpolation, {
        req(spatial_data(), input$grid_res)
        sp_points <- spatial_data()
        
        if (nrow(sp_points) == 0) {
            stop("No valid spatial data points to create an interpolation grid.")
        }
        
        # Define bounding box for the grid in the ANALYSIS CRS
        x_min <- min(sp_points@coords[, 1])
        x_max <- max(sp_points@coords[, 1])
        y_min <- min(sp_points@coords[, 2])
        y_max <- max(sp_points@coords[, 2])
        
        # Ensure min/max values are sensible
        if (is.infinite(x_min) || is.infinite(x_max) || is.infinite(y_min) || is.infinite(y_max) ||
            (x_max - x_min) == 0 || (y_max - y_min) == 0) {
            stop("Invalid coordinate ranges for grid creation. Check your transformed coordinates.")
        }
        
        # Create a raster grid based on the extent and resolution
        r <- raster(
            xmn = x_min, xmx = x_max,
            ymn = y_min, ymx = y_max,
            res = input$grid_res, # Grid cell size in units of Analysis CRS (e.g., meters if UTM)
            crs = sp_points@proj4string # Use the CRS from the spatial_data (which is now the analysis CRS)
        )
        
        # --- DEBUGGING: Check raster grid creation ---
        print("Interpolation grid created:")
        print(r)
        # --- END DEBUGGING ---
        
        as(r, "SpatialPixelsDataFrame") # Convert to SpatialPixelsDataFrame for gstat
    })
    
    # Reactive: Variogram calculation and fitting (for Kriging)
    variogram_results <- eventReactive(input$run_interpolation, {
        req(input$interp_method == "kriging", spatial_data(), input$interp_var, input$variogram_model)
        
        sp_points <- spatial_data()
        formula_str <- paste(input$interp_var, "~ 1")
        
        # Calculate empirical variogram
        v_emp <- tryCatch({
            variogram(as.formula(formula_str), sp_points)
        }, error = function(e) {
            stop(paste("Error calculating empirical variogram: ", e$message, ". Check data or selected variable.", sep=""))
        })
        
        # Ensure empirical variogram has enough points for fitting
        if (nrow(v_emp) < 2) {
            stop("Empirical variogram has too few points to fit a model. Need more data points or smaller grid resolution.")
        }
        
        # Fit theoretical variogram model
        v_fit <- tryCatch({
            fit.variogram(v_emp, model = vgm(psill = max(v_emp$gamma)*0.8,
                                             range = max(v_emp$dist)/3,
                                             model = input$variogram_model,
                                             nugget = min(v_emp$gamma)*0.5))
        }, error = function(e) {
            showNotification(paste("Variogram fitting error: ", e$message, ". Try a different variogram model or check your data (e.g., few points, perfect fit)."), type = "error", duration = 10)
            NULL
        })
        
        # --- DEBUGGING: Check variogram fit ---
        print("Variogram empirical:")
        print(v_emp)
        print("Variogram fitted:")
        print(v_fit)
        # --- END DEBUGGING ---
        
        list(empirical = v_emp, fitted = v_fit)
    })
    
    # Reactive: Perform interpolation
    interpolation_output <- eventReactive(input$run_interpolation, {
        req(spatial_data(), interpolation_grid(), input$interp_var, input$interp_method)
        
        sp_points <- spatial_data()
        new_grid <- interpolation_grid()
        formula_str <- paste(input$interp_var, "~ 1")
        
        if (input$interp_method == "kriging") {
            req(variogram_results()$fitted) # Ensure variogram is fitted for kriging
            
            interp_result <- tryCatch({
                krige(as.formula(formula_str),
                      locations = sp_points,
                      newdata = new_grid,
                      model = variogram_results()$fitted)
            }, error = function(e) {
                showNotification(paste("Kriging error:", e$message, ". This might happen if the variogram model is not well-fitted or data is ill-conditioned."), type = "error", duration = 10)
                NULL
            })
            
            if (!is.null(interp_result)) {
                names(interp_result) <- c("predicted", "variance") # Consistent column names
            }
            interp_result
            
        } else if (input$interp_method == "idw") {
            interp_result <- tryCatch({
                idw(as.formula(formula_str),
                    locations = sp_points,
                    newdata = new_grid,
                    power = input$idw_power)
            }, error = function(e) {
                showNotification(paste("IDW error:", e$message), type = "error", duration = 10)
                NULL
            })
            
            if (!is.null(interp_result)) {
                # IDW output typically has a 'var1.pred' column; 'variance' is NA but keep consistent name
                # We'll create a dummy 'variance' column for IDW for consistency in output structure
                idw_df <- as.data.frame(interp_result) %>%
                    rename(predicted = var1.pred) %>%
                    mutate(variance = NA_real_)
                
                # Convert back to SpatialPixelsDataFrame for consistent return type
                coordinates(idw_df) <- ~x + y
                proj4string(idw_df) <- proj4string(new_grid)
                interp_result <- as(idw_df, "SpatialPixelsDataFrame")
            }
            interp_result
        }
    })
    
    # Reactive for Leaflet map data (raster conversion and transformation to WGS84 for Leaflet)
    interpolated_raster <- reactive({
        req(interpolation_output())
        interpolated_spdf <- interpolation_output() # This is already in the analysis CRS (UTM)
        
        if (is.null(interpolated_spdf) || nrow(as.data.frame(interpolated_spdf)) == 0) {
            stop("Interpolation output is empty or invalid. Cannot create rasters.")
        }
        
        print("interpolated_spdf after interpolation:")
        print(head(as.data.frame(interpolated_spdf)))
        print(paste("Number of interpolated points:", nrow(as.data.frame(interpolated_spdf))))
        print(proj4string(interpolated_spdf)) # Print CRS of interpolated_spdf
        
        # 1. Create prediction raster directly from interpolated_spdf (which is in analysis CRS)
        pred_raster_utm <- tryCatch({
            raster(interpolated_spdf, layer = "predicted")
        }, error = function(e) {
            stop(paste("Error creating prediction raster from SpatialPixelsDataFrame:", e$message))
        })
        
        # 2. Transform the *raster object* to WGS84 for Leaflet display
        pred_raster_wgs84 <- tryCatch({
            projectRaster(pred_raster_utm, crs = CRS("+init=epsg:4326")) # Using CRS object for consistency
        }, error = function(e) {
            stop(paste("Error projecting prediction raster to WGS84:", e$message))
        })
        
        # Create variance raster if Kriging
        var_raster_wgs84 <- NULL
        if (input$interp_method == "kriging") {
            var_raster_utm <- tryCatch({
                raster(interpolated_spdf, layer = "variance")
            }, error = function(e) {
                stop(paste("Error creating variance raster from SpatialPixelsDataFrame:", e$message))
            })
            
            var_raster_wgs84 <- tryCatch({
                projectRaster(var_raster_utm, crs = CRS("+init=epsg:4326"))
            }, error = function(e) {
                stop(paste("Error projecting variance raster to WGS84:", e$message))
            })
        }
        
        # --- DEBUGGING: Check final raster objects ---
        print("Final pred_raster_wgs84 class:")
        print(class(pred_raster_wgs84))
        print(crs(pred_raster_wgs84)) # Use crs() for raster objects
        if (is.null(values(pred_raster_wgs84)) || all(is.na(values(pred_raster_wgs84)))) {
            warning("Final prediction raster has no non-NA values.")
        } else {
            print("Final pred_raster_wgs84 values summary:")
            print(summary(values(pred_raster_wgs84)))
        }
        
        if (!is.null(var_raster_wgs84)) {
            print("Final var_raster_wgs84 class:")
            print(class(var_raster_wgs84))
            print(crs(var_raster_wgs84)) # Use crs() for raster objects
            if (is.null(values(var_raster_wgs84)) || all(is.na(values(var_raster_wgs84)))) {
                warning("Final variance raster has no non-NA values.")
            } else {
                print("Final var_raster_wgs84 values summary:")
                print(summary(values(var_raster_wgs84)))
            }
        }
        # --- END DEBUGGING ---
        
        list(prediction = pred_raster_wgs84, variance = var_raster_wgs84)
    })
    
    # Reactive: Calculate the geographic center of the original data for map centering
    center_map_coords <- reactive({
        req(raw_data(), input$x_coord_col, input$y_coord_col)
        df <- raw_data()
        
        # Get the original (Lat/Lon) coordinates from the raw data
        # This avoids re-transforming after geostatistical processing
        lon_col <- input$x_coord_col
        lat_col <- input$y_coord_col
        
        if (!all(c(lon_col, lat_col) %in% names(df)) || 
            !is.numeric(df[[lon_col]]) || !is.numeric(df[[lat_col]])) {
            warning("Original coordinate columns not found or not numeric for map centering.")
            return(NULL)
        }
        
        mean_lon <- mean(df[[lon_col]], na.rm = TRUE)
        mean_lat <- mean(df[[lat_col]], na.rm = TRUE)
        
        # Validate the calculated means
        if (is.nan(mean_lon) || is.nan(mean_lat) || abs(mean_lat) > 90 || abs(mean_lon) > 180) {
            warning("Calculated center coordinates are invalid (NaN or outside geographic bounds).")
            return(NULL)
        }
        
        # Return as a list suitable for setView
        list(lon = mean_lon, lat = mean_lat)
    })
    
    # Output: Variogram plot
    output$variogram_plot <- renderPlot({
        req(input$interp_method == "kriging", variogram_results()$empirical)
        v_emp <- variogram_results()$empirical
        v_fit <- variogram_results()$fitted
        
        if (is.null(v_fit)) {
            plot(v_emp, main = paste("Empirical Variogram for", input$interp_var, "(Fit Failed)"))
        } else {
            plot(v_emp, v_fit, main = paste("Empirical and Fitted Variogram for", input$interp_var))
        }
    })
    
    # Output: Variogram summary
    output$variogram_summary <- renderPrint({
        req(input$interp_method == "kriging")
        v_fit <- variogram_results()$fitted
        if (!is.null(v_fit)) {
            print(v_fit)
        } else {
            cat("No fitted variogram summary available.\n")
            cat("Check for errors in variogram fitting (see Notifications).")
        }
    })
    
    # Output: Interpolated map
    output$interpolated_map <- renderLeaflet({
        req(interpolated_raster()$prediction)
        pred_raster <- interpolated_raster()$prediction
        map_center <- center_map_coords() # Get the pre-calculated center
        
        # Ensure the raster actually has non-NA values before creating the palette
        non_na_values_pred <- values(pred_raster)[!is.na(values(pred_raster))]
        req(length(non_na_values_pred) > 0)
        
        pal_pred <- colorNumeric(viridis(100), non_na_values_pred, na.color = "transparent")
        
        # Start leaflet map
        m <- leaflet() %>%
            addTiles(group = "OpenStreetMap") %>%
            addRasterImage(pred_raster, colors = pal_pred, opacity = 0.8, group = "Interpolated Values") %>%
            addLegend(pal = pal_pred, values = non_na_values_pred, title = input$interp_var,
                      position = "bottomright") %>%
            addScaleBar(position = "bottomleft") %>%
            addLayersControl(
                baseGroups = "OpenStreetMap",
                overlayGroups = c("Interpolated Values"),
                options = layersControlOptions(collapsed = FALSE)
            )
        
        # Set view using the pre-calculated coordinates or a fallback
        if (!is.null(map_center)) {
            m <- m %>% setView(lng = map_center$lon, lat = map_center$lat, zoom = 13)
        } else {
            # Fallback if map_center is NULL or invalid
            m <- m %>% setView(lng = 125.54, lat = 8.94, zoom = 13) # Default Butuan location
            warning("Using default map center due to invalid calculated coordinates for interpolated map.")
        }
        
        m # Return the map object
    })
    
    # Output: Prediction Variance map (Kriging only)
    output$variance_map <- renderLeaflet({
        req(input$interp_method == "kriging", interpolated_raster()$variance)
        var_raster <- interpolated_raster()$variance
        map_center <- center_map_coords() # Get the pre-calculated center
        
        # Ensure the raster actually has non-NA values before creating the palette
        non_na_values_var <- values(var_raster)[!is.na(values(var_raster))]
        req(length(non_na_values_var) > 0)
        
        pal_var <- colorNumeric("YlOrRd", non_na_values_var, na.color = "transparent", reverse = TRUE)
        
        # Start leaflet map
        m <- leaflet() %>%
            addTiles(group = "OpenStreetMap") %>%
            addRasterImage(var_raster, colors = pal_var, opacity = 0.8, group = "Prediction Variance") %>%
            addLegend(pal = pal_var, values = non_na_values_var, title = "Prediction Variance",
                      position = "bottomright") %>%
            addScaleBar(position = "bottomleft") %>%
            addLayersControl(
                baseGroups = "OpenStreetMap",
                overlayGroups = c("Prediction Variance"),
                options = layersControlOptions(collapsed = FALSE)
            )
        
        # Set view using the pre-calculated coordinates or a fallback
        if (!is.null(map_center)) {
            m <- m %>% setView(lng = map_center$lon, lat = map_center$lat, zoom = 13)
        } else {
            m <- m %>% setView(lng = 125.54, lat = 8.94, zoom = 13) # Default Butuan location
            warning("Using default map center due to invalid calculated coordinates for variance map.")
        }
        
        m # Return the map object
    })
    
    # Download Handlers for PNG maps and CSV data
    output$download_interpolated_map <- downloadHandler(
        filename = function() { paste0(input$interp_var, "_interpolated_map_", Sys.Date(), ".png") },
        content = function(file) {
            req(interpolated_raster()$prediction)
            pred_df <- as.data.frame(interpolated_raster()$prediction, xy=TRUE)
            names(pred_df)[3] <- "value"
            
            p <- ggplot(pred_df, aes(x, y, fill = value)) +
                geom_raster() +
                scale_fill_viridis_c(name = input$interp_var) +
                labs(title = paste("Interpolated", input$interp_var), x = "Longitude", y = "Latitude") +
                theme_minimal() +
                coord_fixed()
            
            ggsave(file, plot = p, width = 8, height = 6, units = "in", dpi = 300)
        }
    )
    
    output$download_interpolated_data <- downloadHandler(
        filename = function() { paste0(input$interp_var, "_interpolated_data_", Sys.Date(), ".csv") },
        content = function(file) {
            req(interpolation_output())
            # Transform back to WGS84 (Lat/Lon) for CSV download if user prefers lat/lon
            # Need to handle case where interpolation_output is SpatialPixelsDataFrame directly
            spdf_to_sf <- st_as_sf(as.data.frame(interpolation_output()), 
                                   coords = c("x", "y"), 
                                   crs = proj4string(interpolation_output()))
            
            interp_spdf_wgs84 <- st_transform(spdf_to_sf, crs = 4326)
            write_csv(as.data.frame(interp_spdf_wgs84) %>% select(-geometry), file)
        }
    )
    
    output$download_variance_map <- downloadHandler(
        filename = function() { paste0(input$interp_var, "_variance_map_", Sys.Date(), ".png") },
        content = function(file) {
            req(input$interp_method == "kriging", interpolated_raster()$variance)
            var_df <- as.data.frame(interpolated_raster()$variance, xy=TRUE)
            names(var_df)[3] <- "variance"
            
            p <- ggplot(var_df, aes(x, y, fill = variance)) +
                geom_raster() +
                scale_fill_viridis_c(option = "plasma", name = "Variance") +
                labs(title = paste("Prediction Variance for", input$interp_var), x = "Longitude", y = "Latitude") +
                theme_minimal() +
                coord_fixed()
            
            ggsave(file, plot = p, width = 8, height = 6, units = "in", dpi = 300)
        }
    )
}

# Run the application
shinyApp(ui = ui, server = server)