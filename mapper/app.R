# app.R

# Load necessary libraries
library(shiny)
library(shinydashboard)
library(readr)     # For flexible CSV reading (read_delim)
library(dplyr)     # For data manipulation
library(sf)        # For spatial vector data (modern R spatial)
library(leaflet)   # For interactive maps
library(DT)        # For interactive data tables
library(htmltools) # For creating richer HTML for popups
library(htmlwidgets) # To save the map as HTML

# --- UI (User Interface) ---
ui <- dashboardPage(
    dashboardHeader(title = "Regional Point Data Explorer"),
    dashboardSidebar(
        sidebarMenu(
            id = "sidebarMenu", # Add ID for updating tabs
            menuItem("Upload Data", tabName = "upload_data", icon = icon("upload")),
            menuItem("Map Explorer", tabName = "map_explorer", icon = icon("map-marked-alt"))
        )
    ),
    dashboardBody(
        tabItems(
            # Tab 1: Data Upload
            tabItem(tabName = "upload_data",
                    h2("Upload Your Regional Point Data"),
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
                            actionButton("load_data_btn", "Load Data & Go to Map Explorer")
                        ),
                        box(
                            title = "Data Preview", status = "info", solidHeader = TRUE,
                            width = 6,
                            DTOutput("contents")
                        )
                    )
            ),
            
            # Tab 2: Map Explorer
            tabItem(tabName = "map_explorer",
                    h2("Explore Your Data Points on a Map"),
                    fluidRow(
                        box(
                            title = "Map Controls", status = "primary", solidHeader = TRUE,
                            width = 4,
                            uiOutput("x_coord_select"),
                            uiOutput("y_coord_select"),
                            selectInput("input_crs_epsg", "Input Data CRS (EPSG Code):",
                                        choices = c("WGS84 (Lat/Lon) - EPSG:4326" = 4326,
                                                    "UTM Zone 51N (Philippines) - EPSG:32651" = 32651,
                                                    "Other (Manual Entry)" = ""),
                                        selected = 4326), # Default for GPS data
                            conditionalPanel(
                                condition = "input.input_crs_epsg == ''",
                                numericInput("manual_input_crs", "Enter Custom Input CRS EPSG Code:", value = NA, min = 1)
                            ),
                            uiOutput("color_var_select"),
                            checkboxInput("show_data_table", "Show Raw Data Table", TRUE),
                            actionButton("update_map_btn", "Update Map"),
                            downloadButton("download_map", "Download Map as HTML") # ADDED DOWNLOAD BUTTON
                        ),
                        box(
                            title = "Interactive Map", status = "info", solidHeader = TRUE,
                            width = 8,
                            leafletOutput("map_viewer", height = "600px")
                        )
                    ),
                    fluidRow(
                        conditionalPanel(
                            condition = "input.show_data_table",
                            box(
                                title = "Raw Data Table", status = "success", solidHeader = TRUE,
                                width = 12,
                                DTOutput("raw_data_table")
                            )
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
                         delim = input$sep,
                         quote = input$quote,
                         show_col_types = FALSE) # Suppress column specification messages
        df
    })
    
    # Output: Data preview table (for Upload tab)
    output$contents <- renderDT({
        datatable(raw_data(), options = list(pageLength = 10))
    })
    
    # Dynamic UI for X coordinate selection
    output$x_coord_select <- renderUI({
        df <- raw_data()
        req(df)
        coords_cols <- names(df)[sapply(df, is.numeric)]
        selectInput("x_coord_col", "Select X (Longitude/Easting) Coordinate Column:",
                    choices = coords_cols,
                    selected = grep("lon|long|x_coord|easting", coords_cols, ignore.case = TRUE, value = TRUE)[1])
    })
    
    # Dynamic UI for Y coordinate selection
    output$y_coord_select <- renderUI({
        df <- raw_data()
        req(df)
        coords_cols <- names(df)[sapply(df, is.numeric)]
        selectInput("y_coord_col", "Select Y (Latitude/Northing) Coordinate Column:",
                    choices = coords_cols,
                    selected = grep("lat|y_coord|northing", coords_cols, ignore.case = TRUE, value = TRUE)[1])
    })
    
    # Dynamic UI for variable to color by
    output$color_var_select <- renderUI({
        df <- raw_data()
        req(df) # Modified: Only require 'df' to be available for this UI.
        
        numeric_cols <- names(df)[sapply(df, is.numeric)]
        
        # Exclude selected coordinate columns if they are available
        exclude_cols <- c()
        if (!is.null(input$x_coord_col) && input$x_coord_col != "") {
            exclude_cols <- c(exclude_cols, input$x_coord_col)
        }
        if (!is.null(input$y_coord_col) && input$y_coord_col != "") {
            exclude_cols <- c(exclude_cols, input$y_coord_col)
        }
        
        variable_cols <- numeric_cols[!numeric_cols %in% exclude_cols]
        
        if (length(variable_cols) == 0) {
            tagList(
                selectInput("color_var", "Variable to Color Points By:", choices = c("None" = ""), selected = ""),
                helpText("No numeric variables available for coloring.")
            )
        } else {
            selectInput("color_var", "Variable to Color Points By:",
                        choices = c("None" = "", variable_cols),
                        selected = {
                            # Try to pre-select a relevant variable, if available
                            grep_result <- grep("yield|ph|om|nitrogen", variable_cols, ignore.case = TRUE, value = TRUE)
                            if (length(grep_result) > 0) grep_result[1] else "" # Select the first match, or "None"
                        }
            )
        }
    })
    
    # Observe button click to switch tab
    observeEvent(input$load_data_btn, {
        updateTabItems(session, "sidebarMenu", selected = "map_explorer")
    })
    
    # Reactive: Convert data to sf object for mapping, triggered by update_map_btn
    spatial_points_sf <- eventReactive(input$update_map_btn, {
        req(input$x_coord_col, input$y_coord_col, raw_data())
        
        df <- raw_data()
        
        # Determine input CRS from user selection
        input_crs_code <- if (input$input_crs_epsg == "") {
            req(input$manual_input_crs)
            as.numeric(input$manual_input_crs)
        } else {
            as.numeric(input$input_crs_epsg)
        }
        
        # Validate coordinate columns exist and are numeric
        if (!all(c(input$x_coord_col, input$y_coord_col) %in% names(df))) {
            stop("Selected X or Y coordinate column not found in data. Please check data columns.")
        }
        if (!is.numeric(df[[input$x_coord_col]]) || !is.numeric(df[[input$y_coord_col]])) {
            stop("Selected coordinate columns are not numeric. Please ensure they contain only numbers.")
        }
        
        # Create sf object
        sf_obj <- tryCatch({
            st_as_sf(df, coords = c(input$x_coord_col, input$y_coord_col), crs = input_crs_code)
        }, error = function(e) {
            stop(paste("Error creating spatial points (st_as_sf): ", e$message, ". Check X/Y columns or Input CRS.", sep=""))
        })
        
        # Ensure coordinates are valid for display on Leaflet (Lat/Lon range)
        coords <- st_coordinates(sf_obj)
        if (any(coords[,2] < -90) || any(coords[,2] > 90) || any(coords[,1] < -180) || any(coords[,1] > 180)) {
            # If coordinates are outside valid Lat/Lon range, it means CRS might be wrong for Leaflet display.
            # Try transforming to WGS84 just for display if not already in it.
            if (st_crs(sf_obj)$epsg != 4326) {
                warning("Coordinates appear to be outside standard Lat/Lon range. Attempting to transform to WGS84 for map display.")
                sf_obj_wgs84 <- tryCatch({
                    st_transform(sf_obj, crs = 4326)
                }, error = function(e) {
                    stop(paste("Coordinates outside valid Lat/Lon range and failed to transform to WGS84. Check input CRS.", e$message))
                })
                # Check transformed coordinates again
                coords_wgs84 <- st_coordinates(sf_obj_wgs84)
                if (any(coords_wgs84[,2] < -90) || any(coords_wgs84[,2] > 90) || any(coords_wgs84[,1] < -180) || any(coords_wgs84[,1] > 180)) {
                    stop("Even after transforming to WGS84, coordinates are invalid. Please check your raw data and input CRS.")
                }
                return(sf_obj_wgs84) # Return the transformed object
            } else {
                stop("Input CRS is WGS84, but coordinates are outside valid Lat/Lon range. Please check your raw data.")
            }
        }
        
        # If already in WGS84 or valid, return as is.
        # Leaflet always expects WGS84 (EPSG:4326) for adding markers and base layers.
        # If the input data is in a projected CRS (e.g., UTM), it must be transformed to WGS84 for Leaflet.
        if (st_crs(sf_obj)$epsg != 4326) {
            sf_obj <- tryCatch({
                st_transform(sf_obj, crs = 4326)
            }, error = function(e) {
                stop(paste("Error transforming data to WGS84 for map display: ", e$message, ". Ensure PROJ data is available.", sep=""))
            })
        }
        
        sf_obj
    })
    
    # Reactive: Calculate the geographic center of the data for map centering
    center_map_coords <- reactive({
        sf_obj <- spatial_points_sf()
        if (is.null(sf_obj) || nrow(sf_obj) == 0) return(NULL)
        
        coords <- st_coordinates(sf_obj)
        
        # Ensure coordinates are in WGS84 (Lat/Lon) before calculating mean
        # spatial_points_sf() already ensures this, but good to be explicit
        mean_lon <- mean(coords[, 1], na.rm = TRUE)
        mean_lat <- mean(coords[, 2], na.rm = TRUE)
        
        # Validate the calculated means
        if (is.nan(mean_lon) || is.nan(mean_lat) || abs(mean_lat) > 90 || abs(mean_lon) > 180) {
            warning("Calculated center coordinates are invalid (NaN or outside geographic bounds).")
            return(NULL)
        }
        list(lon = mean_lon, lat = mean_lat)
    })
    
    # Reactive: Generate the Leaflet map object (used for both display and download)
    # This reactive ensures the map logic is run only once for both outputs.
    final_map_object <- reactive({
        req(spatial_points_sf())
        sf_data <- spatial_points_sf()
        map_center <- center_map_coords()
        
        # Create popups for each point with image and data table
        popups_html <- lapply(seq_len(nrow(sf_data)), function(i) {
            row_data_raw <- st_drop_geometry(sf_data[i, ]) # Get row data excluding geometry
            
            image_html <- ""
            data_for_table <- row_data_raw # Data that will go into the table
            
            # Check for 'photo_url' column
            if ("photo_url" %in% names(row_data_raw) && 
                !is.na(row_data_raw[["photo_url"]]) && 
                nzchar(as.character(row_data_raw[["photo_url"]]))) {
                
                image_url <- as.character(row_data_raw[["photo_url"]])
                # Add styling for centering, max width, and bottom margin
                image_html <- paste0("<img src='", image_url, "' style='max-width:150px; height:auto; display:block; margin: 0 auto 5px auto;'><br>")
                data_for_table <- data_for_table %>% select(-photo_url) # Remove URL from table data
            } 
            # Check for 'photo_path' (for local images, if applicable)
            else if ("photo_path" %in% names(row_data_raw) && 
                     !is.na(row_data_raw[["photo_path"]]) && 
                     nzchar(as.character(row_data_raw[["photo_path"]]))) {
                
                local_path <- as.character(row_data_raw[["photo_path"]])
                image_html <- paste0("<img src='", local_path, "' style='max-width:150px; height:auto; display:block; margin: 0 auto 5px auto;'><br>")
                data_for_table <- data_for_table %>% select(-photo_path) # Remove path from table data
            }
            
            # Build the HTML table string for all remaining attributes
            table_rows_html <- lapply(names(data_for_table), function(col_name) {
                value <- as.character(data_for_table[[col_name]])
                paste0("<tr><td><b>", col_name, ":</b></td><td>", value, "</td></tr>")
            })
            table_html <- paste0(
                "<table class='table table-sm table-condensed table-borderless'>", # Bootstrap classes for styling
                "<tbody>",
                paste(table_rows_html, collapse = ""),
                "</tbody>",
                "</table>"
            )
            
            # Combine image HTML (if any) with attribute table HTML
            htmltools::HTML(paste0(image_html, table_html))
        })
        
        # Determine coloring scheme for map display
        color_by_var <- input$color_var # This input is part of the Shiny app's interactivity
        pal <- NULL
        colors <- "blue" # Default color for markers
        
        if (color_by_var != "" && color_by_var %in% names(sf_data) && is.numeric(sf_data[[color_by_var]])) {
            pal <- colorNumeric("viridis", domain = sf_data[[color_by_var]], na.color = "transparent")
            colors <- pal(sf_data[[color_by_var]])
            colors[is.na(colors)] <- "#808080" # Grey for NA values
        } 
        
        m <- leaflet() %>%
            addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
            addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
            addScaleBar(position = "bottomleft") %>%
            addLayersControl(
                baseGroups = c("OpenStreetMap", "Satellite"),
                options = layersControlOptions(collapsed = FALSE)
            )
        
        # Set initial view
        if (!is.null(map_center)) {
            m <- m %>% setView(lng = map_center$lon, lat = map_center$lat, zoom = 10) # Zoom for regional view
        } else {
            # Default to a general Caraga Region center if no data or invalid coords
            m <- m %>% setView(lng = 125.54, lat = 8.94, zoom = 9) # Butuan City/Caraga Region
            warning("Using default map center due to invalid calculated coordinates.")
        }
        
        # Add markers
        m <- m %>%
            addCircleMarkers(
                data = sf_data,
                radius = 5,
                color = colors, # Use dynamic colors for display in the app
                stroke = TRUE,
                fillOpacity = 0.8,
                popup = popups_html,
                group = "Data Points"
            )
        
        # Add legend if coloring by a variable for app display
        if (!is.null(pal)) {
            m <- m %>% addLegend(pal = pal, values = sf_data[[color_by_var]], title = color_by_var, position = "bottomright")
        }
        
        m
    })
    
    # Output: Interactive Map (for display in the app)
    output$map_viewer <- renderLeaflet({
        final_map_object() # Use the reactive map object
    })
    
    # Download Handler: Save the map as HTML
    output$download_map <- downloadHandler(
        filename = function() {
            paste0("caraga_interactive_map_", Sys.Date(), ".html")
        },
        content = function(file) {
            # For the downloaded HTML, we'll force the color to blue and not include a dynamic legend,
            # as the downloaded map is static.
            # If you want the downloaded map to retain the *currently selected* color variable,
            # you would need to regenerate the map object specifically for download
            # or adjust the final_map_object() to take an argument for static vs. dynamic coloring.
            # For simplicity, I'm setting it to blue for the download.
            
            # Recreate the map object specifically for download to ensure consistent static export.
            # This is a bit redundant but ensures the download isn't affected by UI selection.
            sf_data_download <- spatial_points_sf()
            map_center_download <- center_map_coords()
            
            popups_html_download <- lapply(seq_len(nrow(sf_data_download)), function(i) {
                row_data_raw <- st_drop_geometry(sf_data_download[i, ])
                image_html <- ""
                data_for_table <- row_data_raw
                
                if ("photo_url" %in% names(row_data_raw) && !is.na(row_data_raw[["photo_url"]]) && nzchar(as.character(row_data_raw[["photo_url"]]))) {
                    image_url <- as.character(row_data_raw[["photo_url"]])
                    image_html <- paste0("<img src='", image_url, "' style='max-width:150px; height:auto; display:block; margin: 0 auto 5px auto;'><br>")
                    data_for_table <- data_for_table %>% select(-photo_url)
                } else if ("photo_path" %in% names(row_data_raw) && !is.na(row_data_raw[["photo_path"]]) && nzchar(as.character(row_data_raw[["photo_path"]]))) {
                    local_path <- as.character(row_data_raw[["photo_path"]])
                    image_html <- paste0("<img src='", local_path, "' style='max-width:150px; height:auto; display:block; margin: 0 auto 5px auto;'><br>")
                    data_for_table <- data_for_table %>% select(-photo_path)
                }
                
                table_rows_html <- lapply(names(data_for_table), function(col_name) {
                    value <- as.character(data_for_table[[col_name]])
                    paste0("<tr><td><b>", col_name, ":</b></td><td>", value, "</td></tr>")
                })
                table_html <- paste0(
                    "<table class='table table-sm table-condensed table-borderless'>",
                    "<tbody>",
                    paste(table_rows_html, collapse = ""),
                    "</tbody>",
                    "</table>"
                )
                htmltools::HTML(paste0(image_html, table_html))
            })
            
            m_download <- leaflet() %>%
                addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
                addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
                addScaleBar(position = "bottomleft") %>%
                addLayersControl(
                    baseGroups = c("OpenStreetMap", "Satellite"),
                    options = layersControlOptions(collapsed = FALSE)
                )
            
            if (!is.null(map_center_download)) {
                m_download <- m_download %>% setView(lng = map_center_download$lon, lat = map_center_download$lat, zoom = 10)
            } else {
                m_download <- m_download %>% setView(lng = 125.54, lat = 8.94, zoom = 9)
            }
            
            m_download <- m_download %>%
                addCircleMarkers(
                    data = sf_data_download,
                    radius = 5,
                    color = "blue", # Default color for downloaded static map
                    stroke = TRUE,
                    fillOpacity = 0.8,
                    popup = popups_html_download,
                    group = "Data Points"
                )
            
            htmlwidgets::saveWidget(m_download, file, selfcontained = TRUE)
        }
    )
    
    # Output: Raw data table
    output$raw_data_table <- renderDT({
        req(raw_data())
        datatable(raw_data(), options = list(pageLength = 10))
    })
}

# Run the application
shinyApp(ui = ui, server = server)