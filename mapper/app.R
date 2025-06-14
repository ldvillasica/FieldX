# app.R

# Load necessary libraries
library(shiny)
library(shinydashboard)
library(readr)       # For flexible CSV reading (read_delim)
library(dplyr)       # For data manipulation (explicitly called for select)
library(sf)          # For spatial vector data (modern R spatial)
library(leaflet)     # For interactive maps
library(DT)          # For interactive data tables
library(htmltools)   # For creating richer HTML for popups
library(htmlwidgets) # To save the map as HTML
library(shinyjs)     # For showing/hiding UI elements
library(tibble)      # For rownames_to_column (if not automatically loaded by dplyr/tidyverse)
library(leaflet.extras) # For adding extra Leaflet components like minimap and search

# --- Helper Function for Popup Content (Reusable for app and download) ---
generate_popup_html <- function(data_row) {
    # Ensure data_row is a data frame, not an sf object if it was passed directly
    row_data_raw <- if (inherits(data_row, "sf")) st_drop_geometry(data_row) else data_row
    
    image_html <- ""
    data_for_table <- row_data_raw
    
    # --- Image Handling ---
    # Adding subtle border and radius for images
    if ("photo_url" %in% names(row_data_raw) && !is.na(row_data_raw[["photo_url"]]) && nzchar(as.character(row_data_raw[["photo_url"]]))) {
        image_url <- as.character(row_data_raw[["photo_url"]])
        image_html <- paste0("<img src='", image_url, "' style='max-width:150px; height:auto; display:block; margin: 0 auto 5px auto; border: 1px solid #ddd; border-radius: 4px;'><br>")
        data_for_table <- data_for_table %>% dplyr::select(-photo_url) # Remove from table if handled by image
    } 
    # Handle photo_path (local image file, relative to www folder)
    else if ("photo_path" %in% names(data_for_table) && !is.na(data_for_table[["photo_path"]]) && nzchar(as.character(data_for_table[["photo_path"]]))) {
        local_path <- as.character(data_for_table[["photo_path"]])
        image_html <- paste0("<img src='", local_path, "' style='max-width:150px; height:auto; display:block; margin: 0 auto 5px auto; border: 1px solid #ddd; border-radius: 4px;'><br>")
        data_for_table <- data_for_table %>% dplyr::select(-photo_path) # Remove from table if handled by image
    }
    
    # --- Popup Title ---
    # Try to find a good title for the popup: prioritize common ID/Name/Location columns
    popup_title_content <- "Point Details"
    title_found <- FALSE
    
    if (!title_found && "name" %in% names(row_data_raw) && !is.na(row_data_raw[["name"]]) && nzchar(as.character(row_data_raw[["name"]]))) {
        popup_title_content <- as.character(row_data_raw[["name"]])
        data_for_table <- data_for_table %>% dplyr::select(-name)
        title_found <- TRUE
    } else if (!title_found && "id" %in% names(row_data_raw) && !is.na(row_data_raw[["id"]]) && nzchar(as.character(row_data_raw[["id"]]))) {
        popup_title_content <- paste0("ID: ", as.character(row_data_raw[["id"]]))
        data_for_table <- data_for_table %>% dplyr::select(-id)
        title_found <- TRUE
    } else if (!title_found && "barangay" %in% names(row_data_raw) && !is.na(row_data_raw[["barangay"]]) && nzchar(as.character(row_data_raw[["barangay"]]))) {
        popup_title_content <- as.character(row_data_raw[["barangay"]])
        data_for_table <- data_for_table %>% dplyr::select(-barangay)
        title_found <- TRUE
    } else if (!title_found && ncol(row_data_raw) > 0 && !is.numeric(row_data_raw[[1]])) {
        # Fallback: Use the first non-numeric column if no specific title column found
        first_col_name <- names(row_data_raw)[1]
        if (first_col_name %in% names(data_for_table) && !is.na(row_data_raw[[first_col_name]]) && nzchar(as.character(row_data_raw[[first_col_name]]))) {
            popup_title_content <- as.character(row_data_raw[[first_col_name]])
            data_for_table <- data_for_table %>% dplyr::select(-!!sym(first_col_name))
            title_found <- TRUE
        }
    }
    
    # Escape HTML for the title to prevent XSS (if title comes from user data)
    popup_title_html <- htmlEscape(popup_title_content)
    
    # Ensure internal .row_id, coordinates, and other non-display columns are not shown in table
    data_for_table <- data_for_table %>%
        dplyr::select_if(~!any(grepl(".row_id|lon|lat|x_coord|y_coord|easting|northing|geom", ., ignore.case = TRUE)))
    
    # --- Table Content ---
    table_rows_html <- lapply(names(data_for_table), function(col_name) {
        value <- as.character(data_for_table[[col_name]])
        # Display "N/A" for NA or empty string values
        display_value <- if (is.na(value) || value == "") "N/A" else htmlEscape(value) # Escape values too
        
        paste0(
            "<tr style='border-bottom: 1px solid #eee;'>", # Subtle row separator
            "<td style='padding: 4px 8px; font-weight: bold; vertical-align: top; white-space: nowrap;'>", gsub("_", " ", col_name), ":</td>",
            "<td style='padding: 4px 8px; vertical-align: top;'>", display_value, "</td>",
            "</tr>"
        )
    })
    
    table_html <- paste0(
        "<table style='width:100%; border-collapse: collapse; font-size: 13px;'>", # Basic table styling
        "<tbody>",
        paste(table_rows_html, collapse = ""),
        "</tbody>",
        "</table>"
    )
    
    # --- Combine all parts into a wrapper div ---
    htmltools::HTML(paste0(
        "<div style='max-width:250px; overflow-wrap: break-word; text-align: center; padding: 5px; font-family: Arial, sans-serif;'>", # Wrapper for padding, max-width, and font
        "<h4 style='margin-top:0; margin-bottom: 10px; color: #333; font-size: 16px; border-bottom: 1px solid #ddd; padding-bottom: 5px;'>", popup_title_html, "</h4>",
        image_html,
        "<div style='text-align: left; margin-top: 10px; padding: 0 5px;'>", # Align table text left, add padding
        table_html,
        "</div>",
        "</div>"
    ))
}


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
        useShinyjs(), # Initialize shinyjs
        
        # Explicitly load leaflet.extras dependencies (These are fine to keep here)
        tags$head(
            htmlDependency(
                "leaflet-search", "2.9.0", # Version might vary slightly
                src = c(href = "https://cdn.jsdelivr.net/npm/leaflet-search@2.9.0/dist/"),
                script = "leaflet-search.min.js",
                stylesheet = "leaflet-search.min.css"
            ),
            htmlDependency(
                "leaflet-minimap", "3.6.1", # Version might vary slightly
                src = c(href = "https://cdnjs.cloudflare.com/ajax/libs/leaflet-minimap/3.6.1/"),
                script = "Control.MiniMap.min.js",
                stylesheet = "Control.MiniMap.min.css"
            )
        ),
        
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
                            downloadButton("download_map", "Download Map as HTML")
                        ),
                        box(
                            title = "Interactive Map", status = "info", solidHeader = TRUE,
                            width = 8,
                            leafletOutput("map_viewer", height = "600px")
                        )
                    )
                    , # Close fluidRow for map and controls
                    # Raw Data Table will now be in its own fluidRow, taking full width if no sidebar
                    fluidRow(
                        conditionalPanel(
                            condition = "input.show_data_table", # Show/hide based on checkbox
                            box(
                                title = "Raw Data Table", status = "success", solidHeader = TRUE,
                                width = 12, # Now takes full width of the row
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
        # Add validate to provide user-friendly error message if no file
        validate(
            need(input$file1, "Please upload a CSV file.")
        )
        
        inFile <- input$file1
        df <- tryCatch({
            read_delim(inFile$datapath,
                       col_names = input$header,
                       delim = input$sep,
                       quote = input$quote,
                       show_col_types = FALSE) # Suppress column specification messages
        }, error = function(e) {
            # Catch file reading errors
            stop(paste("Error reading CSV file:", e$message))
        })
        
        print(paste("Raw data loaded. Dimensions:", nrow(df), "rows,", ncol(df), "columns."))
        print("Raw data columns:")
        print(names(df))
        
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
        req(df) # Only require 'df' to be available for this UI.
        
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
        
        # Ensure all necessary inputs are available before proceeding
        validate(
            need(raw_data(), "No data uploaded yet."),
            need(input$x_coord_col, "Please select the X coordinate column."),
            need(input$y_coord_col, "Please select the Y coordinate column.")
        )
        
        df <- raw_data() 
        
        # Determine input CRS from user selection
        input_crs_code <- if (input$input_crs_epsg == "") {
            validate(
                need(input$manual_input_crs, "Please enter a custom EPSG code or select a predefined one.")
            )
            as.numeric(input$manual_input_crs)
        } else {
            as.numeric(input$input_crs_epsg)
        }
        
        print(paste("Selected X column:", input$x_coord_col))
        print(paste("Selected Y column:", input$y_coord_col))
        print(paste("Input CRS EPSG code:", input_crs_code))
        
        # Validate coordinate columns exist and are numeric
        if (!all(c(input$x_coord_col, input$y_coord_col) %in% names(df))) {
            stop("Selected X or Y coordinate column not found in data. Please check data columns.")
        }
        
        # Check if coordinates are truly numeric. If not, try coercing, then check.
        # This handles cases where numbers might be read as characters due to locale or formatting.
        if (!is.numeric(df[[input$x_coord_col]])) {
            df[[input$x_coord_col]] <- suppressWarnings(as.numeric(gsub(",", "", df[[input$x_coord_col]]))) # Handle commas as thousands separators
            if(any(is.na(df[[input$x_coord_col]]))) warning("X coordinate column contains non-numeric values that could not be coerced to numbers.")
        }
        if (!is.numeric(df[[input$y_coord_col]])) {
            df[[input$y_coord_col]] <- suppressWarnings(as.numeric(gsub(",", "", df[[input$y_coord_col]]))) # Handle commas as thousands separators
            if(any(is.na(df[[input$y_coord_col]]))) warning("Y coordinate column contains non-numeric values that could not be coerced to numbers.")
        }
        
        # Remove rows with NA coordinates BEFORE creating sf object
        df_clean <- df %>%
            filter(!is.na(!!sym(input$x_coord_col)) & !is.na(!!sym(input$y_coord_col)))
        
        validate(
            need(nrow(df_clean) > 0, "No valid data points found after cleaning (missing coordinates).")
        )
        
        # Create sf object
        sf_obj <- tryCatch({
            st_as_sf(df_clean, coords = c(input$x_coord_col, input$y_coord_col), crs = input_crs_code)
        }, error = function(e) {
            stop(paste("Error creating spatial points (st_as_sf): ", e$message, ". Check X/Y columns or Input CRS. Common issues: non-numeric coordinates, incorrect EPSG code.", sep=""))
        })
        
        # Create character version of Site_ID for potential future search
        if ("Site_ID" %in% names(sf_obj)) {
            sf_obj$Site_ID_char <- as.character(sf_obj$Site_ID) 
        } else {
            sf_obj$Search_ID_char <- as.character(sf_obj[[names(sf_obj)[1]]]) 
            warning("Site_ID column not found for search. Using first column for search propertyName.")
        }
        
        # Print CRS information
        print(paste("SF object created. Original CRS:", st_crs(sf_obj)$epsg, "or", st_crs(sf_obj)$proj4string))
        
        # Leaflet always expects WGS84 (EPSG:4326) for adding markers and base layers.
        # If the input data is in a projected CRS (e.g., UTM), it must be transformed to WGS84 for Leaflet.
        if (st_crs(sf_obj)$epsg != 4326) {
            print("Transforming data to EPSG:4326 (WGS84) for Leaflet display.")
            sf_obj <- tryCatch({
                st_transform(sf_obj, crs = 4326)
            }, error = function(e) {
                stop(paste("Error transforming data to WGS84 for map display: ", e$message, ". Ensure PROJ data is available or check input CRS.", sep=""))
            })
            print(paste("Data transformed. New CRS:", st_crs(sf_obj)$epsg))
        } else {
            print("Data already in EPSG:4326 (WGS84). No transformation needed.")
        }
        
        # Validate transformed coordinates
        coords_wgs84 <- st_coordinates(sf_obj)
        if (any(coords_wgs84[,2] < -90) || any(coords_wgs84[,2] > 90) || any(coords_wgs84[,1] < -180) || any(coords_wgs84[,1] > 180)) {
            stop("Transformed coordinates are outside valid Lat/Lon range. Please double-check your input data and selected CRS.")
        }
        
        print(paste("Number of points after processing:", nrow(sf_obj)))
        
        sf_obj
    })
    
    # Reactive: Calculate the geographic center of the data for map centering
    center_map_coords <- reactive({
        sf_obj <- spatial_points_sf() # Depend on the *processed* spatial data
        if (is.null(sf_obj) || nrow(sf_obj) == 0) {
            print("No spatial data available to calculate center.")
            return(NULL)
        }
        
        coords <- st_coordinates(sf_obj)
        
        # Ensure coordinates are in WGS84 (Lat/Lon) before calculating mean
        mean_lon <- mean(coords[, 1], na.rm = TRUE)
        mean_lat <- mean(coords[, 2], na.rm = TRUE)
        
        # Validate the calculated means
        if (is.nan(mean_lon) || is.nan(mean_lat) || abs(mean_lat) > 90 || abs(mean_lon) > 180) {
            warning("Calculated center coordinates are invalid (NaN or outside geographic bounds). Using default center.")
            return(NULL)
        }
        print(paste("Calculated map center: Lon =", mean_lon, ", Lat =", mean_lat))
        list(lon = mean_lon, lat = mean_lat)
    })
    
    # Reactive: Generate the Leaflet map object (used for both display and download)
    final_map_object <- reactive({
        # Only proceed if spatial_points_sf is not empty
        validate(
            need(spatial_points_sf(), "Please upload data and click 'Update Map' to view points."),
            need(nrow(spatial_points_sf()) > 0, "No valid data points to display on the map after processing.")
        )
        
        sf_data <- spatial_points_sf()
        map_center <- center_map_coords()
        
        # Determine coloring scheme for map display
        color_by_var <- input$color_var 
        pal <- NULL
        colors <- "red" # Changed default color to red for visibility
        
        if (color_by_var != "" && color_by_var %in% names(sf_data) && is.numeric(sf_data[[color_by_var]])) {
            pal <- colorNumeric("viridis", domain = sf_data[[color_by_var]], na.color = "transparent")
            colors <- pal(sf_data[[color_by_var]])
            colors[is.na(colors)] <- "#808080" # Grey for NA values
            print(paste("Coloring points by:", color_by_var))
        } else {
            print("Not coloring points by a numeric variable (or variable not found/numeric). Using default red.")
        }
        
        # Generate popups for the live app display
        popups_html_live_app <- lapply(seq_len(nrow(sf_data)), function(i) {
            generate_popup_html(sf_data[i, ]) # Use the helper function
        })
        
        m <- leaflet() %>%
            addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
            addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
            addScaleBar(position = "bottomleft") %>%
            addLayersControl(
                baseGroups = c("OpenStreetMap", "Satellite"),
                options = layersControlOptions(collapsed = FALSE)
            ) # Removed addMiniMap and addSearchFeatures
        
        # Set initial view
        if (!is.null(map_center)) {
            m <- m %>% setView(lng = map_center$lon, lat = map_center$lat, zoom = 10) # Zoom for regional view
            print(paste("Map view set to calculated center: Lon=", map_center$lon, ", Lat=", map_center$lat, ", Zoom=10"))
        } else {
            # Default to a general Caraga Region center if no data or invalid coords
            m <- m %>% setView(lng = 125.54, lat = 8.94, zoom = 9) # Butuan City/Caraga Region
            print("Using default map center due to invalid or missing calculated coordinates.")
        }
        
        # Add markers with popups for the live app
        m <- m %>%
            addCircleMarkers(
                data = sf_data,
                radius = 7, # Increased radius for visibility
                color = colors, # Use dynamic colors for display in the app (now red default)
                stroke = TRUE,
                fillOpacity = 0.8,
                popup = popups_html_live_app, # Add popups to live app
                group = "Data Points" # IMPORTANT: Assign a group name for searching (even if search box is not present)
            )
        
        # Add legend if coloring by a variable for app display
        if (!is.null(pal)) {
            m <- m %>% addLegend(pal = pal, values = sf_data[[color_by_var]], title = color_by_var, position = "bottomright")
        }
        
        print("Leaflet map object created.")
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
            
            sf_data_download <- spatial_points_sf()
            map_center_download <- center_map_coords()
            
            # Reuse the coloring logic from the main map display for the downloaded map
            color_by_var_download <- input$color_var # This will capture the variable selected in the app
            pal_download <- NULL
            colors_download <- "red" # Default if no variable selected or not numeric (now red)
            
            if (color_by_var_download != "" && color_by_var_download %in% names(sf_data_download) && is.numeric(sf_data_download[[color_by_var_download]])) {
                pal_download <- colorNumeric("viridis", domain = sf_data_download[[color_by_var_download]], na.color = "transparent")
                colors_download <- pal_download(sf_data_download[[color_by_var_download]])
                colors_download[is.na(colors_download)] <- "#808080" # Grey for NA values
            }
            
            # Generate popups for the downloaded HTML map
            popups_html_download <- lapply(seq_len(nrow(sf_data_download)), function(i) {
                generate_popup_html(sf_data_download[i, ]) # Use the helper function
            })
            
            m_download <- leaflet() %>%
                addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
                addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
                addScaleBar(position = "bottomleft") %>%
                addLayersControl(
                    baseGroups = c("OpenStreetMap", "Satellite"),
                    options = layersControlOptions(collapsed = FALSE)
                ) # Removed addMiniMap and addSearchFeatures
            
            if (!is.null(map_center_download)) {
                m_download <- m_download %>% setView(lng = map_center_download$lon, lat = map_center_download$lat, zoom = 10)
            } else {
                m_download <- m_download %>% setView(lng = 125.54, lat = 8.94, zoom = 9) # Default to Butuan City/Caraga Region
            }
            
            # Add markers with popups for the downloaded map
            m_download <- m_download %>%
                addCircleMarkers(
                    data = sf_data_download,
                    radius = 7, # Increased radius for visibility
                    color = colors_download, # Use the dynamic colors from the app's current state (now red)
                    stroke = TRUE,
                    fillOpacity = 0.8,
                    popup = popups_html_download, # Add popups to downloaded map
                    group = "Data Points" # IMPORTANT: Assign a group name for searching (even if search box is not present)
                )
            
            # Add legend to the downloaded map if coloring by a variable
            if (!is.null(pal_download)) {
                m_download <- m_download %>% addLegend(pal = pal_download, values = sf_data_download[[color_by_var_download]], title = color_by_var_download, position = "bottomright")
            }
            
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