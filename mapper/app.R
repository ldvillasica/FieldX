# app.R

# Load necessary libraries
# If you don't have these installed, run:
# install.packages(c("shiny", "leaflet", "ggplot2", "sf", "rnaturalearth", "rnaturalearthdata"))
library(shiny)
library(leaflet) # For interactive map
library(ggplot2) # For static plot and export
library(sf)      # For spatial data handling
library(rnaturalearth) # For base map data (countries)
library(rnaturalearthdata) # For base map data (countries and states)

# Define UI for application
ui <- fluidPage(
    # Application title
    titlePanel("Coordinate Plotter and Exporter"),
    
    # Sidebar layout with input and output
    sidebarLayout(
        sidebarPanel(
            # Input for CSV file
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Checkbox for header
            checkboxInput("header", "Header", TRUE),
            
            # Radio buttons for separator
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input for Latitude Column Name
            textInput("lat_col", "Latitude Column Name", value = "latitude"),
            # Input for Longitude Column Name
            textInput("lon_col", "Longitude Column Name", value = "longitude"),
            
            hr(), # Horizontal rule
            
            h4("Static Plot Options"),
            # Slider for plot point size
            sliderInput("point_size", "Point Size on Static Plot:",
                        min = 0.5, max = 10, value = 3, step = 0.5),
            
            # Sliders for controlling the static map's bounding box
            h5("Static Map View Bounding Box:"),
            sliderInput("min_lon", "Minimum Longitude:",
                        min = -180, max = 180, value = -180, step = 1),
            sliderInput("max_lon", "Maximum Longitude:",
                        min = -180, max = 180, value = 180, step = 1),
            sliderInput("min_lat", "Minimum Latitude:",
                        min = -90, max = 90, value = -90, step = 1),
            sliderInput("max_lat", "Maximum Latitude:",
                        min = -90, max = 90, value = 90, step = 1),
            
            # Button to download the static plot
            downloadButton("downloadPlot", "Download Static Plot (PNG)")
        ),
        
        # Main panel for displaying outputs
        mainPanel(
            h3("Interactive Map (Leaflet)"),
            leafletOutput("interactiveMap", height = "500px"),
            hr(),
            h3("Static Plot Preview (ggplot2)"),
            plotOutput("staticPlotPreview", height = "500px")
        )
    )
)

# Define server logic
server <- function(input, output, session) {
    
    # Reactive expression to read the uploaded CSV data
    data_points <- reactive({
        req(input$file1) # Ensure a file is uploaded
        
        tryCatch({
            df <- read.csv(input$file1$datapath,
                           header = input$header,
                           sep = input$sep)
            
            # Validate column names
            lat_col_name <- input$lat_col
            lon_col_name <- input$lon_col
            
            if (!lat_col_name %in% names(df) || !lon_col_name %in% names(df)) {
                stop("Specified latitude or longitude column not found in the file. Please check column names.")
            }
            
            # Convert to numeric and remove rows with NA in lat/lon
            df[[lat_col_name]] <- as.numeric(df[[lat_col_name]])
            df[[lon_col_name]] <- as.numeric(df[[lon_col_name]])
            df <- na.omit(df[, c(lat_col_name, lon_col_name)])
            
            # Filter out invalid coordinates (e.g., outside -90 to 90 for lat, -180 to 180 for lon)
            df <- df[df[[lat_col_name]] >= -90 & df[[lat_col_name]] <= 90, ]
            df <- df[df[[lon_col_name]] >= -180 & df[[lon_col_name]] <= 180, ]
            
            if (nrow(df) == 0) {
                stop("No valid coordinate data found after processing.")
            }
            
            df
            
        }, error = function(e) {
            # Display error message to the user
            showNotification(paste("Error reading file or processing data:", e$message), type = "error", duration = NULL)
            return(NULL) # Return NULL on error
        })
    })
    
    # Update slider ranges based on uploaded data
    observe({
        points <- data_points()
        if (!is.null(points) && nrow(points) > 0) {
            # Get the actual min/max for the uploaded data
            min_lon_data <- floor(min(points[[input$lon_col]]))
            max_lon_data <- ceiling(max(points[[input$lon_col]]))
            min_lat_data <- floor(min(points[[input$lat_col]]))
            max_lat_data <- ceiling(max(points[[input$lat_col]]))
            
            # Update sliders with data-driven ranges, ensuring they stay within global bounds
            updateSliderInput(session, "min_lon", value = max(min_lon_data - 5, -180),
                              min = -180, max = max_lon_data + 5) # Adjust max to allow going beyond data
            updateSliderInput(session, "max_lon", value = min(max_lon_data + 5, 180),
                              min = min_lon_data - 5, max = 180) # Adjust min to allow going beyond data
            updateSliderInput(session, "min_lat", value = max(min_lat_data - 5, -90),
                              min = -90, max = max_lat_data + 5) # Adjust max to allow going beyond data
            updateSliderInput(session, "max_lat", value = min(max_lat_data + 5, 90),
                              min = min_lat_data - 5, max = 90) # Adjust min to allow going beyond data
        } else {
            # Reset to global ranges if no data or invalid data
            updateSliderInput(session, "min_lon", value = -180, min = -180, max = 180)
            updateSliderInput(session, "max_lon", value = 180, min = -180, max = 180)
            updateSliderInput(session, "min_lat", value = -90, min = -90, max = 90)
            updateSliderInput(session, "max_lat", value = 90, min = -90, max = 90)
        }
    })
    
    
    # Render the interactive Leaflet map
    output$interactiveMap <- renderLeaflet({
        points <- data_points()
        req(points) # Ensure data is available
        
        leaflet() %>%
            addTiles() %>% # Add default OpenStreetMap tiles
            addMarkers(data = points,
                       lng = ~get(input$lon_col), # Use the specified longitude column
                       lat = ~get(input$lat_col),  # Use the specified latitude column
                       popup = ~paste("Lat:", get(input$lat_col), "<br>Lon:", get(input$lon_col))) %>%
            # Zoom to fit all markers
            fitBounds(min(points[[input$lon_col]]), min(points[[input$lat_col]]),
                      max(points[[input$lon_col]]), max(points[[input$lat_col]]))
    })
    
    # Reactive expression to generate the static plot
    static_plot <- reactive({
        points <- data_points()
        req(points) # Ensure data is available
        
        # Get world map data (countries)
        world <- ne_countries(scale = "medium", returnclass = "sf")
        # Removed the ne_states line to avoid rnaturalearthhires dependency issues.
        # states <- ne_states(country = NULL, returnclass = "sf")
        
        # Create the ggplot
        p <- ggplot() +
            geom_sf(data = world, fill = "lightgray", color = "darkgray", linewidth = 0.5) + # Country boundaries
            # Removed the geom_sf layer for regional boundaries to avoid rnaturalearthhires dependency issues.
            # geom_sf(data = states, fill = NA, color = "darkblue", linewidth = 0.3, linetype = "dotted") + # Regional boundaries
            geom_point(data = points,
                       aes_string(x = input$lon_col, y = input$lat_col), # Use aes_string for dynamic column names
                       color = "red", size = input$point_size, alpha = 0.7) + # Plot points
            # Apply bounding box from sliders
            coord_sf(xlim = c(input$min_lon, input$max_lon),
                     ylim = c(input$min_lat, input$max_lat),
                     crs = 4326) + # Set coordinate system to WGS84 (latitude/longitude)
            labs(title = "Coordinate Plot with Country Boundaries", # Updated title
                 x = "Longitude",
                 y = "Latitude") +
            theme_minimal() +
            theme(
                plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
                axis.title = element_text(size = 12),
                axis.text = element_text(size = 10),
                panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),
                panel.background = element_rect(fill = "aliceblue", color = NA),
                plot.margin = unit(c(1, 1, 1, 1), "cm") # Add some margin around the plot
            )
        
        p
    })
    
    # Render the static plot preview
    output$staticPlotPreview <- renderPlot({
        static_plot()
    })
    
    # Download handler for the static plot
    output$downloadPlot <- downloadHandler(
        filename = function() {
            paste("coordinate_plot-", Sys.Date(), ".png", sep = "")
        },
        content = function(file) {
            # Save the plot as PNG
            ggsave(file, plot = static_plot(), device = "png", width = 10, height = 8, units = "in", dpi = 300)
        }
    )
}

# Run the application
shinyApp(ui = ui, server = server)
