library(shiny)
library(bslib)
library(sf)
library(ggplot2)
library(ggspatial)
library(maptiles)
library(terra)
library(cowplot)
library(rnaturalearth)

# Fetch country outline once for efficiency
ph_outline <- ne_countries(country = "Philippines", scale = "medium", returnclass = "sf")

ui <- page_sidebar(
    title = "FieldX Topo Point Mapper",
    theme = bs_theme(bootswatch = "flatly"),
    
    sidebar = sidebar(
        fileInput("file", "1. Upload CSV Data", accept = c(".csv", ".txt")),
        uiOutput("col_selectors"),
        hr(),
        h6("2. Customize Map Controls"),
        uiOutput("legend_controls"),
        uiOutput("bbox_controls"),
        hr(),
        downloadButton("download_map", "Download Map (300 DPI)", class = "btn-primary w-100")
    ),
    
    navset_card_tab(
        nav_panel("Map Preview", plotOutput("map_plot", height = "650px")),
        nav_panel("CSV Inspector", 
                  verbatimTextOutput("csv_summary"),
                  hr(),
                  tableOutput("csv_preview"))
    )
)

server <- function(input, output, session) {
    
    # 1. Flexible CSV Import
    df_data <- reactive({
        req(input$file)
        path <- input$file$datapath
        
        df <- tryCatch(read.csv(path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
        if (is.null(df) || ncol(df) <= 1) {
            df <- read.csv(path, sep = ";", stringsAsFactors = FALSE, check.names = FALSE)
        }
        df
    })
    
    # 2. Select Coordinate & Location Label Columns
    output$col_selectors <- renderUI({
        req(df_data())
        cols <- names(df_data())
        
        lon_guess <- grep("lon|x|east|lng|coord_x", cols, ignore.case = TRUE, value = TRUE)[1]
        lat_guess <- grep("lat|y|north|coord_y", cols, ignore.case = TRUE, value = TRUE)[1]
        loc_guess <- grep("site|loc|name|station|id|label|place", cols, ignore.case = TRUE, value = TRUE)[1]
        
        if (is.na(lon_guess)) lon_guess <- cols[1]
        if (is.na(lat_guess)) lat_guess <- ifelse(length(cols) > 1, cols[2], cols[1])
        if (is.na(loc_guess)) loc_guess <- cols[1]
        
        tagList(
            selectInput("lon_col", "Longitude Column", choices = cols, selected = lon_guess),
            selectInput("lat_col", "Latitude Column", choices = cols, selected = lat_guess),
            selectInput("loc_col", "Location/Site Label Column", choices = cols, selected = loc_guess)
        )
    })
    
    # 3. Dynamic Legend Title Control
    output$legend_controls <- renderUI({
        req(df_data())
        textInput("legend_title", "Legend Title", value = "Location Sites")
    })
    
    # CSV Inspection Outputs
    output$csv_summary <- renderPrint({
        req(df_data(), input$lon_col, input$lat_col)
        df <- df_data()
        
        raw_lon <- df[[input$lon_col]]
        raw_lat <- df[[input$lat_col]]
        
        clean_lon <- suppressWarnings(as.numeric(as.character(raw_lon)))
        clean_lat <- suppressWarnings(as.numeric(as.character(raw_lat)))
        
        valid <- !is.na(clean_lon) & !is.na(clean_lat)
        
        cat("--- CSV DIAGNOSTIC REPORT ---\n")
        cat("Total Rows Uploaded:", nrow(df), "\n")
        cat("Valid Numeric Coordinate Rows:", sum(valid), "\n")
    })
    
    output$csv_preview <- renderTable({
        req(df_data())
        head(df_data(), 10)
    })
    
    # 4. Dynamic Bounding Sliders
    output$bbox_controls <- renderUI({
        req(df_data(), input$lon_col, input$lat_col)
        
        df <- df_data()
        lon_vals <- suppressWarnings(as.numeric(as.character(df[[input$lon_col]])))
        lat_vals <- suppressWarnings(as.numeric(as.character(df[[input$lat_col]])))
        
        valid <- !is.na(lon_vals) & !is.na(lat_vals)
        lon_vals <- lon_vals[valid]
        lat_vals <- lat_vals[valid]
        
        validate(
            need(length(lon_vals) > 0, "No numeric coordinates found in selected columns.")
        )
        
        lon_min <- min(lon_vals); lon_max <- max(lon_vals)
        lat_min <- min(lat_vals); lat_max <- max(lat_vals)
        
        lon_pad <- if (lon_min == lon_max) 0.05 else (lon_max - lon_min) * 0.2
        lat_pad <- if (lat_min == lat_max) 0.05 else (lat_max - lat_min) * 0.2
        
        tagList(
            sliderInput("lon_range", "Longitude Range",
                        min = round(lon_min - lon_pad * 2, 4),
                        max = round(lon_max + lon_pad * 2, 4),
                        value = c(round(lon_min - lon_pad, 4), round(lon_max + lon_pad, 4)),
                        step = 0.001),
            sliderInput("lat_range", "Latitude Range",
                        min = round(lat_min - lat_pad * 2, 4),
                        max = round(lat_max + lat_pad * 2, 4),
                        value = c(round(lat_min - lat_pad, 4), round(lat_max + lat_pad, 4)),
                        step = 0.001)
        )
    })
    
    # 5. Fetch Map Spatial Data
    map_spatial_data <- reactive({
        req(df_data(), input$lon_col, input$lat_col, input$loc_col, input$lon_range, input$lat_range)
        req(length(input$lon_range) == 2, length(input$lat_range) == 2)
        
        df <- df_data()
        lon_vals <- suppressWarnings(as.numeric(as.character(df[[input$lon_col]])))
        lat_vals <- suppressWarnings(as.numeric(as.character(df[[input$lat_col]])))
        
        valid <- !is.na(lon_vals) & !is.na(lat_vals)
        df_clean <- df[valid, ]
        validate(need(nrow(df_clean) > 0, "No valid data points found."))
        
        # Ensure site labels are clean strings
        df_clean$location_label <- as.character(df_clean[[input$loc_col]])
        
        pts_wgs84 <- st_as_sf(df_clean, coords = c(input$lon_col, input$lat_col), crs = 4326)
        
        tiles <- get_tiles(pts_wgs84, provider = "Esri.WorldTopoMap", crop = FALSE)
        
        list(
            pts = pts_wgs84,
            xlim = input$lon_range,
            ylim = input$lat_range,
            tiles = tiles
        )
    })
    
    # 6. Render Composite Map with External Legend and Inset
    build_map <- reactive({
        data <- map_spatial_data()
        req(data, input$legend_title)
        
        # Bounding box polygon for inset locator
        bbox_poly <- st_as_sfc(st_bbox(c(
            xmin = data$xlim[1], ymin = data$ylim[1],
            xmax = data$xlim[2], ymax = data$ylim[2]
        ), crs = st_crs(4326)))
        
        # Main Map (Legend outside on the right)
        main_map <- ggplot() +
            layer_spatial(data$tiles) +
            geom_sf(data = data$pts, aes(color = location_label), size = 4, alpha = 0.95) +
            scale_color_brewer(palette = "Set1", name = input$legend_title) +
            coord_sf(
                xlim = data$xlim,
                ylim = data$ylim,
                expand = FALSE,
                crs = 4326
            ) +
            annotation_scale(location = "bl", width_hint = 0.25) +
            annotation_north_arrow(
                location = "tr", 
                style = north_arrow_minimal(),
                pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in")
            ) +
            theme_bw() +
            theme(
                axis.title = element_blank(),
                legend.position = "right",
                legend.box.background = element_rect(color = "black", size = 0.3),
                legend.background = element_rect(fill = "white"),
                legend.key = element_blank(),
                plot.margin = margin(5, 5, 5, 5)
            )
        
        # Inset Philippines Overview Map
        inset_map <- ggplot() +
            geom_sf(data = ph_outline, fill = "#f0f0f0", color = "darkgray", size = 0.2) +
            geom_sf(data = bbox_poly, fill = "red", color = "red", alpha = 0.3, size = 0.8) +
            coord_sf(xlim = c(116, 127), ylim = c(4, 22), expand = FALSE) +
            theme_void() +
            theme(
                panel.background = element_rect(fill = "white", color = "black", size = 0.5),
                plot.margin = margin(2, 2, 2, 2)
            )
        
        # Composite combining main map + bottom-left inset (adjust x/y to prevent legend overlap)
        ggdraw() +
            draw_plot(main_map) +
            draw_plot(inset_map, x = 0.08, y = 0.08, width = 0.22, height = 0.32)
    })
    
    output$map_plot <- renderPlot({
        build_map()
    })
    
    output$download_map <- downloadHandler(
        filename = function() { paste0("topo_map_ph_", Sys.Date(), ".png") },
        content = function(file) { ggsave(file, plot = build_map(), width = 10, height = 7, dpi = 300) }
    )
}

shinyApp(ui, server)