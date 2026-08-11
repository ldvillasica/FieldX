library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(leaflet) 
library(sf)
library(gstat)   
library(raster)  

# --- 1. THE TEMPORAL SCIENTIFIC ENGINE ---
# Added 'duration' (hours) to calculate cumulative impact
calc_soil_physics_temporal <- function(sand, silt, clay, vwc, bd, wdc, wds, was, rain_rate, duration, slope, strategy) {
    # Cumulative Rainfall
    total_rain <- rain_rate * duration 
    
    denom_texture <- clay + silt + 0.01
    denom_clay <- clay + 0.01
    dr_val <- ((wdc + wds) / denom_texture) * 100
    dr_dec <- dr_val / 100
    fi_val <- (clay - wdc) / denom_clay
    resilience <- (was / 100) * (fi_val + 0.1)
    
    # SDI and Infiltration
    sdi_risk <- pmin(100, ((dr_dec + 0.01) / (resilience + 0.05)) * (bd / 1.5) * 20)
    infilt_cap <- (100 - clay) * (1.5 / bd) * 0.5 
    clogging_idx <- (wdc * dr_dec) * (bd / 1.5)
    eff_infilt <- pmax(2, (infilt_cap / (1 + (clogging_idx / 10))) * (1 - (vwc / (bd / 2.65))))
    
    # Temporal Runoff Depth
    # Calculation: (Intensity - Infiltration) * Time
    runoff_depth <- pmax(0, (rain_rate - eff_infilt) * duration)
    
    # Slope Risk Factor: Accelerate loss if slope > 20%
    slope_multiplier <- if(slope > 20) 1.5 else 1.0
    
    ki <- (dr_dec / (fi_val + 0.1)) * (wdc / 100) 
    kr <- 1 / (was + 1)                          
    mitigation <- switch(strategy, "None"=1.0, "Gypsum"=0.65, "Cover Crops"=0.55, "Biochar"=0.75)
    
    # Gross Loss scaled by total storm volume and slope risk
    gross_loss <- ((ki * (total_rain^2) * 0.0001) + (kr * runoff_depth * (slope/100))) * 10 * mitigation * slope_multiplier
    
    return(data.frame(SDI=sdi_risk, GrossLoss=gross_loss, Runoff=runoff_depth, DR=dr_val, FI=fi_val))
}

# --- 2. USER INTERFACE ---
ui <- dashboardPage(
    skin = "red",
    dashboardHeader(title = "FieldX Storm Risk"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Project Setup", tabName = "upload", icon = icon("file-upload")),
            menuItem("Storm Impact Map", tabName = "map_view", icon = icon("cloud-showers-heavy")),
            hr(),
            h4(" Storm Profile", style="margin-left:15px;"),
            sliderInput("rain_rate", "Intensity (mm/hr)", 0, 150, 50),
            sliderInput("duration", "Storm Duration (Hours)", 0.5, 24, 2, step=0.5),
            hr(),
            h4(" Field Topography", style="margin-left:15px;"),
            sliderInput("slope_input", "Average Site Slope (%)", 0, 60, 15),
            selectInput("strategy", "Mitigation Strategy", choices = c("None", "Gypsum", "Cover Crops", "Biochar"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "upload",
                    fluidRow(
                        box(title = "Data Import", status = "danger", width = 4, solidHeader = TRUE,
                            fileInput("file1", "Upload CSV", accept = ".csv"),
                            helpText("Ensure CSV has Lat/Long and Soil Physics Columns."),
                            downloadButton("dl_results", "Export Storm Report", class="btn-block")),
                        box(title = "Column Mapping", status = "warning", width = 8, uiOutput("mapping_ui"))
                    )
            ),
            tabItem(tabName = "map_view",
                    fluidRow(
                        box(title = "Cumulative Erosion Potential", status = "danger", width = 9,
                            leafletOutput("idw_map", height = "750px")),
                        box(title = "Map Settings", status = "info", width = 3,
                            selectInput("map_var", "Visualize:", 
                                        choices = c("Gross Loss (Total t/ha)" = "GrossLoss", 
                                                    "Cumulative Runoff (mm)" = "Runoff",
                                                    "Stability (SDI)" = "SDI")),
                            sliderInput("idw_power", "IDW Power:", 1, 5, 2),
                            checkboxInput("mask_land", "Mask to Study Area", TRUE),
                            hr(),
                            h5("Risk Legend:"),
                            helpText("Red zones indicate high-risk convergence of poor soil structure and storm duration.")
                        )
                    )
            )
        )
    )
)

# --- 3. SERVER ---
server <- function(input, output, session) {
    raw_csv <- reactive({ req(input$file1); read.csv(input$file1$datapath) })
    
    output$mapping_ui <- renderUI({
        req(raw_csv()); cols <- colnames(raw_csv())
        tagList(
            fluidRow(
                column(6, selectInput("m_lat", "Latitude", choices = cols, selected = grep("lat", cols, T, T, T)[1])),
                column(6, selectInput("m_lon", "Longitude", choices = cols, selected = grep("lon|long", cols, T, T, T)[1]))
            ), hr(),
            fluidRow(
                column(3, selectInput("m_sand", "Sand", cols, grep("sand", cols, T, T, T)[1])),
                column(3, selectInput("m_silt", "Silt", cols, grep("silt", cols, T, T, T)[1])),
                column(3, selectInput("m_clay", "Clay", cols, grep("clay", cols, T, T, T)[1])),
                column(3, selectInput("m_bd", "Bulk Density", cols, grep("bd", cols, T, T, T)[1]))
            ),
            fluidRow(
                column(3, selectInput("m_wdc", "WDC", cols, grep("wdc", cols, T, T, T)[1])),
                column(3, selectInput("m_wds", "WDS", cols, grep("wds", cols, T, T, T)[1])),
                column(3, selectInput("m_was", "WAS", cols, grep("was", cols, T, T, T)[1])),
                column(3, selectInput("m_vwc", "Initial VWC", cols, grep("vwc|water", cols, T, T, T)[1]))
            )
        )
    })
    
    processed_data <- reactive({
        req(input$m_sand, input$m_lat)
        raw_csv() %>% rowwise() %>%
            mutate(C = list(calc_soil_physics_temporal(
                !!sym(input$m_sand), !!sym(input$m_silt), !!sym(input$m_clay), !!sym(input$m_vwc), 
                !!sym(input$m_bd), !!sym(input$m_wdc), !!sym(input$m_wds), !!sym(input$m_was), 
                input$rain_rate, input$duration, input$slope_input, input$strategy
            ))) %>%
            unnest_wider(C)
    })
    
    output$idw_map <- renderLeaflet({
        req(processed_data()); df <- processed_data(); target <- input$map_var
        pts <- st_as_sf(df, coords = c(input$m_lon, input$m_lat), crs = 4326)
        bbox <- st_bbox(pts)
        grid <- expand.grid(lon = seq(bbox["xmin"], bbox["xmax"], length.out = 100),
                            lat = seq(bbox["ymin"], bbox["ymax"], length.out = 100))
        grid_sf <- st_as_sf(grid, coords = c("lon", "lat"), crs = 4326)
        res <- gstat(formula = as.formula(paste(target, "~ 1")), data = pts, set = list(idp = input$idw_power))
        predict_surface <- predict(res, grid_sf)
        grid$val <- predict_surface$var1.pred
        if(input$mask_land) {
            hull <- st_convex_hull(st_union(pts)); inside <- st_within(grid_sf, hull, sparse = FALSE)
            grid$val[!inside] <- NA
        }
        r <- rasterFromXYZ(grid[, c("lon", "lat", "val")]); crs(r) <- CRS("+init=epsg:4326")
        pal <- colorNumeric(palette = "YlOrRd", domain = grid$val, na.color = "transparent")
        
        leaflet() %>%
            addProviderTiles(providers$OpenTopoMap) %>%
            addRasterImage(r, colors = pal, opacity = 0.7) %>%
            addLegend(pal = pal, values = grid$val, title = target) %>%
            addCircleMarkers(data = df, lng = ~get(input$m_lon), lat = ~get(input$m_lat), 
                             radius = 4, color = "black", weight = 1, fillOpacity = 1, fillColor = "white")
    })
    
    output$dl_results <- downloadHandler(
        filename = function() { paste0("Storm_Loss_Report_", Sys.Date(), ".csv") },
        content = function(file) { write.csv(processed_data(), file, row.names = FALSE) }
    )
}

shinyApp(ui, server)