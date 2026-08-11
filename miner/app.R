library(shiny)
library(bslib)
library(httr2)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(DT)

# --- UI Definition ---
ui <- page_sidebar(
    theme = bs_theme(version = 5, bootswatch = "flatly"),
    title = "Mistelbach Data Miner: Precipitation & Soil Loss",
    
    sidebar = sidebar(
        title = "Data Mining & Model Inputs",
        width = 320,
        
        numericInput("start_year", "Start Year:", value = 2010, min = 1980, max = 2025),
        numericInput("end_year", "End Year:", value = 2024, min = 1980, max = 2025),
        
        hr(),
        h5("RUSLE Soil Factors (Mistelbach)"),
        p(style = "font-size: 0.82em; color: #666;", 
          "Default parameters tuned for agricultural loess soil in Weinviertel:"),
        
        numericInput("factor_k", "K Factor (Soil Erodibility):", value = 0.032, step = 0.005),
        numericInput("factor_ls", "LS Factor (Slope Topography):", value = 1.5, step = 0.1),
        numericInput("factor_c", "C Factor (Cover/Crop Management):", value = 0.20, step = 0.05),
        numericInput("factor_p", "P Factor (Support Practices):", value = 0.80, step = 0.05),
        
        actionButton("fetch_btn", "Mine Data & Run Model", class = "btn-primary w-100 mt-2")
    ),
    
    navset_card_tab(
        nav_panel(
            title = "Dashboard",
            icon = icon("chart-line"),
            fluidRow(
                column(12, 
                       div(class = "p-2 bg-light border rounded mb-3",
                           htmlOutput("summary_boxes")
                       )
                )
            ),
            fluidRow(
                column(12, plotlyOutput("trend_plot", height = "420px"))
            )
        ),
        nav_panel(
            title = "Spatial Context",
            icon = icon("map-marked-alt"),
            leafletOutput("map_output", height = "500px")
        ),
        nav_panel(
            title = "Data & Export",
            icon = icon("table"),
            downloadButton("download_csv", "Export CSV Data", class = "btn-success mb-3"),
            DTOutput("data_table")
        )
    )
)

# --- Server Logic ---
server <- function(input, output, session) {
    
    # Reactive event triggered by button or app initial load
    mined_data <- eventReactive(input$fetch_btn, ignoreNULL = FALSE, {
        
        req(input$start_year, input$end_year)
        
        if (input$start_year > input$end_year) {
            showNotification("Start Year must be less than or equal to End Year.", type = "error")
            return(NULL)
        }
        
        # Target Coordinates: Mistelbach, Lower Austria
        lat <- 48.57
        lon <- 16.57
        
        start_date <- paste0(input$start_year, "-01-01")
        end_date <- paste0(input$end_year, "-12-31")
        
        df <- NULL
        
        # 1. Mine precipitation data via Open-Meteo Historical Archive API
        withProgress(message = "Mining daily climate data for Mistelbach...", value = 0.4, {
            tryCatch({
                api_url <- "https://archive-api.open-meteo.com/v1/archive"
                
                req <- request(api_url) %>%
                    req_url_query(
                        latitude = lat,
                        longitude = lon,
                        start_date = start_date,
                        end_date = end_date,
                        daily = "precipitation_sum",
                        timezone = "Europe/Vienna"
                    )
                
                res <- req_perform(req)
                json <- resp_body_json(res, simplifyVector = TRUE)
                
                daily_df <- data.frame(
                    Date = as.Date(json$daily$time),
                    Precip_mm = json$daily$precipitation_sum,
                    stringsAsFactors = FALSE
                )
                
                # Aggregate daily measurements to annual summaries
                df <- daily_df %>%
                    mutate(Year = as.integer(format(Date, "%Y"))) %>%
                    group_by(Year) %>%
                    summarise(
                        Annual_Precip_mm = round(sum(Precip_mm, na.rm = TRUE), 1),
                        Max_Daily_Precip_mm = round(max(Precip_mm, na.rm = TRUE), 1),
                        Rain_Days = sum(Precip_mm > 1.0, na.rm = TRUE)
                    ) %>%
                    ungroup()
                
            }, error = function(e) {
                showNotification(paste("API Error:", e$message), type = "error")
                return(NULL)
            })
        })
        
        incProgress(0.4, message = "Calculating Soil Loss (RUSLE)...")
        
        # 2. Estimate Rainfall Erosivity (R-factor) and Annual Soil Loss (A)
        # Empirical R-factor equation for Central Europe: R = 0.082 * (P ^ 1.22)
        # Annual Soil Loss A = R * K * LS * C * P (metric tons / ha / year)
        df <- df %>%
            mutate(
                R_Factor = round(0.082 * (Annual_Precip_mm ^ 1.22), 2),
                K_Factor = input$factor_k,
                LS_Factor = input$factor_ls,
                C_Factor = input$factor_c,
                P_Factor = input$factor_p,
                Soil_Loss_t_ha = round(R_Factor * K_Factor * LS_Factor * C_Factor * P_Factor, 2)
            )
        
        return(df)
    })
    
    # KPI Summary Metrics
    output$summary_boxes <- renderUI({
        data <- mined_data()
        req(data)
        
        avg_precip <- round(mean(data$Annual_Precip_mm, na.rm = TRUE), 1)
        avg_loss <- round(mean(data$Soil_Loss_t_ha, na.rm = TRUE), 2)
        peak_year <- data$Year[which.max(data$Soil_Loss_t_ha)]
        
        HTML(paste0(
            "<div class='d-flex justify-content-around text-center'>",
            "<div><span class='text-muted'>Avg Annual Precip</span><h4 class='text-primary'>", avg_precip, " mm</h4></div>",
            "<div><span class='text-muted'>Avg Estimated Soil Loss</span><h4 class='text-danger'>", avg_loss, " t/ha/yr</h4></div>",
            "<div><span class='text-muted'>Highest Erosion Year</span><h4 class='text-warning'>", peak_year, "</h4></div>",
            "</div>"
        ))
    })
    
    # Interactive Combination Chart (Precipitation + Soil Loss)
    output$trend_plot <- renderPlotly({
        data <- mined_data()
        req(data)
        
        # Multi-axis scaling multiplier
        scale_factor <- max(data$Annual_Precip_mm, na.rm = TRUE) / max(data$Soil_Loss_t_ha, na.rm = TRUE)
        
        p <- ggplot(data, aes(x = Year)) +
            geom_col(aes(y = Annual_Precip_mm, text = paste("Precipitation:", Annual_Precip_mm, "mm")), 
                     fill = "#2c3e50", alpha = 0.75) +
            geom_line(aes(y = Soil_Loss_t_ha * scale_factor, group = 1), color = "#e74c3c", linewidth = 1.2) +
            geom_point(aes(y = Soil_Loss_t_ha * scale_factor, text = paste("Soil Loss:", Soil_Loss_t_ha, "t/ha/yr")), 
                       color = "#c0392b", size = 3) +
            scale_y_continuous(
                name = "Annual Precipitation (mm)",
                sec.axis = sec_axis(~ . / scale_factor, name = "Est. Soil Loss (t/ha/yr)")
            ) +
            labs(title = "Historical Precipitation vs. Modeled Soil Loss in Mistelbach", x = "Year") +
            theme_minimal()
        
        ggplotly(p, tooltip = "text")
    })
    
    # Leaflet Spatial View
    output$map_output <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            setView(lng = 16.57, lat = 48.57, zoom = 12) %>%
            addMarkers(
                lng = 16.57, lat = 48.57,
                popup = "<b>Mistelbach, Lower Austria</b><br>Coordinates: 48.57°N, 16.57°E"
            ) %>%
            addCircles(
                lng = 16.57, lat = 48.57,
                radius = 4000, color = "#e74c3c", fillColor = "#e74c3c", fillOpacity = 0.15,
                popup = "Target Mining Region"
            )
    })
    
    # Interactive Data Table
    output$data_table <- renderDT({
        data <- mined_data()
        req(data)
        
        datatable(
            data,
            options = list(pageLength = 10, scrollX = TRUE),
            rownames = FALSE,
            colnames = c("Year", "Precip (mm)", "Max Day (mm)", "Rain Days", "R Factor", "K Factor", "LS Factor", "C Factor", "P Factor", "Soil Loss (t/ha/yr)")
        )
    })
    
    # Download Handler for CSV Export
    output$download_csv <- downloadHandler(
        filename = function() {
            paste0("mistelbach_precipitation_soilloss_", input$start_year, "_", input$end_year, ".csv")
        },
        content = function(file) {
            write.csv(mined_data(), file, row.names = FALSE)
        }
    )
}

# Run Application
shinyApp(ui = ui, server = server)