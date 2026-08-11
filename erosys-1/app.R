# --- LOAD LIBRARIES ---
library(shiny)
library(ggplot2)
library(shinythemes)
library(dplyr)
library(gstat)
library(sp)
library(raster)
library(viridis)

# --- 1. THE DYNAMIC PHYSICS ENGINE ---
# This function processes raw lab data into erosion and health metrics
calc_all_metrics <- function(wdc, wds, was, bd, clay, sand, silt, rain, slope, strategy, vwc) {
    
    # Internal Calculation of DR and FI
    # DR = (WDC + WDS) / (Clay + Silt)
    dr_val <- ((wdc + wds) / (clay + silt + 0.01)) * 100
    dr_dec <- dr_val / 100
    
    # FI = (Total Clay - WDC) / Total Clay
    fi_val <- (clay - wdc) / (clay + 0.01)
    
    # A. Internal Health (SDI)
    structural_res <- (was / 100) * (fi_val + 0.1)
    stability_gap <- (dr_dec + 0.01) / (structural_res + 0.05)
    sdi_risk <- pmin(100, (stability_gap * (bd / 1.5) * 20))
    
    # B. Hydrology: Infiltration and Surface Sealing
    porosity <- 1 - (bd / 2.65)
    sat_ratio <- pmin(1, vwc / porosity) 
    base_infilt <- (100 - clay) * (1.5 / bd) * 0.5 
    clogging_index <- (wdc * dr_dec) * (bd / 1.5)
    eff_infilt <- pmax(2, (base_infilt / (1 + (clogging_index / 10))) * (1 - sat_ratio))
    
    # C. Runoff Generation
    runoff_depth <- pmax(0, rain - eff_infilt)
    
    # D. WEPP Erodibility Factors
    custom_Ki <- (dr_dec / (fi_val + 0.1)) * (wdc / 100)
    custom_Kr <- 1 / (was + 1) 
    tau_c <- (bd * (clay/100) * (1 + silt/100)) * 5 
    
    mit <- switch(strategy, "None"=1.0, "Gypsum"=0.65, "Cover Crops"=0.55, "Biochar"=0.75)
    
    # E. Erosion Physics
    interrill <- (custom_Ki * (rain^2) * 0.0001) * mit
    shear_stress <- (10 * (runoff_depth/100) * (slope/100)) 
    rill <- pmax(0, custom_Kr * (shear_stress - tau_c)) * mit
    
    loss_tons <- (interrill + rill) * 10
    yield_tons <- loss_tons * pmin(1, (slope / 20)^1.5)
    
    return(data.frame(DR = dr_val, FI = fi_val, SDI = sdi_risk, Clog = clogging_index, 
                      Infilt = eff_infilt, Runoff = runoff_depth, Loss = loss_tons, Yield = yield_tons))
}

# --- 2. THE USER INTERFACE ---
ui <- fluidPage(
    theme = shinytheme("flatly"),
    titlePanel("Geo-Spatial Soil Physics Lab: WEPP + Kriging Mapper"),
    
    sidebarLayout(
        sidebarPanel(
            tabsetPanel(id = "input_mode",
                        tabPanel("Bulk & Spatial",
                                 br(),
                                 fileInput("bulk_csv", "Upload CSV Data", accept = ".csv"),
                                 helpText("Ensure CSV has: LAT, LON, SAND, SILT, CLAY, WDC, WDS, WAS, BD, VWC"),
                                 uiOutput("group_selector_ui")
                        ),
                        tabPanel("Manual Check",
                                 br(),
                                 fluidRow(
                                     column(6, numericInput("lat", "Lat", 14.51)),
                                     column(6, numericInput("lon", "Lon", 121.01))
                                 ),
                                 fluidRow(
                                     column(4, numericInput("clay_m", "Clay%", 40)),
                                     column(4, numericInput("silt_m", "Silt%", 30)),
                                     column(4, numericInput("sand_m", "Sand%", 30))
                                 ),
                                 sliderInput("wdc_m", "Water Disp. Clay%", 0, 50, 15),
                                 sliderInput("wds_m", "Water Disp. Silt%", 0, 50, 5)
                        )
            ),
            hr(),
            h4("Global Parameters"),
            sliderInput("rain", "Rain Intensity (mm/hr)", 0, 150, 60),
            sliderInput("slope_global", "Default Slope (%)", 0, 50, 10),
            selectInput("strategy", "Mitigation Strategy", choices = c("None", "Gypsum", "Cover Crops", "Biochar")),
            hr(),
            downloadButton("download_report", "Export Analysis", class = "btn-success")
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("Spatial Risk Maps",
                         br(),
                         fluidRow(
                             column(6, wellPanel(h4("Kriged SDI (Health)"), plotOutput("map_sdi"))),
                             column(6, wellPanel(h4("Kriged Soil Loss (t/ha)"), plotOutput("map_loss")))
                         ),
                         helpText("Spatial maps use Ordinary Kriging (Spherical Model). Maps represent predicted values between sampling points.")
                ),
                tabPanel("Site Scenario Analysis",
                         br(),
                         fluidRow(
                             column(6, wellPanel(h4("Derived DR %"), h2(textOutput("dr_out")))),
                             column(6, wellPanel(h4("Derived FI"), h2(textOutput("fi_out"))))
                         ),
                         wellPanel(
                             h4("Soil Degradation Index (SDI) Status"),
                             h3(textOutput("sdi_status")),
                             plotOutput("sdi_gauge", height = "35px")
                         ),
                         plotOutput("erosion_curve", height = "300px"),
                         hr(),
                         h4("Management Sensitivity"),
                         tableOutput("sens_table")
                ),
                tabPanel("Bulk Comparison",
                         br(),
                         plotOutput("bulk_plot"),
                         tableOutput("bulk_table")
                )
            )
        )
    )
)

# --- 3. THE SERVER ---
server <- function(input, output) {
    
    # Process the CSV data
    processed_data <- reactive({
        req(input$bulk_csv)
        df <- read.csv(input$bulk_csv$datapath)
        colnames(df) <- toupper(gsub("[[:punct:]]| ", "", colnames(df)))
        
        # Apply physics engine to each row
        results <- do.call(rbind, lapply(1:nrow(df), function(i) {
            # Use CSV slope if it exists, otherwise use slider
            curr_slope <- if("SLOPE" %in% names(df)) df$SLOPE[i] else input$slope_global
            
            calc_all_metrics(df$WDC[i], df$WDS[i], df$WAS[i], df$BD[i], df$CLAY[i], 
                             df$SAND[i], df$SILT[i], input$rain, curr_slope, input$strategy, df$VWC[i])
        }))
        return(cbind(df, results))
    })
    
    # Ordinary Kriging Logic
    do_kriging <- function(df, target_var) {
        # Convert to SpatialPointsDataFrame
        temp_df <- df
        coordinates(temp_df) <- ~LON+LAT
        
        # Create grid
        grid <- expand.grid(LON = seq(min(df$LON), max(df$LON), length.out = 100),
                            LAT = seq(min(df$LAT), max(df$LAT), length.out = 100))
        coordinates(grid) <- ~LON+LAT
        gridded(grid) <- TRUE
        
        # Simple Variogram Model
        vgm_mod <- vgm(psill = var(df[[target_var]]), model = "Sph", range = 0.1, nugget = 0.05)
        k_res <- krige(as.formula(paste(target_var, "~ 1")), temp_df, grid, model = vgm_mod)
        return(as.data.frame(k_res))
    }
    
    # Output Maps
    output$map_sdi <- renderPlot({
        dat <- processed_data()
        k_df <- do_kriging(dat, "SDI")
        ggplot() +
            geom_tile(data = k_df, aes(x = LON, y = LAT, fill = var1.pred)) +
            geom_point(data = dat, aes(x = LON, y = LAT), color = "white", size = 3) +
            scale_fill_gradientn(colors = c("#3498db", "#f1c40f", "#e74c3c"), name = "SDI %") +
            theme_minimal() + coord_fixed()
    })
    
    output$map_loss <- renderPlot({
        dat <- processed_data()
        k_df <- do_kriging(dat, "Yield")
        ggplot() +
            geom_tile(data = k_df, aes(x = LON, y = LAT, fill = var1.pred)) +
            geom_point(data = dat, aes(x = LON, y = LAT), color = "black", alpha = 0.4) +
            scale_fill_viridis_c(option = "magma", name = "t/ha") +
            theme_minimal() + coord_fixed()
    })
    
    # Rest of UI outputs for Scenario Tab
    res_m <- reactive({
        calc_all_metrics(input$wdc_m, input$wds_m, 60, 1.3, input$clay_m, 
                         input$sand_m, input$silt_m, input$rain, input$slope_global, input$strategy, 0.15)
    })
    
    output$dr_out <- renderText({ round(res_m()$DR, 1) })
    output$fi_out <- renderText({ round(res_m()$FI, 2) })
    
    output$sdi_status <- renderText({
        s <- res_m()$SDI
        status <- if(s < 35) "STABLE" else if(s < 70) "ERODIBLE" else "CRITICAL"
        paste0(status, " (", round(s, 1), "%)")
    })
    
    output$sdi_gauge <- renderPlot({
        ggplot() + geom_rect(aes(xmin=0, xmax=100, ymin=0, ymax=1), fill="#ecf0f1") +
            geom_rect(aes(xmin=0, xmax=res_m()$SDI, ymin=0, ymax=1), 
                      fill=ifelse(res_m()$SDI > 70, "#e74c3c", "#3498db")) + theme_void()
    })
    
    output$erosion_curve <- renderPlot({
        s_range <- seq(0, 50, by = 1)
        df <- do.call(rbind, lapply(s_range, function(s) {
            r <- calc_all_metrics(input$wdc_m, input$wds_m, 60, 1.3, input$clay_m, 
                                  input$sand_m, input$silt_m, input$rain, s, input$strategy, 0.15)
            data.frame(Slope = s, Yield = r$Yield)
        }))
        ggplot(df, aes(x=Slope, y=Yield)) +
            geom_area(fill="#27ae60", alpha=0.2) + geom_line(color="#27ae60", size=1.5) +
            geom_hline(yintercept = 11, linetype="dotted", color="red") +
            theme_minimal() + labs(title="Topographic Impact (Slider Scenario)")
    })
    
    # Bulk Table & Plot
    output$group_selector_ui <- renderUI({
        req(processed_data())
        selectInput("group_var", "Group For Comparison:", choices = colnames(processed_data()))
    })
    
    output$bulk_table <- renderTable({
        req(input$group_var)
        processed_data() %>% group_by(across(all_of(input$group_var))) %>%
            summarise(Mean_SDI = mean(SDI), Mean_Yield = mean(Yield), Max_Loss = max(Loss))
    })
}

shinyApp(ui, server)