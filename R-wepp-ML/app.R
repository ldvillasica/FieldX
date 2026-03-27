# --- Required Libraries ---
library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(googlesheets4)
library(randomForest)

# --- 1. CORE CALCULATIONS (Enhanced Physics) ---
calc_all_metrics <- function(wdc, di, fi, was, bd, clay, sand, silt, rain, slope, strategy, vwc) {
    # Structural Stability
    structural_res <- (was / 100) * (fi + 0.1)
    stability_gap <- (di + 0.01) / (structural_res + 0.05)
    sdi_risk <- pmin(100, (stability_gap * (bd / 1.5) * 20))
    
    # Hydrology
    porosity <- 1 - (bd / 2.65)
    sat_ratio <- pmin(1, vwc / porosity) 
    base_infilt <- (100 - clay) * (1.5 / bd) * 0.5 
    clogging_index <- (wdc * di) * (bd / 1.5)
    eff_infilt <- pmax(2, (base_infilt / (1 + (clogging_index / 10))) * (1 - sat_ratio))
    
    # Erosion Physics
    runoff_depth <- pmax(0, rain - eff_infilt)
    custom_Ki <- (di / (fi + 0.1)) * (wdc / 100)
    custom_Kr <- 1 / (was + 1) 
    tau_c <- (bd * (clay/100) * (1 + silt/100)) * 5 
    mit <- switch(strategy, "None"=1.0, "Gypsum"=0.65, "Cover Crops"=0.55, "Biochar"=0.75)
    
    interrill <- (custom_Ki * (rain^2) * 0.0001) * mit
    shear_stress <- (10 * (runoff_depth/100) * (slope/100)) 
    rill <- pmax(0, custom_Kr * (shear_stress - tau_c)) * mit
    
    # Output Metrics
    soil_loss_kg <- (interrill + rill) # Unit area displacement
    yield_tons <- soil_loss_kg * 10 * pmin(1, (slope / 20)^1.5) # Scaled sediment export
    
    return(data.frame(SDI = sdi_risk, Clog = clogging_index, Infilt = eff_infilt, 
                      Runoff = runoff_depth, Loss = soil_loss_kg, Yield = yield_tons))
}

# --- 2. USER INTERFACE ---
ui <- fluidPage(
    theme = shinytheme("flatly"),
    tags$head(tags$style(HTML("
    .well { background-color: #ffffff !important; border: 1px solid #dce4ec !important; }
    .unit-label { font-size: 0.8em; color: #95a5a6; margin-left: 4px; font-weight: 400; }
    .res-val { font-weight: 700; color: #2c3e50; }
    .header-box { border-top: 5px solid #34495e; padding: 15px; margin-bottom: 20px; }
  "))),
    
    titlePanel("R-WEPP AI: Field Intelligence Suite v16.2"),
    
    sidebarLayout(
        sidebarPanel(
            tabsetPanel(id = "input_mode",
                        tabPanel("Manual Lab",
                                 br(),
                                 fluidRow(
                                     column(6, numericInput("bd", "Bulk Density", 1.3, step = 0.1)),
                                     column(6, sliderInput("vwc", "Vol. Water", 0, 0.5, 0.15))
                                 ),
                                 hr(),
                                 h5("Soil Texture (%)"),
                                 fluidRow(
                                     column(4, numericInput("clay", "Clay", 40)),
                                     column(4, numericInput("silt", "Silt", 30)),
                                     column(4, numericInput("sand", "Sand", 30))
                                 ),
                                 hr(),
                                 sliderInput("was", "WAS (%)", 0, 100, 60),
                                 sliderInput("wdc", "WDC (%)", 0, 50, 15),
                                 sliderInput("di", "Disp. Index", 0, 1, 0.3),
                                 sliderInput("fi", "Flocc. Index", 0, 1, 0.7)
                        ),
                        tabPanel("Cloud Sync",
                                 br(),
                                 textInput("sheet_url", "Google Sheet URL:", ""),
                                 actionButton("load_sheet", "Sync & Train Cloud AI", class = "btn-primary", width="100%")
                        )
            ),
            hr(),
            sliderInput("rain", "Rain Intensity (mm/hr)", 0, 150, 60),
            sliderInput("slope", "Slope Gradient (%)", 0, 50, 10),
            selectInput("strategy", "Mitigation Strategy", choices = c("None", "Gypsum", "Cover Crops", "Biochar"))
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("Scenario Analysis",
                         br(),
                         wellPanel(class="header-box",
                                   h4("Structural Stability (SDI)"),
                                   h3(textOutput("sdi_status")),
                                   plotOutput("sdi_gauge", height = "40px")
                         ),
                         # PRIMARY RESULTS ROW
                         fluidRow(
                             column(6, wellPanel(h4("Soil Loss (A)"), h2(class="res-val", textOutput("loss_val"), span("kg/m²", class="unit-label")))),
                             column(6, wellPanel(h4("Sediment Yield"), h2(class="res-val", textOutput("yield_phys"), span("tons/ha", class="unit-label"))))
                         ),
                         # AI & HYDROLOGY ROW
                         fluidRow(
                             column(4, wellPanel(h5("AI Prediction"), h3(style="color:#8e44ad;", textOutput("yield_ai"), span("t/ha", class="unit-label")))),
                             column(4, wellPanel(h5("Infiltration"), h3(class="res-val", textOutput("infilt_man"), span("mm/hr", class="unit-label")))),
                             column(4, wellPanel(h5("Runoff"), h3(style="color:#2980b9;", textOutput("runoff_man"), span("mm/hr", class="unit-label"))))
                         ),
                         plotOutput("erosion_curve", height = "350px")
                ),
                tabPanel("AI Insights",
                         br(),
                         uiOutput("ai_status_ui"),
                         hr(),
                         fluidRow(
                             column(6, plotOutput("rf_importance")),
                             column(6, plotOutput("data_diversity"))
                         )
                )
            )
        )
    )
)

# --- 3. SERVER ---
server <- function(input, output, session) {
    gs4_deauth()
    store <- reactiveValues(data = NULL, rf = NULL, status = "Awaiting Cloud Sync...")
    
    observeEvent(input$load_sheet, {
        req(input$sheet_url)
        tryCatch({
            df <- read_sheet(input$sheet_url)
            colnames(df) <- toupper(gsub("[[:punct:]]| ", "", colnames(df)))
            df_train <- df %>% rowwise() %>%
                mutate(Yield = calc_all_metrics(WDC, DI, FI, WAS, BD, CLAY, SAND, SILT, input$rain, input$slope, "None", VWC)$Yield)
            
            if(nrow(df_train) >= 10) {
                store$rf <- randomForest(Yield ~ WAS+DI+FI+WDC+BD+VWC+CLAY, data=df_train, ntree=500, importance=TRUE)
                store$data <- df_train
                store$status <- "AI ENGINE ACTIVE"
            } else { store$status <- "INSUFFICIENT DATA (Need 10+ Rows)" }
        }, error = function(e) { store$status <- "SYNC ERROR" })
    })
    
    res_man <- reactive({
        calc_all_metrics(input$wdc, input$di, input$fi, input$was, input$bd, 
                         input$clay, input$sand, input$silt, input$rain, input$slope, input$strategy, input$vwc)
    })
    
    # Text Outputs with units
    output$sdi_status <- renderText({ 
        s <- res_man()$SDI
        status <- if(s < 35) "HEALTHY" else if(s < 70) "STRESS" else "COLLAPSE"
        paste0(status, " (", round(s, 1), "%)")
    })
    
    output$loss_val <- renderText({ round(res_man()$Loss, 3) })
    output$yield_phys <- renderText({ round(res_man()$Yield, 2) })
    output$yield_ai <- renderText({ 
        if(is.null(store$rf)) return("---")
        new_data <- data.frame(WAS=input$was, DI=input$di, FI=input$fi, WDC=input$wdc, BD=input$bd, VWC=input$vwc, CLAY=input$clay)
        round(predict(store$rf, new_data), 2)
    })
    
    output$infilt_man <- renderText({ round(res_man()$Infilt, 1) })
    output$runoff_man <- renderText({ round(res_man()$Runoff, 1) })
    
    # Visuals
    output$erosion_curve <- renderPlot({
        s_range <- seq(0, 50, by = 1)
        df_c <- data.frame(Slope = s_range, 
                           Yield = sapply(s_range, function(s) calc_all_metrics(input$wdc, input$di, input$fi, input$was, input$bd, input$clay, input$sand, input$silt, input$rain, s, input$strategy, input$vwc)$Yield))
        
        ggplot(df_c, aes(Slope, Yield)) + 
            geom_area(fill="#27ae60", alpha=0.1) + 
            geom_line(color="#27ae60", size=1.2) + 
            geom_hline(yintercept = 11, linetype = "dashed", color = "#e74c3c", size = 1) +
            annotate("text", x = 10, y = 12.5, label = "Threshold (11 t/ha)", color = "#e74c3c", fontface = "bold") +
            labs(title="Slope Gradient vs. Sediment Yield", y="Yield (tons/ha)", x="Slope Gradient (%)") +
            theme_minimal()
    })
    
    output$sdi_gauge <- renderPlot({
        ggplot() + geom_rect(aes(xmin=0, xmax=100, ymin=0, ymax=1), fill="#ecf0f1") +
            geom_rect(aes(xmin=0, xmax=res_man()$SDI, ymin=0, ymax=1), 
                      fill=ifelse(res_man()$SDI > 70, "#e74c3c", "#3498db")) + theme_void()
    })
    
    output$ai_status_ui <- renderUI({
        color <- if(grepl("ACTIVE", store$status)) "#27ae60" else "#7f8c8d"
        div(style=paste0("background:", color, "; color:white; padding:15px; border-radius:5px; font-weight:600;"), store$status)
    })
    
    output$rf_importance <- renderPlot({ req(store$rf); varImpPlot(store$rf, main="Key AI Drivers") })
    output$data_diversity <- renderPlot({
        req(store$data)
        ggplot(store$data, aes(x=WAS, y=DI, color=Yield)) + geom_point(size=4) + theme_minimal() + labs(title="Cloud Data Spread")
    })
}

shinyApp(ui, server)