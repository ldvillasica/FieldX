library(shiny)
library(ggplot2)
library(shinythemes)
library(dplyr)
library(tidyr)

# --- 1. THE PHYSICS ENGINE ---
calc_soil_physics <- function(sand, silt, clay, vwc, bd, wdc, wds, was, rain, slope, strategy) {
    # Safe math: Add small constant to prevent division by zero (NaN/Inf)
    denom_texture <- clay + silt + 0.01
    denom_clay <- clay + 0.01
    
    # A. Stability Indices
    dr_val <- ((wdc + wds) / denom_texture) * 100
    dr_dec <- dr_val / 100
    fi_val <- (clay - wdc) / denom_clay
    
    # B. Soil Degradation Index (SDI)
    resilience <- (was / 100) * (fi_val + 0.1)
    sdi_risk <- pmin(100, ((dr_dec + 0.01) / (resilience + 0.05)) * (bd / 1.5) * 20)
    
    # C. Hydrology
    porosity <- 1 - (bd / 2.65)
    sat_ratio <- pmin(1, vwc / (porosity + 0.01)) 
    infilt_cap <- (100 - clay) * (1.5 / bd) * 0.5 
    clogging_idx <- (wdc * dr_dec) * (bd / 1.5)
    eff_infilt <- pmax(2, (infilt_cap / (1 + (clogging_idx / 10))) * (1 - sat_ratio))
    
    # D. Erosion Modeling
    runoff <- pmax(0, rain - eff_infilt)
    ki <- (dr_dec / (fi_val + 0.1)) * (wdc / 100) 
    kr <- 1 / (was + 1)                          
    
    mitigation <- switch(strategy, "None"=1.0, "Gypsum"=0.65, "Cover Crops"=0.55, "Biochar"=0.75)
    
    soil_loss <- ((ki * (rain^2) * 0.0001) + (kr * runoff * (slope/100))) * 10 * mitigation
    sed_yield <- soil_loss * pmin(1, (slope / 20)^1.5)
    
    return(data.frame(
        DR = dr_val, FI = fi_val, SDI = sdi_risk, 
        Loss = soil_loss, Yield = sed_yield, 
        Infilt = eff_infilt, Clog = clogging_idx, Runoff = runoff
    ))
}

# --- 2. THE USER INTERFACE ---
ui <- fluidPage(
    theme = shinytheme("flatly"),
    tags$head(tags$style(HTML("
    .metric-box { background: #f8f9fa; border-top: 5px solid #2c3e50; padding: 15px; border-radius: 8px; text-align: center; margin-bottom: 15px; }
    .label-bold { font-weight: bold; color: #7f8c8d; text-transform: uppercase; font-size: 0.85em; margin-bottom: 5px; }
    .status-text { font-weight: bold; font-size: 0.9em; }
  "))),
    
    titlePanel("Soil Physics Lab: Scenario Diagnostic Tool"),
    hr(),
    
    sidebarLayout(
        sidebarPanel(
            width = 4,
            h4("Step 1: Input Lab Data"),
            fluidRow(
                column(4, numericInput("sand", "Sand %", 30)),
                column(4, numericInput("silt", "Silt %", 30)),
                column(4, numericInput("clay", "Clay %", 40))
            ),
            fluidRow(
                column(6, numericInput("wdc", "WDC %", 15)),
                column(6, numericInput("wds", "WDS %", 5))
            ),
            sliderInput("was", "Wet Aggregate Stability (%)", 0, 100, 60),
            numericInput("bd", "Bulk Density (g/cm³)", 1.3, step=0.1),
            sliderInput("vwc", "Initial Water Content (VWC)", 0, 0.5, 0.15),
            
            hr(),
            h4("Step 2: Field Conditions"),
            sliderInput("rain", "Rain Intensity (mm/hr)", 0, 150, 60),
            sliderInput("slope", "Slope (%)", 0, 50, 10),
            selectInput("strategy", "Mitigation Strategy", 
                        choices = c("None", "Gypsum", "Cover Crops", "Biochar"))
        ),
        
        mainPanel(
            width = 8,
            fluidRow(
                column(4, div(class = "metric-box",
                              div(class="label-bold", "Soil Degradation Index"),
                              h2(textOutput("sdi_val")),
                              span(class="status-text", textOutput("sdi_status")))),
                column(4, div(class = "metric-box",
                              div(class="label-bold", "Predicted Soil Loss"),
                              h2(textOutput("yield_val")),
                              p("tons / hectare"))),
                column(4, div(class = "metric-box",
                              div(class="label-bold", "Effective Infiltration"),
                              h2(textOutput("infilt_val")),
                              p("mm / hour")))
            ),
            br(),
            
            fluidRow(
                column(7, 
                       wellPanel(
                           h4("Topographic Sensitivity"),
                           plotOutput("slope_curve", height = "300px")
                       )
                ),
                column(5, 
                       wellPanel(
                           h4("Sensitivity Analysis"),
                           p(tags$small("Projected impact on Soil Loss if property improves by 10%")),
                           tableOutput("sens_table")
                       )
                )
            ),
            
            wellPanel(
                h4("Physics Summary Detail"),
                fluidRow(
                    column(3, helpText("Dispersion Ratio (DR):"), strong(textOutput("dr_out"), "%")),
                    column(3, helpText("Flocculation Index (FI):"), strong(textOutput("fi_out"))),
                    column(3, helpText("Runoff Depth (mm/hr):"), strong(textOutput("runoff_out"))),
                    column(3, helpText("Pore Clogging Index:"), strong(textOutput("clog_out")))
                )
            )
        )
    )
)

# --- 3. THE SERVER ---
server <- function(input, output) {
    
    # Reactive Engine with strict validation
    res <- reactive({
        req(input$sand, input$silt, input$clay, input$wdc, input$wds, input$was, input$bd)
        
        calc_soil_physics(
            as.numeric(input$sand), as.numeric(input$silt), as.numeric(input$clay), 
            as.numeric(input$vwc), as.numeric(input$bd), as.numeric(input$wdc), 
            as.numeric(input$wds), as.numeric(input$was), as.numeric(input$rain), 
            as.numeric(input$slope), input$strategy
        )
    })
    
    # KPI Outputs
    output$sdi_val <- renderText({ 
        val <- res()$SDI
        if(is.null(val) || is.na(val)) return("0.0%")
        paste0(round(val, 1), "%") 
    })
    
    output$yield_val <- renderText({ round(res()$Yield, 2) })
    output$infilt_val <- renderText({ round(res()$Infilt, 1) })
    
    output$sdi_status <- renderText({
        s <- res()$SDI
        if(s < 35) "Condition: STABLE" 
        else if(s < 70) "Condition: ERODIBLE" 
        else "Condition: CRITICAL"
    })
    
    # Summary Data Outputs
    output$dr_out <- renderText({ round(res()$DR, 1) })
    output$fi_out <- renderText({ round(res()$FI, 2) })
    output$runoff_out <- renderText({ round(res()$Runoff, 2) })
    output$clog_out <- renderText({ 
        val <- res()$Clog
        if(is.na(val)) return("0.00")
        round(val, 2) 
    })
    
    # Slope Curve Plot
    output$slope_curve <- renderPlot({
        req(res())
        s_range <- seq(0, 50, 1)
        df_curve <- do.call(rbind, lapply(s_range, function(s){
            r <- calc_soil_physics(input$sand, input$silt, input$clay, input$vwc, 
                                   input$bd, input$wdc, input$wds, input$was, 
                                   input$rain, s, input$strategy)
            data.frame(Slope=s, Yield=r$Yield)
        }))
        
        ggplot(df_curve, aes(x=Slope, y=Yield)) + 
            geom_area(fill="#3498db", alpha=0.15) +
            geom_line(color="#2980b9", size=1.1) +
            geom_vline(xintercept = input$slope, linetype="dashed", color="#e74c3c") +
            labs(subtitle="Red line indicates current slope setting",
                 y="Sediment Yield (t/ha)", x="Slope Gradient (%)") +
            theme_minimal()
    })
    
    # Sensitivity Table
    output$sens_table <- renderTable({
        req(res())
        base_loss <- res()$Yield
        
        calc_reduction <- function(new_was=input$was, new_wdc=input$wdc, new_bd=input$bd) {
            r <- calc_soil_physics(input$sand, input$silt, input$clay, input$vwc, 
                                   new_bd, new_wdc, input$wds, new_was, 
                                   input$rain, input$slope, input$strategy)
            return(((base_loss - r$Yield) / (base_loss + 0.0001)) * 100)
        }
        
        data.frame(
            Property = c("Stability (WAS)", "Dispersibility (WDC)", "Compaction (BD)"),
            Action = c("+10% Improvement", "-10% Reduction", "-10% Reduction"),
            Soil_Loss_Red = c(
                paste0(round(calc_reduction(new_was = input$was * 1.1), 2), "%"),
                paste0(round(calc_reduction(new_wdc = input$wdc * 0.9), 2), "%"),
                paste0(round(calc_reduction(new_bd = input$bd * 0.9), 2), "%")
            )
        )
    }, striped = TRUE, hover = TRUE, align = 'c')
}

shinyApp(ui, server)