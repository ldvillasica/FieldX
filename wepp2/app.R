library(shiny)
library(ggplot2)
library(shinythemes)
library(dplyr)

# --- 1. THE REFINED PHYSICS ENGINE ---
calc_custom_wepp <- function(wdc, di, fi, was, bd, clay, sand, silt, rain_int, slope, strategy) {
    
    # A. DERIVE WEPP PARAMETERS FROM YOUR LAB DATA
    # Interrill Erodibility (Ki) scaled by Chemical Instability
    custom_Ki <- (di / (fi + 0.1)) * (wdc / 100)
    
    # Rill Erodibility (Kr) scaled by Physical Stability (Inverse of WAS)
    custom_Kr <- 1 / (was + 1) 
    
    # Critical Shear (Tau_c) based on Cohesion (Clay/BD ratio)
    # We use Silt/Sand to slightly adjust the resistance
    tau_critical <- (bd * (clay/100) * (1 + silt/100)) * 5 
    
    # B. GROSS SOIL LOSS (Detachment phase)
    interrill_det <- custom_Ki * (rain_int^2) * 0.0001
    shear_stress <- 10 * (rain_int/100) * (slope/100)
    rill_det <- pmax(0, custom_Kr * (shear_stress - tau_critical))
    
    # Mitigation impact
    mitigation <- switch(strategy, "None" = 1.0, "Gypsum (Chemical)" = 0.65, 
                         "Cover Crops (Bio)" = 0.55, "Biochar (Physical)" = 0.75)
    
    gross_loss_kg <- (interrill_det + rill_det) * mitigation
    
    # C. NET SEDIMENT YIELD (Transport phase)
    # If slope is 0, nothing is transported. If slope is steep, yield = loss.
    transport_cap <- pmin(1, (slope / 20)^1.5) 
    yield_kg <- gross_loss_kg * transport_cap
    
    return(data.frame(
        Ki = custom_Ki, Kr = custom_Kr, TauC = tau_critical, 
        Loss_tons = gross_loss_kg * 10,  # 1 kg/m2 = 10 t/ha
        Yield_tons = yield_kg * 10
    ))
}

# --- 2. THE USER INTERFACE ---
ui <- fluidPage(
    theme = shinytheme("flatly"),
    titlePanel("Advanced Soil Degradation & R-WEPP Designer"),
    
    sidebarLayout(
        sidebarPanel(
            tabsetPanel(id = "input_mode",
                        tabPanel("Manual Soil Lab",
                                 br(),
                                 h4("Textural Composition"),
                                 numericInput("sand", "Sand (%)", 30),
                                 numericInput("silt", "Silt (%)", 30),
                                 numericInput("clay", "Clay (%)", 40),
                                 hr(),
                                 h4("Stability Indices"),
                                 sliderInput("was", "Wet Aggregate Stability (%)", 0, 100, 60),
                                 sliderInput("wdc", "Water Dispersible Clay (%)", 0, 50, 15),
                                 sliderInput("di", "Dispersion Index (DI)", 0, 1, 0.3),
                                 sliderInput("fi", "Flocculation Index (FI)", 0, 1, 0.7),
                                 numericInput("bd", "Bulk Density (g/cm³)", 1.3, step = 0.1)
                        ),
                        tabPanel("Bulk Comparison",
                                 br(),
                                 fileInput("bulk_csv", "Upload CSV (SampleID, WAS, DI, FI, WDC, BD, CLAY, SAND, SILT)", accept = ".csv"),
                                 uiOutput("group_selector_ui")
                        )
            ),
            hr(),
            h4("Environmental Stressors"),
            sliderInput("rain", "Rain Intensity (mm/hr)", 0, 150, 60),
            sliderInput("slope", "Slope Gradient (%)", 0, 50, 10),
            selectInput("strategy", "Mitigation Strategy", 
                        choices = c("None", "Gypsum (Chemical)", "Cover Crops (Bio)", "Biochar (Physical)")),
            hr(),
            downloadButton("download_report", "Export All Results", class = "btn-success")
        ),
        
        mainPanel(
            tabsetPanel(id = "main_tabs",
                        tabPanel("Scenario Analysis",
                                 br(),
                                 fluidRow(
                                     column(4, wellPanel(h5("Derived Ki"), h3(textOutput("ki_val"), style="color:#2980b9"))),
                                     column(4, wellPanel(h5("Derived Kr"), h3(textOutput("kr_val"), style="color:#27ae60"))),
                                     column(4, wellPanel(h5("Crit. Shear (\u03c4c)"), h3(textOutput("tau_val"), style="color:#e67e22")))
                                 ),
                                 fluidRow(
                                     column(6, wellPanel(style="background:#fdf2e9; border-left: 5px solid #e67e22;", 
                                                         h4("Gross Soil Loss"), h2(textOutput("loss_man")), "tons/ha")),
                                     column(6, wellPanel(style="background:#eafaf1; border-left: 5px solid #27ae60;", 
                                                         h4("Net Sediment Yield"), h2(textOutput("yield_man")), "tons/ha"))
                                 ),
                                 plotOutput("loss_yield_plot")
                        ),
                        tabPanel("Bulk Results",
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
    
    man_res <- reactive({
        calc_custom_wepp(input$wdc, input$di, input$fi, input$was, input$bd, 
                         input$clay, input$sand, input$silt, input$rain, input$slope, input$strategy)
    })
    
    output$ki_val <- renderText({ round(man_res()$Ki, 4) })
    output$kr_val <- renderText({ round(man_res()$Kr, 4) })
    output$tau_val <- renderText({ round(man_res()$TauC, 2) })
    output$loss_man <- renderText({ round(man_res()$Loss_tons, 2) })
    output$yield_man <- renderText({ round(man_res()$Yield_tons, 2) })
    
    output$loss_yield_plot <- renderPlot({
        s_range <- seq(0, 50, by = 2)
        df_plot <- do.call(rbind, lapply(s_range, function(s) {
            r <- calc_custom_wepp(input$wdc, input$di, input$fi, input$was, input$bd, 
                                  input$clay, input$sand, input$silt, input$rain, s, input$strategy)
            data.frame(Slope = s, Gross_Loss = r$Loss_tons, Net_Yield = r$Yield_tons)
        }))
        ggplot(df_plot, aes(x=Slope)) +
            geom_area(aes(y=Gross_Loss, fill="Gross Soil Loss"), alpha=0.3) +
            geom_line(aes(y=Net_Yield, color="Net Sediment Yield"), size=1.5) +
            scale_fill_manual(values=c("Gross Soil Loss"="#e67e22")) +
            scale_color_manual(values=c("Net Sediment Yield"="#27ae60")) +
            labs(title="Erosion Dynamics: Detachment vs. Transport", y="tons/ha", fill="", color="") +
            theme_minimal()
    })
    
    # Bulk Logic
    raw_bulk <- reactive({
        req(input$bulk_csv)
        df <- read.csv(input$bulk_csv$datapath)
        colnames(df) <- toupper(gsub("[[:punct:]]| ", "", colnames(df)))
        return(df)
    })
    
    output$group_selector_ui <- renderUI({
        req(raw_bulk()); selectInput("group_var", "Group By:", choices = colnames(raw_bulk()))
    })
    
    processed_bulk <- reactive({
        req(raw_bulk(), input$group_var)
        df <- raw_bulk()
        res <- calc_custom_wepp(df$WDC, df$DI, df$FI, df$WAS, df$BD, df$CLAY, df$SAND, df$SILT, input$rain, input$slope, input$strategy)
        df_final <- cbind(df, res)
        df_final %>% group_by(across(all_of(input$group_var))) %>%
            summarise(Mean_Loss = round(mean(Loss_tons),2), Mean_Yield = round(mean(Yield_tons),2), Mean_Ki = round(mean(Ki),4))
    })
    
    output$bulk_plot <- renderPlot({
        req(processed_bulk())
        ggplot(processed_bulk(), aes_string(x=input$group_var, y="Mean_Loss", fill="Mean_Loss")) +
            geom_bar(stat="identity") + scale_fill_gradient(low="#f9e79f", high="#e67e22") +
            theme_minimal() + labs(title="Average Soil Loss by Group", y="tons/ha")
    })
    
    output$bulk_table <- renderTable({ processed_bulk() })
    
    output$download_report <- downloadHandler(
        filename = function() { paste("Soil-Analysis-Report-", Sys.Date(), ".csv", sep="") },
        content = function(file) { write.csv(processed_bulk(), file) }
    )
}

shinyApp(ui, server)