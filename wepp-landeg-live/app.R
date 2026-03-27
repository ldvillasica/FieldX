library(shiny)
library(ggplot2)
library(shinythemes)
library(dplyr)
library(googlesheets4)

# --- 1. THE DYNAMIC HYDRO-PHYSICS ENGINE ---
calc_all_metrics <- function(wdc, di, fi, was, bd, clay, sand, silt, rain, slope, strategy, vwc) {
    # A. Internal Health (SDI)
    structural_res <- (was / 100) * (fi + 0.1)
    stability_gap <- (di + 0.01) / (structural_res + 0.05)
    sdi_risk <- pmin(100, (stability_gap * (bd / 1.5) * 20))
    
    # B. Hydrology: The Infiltration Storage Gap
    porosity <- 1 - (bd / 2.65)
    sat_ratio <- pmin(1, vwc / porosity) 
    base_infilt <- (100 - clay) * (1.5 / bd) * 0.5 
    clogging_index <- (wdc * di) * (bd / 1.5)
    
    # Effective Infiltration (Reduced by Clogging and Antecedent Moisture)
    eff_infilt <- pmax(2, (base_infilt / (1 + (clogging_index / 10))) * (1 - sat_ratio))
    
    # C. Runoff Depth Calculation
    runoff_depth <- pmax(0, rain - eff_infilt)
    
    # D. WEPP Erodibility Factors
    custom_Ki <- (di / (fi + 0.1)) * (wdc / 100)
    custom_Kr <- 1 / (was + 1) 
    tau_c <- (bd * (clay/100) * (1 + silt/100)) * 5 
    
    mit <- switch(strategy, "None"=1.0, "Gypsum"=0.65, "Cover Crops"=0.55, "Biochar"=0.75)
    
    # E. Erosion Physics
    interrill <- (custom_Ki * (rain^2) * 0.0001) * mit
    shear_stress <- (10 * (runoff_depth/100) * (slope/100)) 
    rill <- pmax(0, custom_Kr * (shear_stress - tau_c)) * mit
    
    loss_tons <- (interrill + rill) * 10
    yield_tons <- loss_tons * pmin(1, (slope / 20)^1.5)
    
    return(data.frame(SDI = sdi_risk, Clog = clogging_index, Infilt = eff_infilt, 
                      Runoff = runoff_depth, Loss = loss_tons, Yield = yield_tons))
}

# --- 2. THE USER INTERFACE ---
ui <- fluidPage(
    theme = shinytheme("flatly"),
    titlePanel("R-based WEPP: Live Data Sync"),
    
    sidebarLayout(
        sidebarPanel(
            tabsetPanel(id = "input_mode",
                        tabPanel("Manual Lab",
                                 br(),
                                 fluidRow(
                                     column(6, numericInput("bd", "Bulk Density", 1.3, step=0.1)),
                                     column(6, sliderInput("vwc", "Initial VWC", 0, 0.5, 0.15))
                                 ),
                                 hr(),
                                 fluidRow(
                                     column(4, numericInput("sand", "Sand%", 30)),
                                     column(4, numericInput("silt", "Silt%", 30)),
                                     column(4, numericInput("clay", "Clay%", 40))
                                 ),
                                 sliderInput("was", "WAS (%)", 60, min=0, max=100),
                                 sliderInput("wdc", "WDC (%)", 15, min=0, max=50),
                                 sliderInput("di", "Disp. Index (DI)", 0.3, min=0, max=1),
                                 sliderInput("fi", "Flocc. Index (FI)", 0.7, min=0, max=1)
                        ),
                        tabPanel("Google Sheets Sync",
                                 br(),
                                 textInput("sheet_url", "Google Sheet URL/ID:", ""),
                                 actionButton("load_sheet", "Sync Now", class = "btn-primary"),
                                 hr(),
                                 helpText("Requires columns: GROUP, WAS, DI, FI, WDC, BD, CLAY, SAND, SILT, VWC"),
                                 uiOutput("group_selector_ui")
                        )
            ),
            hr(),
            h4("Simulation Settings"),
            sliderInput("rain", "Rain Intensity (mm/hr)", 60, min=0, max=150),
            sliderInput("slope", "Slope Gradient (%)", 10, min=0, max=50),
            selectInput("strategy", "Mitigation Strategy", choices = c("None", "Gypsum", "Cover Crops", "Biochar")),
            hr(),
            downloadButton("download_report", "Export Results", class = "btn-success")
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("Scenario Analysis",
                         br(),
                         wellPanel(
                             style = "background: #fdfefe;",
                             h4("Soil Degradation Index (Internal Health)"),
                             h3(textOutput("sdi_status")),
                             plotOutput("sdi_gauge", height = "40px")
                         ),
                         fluidRow(
                             column(4, wellPanel(h5("Est. Infiltration"), h3(textOutput("infilt_man")), "mm/hr")),
                             column(4, wellPanel(h5("Runoff Depth"), h3(textOutput("runoff_man"), style="color:#2980b9"), "mm/hr")),
                             column(4, wellPanel(h5("Pore Clogging"), h3(textOutput("clog_man"), style="color:#8e44ad")))
                         ),
                         plotOutput("erosion_curve", height = "300px"),
                         hr(),
                         h4("Sensitivity Table"),
                         tableOutput("sens_table")
                ),
                tabPanel("Bulk Live Analysis",
                         br(),
                         plotOutput("bulk_plot"),
                         tableOutput("bulk_table")
                )
            )
        )
    )
)

# --- 3. THE SERVER ---
server <- function(input, output, session) {
    
    # Deauthorize Google Sheets (allows reading public sheets without login)
    gs4_deauth()
    
    # Reactive values to hold the Google Sheets data
    sheet_data <- reactiveVal(NULL)
    
    observeEvent(input$load_sheet, {
        req(input$sheet_url)
        tryCatch({
            df <- read_sheet(input$sheet_url)
            colnames(df) <- toupper(gsub("[[:punct:]]| ", "", colnames(df)))
            sheet_data(df)
            showNotification("Sheet synced successfully!", type = "message")
        }, error = function(e) {
            showNotification("Error: Check URL and permissions.", type = "error")
        })
    })
    
    # Manual Calculation Reactive
    res_man <- reactive({
        calc_all_metrics(input$wdc, input$di, input$fi, input$was, input$bd, 
                         input$clay, input$sand, input$silt, input$rain, input$slope, input$strategy, input$vwc)
    })
    
    # UI Outputs for Scenario Tab
    output$sdi_status <- renderText({
        s <- res_man()$SDI
        status <- if(s < 35) "HEALTHY" else if(s < 70) "STRESS WARNING" else "STRUCTURAL COLLAPSE"
        paste0(status, " (", round(s, 1), "%)")
    })
    
    output$sdi_gauge <- renderPlot({
        ggplot() + geom_rect(aes(xmin=0, xmax=100, ymin=0, ymax=1), fill="#ecf0f1") +
            geom_rect(aes(xmin=0, xmax=res_man()$SDI, ymin=0, ymax=1), 
                      fill=ifelse(res_man()$SDI > 70, "#e74c3c", "#3498db")) + theme_void()
    })
    
    output$infilt_man <- renderText({ round(res_man()$Infilt, 1) })
    output$runoff_man <- renderText({ round(res_man()$Runoff, 1) })
    output$clog_man <- renderText({ round(res_man()$Clog, 2) })
    
    output$erosion_curve <- renderPlot({
        s_range <- seq(0, 50, by = 1)
        df <- do.call(rbind, lapply(s_range, function(s) {
            r <- calc_all_metrics(input$wdc, input$di, input$fi, input$was, input$bd, 
                                  input$clay, input$sand, input$silt, input$rain, s, input$strategy, input$vwc)
            data.frame(Slope = s, Yield = r$Yield)
        }))
        ggplot(df, aes(x=Slope, y=Yield)) + geom_area(fill="#27ae60", alpha=0.2) + geom_line(color="#27ae60", size=1.5) +
            geom_hline(yintercept = 11, linetype="dotted", color="red") + theme_minimal() + labs(title="Sediment Yield vs. Topography")
    })
    
    output$sens_table <- renderTable({
        base_loss <- res_man()$Loss
        test_p <- function(wdc=input$wdc, di=input$di, fi=input$fi, was=input$was, bd=input$bd, vwc=input$vwc) {
            new <- calc_all_metrics(wdc, di, fi, was, bd, input$clay, input$sand, input$silt, input$rain, input$slope, input$strategy, vwc)
            return(((base_loss - new$Loss) / (base_loss + 0.001)) * 100)
        }
        data.frame(
            Parameter = c("WAS Stability", "Dispersion (DI)", "Density (BD)", "Initial Moisture (VWC)"),
            Action = c("Increase 10%", "Decrease 10%", "Decrease 10%", "Decrease 10%"),
            Erosion_Reduction = c(
                paste0("-", round(test_p(was = input$was * 1.1), 2), "%"),
                paste0("-", round(test_p(di = input$di * 0.9), 2), "%"),
                paste0("-", round(test_p(bd = input$bd * 0.9), 2), "%"),
                paste0("-", round(test_p(vwc = input$vwc * 0.9), 2), "%")
            )
        )
    })
    
    # Bulk Live Processing
    output$group_selector_ui <- renderUI({
        req(sheet_data()); selectInput("group_var", "Group By Variable:", choices = colnames(sheet_data()))
    })
    
    processed_bulk <- reactive({
        req(sheet_data(), input$group_var); df <- sheet_data()
        res <- calc_all_metrics(df$WDC, df$DI, df$FI, df$WAS, df$BD, df$CLAY, df$SAND, df$SILT, 
                                input$rain, input$slope, input$strategy, df$VWC)
        cbind(df, res) %>% group_by(across(all_of(input$group_var))) %>%
            summarise(Mean_SDI = round(mean(SDI),1), Mean_Yield = round(mean(Yield),2), 
                      Mean_Runoff = round(mean(Runoff),1), Mean_Infilt = round(mean(Infilt),1))
    })
    
    output$bulk_plot <- renderPlot({
        req(processed_bulk())
        ggplot(processed_bulk(), aes_string(x=input$group_var, y="Mean_Yield", fill="Mean_SDI")) +
            geom_bar(stat="identity") + scale_fill_gradient(low="#3498db", high="#e74c3c") +
            labs(title="Bulk Live Comparison", subtitle="Yield vs. Internal Degradation Risk", y="Avg Yield (t/ha)") + theme_minimal()
    })
    
    output$bulk_table <- renderTable({ processed_bulk() })
}

shinyApp(ui, server)