library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(multcompView)
library(agricolae) 

# --- 1. THE SCIENTIFIC ENGINE (UNTOUCHED) ---
calc_soil_physics <- function(sand, silt, clay, vwc, bd, wdc, wds, was, rain, slope, strategy) {
    denom_texture <- clay + silt + 0.01
    denom_clay <- clay + 0.01
    dr_val <- ((wdc + wds) / denom_texture) * 100
    dr_dec <- dr_val / 100
    fi_val <- (clay - wdc) / denom_clay
    resilience <- (was / 100) * (fi_val + 0.1)
    sdi_risk <- pmin(100, ((dr_dec + 0.01) / (resilience + 0.05)) * (bd / 1.5) * 20)
    porosity <- 1 - (bd / 2.65)
    infilt_cap <- (100 - clay) * (1.5 / bd) * 0.5 
    clogging_idx <- (wdc * dr_dec) * (bd / 1.5)
    eff_infilt <- pmax(2, (infilt_cap / (1 + (clogging_idx / 10))) * (1 - (vwc / (porosity + 0.01))))
    runoff_depth <- pmax(0, rain - eff_infilt)
    ki <- (dr_dec / (fi_val + 0.1)) * (wdc / 100) 
    kr <- 1 / (was + 1)                          
    mitigation <- switch(strategy, "None"=1.0, "Gypsum"=0.65, "Cover Crops"=0.55, "Biochar"=0.75)
    gross_loss <- ((ki * (rain^2) * 0.0001) + (kr * runoff_depth * (slope/100))) * 10 * mitigation
    net_yield <- gross_loss * pmin(1, (slope / 20)^1.5)
    
    return(data.frame(SDI=sdi_risk, GrossLoss=gross_loss, NetYield=net_yield, 
                      Infilt=eff_infilt, Runoff=runoff_depth, Clog=clogging_idx,
                      DR=dr_val, FI=fi_val))
}

# --- 2. THE USER INTERFACE ---
ui <- dashboardPage(
    skin = "green",
    dashboardHeader(title = "FieldX Advanced Lab"),
    dashboardSidebar(
        sidebarMenu(
            id = "tabs",
            menuItem("Diagnostic Studio", tabName = "single", icon = icon("microscope")),
            menuItem("Batch Analysis", tabName = "bulk", icon = icon("layer-group")),
            hr(),
            sliderInput("rain", "Precipitation (mm/hr)", 0, 200, 60),
            sliderInput("slope", "Slope Gradient (%)", 0, 60, 10),
            selectInput("strategy", "Management Intervention", choices = c("None", "Gypsum", "Cover Crops", "Biochar"))
        )
    ),
    dashboardBody(
        tags$head(tags$style(HTML(".content-wrapper { background-color: #f4f7f6; }"))),
        tabItems(
            # TAB 1: DIAGNOSTIC STUDIO (SINGLE SCENARIO) - UNTOUCHED
            tabItem(tabName = "single",
                    fluidRow(
                        valueBoxOutput("sdi_box", width = 3),
                        valueBoxOutput("loss_box", width = 3),
                        valueBoxOutput("yield_box", width = 3),
                        valueBoxOutput("infilt_box", width = 3)
                    ),
                    fluidRow(
                        column(width = 4,
                               box(title = "I. Laboratory Data Entry", status = "primary", solidHeader = TRUE, width = NULL,
                                   fluidRow(column(6, numericInput("sand", "Sand (%)", 30)), column(6, numericInput("silt", "Silt (%)", 30))),
                                   fluidRow(column(6, numericInput("clay", "Clay (%)", 40)), column(6, numericInput("bd", "Bulk Density", 1.3))),
                                   hr(),
                                   fluidRow(column(6, numericInput("wdc", "WDC (%)", 15)), column(6, numericInput("wds", "WDS (%)", 5))),
                                   fluidRow(column(6, numericInput("was", "WAS (%)", 60)), column(6, numericInput("vwc", "Initial VWC", 0.15)))
                               ),
                               box(title = "II. Structural Stability Verdict", status = "info", width = NULL, tableOutput("verdict_table"))
                        ),
                        column(width = 8,
                               fluidRow(
                                   box(title = "III. Sediment Yield Risk Matrix", status = "success", solidHeader = TRUE, width = 8, plotOutput("heatmap", height = "320px")),
                                   box(title = "IV. Surface Hydrology", status = "warning", width = 4, infoBoxOutput("clog_stat", width = 12), infoBoxOutput("runoff_stat", width = 12))
                               ),
                               box(title = "V. Physical Property Sensitivity (Yield Reduction %)", status = "danger", width = 12, tableOutput("sens_table"))
                        )
                    )
            ),
            # TAB 2: BATCH ANALYSIS - UPDATED WITH ALL 8 PARAMETERS
            tabItem(tabName = "bulk",
                    fluidRow(
                        box(title = "Batch Configuration (8 Parameters)", status = "primary", width = 4,
                            fileInput("file1", "1. Upload CSV Dataset", accept = ".csv"),
                            uiOutput("mapping_ui"),
                            hr(),
                            h4("Statistical Method"),
                            selectInput("stat_method", "Select Post-Hoc Test:", 
                                        choices = c("Tukey HSD", "Fisher's LSD", "Student's t-test")),
                            numericInput("alpha_lvl", "Significance Level (α)", 0.05, 0.01, 0.1, 0.01),
                            br(),
                            downloadButton("dl_bulk", "Export Calculated Physics", class="btn-block btn-success")),
                        box(title = "Population Variance & Significance", status = "info", width = 8, 
                            plotOutput("bulk_plot", height = "550px"),
                            verbatimTextOutput("stat_summary"))
                    )
            )
        )
    )
)

# --- 3. SERVER LOGIC ---
server <- function(input, output, session) {
    
    # --- SINGLE MODE LOGIC (UNTOUCHED) ---
    data_core <- reactive({
        req(input$sand, input$wds)
        calc_soil_physics(input$sand, input$silt, input$clay, input$vwc, input$bd, input$wdc, input$wds, input$was, input$rain, input$slope, input$strategy)
    })
    
    output$sdi_box <- renderValueBox({ s <- data_core()$SDI; status <- if(s < 35) "STABLE" else if(s < 70) "VULNERABLE" else "CRITICAL"; valueBox(value = paste0(round(s, 1), "%"), subtitle = paste("SDI Status:", status), icon = icon("shield-halved"), color = if(s < 35) "green" else if(s < 70) "yellow" else "red") })
    output$loss_box <- renderValueBox({ valueBox(value = paste(round(data_core()$GrossLoss, 2), "t/ha"), subtitle = "Soil Loss (Source Erosion)", icon = icon("arrows-down-to-line"), color = "purple") })
    output$yield_box <- renderValueBox({ valueBox(value = paste(round(data_core()$NetYield, 2), "t/ha"), subtitle = "Sediment Yield (Outlet Export)", icon = icon("truck-ramp-box"), color = "blue") })
    output$infilt_box <- renderValueBox({ valueBox(value = paste(round(data_core()$Infilt, 1), "mm/hr"), subtitle = "Infiltration (Saturated)", icon = icon("droplet"), color = "teal") })
    output$verdict_table <- renderTable({ d <- data_core(); data.frame(Index = c("Dispersion Ratio", "Flocculation Index"), Value = c(round(d$DR, 2), round(d$FI, 2)), Verdict = c(if(d$DR > 30) "High Detachment" else "Low Dispersion", if(d$FI > 0.7) "Strong Bonding" else "Weak Cohesion")) })
    output$clog_stat <- renderInfoBox({ infoBox("Pore Clogging Index", round(data_core()$Clog, 2), icon = icon("filter-circle-xmark"), color = "orange", fill = TRUE) })
    output$runoff_stat <- renderInfoBox({ infoBox("Runoff Depth", paste0(round(data_core()$Runoff, 1), " mm"), icon = icon("water"), color = "navy", fill = TRUE) })
    output$heatmap <- renderPlot({ r_range <- seq(0, 200, length.out = 20); s_range <- seq(0, 60, length.out = 20); grid <- expand.grid(Rain = r_range, Slope = s_range); grid$Yield <- apply(grid, 1, function(row) calc_soil_physics(input$sand, input$silt, input$clay, input$vwc, input$bd, input$wdc, input$wds, input$was, row[1], row[2], input$strategy)$NetYield); ggplot(grid, aes(Rain, Slope, z = Yield)) + geom_tile(aes(fill = Yield)) + scale_fill_gradientn(colors = c("#27ae60", "#f1c40f", "#e67e22", "#e74c3c", "#c0392b"), name = "t/ha") + geom_point(aes(x = input$rain, y = input$slope), color = "black", size = 5, shape = 10) + theme_minimal() + labs(x = "Precipitation (mm/hr)", y = "Slope Gradient (%)") })
    output$sens_table <- renderTable({ base <- data_core()$NetYield; calc_delta <- function(n_was=input$was, n_wdc=input$wdc, n_bd=input$bd) { r <- calc_soil_physics(input$sand, input$silt, input$clay, input$vwc, n_bd, n_wdc, input$wds, n_was, input$rain, input$slope, input$strategy); paste0(round(((base - r$NetYield) / (base + 1e-5)) * 100, 2), "%") }; data.frame(Property = c("Stability (WAS)", "Dispersion (WDC)", "Compaction (BD)"), Action = c("Increase WAS by 10%", "Decrease WDC by 10%", "Decrease BD by 10%"), "Yield Reduction" = c(calc_delta(n_was=input$was*1.1), calc_delta(n_wdc=input$wdc*0.9), calc_delta(n_bd=input$bd*0.9))) })
    
    # --- BATCH MODE LOGIC (UPDATED MAPPING) ---
    raw_csv <- reactive({ req(input$file1); read.csv(input$file1$datapath) })
    
    output$mapping_ui <- renderUI({
        req(raw_csv()); cols <- colnames(raw_csv())
        tagList(
            selectInput("m_x", "Grouping (Categorical) Column", choices = cols),
            hr(),
            fluidRow(
                column(6, selectInput("m_sand", "Sand (%)", choices = cols, selected = grep("sand", cols, T, T, T)[1])),
                column(6, selectInput("m_silt", "Silt (%)", choices = cols, selected = grep("silt", cols, T, T, T)[1]))
            ),
            fluidRow(
                column(6, selectInput("m_clay", "Clay (%)", choices = cols, selected = grep("clay", cols, T, T, T)[1])),
                column(6, selectInput("m_bd", "Bulk Density", choices = cols, selected = grep("bd|bulk", cols, T, T, T)[1]))
            ),
            fluidRow(
                column(6, selectInput("m_wdc", "WDC (%)", choices = cols, selected = grep("wdc", cols, T, T, T)[1])),
                column(6, selectInput("m_wds", "WDS (%)", choices = cols, selected = grep("wds", cols, T, T, T)[1]))
            ),
            fluidRow(
                column(6, selectInput("m_was", "WAS (%)", choices = cols, selected = grep("was", cols, T, T, T)[1])),
                column(6, selectInput("m_vwc", "Initial VWC", choices = cols, selected = grep("vwc|water", cols, T, T, T)[1]))
            )
        )
    })
    
    batch_data_final <- reactive({
        req(input$m_sand, input$m_silt, input$m_clay, input$m_bd, input$m_wdc, input$m_wds, input$m_was, input$m_vwc)
        raw_csv() %>% rowwise() %>%
            mutate(C = list(calc_soil_physics(
                !!sym(input$m_sand), !!sym(input$m_silt), !!sym(input$m_clay), !!sym(input$m_vwc), 
                !!sym(input$m_bd), !!sym(input$m_wdc), !!sym(input$m_wds), !!sym(input$m_was), 
                input$rain, input$slope, input$strategy
            ))) %>%
            unnest_wider(C)
    })
    
    output$bulk_plot <- renderPlot({
        df <- batch_data_final(); grp <- input$m_x; alph <- input$alpha_lvl
        df_sum <- df %>% group_by(!!sym(grp)) %>% summarise(mY = mean(NetYield), sdY = sd(NetYield), mSDI = mean(SDI))
        
        # Statistical engine
        model <- aov(NetYield ~ as.factor(get(grp)), data = df)
        
        if(input$stat_method == "Tukey HSD") {
            hsd <- TukeyHSD(model, conf.level = 1 - alph)
            res_letters <- multcompLetters4(model, hsd)[[1]]$Letters
        } else if(input$stat_method == "Fisher's LSD") {
            lsd_res <- LSD.test(model, "as.factor(get(grp))", alpha = alph)
            res_letters <- lsd_res$groups$groups; names(res_letters) <- rownames(lsd_res$groups)
        } else {
            lvls <- unique(df[[grp]])
            res_letters <- setNames(rep("", length(lvls)), lvls)
            if(length(lvls) == 2) {
                p_val <- t.test(NetYield ~ get(grp), data = df)$p.value
                res_letters <- setNames(rep(ifelse(p_val < alph, "*", "ns"), 2), lvls)
            }
        }
        
        df_sum$letters <- res_letters[as.character(df_sum[[grp]])]
        
        ggplot(df_sum, aes_string(x=grp, y="mY", fill="mSDI")) + 
            geom_bar(stat="identity", color="black", width=0.7) +
            geom_errorbar(aes(ymin=mY, ymax=mY+sdY), width=0.2) +
            geom_text(aes(label=letters, y=mY+sdY+(max(mY)*0.05)), size=6, fontface="bold") +
            scale_fill_gradientn(colors = c("#27ae60", "#f1c40f", "#e74c3c")) +
            theme_minimal() + labs(title=paste("Comparison via", input$stat_method), y="Mean Yield (t/ha)", fill="Avg SDI %")
    })
    
    output$stat_summary <- renderPrint({
        df <- batch_data_final(); grp <- input$m_x; alph <- input$alpha_lvl
        model <- aov(NetYield ~ as.factor(get(grp)), data = df)
        if(input$stat_method == "Tukey HSD") print(TukeyHSD(model))
        else if(input$stat_method == "Fisher's LSD") print(LSD.test(model, "as.factor(get(grp))", alpha = alph)$groups)
        else summary(model)
    })
    
    output$dl_bulk <- downloadHandler(
        filename = function() { paste0("FieldX_Batch_Full_", Sys.Date(), ".csv") },
        content = function(file) { write.csv(batch_data_final(), file, row.names = FALSE) }
    )
}

shinyApp(ui, server)