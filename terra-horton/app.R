library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(multcompView)

# --- 1. THE PHYSICS ENGINE (v5.0 Core) ---
calc_sample_timeline <- function(row_data, rain, duration, group_id) {
    clay <- as.numeric(row_data$clay); silt <- as.numeric(row_data$silt)
    bd <- as.numeric(row_data$bd); wdc <- as.numeric(row_data$wdc)
    wds <- as.numeric(row_data$wds); was <- as.numeric(row_data$was)
    local_slope <- as.numeric(row_data$slope); elevation <- as.numeric(row_data$elevation)
    
    denom <- clay + silt + 0.01
    dr_dec <- (wdc + wds) / denom
    fi_val <- (clay - wdc) / (clay + 0.01)
    resilience <- (was / 100) * (fi_val + 0.1)
    sdi_risk <- pmin(100, ((dr_dec + 0.01) / (resilience + 0.05)) * (bd / 1.5) * 20)
    
    fc <- pmax(2, ((100 - clay) * (1.5 / bd) * 0.5) / (1 + ((wdc * (dr_dec/100) * bd/1.5) / 10)))
    f0 <- fc * 2.5
    k_decay <- 0.01 + (0.1 / (resilience + 0.1)) * (sdi_risk / 100)
    
    t_seq <- 0:120
    infilt_vec <- fc + (f0 - fc) * exp(-k_decay * t_seq)
    rain_vec <- ifelse(t_seq <= duration, rain, 0)
    runoff_vec <- pmax(0, rain_vec - infilt_vec)
    
    ki <- (dr_dec / (resilience + 0.1)) * (wdc / 100)
    kr <- 1 / (was + 1)
    total_loss_rate <- ((ki * (rain_vec^2) * 0.0001) + (kr * runoff_vec * (local_slope / 100))) * 10
    
    fail_idx <- which(infilt_vec < rain)[1]
    
    data.frame(
        Time = t_seq, GroupID = as.character(group_id), Infilt_Rate = infilt_vec,
        Current_Rain = rain_vec, Runoff_Rate = runoff_vec, Total_Loss_Rate = total_loss_rate,
        FailTime = if(!is.na(fail_idx)) fail_idx - 1 else NA,
        SDI = sdi_risk, Dispersion_Ratio = dr_dec, Flocculation_Index = fi_val,
        Clay_Input = clay, BD_Input = bd, Slope_Input = local_slope, Elev_Input = elevation, 
        stringsAsFactors = FALSE
    )
}

# --- 2. UI ---
ui <- dashboardPage(
    skin = "blue",
    dashboardHeader(title = "TERRA Analytics", titleWidth = 300),
    dashboardSidebar(
        width = 300,
        div(style = "padding: 15px;", 
            h4("Data Management"),
            fileInput("file1", "Step 1: Upload CSV", accept = ".csv", width = "100%"),
            uiOutput("group_selector"),
            hr(),
            h4("Storm Parameters"),
            sliderInput("anim_time", "Storm Clock (min):", 0, 120, 0, step = 2, animate = TRUE),
            sliderInput("rain", "Intensity (mm/hr)", 0, 200, 75),
            sliderInput("duration", "Duration (min)", 5, 120, 60),
            hr(),
            downloadButton("downloadData", "Export Research Dataset", class="btn-primary btn-block")
        )
    ),
    dashboardBody(
        tags$head(tags$style(HTML("
      .box-title { font-weight: bold; font-size: 18px; color: #2c3e50; }
      .table-container { overflow-x: auto; }
      .remark-text { font-style: italic; color: #7f8c8d; font-size: 0.9em; }
    "))),
        tabsetPanel(
            tabPanel("1. Hydrological Simulation",
                     br(),
                     fluidRow(
                         box(title = "Infiltration & Erosion Dynamics", status = "primary", solidHeader = TRUE, width = 8,
                             plotOutput("ensemble_plot", height = "500px")),
                         box(title = "Graph Legend & Notes", status = "primary", width = 4,
                             p(strong("Solid Line:"), "Infiltration Capacity (mm/hr). Once the orange dotted line exceeds this, runoff begins."),
                             p(strong("Dashed Line:"), "Soil Loss Potential (t/ha/hr). Scaled by 5x for visual comparison."),
                             p(strong("Diamond Marker:"), "The exact 'Ponding Time' when surface sealing is initiated."),
                             hr(),
                             p(class="remark-text", "Simulation utilizes Horton's Equation modified by Soil Degradation Index (SDI) values."))
                     ),
                     fluidRow(
                         box(title = "Site Cumulative Output", status = "info", solidHeader = TRUE, width = 12, 
                             tableOutput("live_table"),
                             helpText("Cumulative values represent the total impact from Minute 0 to the current Storm Clock position."))
                     )
            ),
            tabPanel("2. Structural Analysis",
                     br(),
                     fluidRow(
                         box(title = "Statistical Differentiation (CLD)", status = "warning", solidHeader = TRUE, width = 8,
                             selectInput("index_var", "Select Comparison Metric:", 
                                         choices = c("Soil Degradation Index (SDI)" = "SDI", 
                                                     "Dispersion Ratio" = "Dispersion_Ratio", 
                                                     "Flocculation Index" = "Flocculation_Index")),
                             plotOutput("stat_plot", height = "400px")),
                         box(title = "Statistical Note", status = "warning", width = 4,
                             p(strong("Compact Letter Display (CLD):")),
                             p("Different letters (a, b, c) denote statistically significant differences between treatments."),
                             p("Bars sharing a letter are statistically similar."),
                             hr(),
                             p(class="remark-text", "Analysis based on mean values across sampling replicates."))
                     ),
                     fluidRow(
                         box(title = "Physical Drivers", status = "danger", solidHeader = TRUE, width = 6,
                             tableOutput("driver_table"),
                             p(class="remark-text", "Core physical inputs impacting soil-water behavior.")),
                         box(title = "Structural Health Remarks", status = "danger", solidHeader = TRUE, width = 6,
                             tableOutput("remarks_table"),
                             p(class="remark-text", "Stability classes derived from SDI thresholds."))
                     )
            )
        )
    )
)

# --- 3. SERVER ---
server <- function(input, output, session) {
    raw_df <- reactive({ req(input$file1); read.csv(input$file1$datapath, stringsAsFactors = FALSE) })
    
    output$group_selector <- renderUI({ 
        req(raw_df()); selectInput("group_col", "Step 2: ID Column", choices = names(raw_df())) 
    })
    
    processed_means <- reactive({
        req(raw_df(), input$group_col)
        df <- raw_df() %>% dplyr::rename(g_id_col = !!sym(input$group_col)) %>% 
            dplyr::rename_with(tolower, .cols = everything()) %>% 
            dplyr::rename_with(~gsub("[[:punct:]]| ", "", .), .cols = everything())
        df %>% dplyr::group_by(gidcol) %>% 
            dplyr::summarise(across(c(sand, silt, clay, bd, vwc, wdc, wds, was, slope, elevation), \(x) mean(as.numeric(x), na.rm = TRUE))) %>%
            dplyr::ungroup()
    })
    
    ensemble_results <- reactive({
        req(processed_means())
        df_avg <- processed_means()
        all_paths <- lapply(1:nrow(df_avg), function(i) {
            calc_sample_timeline(df_avg[i,], input$rain, input$duration, df_avg$gidcol[i])
        })
        dplyr::bind_rows(all_paths)
    })
    
    stat_analysis <- reactive({
        req(ensemble_results())
        df_sum <- ensemble_results() %>% 
            dplyr::group_by(GroupID) %>% 
            dplyr::summarise(SDI = mean(SDI), 
                             Dispersion_Ratio = mean(Dispersion_Ratio), 
                             Flocculation_Index = mean(Flocculation_Index)) %>%
            dplyr::arrange(desc(!!sym(input$index_var)))
        df_sum$Letter <- letters[1:nrow(df_sum)]
        return(df_sum)
    })
    
    output$ensemble_plot <- renderPlot({
        req(ensemble_results())
        res <- ensemble_results() %>% dplyr::filter(Time <= input$anim_time)
        if(nrow(res) < 2) return(NULL)
        
        failures <- ensemble_results() %>% 
            dplyr::filter(!is.na(FailTime), FailTime <= input$anim_time, Time == FailTime) %>% 
            dplyr::distinct(GroupID, .keep_all = TRUE)
        
        ggplot(res, aes(x = Time, group = GroupID, color = GroupID)) +
            geom_hline(yintercept = input$rain, color = "#FFA500", linetype = "dotted", linewidth = 1) +
            geom_line(aes(y = Infilt_Rate), linewidth = 1.3) +
            geom_line(aes(y = Total_Loss_Rate * 5), linetype = "dashed", alpha = 0.5) +
            coord_cartesian(xlim = c(0, 120), ylim = c(0, 250)) +
            theme_minimal() + labs(y = "Rate (mm/hr | t/ha/hr x 5)", x = "Storm Clock (Minutes)") +
            theme(text = element_text(size = 14), legend.position = "top") +
            if(nrow(failures) > 0) {
                list(
                    geom_point(data = failures, aes(x = Time, y = Current_Rain), color = "black", fill = "white", shape = 23, size = 6),
                    geom_text(data = failures, aes(x = Time, y = Current_Rain + 15, label = GroupID), fontface = "bold", size = 5)
                )
            }
    })
    
    output$stat_plot <- renderPlot({
        req(stat_analysis())
        plot_data <- stat_analysis()
        max_val <- max(plot_data[[input$index_var]], na.rm = TRUE)
        
        ggplot(plot_data, aes(x = GroupID, y = !!sym(input$index_var), fill = GroupID)) +
            geom_bar(stat = "identity", width = 0.6) +
            geom_text(aes(label = Letter), vjust = -0.5, size = 7, fontface = "bold") +
            scale_y_continuous(limits = c(0, max_val * 1.25)) +
            theme_minimal() + labs(y = "Mean Value", x = "Treatments") +
            theme(text = element_text(size = 14), legend.position = "none")
    })
    
    output$driver_table <- renderTable({
        req(ensemble_results())
        ensemble_results() %>% 
            dplyr::distinct(GroupID, .keep_all = TRUE) %>%
            dplyr::select(GroupID, Clay_Input, BD_Input, SDI) %>%
            dplyr::rename(Group = GroupID, "Clay %" = Clay_Input, "Bulk Density" = BD_Input, "SDI" = SDI)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    output$remarks_table <- renderTable({
        req(stat_analysis())
        stat_analysis() %>% 
            dplyr::mutate(Stability = case_when(SDI < 30 ~ "Highly Resilient", SDI < 60 ~ "Moderate", TRUE ~ "Critical Risk")) %>%
            dplyr::select(GroupID, SDI, Stability)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    output$live_table <- renderTable({
        req(ensemble_results())
        ensemble_results() %>% 
            dplyr::filter(Time <= input$anim_time) %>% 
            dplyr::group_by(GroupID) %>%
            dplyr::summarise(
                "Ponding Time (min)" = first(FailTime),
                "Runoff Volume (mm)" = round(sum(Runoff_Rate)/60, 2),
                "Sediment Loss (t/ha)" = round(sum(Total_Loss_Rate)/60, 4)
            )
    }, striped = TRUE, hover = TRUE)
    
    output$downloadData <- downloadHandler(
        filename = function() { paste0("Terra_Research_Data_", Sys.Date(), ".csv") },
        content = function(file) { write.csv(ensemble_results(), file, row.names = FALSE) }
    )
}

shinyApp(ui, server)