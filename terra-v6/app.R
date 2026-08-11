library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(multcompView)

# Ensure dplyr priority
select <- dplyr::select

# --- 1. THE PHYSICS ENGINE (v6.4.8) ---
calc_sample_timeline <- function(row_data, rain, duration, group_id) {
    tryCatch({
        clay <- as.numeric(row_data[["clay"]]); silt <- as.numeric(row_data[["silt"]])
        bd <- as.numeric(row_data[["bd"]]); wdc <- as.numeric(row_data[["wdc"]])
        wds <- as.numeric(row_data[["wds"]]); was <- as.numeric(row_data[["was"]])
        local_slope <- if("slope" %in% names(row_data)) as.numeric(row_data[["slope"]]) else 5
        vwc_initial <- as.numeric(row_data[["vwc"]]) / 100 
        
        if(any(is.na(c(clay, bd, wdc, was)))) return(NULL)
        
        denom <- clay + silt + 0.01
        dr_dec <- (wdc + wds) / denom
        fi_val <- (clay - wdc) / (clay + 0.01)
        resilience <- (was / 100) * (fi_val + 0.1)
        sdi_risk <- pmin(100, ((dr_dec + 0.01) / (resilience + 0.05)) * (bd / 1.5) * 20)
        
        total_porosity <- 1 - (bd / 2.65)
        sat_deficit <- pmax(0, total_porosity - vwc_initial)
        fc <- pmax(2, ((100 - clay) * (1.5 / bd) * 0.5) / (1 + ((wdc * (dr_dec/100) * bd/1.5) / 10)))
        f0 <- fc * (2.5 + (10 * sat_deficit)) 
        k_decay <- 0.01 + ((0.1 / (resilience + 0.1)) * (sdi_risk / 100)) + (0.05 * sat_deficit)
        
        t_seq <- 0:120
        infilt_vec <- fc + (f0 - fc) * exp(-k_decay * t_seq)
        rain_vec <- ifelse(t_seq <= duration, rain, 0)
        runoff_vec <- pmax(0, rain_vec - infilt_vec)
        
        ki <- (dr_dec / (resilience + 0.1)) * (wdc / 100)
        kr <- 1 / (was + 1)
        total_loss_rate <- ((ki * (rain_vec^2) * 0.0001) + (kr * runoff_vec * (local_slope / 100))) * 10
        fail_idx <- which(infilt_vec < rain)[1]
        
        data.frame(
            Time = t_seq, GroupID = as.character(group_id), 
            Infilt_Rate = infilt_vec, Runoff_Rate = runoff_vec, Total_Loss_Rate = total_loss_rate,
            FailTime = if(!is.na(fail_idx)) fail_idx - 1 else NA,
            SDI = sdi_risk, Dispersion_Ratio = dr_dec, Flocculation_Index = fi_val,
            VWC_Value = vwc_initial * 100, BD_Input = bd, Clay = clay, Silt = silt, 
            WAS = was, Steady_State_Inf = fc, Decay_Constant_k = k_decay,
            stringsAsFactors = FALSE
        )
    }, error = function(e) return(NULL))
}

# --- 2. USER INTERFACE ---
ui <- dashboardPage(
    skin = "black",
    dashboardHeader(title = "TERRA v6.4.8 | CORE", titleWidth = 300),
    dashboardSidebar(
        width = 300,
        div(style = "padding: 15px;", 
            h4("Control Panel"),
            fileInput("file1", "1. Load Replicated Data", accept = ".csv", width = "100%"),
            uiOutput("group_selector"),
            hr(),
            h4("Environmental Forcing"),
            sliderInput("anim_time", "Storm Timeline (min):", 0, 120, 0, step = 1, 
                        animate = animationOptions(interval = 300, loop = FALSE)),
            sliderInput("rain", "Intensity (mm/hr):", 0, 200, 75),
            sliderInput("duration", "Duration (min):", 5, 120, 60),
            hr(),
            downloadButton("downloadData", "Download Full Model Data", class="btn-block")
        )
    ),
    dashboardBody(
        tags$head(tags$style(HTML("
      .main-sidebar { font-size: 14px; }
      .box-title { font-weight: bold; text-transform: uppercase; letter-spacing: 1px; }
      .table.shiny-table { font-size: 13px; }
      .content-wrapper { background-color: #f4f6f9; }
    "))),
        tabsetPanel(
            tabPanel("Time-Series Simulation",
                     br(),
                     fluidRow(
                         box(title = "Dynamic Infiltration & Soil Stability", status = "primary", solidHeader = TRUE, width = 9,
                             plotOutput("ensemble_plot", height = "550px")),
                         box(title = "Physics Snapshot", status = "info", width = 3,
                             helpText("Mean Soil Properties:"),
                             tableOutput("physics_params_table"),
                             hr(),
                             p(strong("Legend:")),
                             p("🟠 Orange Dotted: Rain Intensity"),
                             p("📈 Solid Line: Infiltration Capacity"),
                             p("💎 Diamond: Ponding Failure"))
                     ),
                     fluidRow(
                         box(title = "Cumulative Hydrological Response", status = "primary", width = 12, 
                             tableOutput("live_table"))
                     )
            ),
            tabPanel("Statistical Diagnostics",
                     br(),
                     fluidRow(
                         box(title = "Comparative Analysis", status = "warning", solidHeader = TRUE, width = 8,
                             selectInput("index_var", "Select Metric:", 
                                         choices = c("SDI", "Dispersion_Ratio", "Flocculation_Index", "BD_Input", "VWC_Value", "Clay", "Silt", "WAS", "Steady_State_Inf", "Decay_Constant_k")),
                             plotOutput("stat_plot", height = "480px")),
                         box(title = "Variance Analysis (ANOVA)", status = "danger", solidHeader = TRUE, width = 4,
                             verbatimTextOutput("anova_text"),
                             hr(),
                             h4("Tukey Groupings"),
                             tableOutput("remarks_table"),
                             p(class="text-muted", "Significance Level: alpha = 0.05"))
                     )
            )
        )
    )
)

# --- 3. SERVER ---
server <- function(input, output, session) {
    
    raw_df <- reactive({ 
        req(input$file1); df <- read.csv(input$file1$datapath, stringsAsFactors = FALSE) 
        names(df) <- tolower(gsub("[[:punct:]]| ", "", names(df))); df
    })
    
    output$group_selector <- renderUI({ 
        req(raw_df()); selectInput("group_col", "2. Target ID Column:", choices = names(raw_df())) 
    })
    
    replicate_indices <- reactive({
        req(raw_df(), input$group_col); df <- raw_df(); gid <- input$group_col
        datalist <- lapply(1:nrow(df), function(i) {
            res <- calc_sample_timeline(df[i,], input$rain, input$duration, df[i, gid])
            if(!is.null(res)) res %>% dplyr::filter(Time == 0) else NULL
        }); dplyr::bind_rows(datalist)
    })
    
    group_means <- reactive({
        req(raw_df(), input$group_col); gid <- input$group_col
        raw_df() %>% group_by(!!sym(gid)) %>%
            summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
            rename(GroupID_Internal = !!sym(gid))
    })
    
    ensemble_data <- reactive({
        req(group_means()); df_avg <- group_means()
        paths_list <- lapply(1:nrow(df_avg), function(i) {
            calc_sample_timeline(df_avg[i,], input$rain, input$duration, df_avg$GroupID_Internal[i])
        }); dplyr::bind_rows(paths_list)
    })
    
    output$ensemble_plot <- renderPlot({
        req(ensemble_data())
        res <- ensemble_data() %>% filter(Time <= input$anim_time)
        failures <- ensemble_data() %>% filter(!is.na(FailTime), FailTime <= input$anim_time, Time == FailTime)
        
        p <- ggplot(res, aes(x = Time, y = Infilt_Rate, color = GroupID, group = GroupID)) +
            geom_hline(yintercept = input$rain, linetype = "dotted", color = "#e67e22", linewidth = 1.2) +
            geom_line(linewidth = 1.8, alpha = 0.9) +
            scale_x_continuous(limits = c(0, 120), breaks = seq(0, 120, 10)) +
            scale_y_continuous(limits = c(0, max(ensemble_data()$Infilt_Rate, input$rain) * 1.1)) +
            theme_minimal() +
            labs(subtitle = paste("Simulation Progress:", input$anim_time, "Minutes"),
                 y = "Infiltration Rate (mm/hr)", x = "Storm Minutes", color = "Treatment") +
            theme(text = element_text(family = "sans", size = 15),
                  legend.position = "bottom",
                  panel.grid.minor = element_blank(),
                  plot.title = element_text(face = "bold"))
        
        if(nrow(failures) > 0) {
            p <- p + 
                geom_point(data = failures, aes(y = input$rain), shape = 23, size = 6, fill = "white", stroke = 2) +
                geom_text(data = failures, aes(y = input$rain, label = paste0(GroupID, " (", FailTime, "m)")), 
                          vjust = -2, fontface = "bold", size = 5, show.legend = FALSE)
        }
        p
    })
    
    output$live_table <- renderTable({
        req(ensemble_data())
        ensemble_data() %>% filter(Time <= input$anim_time) %>% group_by(GroupID) %>%
            summarise("Ponding Point" = paste(first(FailTime), "min"),
                      "Cumulative Runoff (mm)" = round(sum(Runoff_Rate)/60, 2),
                      "Total Soil Loss (t/ha)" = round(sum(Total_Loss_Rate)/60, 4))
    }, striped = TRUE, bordered = TRUE, align = 'c')
    
    output$physics_params_table <- renderTable({
        req(group_means())
        group_means() %>% dplyr::select(GroupID_Internal, clay, bd, was) %>% 
            dplyr::rename(ID = GroupID_Internal, "Clay%" = clay, "BD" = bd, "WAS" = was)
    }, striped = TRUE, digits = 2)
    
    stat_analysis <- reactive({
        req(replicate_indices()); df <- replicate_indices(); df$GroupID <- as.factor(df$GroupID)
        fit <- aov(as.formula(paste(input$index_var, "~ GroupID")), data = df)
        tukey <- TukeyHSD(fit); cld <- multcompLetters(tukey[[1]][,4])$Letters
        df_sum <- df %>% group_by(GroupID) %>%
            summarise(Mean = mean(!!sym(input$index_var), na.rm = TRUE), SD = sd(!!sym(input$index_var), na.rm = TRUE), .groups = "drop") %>%
            mutate(Letter = cld[as.character(GroupID)])
        list(summary = df_sum, fit = fit)
    })
    
    output$stat_plot <- renderPlot({
        req(stat_analysis())
        ggplot(stat_analysis()$summary, aes(x = reorder(GroupID, -Mean), y = Mean, fill = GroupID)) +
            geom_bar(stat = "identity", width = 0.7, color = "#2c3e50", alpha = 0.8) +
            geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2, linewidth = 0.8) +
            geom_text(aes(label = Letter, y = Mean + SD), vjust = -1, size = 7, fontface = "bold") +
            theme_minimal() + labs(y = "Mean Observed Value", x = "Treatment Group") +
            theme(text = element_text(size = 15), legend.position = "none")
    })
    
    output$anova_text <- renderPrint({ req(stat_analysis()); summary(stat_analysis()$fit) })
    output$remarks_table <- renderTable({ req(stat_analysis()); stat_analysis()$summary %>% dplyr::select(GroupID, Mean, Letter) }, striped = TRUE)
    
    output$downloadData <- downloadHandler(
        filename = function() { paste0("TERRA_Modeling_Report_", Sys.Date(), ".csv") },
        content = function(file) { write.csv(ensemble_data(), file, row.names = FALSE) }
    )
}

shinyApp(ui, server)