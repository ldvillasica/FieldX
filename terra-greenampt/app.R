library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)

# --- 1. THE PHYSICS ENGINE (GREEN-AMPT) ---
calc_green_ampt <- function(clay, vwc_perc, bd, was, rain_rate) {
    # 1. Derive Physical Parameters from Texture/BD
    porosity <- 1 - (bd / 2.65)
    vwc_decimal <- vwc_perc / 100
    
    # Effective Porosity (available space for water)
    theta_e <- pmax(0.01, porosity - vwc_decimal)
    
    # Saturated Hydraulic Conductivity (Ke) 
    # Using a simplified Rawls & Brakensiek approach based on your clay/BD
    ke <- pmax(0.1, (100 - clay) * (1.5 / bd) * 0.2) 
    
    # Suction Head (psi) in mm - Clay exerts more "pull" than sand
    psi <- 10 * (clay^0.5) * (bd / 1.3)
    
    # 2. Simulation over 120 minutes
    dt <- 1/60 # 1 minute time step in hours
    times <- seq(0, 2, by = dt) # 2 hours
    
    results <- data.frame(
        Time_min = seq(0, 120),
        Cum_Infilt = 0,
        Infilt_Rate = 0,
        Rain_Rate = rain_rate
    )
    
    F_cum <- 0 # Cumulative infiltration depth (mm)
    
    for(i in 2:nrow(results)) {
        # Green-Ampt Equation: f = Ke * (1 + (psi * theta_e / F))
        # If F is 0, f is infinite, so we use a small epsilon
        if(F_cum <= 0) {
            f_pot <- 1000 # Very high initial potential
        } else {
            f_pot <- ke * (1 + (psi * theta_e / F_cum))
        }
        
        # Actual infiltration is the lesser of rain rate or soil potential
        f_act <- min(rain_rate, f_pot)
        
        # Update cumulative infiltration
        F_cum <- F_cum + (f_act * dt)
        
        results$Infilt_Rate[i] <- f_pot
        results$Cum_Infilt[i] <- F_cum
    }
    
    return(list(df = results, ke = ke, psi = psi, theta_e = theta_e))
}

# --- 2. USER INTERFACE ---
ui <- dashboardPage(
    skin = "green",
    dashboardHeader(title = "Green-Ampt Lab"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Physics Simulator", tabName = "sim", icon = icon("flask")),
            hr(),
            sliderInput("rain", "Rainfall Intensity (mm/hr)", 0, 150, 60),
            numericInput("bd", "Bulk Density (g/cc)", 1.16, step = 0.05),
            numericInput("clay", "Clay %", 14.3, step = 1),
            sliderInput("vwc", "Initial Moisture (VWC %)", 0, 50, 4.5),
            helpText("Note: If VWC > Porosity, runoff is instant.")
        )
    ),
    dashboardBody(
        fluidRow(
            valueBoxOutput("ke_box", width = 4),
            valueBoxOutput("psi_box", width = 4),
            valueBoxOutput("ponding_box", width = 4)
        ),
        fluidRow(
            box(title = "Infiltration Capacity Curve", status = "success", solidHeader = TRUE, width = 8,
                plotOutput("ga_plot")),
            box(title = "Hydraulic Parameters", status = "info", width = 4,
                tableOutput("param_table"),
                helpText("Ke: Saturated Conductivity (mm/hr)"),
                helpText("Psi: Wetting Front Suction (mm)"))
        )
    )
)

# --- 3. SERVER LOGIC ---
server <- function(input, output) {
    
    sim_res <- reactive({
        calc_green_ampt(input$clay, input$vwc, input$bd, 80, input$rain)
    })
    
    output$ga_plot <- renderPlot({
        df <- sim_res()$df
        # Finding ponding point (where potential rate falls below rain rate)
        ponding_idx <- which(df$Infilt_Rate < input$rain)[1]
        ponding_time <- if(!is.na(ponding_idx)) df$Time_min[ponding_idx] else NULL
        
        ggplot(df, aes(x = Time_min)) +
            geom_line(aes(y = Infilt_Rate, color = "Infilt. Capacity (Potential)"), size = 1.2) +
            geom_line(aes(y = Rain_Rate, color = "Rainfall Intensity"), linetype = "dashed", size = 1) +
            geom_area(aes(y = pmax(0, Rain_Rate - Infilt_Rate)), fill = "red", alpha = 0.3) +
            scale_color_manual(values = c("Infilt. Capacity (Potential)" = "#27ae60", "Rainfall Intensity" = "#2c3e50")) +
            labs(x = "Time (min)", y = "Rate (mm/hr)", title = "Green-Ampt Dynamic Infiltration") +
            theme_minimal() +
            coord_cartesian(ylim = c(0, pmax(200, input$rain + 20))) +
            if(!is.null(ponding_time)) geom_vline(xintercept = ponding_time, color = "red", linetype = "dotted")
    })
    
    output$ke_box <- renderValueBox({
        valueBox(round(sim_res()$ke, 2), "Effective Ke (mm/hr)", icon = icon("faucet"), color = "teal")
    })
    
    output$psi_box <- renderValueBox({
        valueBox(round(sim_res()$psi, 1), "Suction Head (mm)", icon = icon("magnet"), color = "blue")
    })
    
    output$ponding_box <- renderValueBox({
        df <- sim_res()$df
        p_idx <- which(df$Infilt_Rate < input$rain)[1]
        p_time <- if(is.na(p_idx)) "Never" else paste(df$Time_min[p_idx], "min")
        valueBox(p_time, "Time to Ponding", icon = icon("clock"), color = "orange")
    })
    
    output$param_table <- renderTable({
        res <- sim_res()
        data.frame(
            Parameter = c("Eff. Porosity (θe)", "Suction (ψ)", "Conductivity (Ke)"),
            Value = c(round(res$theta_e, 3), round(res$psi, 2), round(res$ke, 2))
        )
    })
}

shinyApp(ui, server)