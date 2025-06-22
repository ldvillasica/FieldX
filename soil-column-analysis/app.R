# app.R
library(shiny)
library(dplyr) # For data manipulation, especially for the table

ui <- fluidPage(
    titlePanel("Basic Two-Layer Saturated Soil Flow Solver"),
    
    sidebarLayout(
        sidebarPanel(
            h3("Layer 1 (Top Layer)"),
            numericInput("L1", "Thickness of Layer 1 ($L_1$, cm):", value = 10, min = 0.1),
            numericInput("Ks1", "Saturated Hydraulic Conductivity of Layer 1 ($K_{s1}$, cm/h):", value = 5.0, min = 0.01),
            hr(), # Horizontal rule for separation
            
            h3("Layer 2 (Bottom Layer)"),
            numericInput("L2", "Thickness of Layer 2 ($L_2$, cm):", value = 10, min = 0.1),
            numericInput("Ks2", "Saturated Hydraulic Conductivity of Layer 2 ($K_{s2}$, cm/h):", value = 10.0, min = 0.01),
            hr(),
            
            h3("Boundary Conditions"),
            numericInput("Hw", "Ponded Water Depth ($h_w$, cm):", value = 10, min = 0)
        ),
        mainPanel(
            h3("Calculated Results"),
            
            h4("Total Hydraulic Head Difference (ΔH):"),
            textOutput("deltaH_output", inline = TRUE),
            br(),
            
            h4("Hydraulic Head Table:"),
            tableOutput("headTable_output"),
            br(),
            
            h4("Hydraulic Resistance:"),
            p("Resistance of Layer 1 ($R_1$): ", textOutput("R1_output", inline = TRUE)),
            p("Resistance of Layer 2 ($R_2$): ", textOutput("R2_output", inline = TRUE)),
            p("Total Hydraulic Resistance ($R_{total}$): ", textOutput("Rtotal_output", inline = TRUE)),
            br(),
            
            h4("Flux of Water ($q$):"),
            textOutput("flux_output", inline = TRUE),
            br(),
            
            h4("Pressure Head at Layer Interface ($h_{p,interface}$):"),
            p("Elevation Head at Interface ($z_{interface}$): ", textOutput("z_interface_output", inline = TRUE)),
            p("Pressure Head at Interface ($h_{p,interface}$): ", textOutput("hp_interface_output", inline = TRUE)),
            p("Total Head at Interface ($H_{interface}$): ", textOutput("H_interface_output", inline = TRUE))
        )
    )
)

server <- function(input, output) {
    
    # Reactive expression to perform all calculations
    calculations <- reactive({
        # Ensure all inputs are available and valid before proceeding
        req(input$L1, input$Ks1, input$L2, input$Ks2, input$Hw)
        
        L1 <- input$L1
        Ks1 <- input$Ks1
        L2 <- input$L2
        Ks2 <- input$Ks2
        Hw <- input$Hw
        
        # --- Input Validation ---
        if (L1 <= 0 || Ks1 <= 0 || L2 <= 0 || Ks2 <= 0 || Hw < 0) {
            return(NULL) # Return NULL for invalid inputs
        }
        
        # --- 1. Hydraulic Head Difference ---
        # Datum (z=0) is at the bottom of the column
        
        # Top of column (water surface)
        z_top <- L1 + L2
        hp_top <- Hw
        H_top <- z_top + hp_top
        
        # Bottom of column (drains freely to atmosphere)
        z_bottom <- 0
        hp_bottom <- 0 # Atmospheric pressure head
        H_bottom <- z_bottom + hp_bottom
        
        delta_H <- H_top - H_bottom
        
        # --- 2. Hydraulic Resistance for each layer ---
        R1 <- L1 / Ks1
        R2 <- L2 / Ks2
        R_total <- R1 + R2
        
        # --- 3. Flux of water through the column ---
        q <- delta_H / R_total
        
        # --- 4. Pressure head at the layer interface ---
        # The interface is at elevation z = L2 (from the bottom datum)
        z_interface <- L2
        
        # Calculate total head at interface by working down from the top
        # H_top - H_interface = q * R1 (applying Darcy's Law to the top layer)
        H_interface <- H_top - (q * R1)
        
        # Calculate pressure head at interface: H = z + hp => hp = H - z
        hp_interface <- H_interface - z_interface
        
        # Return all calculated values as a list
        list(
            delta_H = delta_H,
            z_top = z_top, hp_top = hp_top, H_top = H_top,
            z_bottom = z_bottom, hp_bottom = hp_bottom, H_bottom = H_bottom,
            z_interface = z_interface, hp_interface = hp_interface, H_interface = H_interface,
            R1 = R1, R2 = R2, R_total = R_total,
            q = q
        )
    })
    
    # Render Outputs
    output$deltaH_output <- renderText({
        data <- calculations()
        if (is.null(data)) return("Please enter valid positive numbers for all inputs.")
        paste0(sprintf("%.2f", data$delta_H), " cm")
    })
    
    output$headTable_output <- renderTable({
        data <- calculations()
        if (is.null(data)) return(NULL)
        
        df <- data.frame(
            Location = c("Surface (Top of Ponded Water)", "Interface (Layer 1-Layer 2)", "Bottom of Column (Atmospheric)"),
            `Elevation Head ($z$, cm)` = c(data$z_top, data$z_interface, data$z_bottom),
            `Pressure Head ($h_p$, cm)` = c(data$hp_top, data$hp_interface, data$hp_bottom),
            `Total Head ($H$, cm)` = c(data$H_top, data$H_interface, data$H_bottom)
        )
        df %>%
            mutate_at(vars(-Location), ~sprintf("%.2f", .))
    }, striped = TRUE, bordered = TRUE, hover = TRUE, align = 'lrrr', sanitize.text.function = function(x) x)
    
    
    output$R1_output <- renderText({
        data <- calculations()
        if (is.null(data)) return("N/A")
        paste0(sprintf("%.2f", data$R1), " h")
    })
    
    output$R2_output <- renderText({
        data <- calculations()
        if (is.null(data)) return("N/A")
        paste0(sprintf("%.2f", data$R2), " h")
    })
    
    output$Rtotal_output <- renderText({
        data <- calculations()
        if (is.null(data)) return("N/A")
        paste0(sprintf("%.2f", data$R_total), " h")
    })
    
    output$flux_output <- renderText({
        data <- calculations()
        if (is.null(data)) return("N/A")
        paste0(sprintf("%.4f", data$q), " cm/h")
    })
    
    output$z_interface_output <- renderText({
        data <- calculations()
        if (is.null(data)) return("N/A")
        paste0(sprintf("%.2f", data$z_interface), " cm")
    })
    
    output$hp_interface_output <- renderText({
        data <- calculations()
        if (is.null(data)) return("N/A")
        paste0(sprintf("%.2f", data$hp_interface), " cm")
    })
    
    output$H_interface_output <- renderText({
        data <- calculations()
        if (is.null(data)) return("N/A")
        paste0(sprintf("%.2f", data$H_interface), " cm")
    })
}

# Run the application
shinyApp(ui = ui, server = server)