library(shiny)
library(ggplot2)
library(shinythemes)
library(dplyr)

# --- 1. THE CORE DEGRADATION ENGINE ---
# This function calculates the physical failure risk based on your research parameters
calc_degradation <- function(was, di, fi, bd, vwc, slope, rain, strategy) {
    # Structural Resistance (Physical + Chemical)
    # High WAS and FI act as buffers; DI is the primary driver of failure
    structural_resistance <- (was / 100) * (fi + 0.1)
    chemical_vulnerability <- di + 0.01
    
    # The 'Stability Gap' 
    stability_gap <- chemical_vulnerability / (structural_resistance + 0.05)
    
    # Physical Stressors
    porosity_stress <- (bd / 1.5) * (1 + vwc/100)
    energy_stress <- (rain / 80) * (slope / 15)
    
    # Mitigation Factor logic
    mitigation <- switch(strategy,
                         "None" = 1.0,
                         "Gypsum (Chemical)" = 0.65, 
                         "Cover Crops (Bio)" = 0.55, 
                         "Biochar (Physical)" = 0.75)
    
    # Final Risk Score (Scaled 0-100)
    risk <- (stability_gap * porosity_stress * energy_stress * 20) * mitigation
    return(pmin(risk, 100))
}

# --- 2. THE USER INTERFACE ---
ui <- fluidPage(
    theme = shinytheme("flatly"),
    titlePanel("Soil Physical Degradation Explorer"),
    
    sidebarLayout(
        sidebarPanel(
            tabsetPanel(id = "sidebar_tabs",
                        tabPanel("Manual Scenario",
                                 br(),
                                 h4("Soil Lab Data"),
                                 sliderInput("was", "Wet Aggregate Stability (%):", 0, 100, 60),
                                 sliderInput("di", "Dispersion Index (DI):", 0, 1, 0.3),
                                 sliderInput("fi", "Flocculation Index (FI):", 0, 1, 0.7),
                                 numericInput("bd", "Bulk Density (g/cm³):", 1.3, step = 0.1)
                        ),
                        tabPanel("Bulk Upload",
                                 br(),
                                 h4("Multi-Sample Analysis"),
                                 fileInput("bulk_csv", "Upload Lab CSV", accept = ".csv"),
                                 helpText("Requires: SampleID, WAS, DI, FI, BD")
                        )
            ),
            hr(),
            h4("Environmental Stressors"),
            sliderInput("rain", "Rain Intensity (mm/hr):", 0, 150, 60),
            sliderInput("slope", "Slope Gradient (%):", 0, 50, 10),
            sliderInput("vwc", "Initial Water (vol%):", 0, 50, 20),
            selectInput("strategy", "Rehabilitation Strategy:", 
                        choices = c("None", "Gypsum (Chemical)", "Cover Crops (Bio)", "Biochar (Physical)")),
            hr(),
            downloadButton("download_report", "Download Results", class = "btn-success")
        ),
        
        mainPanel(
            tabsetPanel(id = "main_tabs",
                        tabPanel("Analysis & Prediction",
                                 br(),
                                 wellPanel(
                                     h3(textOutput("risk_status")),
                                     plotOutput("gauge_plot", height = "80px")
                                 ),
                                 plotOutput("sensitivity_plot")
                        ),
                        tabPanel("Comparison View",
                                 br(),
                                 h4("Comparative Degradation Risk"),
                                 plotOutput("comparison_plot"),
                                 hr(),
                                 h4("Data Table"),
                                 tableOutput("comparison_table")
                        )
            )
        )
    )
)

# --- 3. THE SERVER ---
server <- function(input, output, session) {
    
    # Reactive Calculation for the Manual Scenario
    risk_val <- reactive({
        calc_degradation(input$was, input$di, input$fi, input$bd, 
                         input$vwc, input$slope, input$rain, input$strategy)
    })
    
    # Reactive logic for Bulk Upload with Data Cleaning
    bulk_data <- reactive({
        req(input$bulk_csv)
        df <- read.csv(input$bulk_csv$datapath)
        
        # Normalize column names (remove dots, spaces, and make uppercase for matching)
        colnames(df) <- toupper(gsub("[[:punct:]]| ", "", colnames(df)))
        
        # Validation: Check if the cleaned columns exist
        validate(
            need(all(c("SAMPLEID", "WAS", "DI", "FI", "BD") %in% colnames(df)), 
                 "Column mismatch! Please ensure your CSV has: SampleID, WAS, DI, FI, BD")
        )
        
        # Run the engine across all rows
        # Note: We use the cleaned column names here
        df$Degradation_Risk <- mapply(calc_degradation, 
                                      df$WAS, df$DI, df$FI, df$BD, 
                                      MoreArgs = list(vwc = input$vwc, slope = input$slope, 
                                                      rain = input$rain, strategy = input$strategy))
        return(df)
    })
    
    # --- OUTPUTS: SCENARIO TAB ---
    output$risk_status <- renderText({
        r <- risk_val()
        status <- if(r < 35) "STRUCTURAL HEALTHY" else if(r < 70) "DEGRADATION WARNING" else "STRUCTURAL COLLAPSE"
        paste0(status, " (", round(r, 1), "%)")
    })
    
    output$gauge_plot <- renderPlot({
        ggplot() + 
            geom_rect(aes(xmin=0, xmax=100, ymin=0, ymax=1), fill="#ecf0f1") +
            geom_rect(aes(xmin=0, xmax=risk_val(), ymin=0, ymax=1), 
                      fill=ifelse(risk_val() > 70, "#e74c3c", "#3498db")) +
            theme_void()
    })
    
    output$sensitivity_plot <- renderPlot({
        was_seq <- seq(0, 100, length.out = 50)
        res <- sapply(was_seq, function(w) calc_degradation(w, input$di, input$fi, 
                                                            input$bd, input$vwc, 
                                                            input$slope, input$rain, input$strategy))
        ggplot(data.frame(WAS=was_seq, Risk=res), aes(WAS, Risk)) +
            geom_line(size=1.5, color="#2c3e50") +
            geom_area(fill="#3498db", alpha=0.2) +
            geom_vline(xintercept = input$was, linetype="dashed", color="red") +
            labs(title = "Stability vs. Degradation Threshold", x="Wet Aggregate Stability (%)", y="Risk Index (%)") +
            theme_minimal()
    })
    
    # --- OUTPUTS: COMPARISON TAB ---
    output$comparison_plot <- renderPlot({
        req(bulk_data())
        ggplot(bulk_data(), aes(x = reorder(SAMPLEID, Degradation_Risk), y = Degradation_Risk, fill = Degradation_Risk)) +
            geom_bar(stat = "identity") +
            coord_flip() +
            scale_fill_gradient(low = "#2ecc71", high = "#e74c3c") +
            labs(x = "Sample", y = "Degradation Risk (%)", title = "Site Comparison") +
            theme_minimal()
    })
    
    output$comparison_table <- renderTable({ req(bulk_data()); bulk_data() })
    
    # --- OUTPUTS: REPORTING ---
    output$download_report <- downloadHandler(
        filename = function() { paste("Soil-Analysis-", Sys.Date(), ".csv", sep="") },
        content = function(file) {
            report_df <- data.frame(
                Sample = "Manual_Scenario",
                WAS = input$was, DI = input$di, FI = input$fi, BD = input$bd,
                Rain = input$rain, Slope = input$slope, Strategy = input$strategy,
                Final_Risk = round(risk_val(), 2)
            )
            write.csv(report_df, file, row.names = FALSE)
        }
    )
}

shinyApp(ui, server)