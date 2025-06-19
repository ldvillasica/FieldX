# Install necessary packages if you haven't already
# install.packages(c("shiny", "ggplot2", "plotly", "dplyr", "readr"))

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr) # For data manipulation
library(readr) # For read_csv if you prefer over base R read.csv

# Define UI
ui <- fluidPage(
    titlePanel("Basic Soil Water Balance Simulator"),
    sidebarLayout(
        sidebarPanel(
            h4("Soil Properties"),
            selectInput("soil_type", "Select Soil Type:",
                        choices = c("Sand", "Loamy Sand", "Sandy Loam", "Loam",
                                    "Silt Loam", "Clay Loam", "Clay"),
                        selected = "Loamy Sand"),
            numericInput("rooting_depth", "Rooting Depth (cm):", value = 50, min = 10, max = 200, step = 5),
            sliderInput("initial_swc_percent", "Initial SWC (% of Field Capacity):",
                        min = 0, max = 100, value = 80, step = 5),
            
            h4("Meteorological Data"),
            fileInput("met_file", "Upload Daily Met Data (CSV):",
                      accept = c(".csv", ".txt")),
            helpText("CSV must have columns: 'Date' (YYYY-MM-DD), 'Precipitation_mm', 'ET_mm'"),
            
            hr(),
            actionButton("run_simulation", "Run Simulation", class = "btn-primary")
        ),
        mainPanel(
            h3("Simulated Soil Water Content Over Time"),
            plotlyOutput("swc_time_series_plot", height = "500px"),
            hr(),
            h3("Simulation Summary"),
            verbatimTextOutput("summary_output")
        )
    )
)

# Define server logic
server <- function(input, output, session) {
    
    # Reactive values for soil properties (FC, PWP, SWC_sat in mm for the profile)
    soil_params <- reactive({
        req(input$soil_type, input$rooting_depth)
        
        # Lookup table for typical volumetric soil water content percentages
        # Source: These are generalized values and should be treated as illustrative.
        # Real-world applications would use site-specific measurements or pedotransfer functions.
        soil_props_lookup <- tribble(
            ~soil_type, ~fc_vol_perc, ~pwp_vol_perc, ~swc_sat_vol_perc,
            "Sand",          10,        5,       40,
            "Loamy Sand",    15,        7,       42,
            "Sandy Loam",    20,       10,       45,
            "Loam",          25,       12,       48,
            "Silt Loam",     28,       14,       50,
            "Clay Loam",     30,       16,       52,
            "Clay",          35,       20,       55
        )
        
        selected_props <- soil_props_lookup %>%
            filter(soil_type == input$soil_type)
        
        root_depth_mm <- input$rooting_depth * 10 # Convert cm to mm
        
        list(
            FC_mm = (selected_props$fc_vol_perc / 100) * root_depth_mm,
            PWP_mm = (selected_props$pwp_vol_perc / 100) * root_depth_mm,
            SWC_sat_mm = (selected_props$swc_sat_vol_perc / 100) * root_depth_mm
        )
    })
    
    # Reactive for uploaded meteorological data
    met_data <- reactive({
        req(input$met_file) # Require file input
        inFile <- input$met_file
        if (is.null(inFile)) return(NULL)
        
        df <- read_csv(inFile$datapath) # Using read_csv from readr for robustness
        
        # Ensure Date column is in Date format
        df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
        
        # Basic validation: Check for expected columns
        if (!all(c("Date", "Precipitation_mm", "ET_mm") %in% names(df))) {
            stop("Uploaded file must contain 'Date', 'Precipitation_mm', and 'ET_mm' columns.")
        }
        df
    })
    
    # Reactive for initial soil water content (converted to mm)
    initial_swc_mm <- reactive({
        req(input$initial_swc_percent, input$rooting_depth)
        params <- soil_params()
        # Initial SWC is a percentage of the *available water capacity* (FC - PWP),
        # then added to PWP, for a more realistic initial state.
        # Or, as in the example above, a percentage of FC. Let's stick to % of FC for simplicity as initially suggested.
        # This means 100% initial SWC = Field Capacity value.
        (input$initial_swc_percent / 100) * params$FC_mm
    })
    
    # --- Water Balance Simulation Logic ---
    simulated_data <- eventReactive(input$run_simulation, {
        req(met_data(), soil_params(), initial_swc_mm())
        
        daily_met <- met_data()
        params <- soil_params()
        current_swc_mm <- initial_swc_mm()
        
        # Data frame to store results
        results_df <- data.frame(
            Date = as.Date(character()),
            Daily_SWC_mm = numeric(),
            Daily_Precip_mm = numeric(),
            Daily_ET_mm = numeric(),
            Daily_Runoff_mm = numeric(),
            Daily_DeepPerc_mm = numeric(),
            stringsAsFactors = FALSE
        )
        
        for (i in 1:nrow(daily_met)) {
            date <- daily_met$Date[i]
            P_t <- daily_met$Precipitation_mm[i]
            ET_t <- daily_met$ET_mm[i]
            
            # 1. Add incoming water
            swc_after_inflow <- current_swc_mm + P_t
            
            # 2. Calculate Runoff
            RO_t <- 0
            if (swc_after_inflow > params$SWC_sat_mm) {
                RO_t <- swc_after_inflow - params$SWC_sat_mm
                swc_after_inflow <- params$SWC_sat_mm # Cap at saturation
            }
            
            # 3. Apply ET (Actual ET)
            # A simple approach: ET occurs if water is available
            actual_ET_t <- min(ET_t, swc_after_inflow)
            swc_after_et <- swc_after_inflow - actual_ET_t
            
            # 4. Calculate Deep Percolation
            DP_t <- 0
            if (swc_after_et > params$FC_mm) {
                DP_t <- swc_after_et - params$FC_mm
                swc_after_et <- params$FC_mm # Cap at field capacity (water that stays within root zone)
            }
            
            # 5. Final SWC for the day (cannot go below 0)
            current_swc_mm <- max(0, swc_after_et)
            
            # Store results for the current day
            results_df[i, ] <- c(
                Date = as.character(date), # Convert Date to character for data.frame row binding
                Daily_SWC_mm = current_swc_mm,
                Daily_Precip_mm = P_t,
                Daily_ET_mm = actual_ET_t, # Use actual ET
                Daily_Runoff_mm = RO_t,
                Daily_DeepPerc_mm = DP_t
            )
        }
        # Convert Date column back to Date type after loop
        results_df$Date <- as.Date(results_df$Date)
        # Convert numeric columns
        results_df <- results_df %>%
            mutate(across(c(Daily_SWC_mm, Daily_Precip_mm, Daily_ET_mm, Daily_Runoff_mm, Daily_DeepPerc_mm), as.numeric))
        
        results_df
    }, ignoreNULL = FALSE) # run initially with default values if any
    
    
    # Render the Plotly Time Series Plot
    output$swc_time_series_plot <- renderPlotly({
        req(simulated_data()) # Ensure simulation results are available
        params <- soil_params() # Get soil parameters for reference lines
        plot_df <- simulated_data()
        
        # Create the ggplot
        p <- ggplot(plot_df, aes(x = Date, y = Daily_SWC_mm)) +
            geom_line(aes(text = paste("Date:", Date, "<br>SWC:", round(Daily_SWC_mm, 2), "mm<br>Precip:", Daily_Precip_mm, "mm<br>ET:", Daily_ET_mm, "mm")),
                      color = "#1f77b4", size = 0.8) + # Blue line for SWC with hover text
            geom_hline(aes(yintercept = params$FC_mm, linetype = "Field Capacity"), color = "darkgreen", size = 0.9) +
            geom_hline(aes(yintercept = params$PWP_mm, linetype = "Permanent Wilting Point"), color = "red", size = 0.9) +
            scale_linetype_manual(name = "Threshold", values = c("Field Capacity" = "dashed", "Permanent Wilting Point" = "dotted")) +
            labs(
                title = paste("Simulated Soil Water Content for", input$soil_type),
                x = "Date",
                y = paste0("Soil Water Content (mm in ", input$rooting_depth, " cm profile)")
            ) +
            theme_minimal() +
            theme(legend.position = "bottom")
        
        # Convert to plotly for interactivity
        ggplotly(p, tooltip = "text") %>% # Use "text" to show our custom hover text
            layout(hovermode = "x unified") # Nice hover effect for time series, showing multiple traces if added
    })
    
    # Optional: Summary output
    output$summary_output <- renderPrint({
        req(simulated_data())
        df <- simulated_data()
        params <- soil_params() # Get soil params for PWP comparison
        
        total_precip <- sum(df$Daily_Precip_mm)
        total_et <- sum(df$Daily_ET_mm)
        total_runoff <- sum(df$Daily_Runoff_mm)
        total_deep_perc <- sum(df$Daily_DeepPerc_mm)
        # Count days where SWC is strictly below PWP
        days_below_pwp <- sum(df$Daily_SWC_mm < params$PWP_mm)
        
        cat("--- Simulation Summary ---\n")
        cat(sprintf("Simulation Period: %s to %s\n", min(df$Date), max(df$Date)))
        cat(sprintf("Total Precipitation during period: %.2f mm\n", total_precip))
        cat(sprintf("Total Actual Evapotranspiration during period: %.2f mm\n", total_et))
        cat(sprintf("Total Runoff during period: %.2f mm\n", total_runoff))
        cat(sprintf("Total Deep Percolation during period: %.2f mm\n", total_deep_perc))
        cat(sprintf("Number of days soil water content was below Permanent Wilting Point: %d days\n", days_below_pwp))
        cat(sprintf("Final Soil Water Content: %.2f mm\n", df$Daily_SWC_mm[nrow(df)]))
    })
    
}

# Run the application
shinyApp(ui = ui, server = server)