# Install and load necessary packages
# install.packages(c("shiny", "ggplot2", "DT", "dplyr", "tidyr")) # tidyr for pivot_longer
library(shiny)
library(ggplot2)
library(DT)
library(dplyr) # For data manipulation
library(tidyr) # For pivot_longer

# Define the UI for the Shiny app
ui <- fluidPage(
    # Application title
    titlePanel(div(style = "color: #2c3e50; text-align: center; font-weight: bold;",
                   "Bagnall et al. (2022) Pedotransfer Functions for Plant Available Water")),
    
    # Sidebar layout with input and output sections
    sidebarLayout(
        sidebarPanel(
            style = "background-color: #ecf0f1; padding: 20px; border-radius: 8px;",
            h4(style = "color: #34495e;", "Soil Properties Input"),
            
            tabsetPanel(
                id = "input_mode",
                tabPanel("Single Sample",
                         br(),
                         # Input for Soil Type
                         radioButtons("soil_type", "Select Soil Type:",
                                      choices = c("Non-Calcareous" = "non_calcareous",
                                                  "Calcareous" = "calcareous"),
                                      selected = "non_calcareous"),
                         
                         # Numeric input for Sand content (%)
                         numericInput("sand_content", "Sand Content (%):",
                                      value = 30, min = 0, max = 100),
                         
                         # Numeric input for Clay content (%)
                         numericInput("clay_content", "Clay Content (%):",
                                      value = 20, min = 0, max = 100),
                         
                         # Numeric input for Soil Organic Carbon (SOC) content (g/kg)
                         numericInput("soc_content", "Soil Organic Carbon (SOC) (g/kg):",
                                      value = 10, min = 0),
                         
                         # Action button to trigger single sample calculations
                         actionButton("calculate_single", "Calculate AWHC for Single Sample",
                                      class = "btn-primary",
                                      style = "background-color: #3498db; border-color: #3498db; color: white; width: 100%; padding: 10px; font-size: 16px; border-radius: 5px;")
                ),
                tabPanel("Batch Calculation (CSV Upload)",
                         br(),
                         # File input for CSV
                         fileInput("csv_file", "Upload CSV File:",
                                   multiple = FALSE,
                                   accept = c("text/csv",
                                              "text/comma-separated-values,text/plain",
                                              ".csv")),
                         tags$hr(),
                         p("Expected CSV columns: 'Sand' (%), 'Clay' (%), 'SOC' (g/kg), 'SoilType' ('Non-Calcareous' or 'Calcareous')."),
                         uiOutput("download_button_ui"), # Placeholder for conditional download button
                         textOutput("file_upload_message") # Message for file upload status
                )
            )
        ),
        
        # Main panel for displaying outputs
        mainPanel(
            style = "background-color: #ffffff; padding: 20px; border-radius: 8px; box-shadow: 0 4px 8px rgba(0,0,0,0.1);",
            h4(style = "color: #34495e;", "Calculated Water Contents"),
            tabsetPanel(
                tabPanel("Results",
                         br(),
                         # Display results for single or batch calculation
                         verbatimTextOutput("results"),
                         DTOutput("batch_results_table"), # Table for batch results
                         h4(style = "color: #34495e;", "About the Pedotransfer Functions"),
                         p("The pedotransfer functions used in this app are derived from the article:"),
                         tags$blockquote(
                             tags$em("Bagnall, D. K., Morgan, C. L. S., Cope, M., Bean, G. M., Cappellazzi, S., Greub, K., ... & Honeycutt, C. W. (2022). Carbon-sensitive pedotransfer functions for plant available water. Soil Science Society of America Journal, 86(3), 612-629."),
                             tags$br(),
                             tags$a(href = "https://doi.org/10.1002/saj2.20395", "DOI: 10.1002/saj2.20395", target = "_blank")
                         )
                ),
                tabPanel("Visualizations", # New tab for plots
                         br(),
                         h4(style = "color: #34495e;", "Single Sample Water Content Plot"),
                         plotOutput("single_sample_plot", height = "350px"),
                         tags$hr(),
                         h4(style = "color: #34495e;", "Batch Samples Plant Available Water Holding Capacity (AWHC) Plot"),
                         plotOutput("batch_awhc_plot", height = "450px"),
                         tags$hr(),
                         h4(style = "color: #34495e;", "Batch Samples Field Capacity (FC) and Permanent Wilting Point (PWP) Plot"),
                         plotOutput("batch_fc_pwp_plot", height = "450px")
                ),
                tabPanel("Input Summary",
                         br(),
                         DTOutput("input_summary_table")
                )
            )
        )
    )
)

# Define the server logic for the Shiny app
server <- function(input, output, session) {
    
    # Function to perform calculation for a single row of data
    calculate_awhc <- function(sand, clay, soc, soil_type) {
        # Input validation: Ensure sand + clay <= 100 and other numeric checks
        if (is.na(sand) || is.na(clay) || is.na(soc) || is.null(soil_type) ||
            !is.numeric(sand) || !is.numeric(clay) || !is.numeric(soc) ||
            sand < 0 || clay < 0 || soc < 0 || sand > 100 || clay > 100) {
            return(list(theta_pwp = NA, theta_fc = NA, theta_awhc = NA, error_msg = "Invalid input values."))
        }
        
        if (sand + clay > 100) {
            return(list(theta_pwp = NA, theta_fc = NA, theta_awhc = NA, error_msg = "Sand + Clay > 100%."))
        }
        
        # Initialize variables for calculated water contents
        theta_pwp <- NA
        theta_fc <- NA
        theta_awhc <- NA
        error_msg <- NULL
        
        # Apply the appropriate pedotransfer function based on soil type
        if (tolower(soil_type) == "non-calcareous" || tolower(soil_type) == "non_calcareous") {
            # Non-calcareous soil equations (Equations 1 and 2 from the paper)
            # All units are in 10 g kg^-1 for SOC in the original equations
            # The input `soc` is in g/kg, so we divide by 10 for the equation
            soc_eq_unit <- soc / 10
            
            # Equation 1: theta_PWP for non-calcareous soils
            theta_pwp <- 7.222 + 0.296 * clay - 0.074 * sand - 0.309 * soc_eq_unit +
                0.022 * (sand * soc_eq_unit) + 0.022 * (clay * soc_eq_unit)
            
            # Equation 2: theta_FC for non-calcareous soils
            theta_fc <- 37.217 - 0.140 * clay - 0.304 * sand - 0.22 * soc_eq_unit +
                0.051 * (sand * soc_eq_unit) + 0.085 * (clay * soc_eq_unit) +
                0.002 * (clay * sand)
            
        } else if (tolower(soil_type) == "calcareous") {
            # Calcareous soil equations (Equations 3 and 4 from the paper)
            # All units are in 10 g kg^-1 for SOC in the original equations
            # The input `soc` is in g/kg, so we divide by 10 for the equation
            soc_eq_unit <- soc / 10
            
            # Equation 3: theta_PWP for calcareous soils
            theta_pwp <- 7.907 + 0.236 * clay - 0.082 * sand + 0.441 * soc_eq_unit +
                0.002 * (clay * sand)
            
            # Equation 4: theta_FC for calcareous soils
            theta_fc <- 33.351 + 0.020 * clay - 0.446 * sand + 1.398 * soc_eq_unit +
                0.052 * (sand * soc_eq_unit) - 0.077 * (clay * soc_eq_unit) +
                0.011 * (clay * sand)
        } else {
            error_msg <- "Invalid Soil Type. Must be 'Non-Calcareous' or 'Calcareous'."
        }
        
        # Calculate theta_AWHC if FC and PWP are valid
        if (!is.na(theta_fc) && !is.na(theta_pwp)) {
            theta_awhc <- theta_fc - theta_pwp
        } else {
            theta_awhc <- NA
        }
        
        # Return results as a list
        list(
            theta_pwp = theta_pwp,
            theta_fc = theta_fc,
            theta_awhc = theta_awhc,
            error_msg = error_msg
        )
    }
    
    
    # Reactive value to store single sample calculation results
    single_calculation_results <- eventReactive(input$calculate_single, {
        # Get input values
        sand <- input$sand_content
        clay <- input$clay_content
        soc <- input$soc_content
        soil_type <- input$soil_type
        
        # Call the calculation function
        results <- calculate_awhc(sand, clay, soc, soil_type)
        
        # Add input values to the results list
        results$sand <- sand
        results$clay <- clay
        results$soc <- soc
        results$soil_type <- ifelse(soil_type == "non_calcareous", "Non-Calcareous", "Calcareous")
        
        results
    })
    
    # Reactive value to read CSV file
    file_data <- reactive({
        req(input$csv_file) # Require a file input
        inFile <- input$csv_file
        tryCatch(
            {
                read.csv(inFile$datapath, header = TRUE, stringsAsFactors = FALSE)
            },
            error = function(e) {
                output$file_upload_message <- renderText({
                    paste("Error reading file:", e$message)
                })
                NULL
            }
        )
    })
    
    # Reactive value to store batch calculation results
    batch_calculation_results <- eventReactive(file_data(), {
        df <- file_data()
        if (is.null(df)) {
            return(NULL)
        }
        
        # Check for required columns
        required_cols <- c("Sand", "Clay", "SOC", "SoilType")
        if (!all(required_cols %in% names(df))) {
            output$file_upload_message <- renderText({
                paste("Error: Missing required columns in CSV. Expected:", paste(required_cols, collapse = ", "))
            })
            return(NULL)
        }
        
        # Initialize empty data frame for results
        results_df <- data.frame(
            SampleID = character(), # Added SampleID for plotting
            Sand = numeric(),
            Clay = numeric(),
            SOC = numeric(),
            SoilType = character(),
            theta_PWP = numeric(),
            theta_FC = numeric(),
            theta_AWHC = numeric(),
            Calculation_Error = character(),
            stringsAsFactors = FALSE
        )
        
        # Loop through each row and perform calculations
        for (i in 1:nrow(df)) {
            row_data <- df[i, ]
            sand_val <- row_data$Sand
            clay_val <- row_data$Clay
            soc_val <- row_data$SOC
            soil_type_val <- row_data$SoilType
            
            calc_res <- calculate_awhc(sand_val, clay_val, soc_val, soil_type_val)
            
            # Append results to the results_df
            results_df <- rbind(results_df, data.frame(
                SampleID = paste0("Sample_", i), # Assign a unique ID for plotting
                Sand = sand_val,
                Clay = clay_val,
                SOC = soc_val,
                SoilType = soil_type_val,
                theta_PWP = calc_res$theta_pwp,
                theta_FC = calc_res$theta_fc,
                theta_AWHC = calc_res$theta_awhc,
                Calculation_Error = ifelse(is.null(calc_res$error_msg), "", calc_res$error_msg),
                stringsAsFactors = FALSE
            ))
        }
        
        output$file_upload_message <- renderText({
            paste("CSV file processed successfully. Calculated", nrow(results_df), "samples.")
        })
        
        results_df
    })
    
    # Render the results (either single or batch)
    output$results <- renderPrint({
        if (input$input_mode == "Single Sample") {
            res <- single_calculation_results()
            if (!is.null(res$error_msg) && res$error_msg != "") {
                cat("Error: ", res$error_msg, "\n")
            } else if (!is.null(res$theta_awhc)) {
                cat("Calculated Volumetric Water Contents (mm/100mm soil):\n")
                cat("---------------------------------------------------\n")
                cat(sprintf("Volumetric Water Content at Permanent Wilting Point (θ_PWP): %.3f\n", res$theta_pwp))
                cat(sprintf("Volumetric Water Content at Field Capacity (θ_FC): %.3f\n", res$theta_fc))
                cat(sprintf("Plant Available Water Holding Capacity (θ_AWHC): %.3f\n", res$theta_awhc))
                cat("---------------------------------------------------\n")
                cat("Note: AWHC = θ_FC - θ_PWP\n")
            }
        } else {
            # Hide this output for batch mode
            cat("")
        }
    })
    
    # Render the batch results table
    output$batch_results_table <- renderDT({
        if (input$input_mode == "Batch Calculation (CSV Upload)") {
            df <- batch_calculation_results()
            if (!is.null(df)) {
                datatable(df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
            } else {
                NULL
            }
        } else {
            NULL # Hide table in single sample mode
        }
    }, escape = FALSE)
    
    # Render the input summary table
    output$input_summary_table <- renderDT({
        if (input$input_mode == "Single Sample") {
            res <- single_calculation_results()
            if (!is.null(res$error_msg) && res$error_msg != "") {
                data.frame(Message = res$error_msg)
            } else {
                data.frame(
                    Property = c("Soil Type", "Sand Content (%)", "Clay Content (%)", "SOC Content (g/kg)"),
                    Value = c(res$soil_type, res$sand, res$clay, res$soc)
                ) %>%
                    datatable(options = list(dom = 't', paging = FALSE, searching = FALSE), rownames = FALSE)
            }
        } else if (input$input_mode == "Batch Calculation (CSV Upload)") {
            df <- file_data()
            if (!is.null(df)) {
                data.frame(
                    Property = c("CSV Uploaded", "Number of Samples", "First 5 Rows Preview"),
                    Value = c(input$csv_file$name, nrow(df), paste(capture.output(print(head(df, 5))), collapse = "\n"))
                ) %>%
                    datatable(options = list(dom = 't', paging = FALSE, searching = FALSE), rownames = FALSE, escape = FALSE)
            } else {
                data.frame(Message = "No CSV file uploaded yet.") %>%
                    datatable(options = list(dom = 't', paging = FALSE, searching = FALSE), rownames = FALSE)
            }
        }
    }, escape = FALSE)
    
    
    # Render the download button only when batch results are available
    output$download_button_ui <- renderUI({
        if (!is.null(batch_calculation_results())) {
            downloadButton("download_results_csv", "Download Calculated Results CSV",
                           style = "background-color: #28a745; border-color: #28a745; color: white; width: 100%; padding: 10px; font-size: 16px; border-radius: 5px;")
        }
    })
    
    # Download handler for batch results
    output$download_results_csv <- downloadHandler(
        filename = function() {
            paste("pedotransfer_results-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(batch_calculation_results(), file, row.names = FALSE)
        }
    )
    
    # Plot for single sample results
    output$single_sample_plot <- renderPlot({
        res <- single_calculation_results()
        # Check for valid results and no errors
        if (!is.null(res) && !is.null(res$theta_awhc) && !is.na(res$theta_awhc) && (is.null(res$error_msg) || res$error_msg == "")) {
            plot_data <- data.frame(
                Category = factor(c("θ_PWP", "θ_FC", "θ_AWHC"), levels = c("θ_PWP", "θ_FC", "θ_AWHC")), # Ensure order
                Value = c(res$theta_pwp, res$theta_fc, res$theta_awhc)
            )
            
            ggplot(plot_data, aes(x = Category, y = Value, fill = Category)) +
                geom_bar(stat = "identity", width = 0.6) +
                geom_text(aes(label = sprintf("%.2f", Value)), vjust = -0.5, size = 4) + # Add value labels
                scale_fill_manual(values = c("θ_PWP" = "#f39c12", "θ_FC" = "#2ecc71", "θ_AWHC" = "#3498db")) +
                labs(
                    title = paste("Water Contents for", res$soil_type, "Soil (Sand:", res$sand, "%, Clay:", res$clay, "%, SOC:", res$soc, "g/kg)"),
                    x = "Water Content Type",
                    y = "Volumetric Water Content (mm/100mm soil)",
                    fill = "Category"
                ) +
                theme_minimal() +
                theme(
                    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
                    axis.title = element_text(size = 12),
                    axis.text = element_text(size = 10),
                    legend.position = "none" # Hide legend as categories are on x-axis
                )
        } else if (!is.null(res) && !is.null(res$error_msg) && res$error_msg != "") {
            # Display an error message if calculation failed
            ggplot() +
                annotate("text", x = 0.5, y = 0.5, label = paste("Error generating plot for single sample:", res$error_msg),
                         color = "red", size = 5) +
                theme_void()
        } else {
            # Inform the user to calculate
            ggplot() +
                annotate("text", x = 0.5, y = 0.5, label = "Click 'Calculate AWHC for Single Sample' to view plot.",
                         color = "gray", size = 5) +
                theme_void()
        }
    })
    
    # Plot for batch calculation AWHC results
    output$batch_awhc_plot <- renderPlot({
        df <- batch_calculation_results()
        if (!is.null(df) && nrow(df) > 0) {
            # Filter out rows with calculation errors for plotting
            plot_data <- df %>%
                filter(is.na(Calculation_Error) | Calculation_Error == "") %>%
                mutate(SampleID_Plot = paste(SampleID, "-", SoilType)) # Combine SampleID and SoilType for better labels
            
            if (nrow(plot_data) == 0) {
                ggplot() +
                    annotate("text", x = 0.5, y = 0.5, label = "No valid data to plot for AWHC from batch calculation. Check for errors.",
                             color = "red", size = 5) +
                    theme_void()
            } else {
                ggplot(plot_data, aes(x = reorder(SampleID_Plot, theta_AWHC), y = theta_AWHC, fill = SoilType)) + # Order by AWHC
                    geom_bar(stat = "identity", position = "dodge") +
                    geom_text(aes(label = sprintf("%.2f", theta_AWHC)), vjust = -0.5, position = position_dodge(width = 0.9), size = 3) +
                    scale_fill_manual(values = c("Non-Calcareous" = "#3498db", "Calcareous" = "#e74c3c")) +
                    labs(
                        title = "Plant Available Water Holding Capacity (AWHC) for Batch Samples",
                        x = "Sample ID and Soil Type",
                        y = "AWHC (mm/100mm soil)",
                        fill = "Soil Type"
                    ) +
                    theme_minimal() +
                    theme(
                        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
                        axis.title = element_text(size = 12),
                        axis.text.x = element_text(angle = 45, hjust = 1, size = 9), # Rotate labels for readability
                        axis.text.y = element_text(size = 10),
                        legend.position = "bottom"
                    )
            }
        } else {
            ggplot() +
                annotate("text", x = 0.5, y = 0.5, label = "Upload a CSV file in 'Batch Calculation' tab to view AWHC plot.",
                         color = "gray", size = 5) +
                theme_void()
        }
    })
    
    # Plot for batch calculation FC and PWP results
    output$batch_fc_pwp_plot <- renderPlot({
        df <- batch_calculation_results()
        if (!is.null(df) && nrow(df) > 0) {
            # Filter out rows with calculation errors for plotting
            plot_data <- df %>%
                filter(is.na(Calculation_Error) | Calculation_Error == "") %>%
                select(SampleID, SoilType, theta_FC, theta_PWP) %>%
                # Convert to long format for plotting FC and PWP side-by-side
                pivot_longer(
                    cols = c(theta_FC, theta_PWP),
                    names_to = "Water_Content_Type",
                    values_to = "Value"
                ) %>%
                mutate(
                    Water_Content_Type = factor(Water_Content_Type, levels = c("theta_PWP", "theta_FC")), # Ensure order
                    SampleID_Plot = paste(SampleID, "-", SoilType) # Combine SampleID and SoilType for better labels
                )
            
            if (nrow(plot_data) == 0) {
                ggplot() +
                    annotate("text", x = 0.5, y = 0.5, label = "No valid data to plot for FC/PWP from batch calculation. Check for errors.",
                             color = "red", size = 5) +
                    theme_void()
            } else {
                ggplot(plot_data, aes(x = SampleID_Plot, y = Value, fill = Water_Content_Type)) +
                    geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
                    geom_text(aes(label = sprintf("%.2f", Value)),
                              position = position_dodge(width = 0.8), vjust = -0.5, size = 3) +
                    scale_fill_manual(values = c("theta_PWP" = "#f39c12", "theta_FC" = "#2ecc71"),
                                      labels = c("theta_PWP" = "Permanent Wilting Point (θ_PWP)",
                                                 "theta_FC" = "Field Capacity (θ_FC)")) +
                    labs(
                        title = "Field Capacity (FC) and Permanent Wilting Point (PWP) for Batch Samples",
                        x = "Sample ID and Soil Type",
                        y = "Volumetric Water Content (mm/100mm soil)",
                        fill = "Water Content Type"
                    ) +
                    theme_minimal() +
                    theme(
                        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
                        axis.title = element_text(size = 12),
                        axis.text.x = element_text(angle = 45, hjust = 1, size = 9), # Rotate labels for readability
                        axis.text.y = element_text(size = 10),
                        legend.position = "bottom"
                    )
            }
        } else {
            ggplot() +
                annotate("text", x = 0.5, y = 0.5, label = "Upload a CSV file in 'Batch Calculation' tab to view FC/PWP plot.",
                         color = "gray", size = 5) +
                theme_void()
        }
    })
    
}

# Run the Shiny app
shinyApp(ui = ui, server = server)