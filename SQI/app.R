# Make sure you have these packages installed:
# install.packages(c("shiny", "shinydashboard", "readr", "dplyr", "FactoMineR", "ggplot2", "DT", "forcats", "tibble", "tidyr", "factoextra"))

library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(FactoMineR)
library(ggplot2)
library(DT)
library(forcats)
library(tibble)
library(tidyr)
library(factoextra)

# --- Helper Functions based on the paper's specific NL forms ---

# Non-linear "More is Better" sigmoid scoring function (Type I from paper)
score_more_is_better <- function(x, x0, b = 2.5) {
    1 / (1 + exp(-b * (x - x0)))
}

# Non-linear "Less is Better" sigmoid scoring function (Type II from paper)
score_less_is_better <- function(x, x0, b = 2.5) {
    1 / (1 + exp(b * (x - x0)))
}

# Non-linear "Optimum Range" (Type III from paper - Gaussian-like for illustration)
score_optimum_range <- function(x, opt_val, width) {
    exp(-((x - opt_val)^2) / (2 * width^2))
}


# --- UI Definition (remains unchanged for this modification) ---
ui <- dashboardPage(
    dashboardHeader(title = "Soil Quality Index (SQI) App - PCA & Non-Linear Scoring"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Data Upload", tabName = "data_upload", icon = icon("upload")),
            menuItem("PCA & MDS Selection", tabName = "pca_mds", icon = icon("chart-bar")),
            menuItem("Scoring & SQI Calculation", tabName = "scoring_sqi", icon = icon("calculator")),
            menuItem("Results", tabName = "results", icon = icon("table"))
        )
    ),
    dashboardBody(
        tabItems(
            # Data Upload Tab
            tabItem(tabName = "data_upload",
                    h2("Upload Your Soil Data"),
                    fluidRow(
                        box(
                            title = "Instructions", status = "primary", solidHeader = TRUE,
                            width = 12,
                            p("1. Upload a CSV file with your soil property data."),
                            p("2. Each column should represent a soil property (e.g., pH, Organic Carbon, Bulk Density)."),
                            p("3. Each row should represent a soil sample."),
                            p("4. Ensure column names are unique and descriptive.")
                        ),
                        box(
                            title = "File Input", status = "info", solidHeader = TRUE,
                            width = 6,
                            fileInput("file1", "Choose CSV File",
                                      multiple = FALSE,
                                      accept = c("text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv")),
                            checkboxInput("header", "Header", TRUE),
                            radioButtons("sep", "Separator",
                                         choices = c(Comma = ",",
                                                     Semicolon = ";",
                                                     Tab = "\t"),
                                         selected = ",")
                        ),
                        box(
                            title = "Uploaded Data Preview", status = "info", solidHeader = TRUE,
                            width = 6,
                            DTOutput("data_preview")
                        )
                    )
            ),
            
            # PCA & MDS Selection Tab
            tabItem(tabName = "pca_mds",
                    h2("Principal Component Analysis & Minimum Data Set (MDS) Selection"),
                    fluidRow(
                        box(
                            title = "PCA Configuration", status = "primary", solidHeader = TRUE,
                            width = 4,
                            p("Click 'Run PCA' to analyze your data and identify significant components."),
                            actionButton("run_pca", "Run PCA", icon = icon("play")),
                            hr(),
                            h4("MDS Indicator Selection"),
                            p("Select MDS indicators from the variables with high loadings on PCs with Eigenvalue ≥ 1. The app suggests variables with the highest absolute loading per PC."),
                            uiOutput("mds_selection_ui") # Dynamic UI for MDS selection
                        ),
                        box(
                            title = "Scree Plot (Eigenvalues of Principal Components)", status = "info", solidHeader = TRUE,
                            width = 8,
                            plotOutput("scree_plot")
                        ),
                        box(
                            title = "PCA Loadings Table (Variables and their contributions to PCs)", status = "info", solidHeader = TRUE,
                            width = 12,
                            p("PCs with Eigenvalue ≥ 1 are considered significant. Identify variables with high absolute loadings within these PCs."),
                            DTOutput("pca_loadings_table")
                        )
                    )
            ),
            
            # Scoring & SQI Tab
            tabItem(tabName = "scoring_sqi",
                    h2("Define Non-Linear Scoring Functions and Calculate SQI"),
                    fluidRow(
                        box(
                            title = "Scoring Parameters for MDS Indicators", status = "primary", solidHeader = TRUE,
                            width = 12,
                            p("For each selected MDS indicator, choose a scoring function type and define its parameters."),
                            uiOutput("scoring_params_ui"), # Dynamic UI for scoring parameters
                            hr(),
                            actionButton("calculate_sqi", "Calculate Soil Quality Index", icon = icon("cogs"))
                        )
                    )
            ),
            
            # Results Tab
            tabItem(tabName = "results",
                    h2("Soil Quality Index Results"),
                    fluidRow(
                        box(
                            title = "Calculated SQI Values and Individual Scores", status = "success", solidHeader = TRUE,
                            width = 12,
                            DTOutput("sqi_results_table"),
                            downloadButton("download_sqi", "Download SQI Results")
                        ),
                        box(
                            title = "SQI Distribution", status = "success", solidHeader = TRUE,
                            width = 6,
                            plotOutput("sqi_histogram_plot")
                        ),
                        box(
                            title = "MDS Weights Used", status = "success", solidHeader = TRUE,
                            width = 6,
                            DTOutput("mds_weights_table")
                        )
                    ),
                    fluidRow( # New row for the grouping variable selection and bar graph
                        box(
                            title = "Grouped SQI Bar Graph", status = "info", solidHeader = TRUE,
                            width = 12,
                            uiOutput("grouping_variable_selector_ui"), # Dropdown for grouping variable
                            plotOutput("sqi_group_bar_plot")
                        )
                    )
            )
        )
    )
)

# --- Server Logic ---
server <- function(input, output, session) {
    
    # Reactive for uploaded data
    data_raw <- reactive({
        req(input$file1)
        df <- read.csv(input$file1$datapath, header = input$header, sep = input$sep) # Corrected read.csv
        
        # Check if there are any numeric columns for PCA
        df_numeric_check <- df %>% select(where(is.numeric))
        if(ncol(df_numeric_check) == 0) {
            showNotification("No numeric columns found in the uploaded file for PCA. Please check your data.", type = "error")
            return(NULL)
        }
        df # Return original for later merging with scores
    })
    
    # Display data preview
    output$data_preview <- renderDT({
        data_raw()
    }, options = list(pageLength = 5))
    
    # Reactive for normalized data (for PCA)
    normalized_data <- eventReactive(input$run_pca, {
        req(data_raw())
        df_numeric <- data_raw() %>% select(where(is.numeric))
        if(ncol(df_numeric) == 0) return(NULL) # Redundant check, but safe
        scale(df_numeric, center = TRUE, scale = TRUE) # Z-score normalization
    })
    
    # Reactive for PCA results
    pca_results <- eventReactive(input$run_pca, {
        req(normalized_data())
        PCA(normalized_data(), graph = FALSE)
    })
    
    # Filtered PCA loadings (only for PCs with eigenvalue >= 1)
    pca_loadings_filtered <- reactive({
        req(pca_results())
        res_pca <- pca_results()
        eigenvalues <- res_pca$eig[, "eigenvalue"]
        # Select PCs with eigenvalue >= 1
        selected_pc_indices <- which(eigenvalues >= 1)
        
        if (length(selected_pc_indices) == 0) {
            return(data.frame(Variable = character(0), PC = character(0), Loading = numeric(0), stringsAsFactors = FALSE))
        }
        
        # Extract loadings for selected PCs
        loadings_df <- as.data.frame(res_pca$var$coord[, selected_pc_indices, drop = FALSE]) # drop=FALSE to handle single PC
        colnames(loadings_df) <- paste0("Dim.", selected_pc_indices)
        loadings_df <- rownames_to_column(loadings_df, var = "Variable") %>%
            pivot_longer(cols = starts_with("Dim."), names_to = "PC", values_to = "Loading") %>%
            mutate(PC = gsub("Dim.", "PC", PC)) %>%
            arrange(PC, desc(abs(Loading))) # Sort by PC then by absolute loading
        
        loadings_df
    })
    
    # Render Scree Plot
    output$scree_plot <- renderPlot({
        req(pca_results())
        fviz_eig(pca_results(), addlabels = TRUE, ylim = c(0, max(pca_results()$eig[, "eigenvalue"]) * 1.1))
    })
    
    # Render PCA Loadings Table
    output$pca_loadings_table <- renderDT({
        req(pca_loadings_filtered())
        pca_loadings_filtered()
    }, options = list(pageLength = 10))
    
    # Dynamic UI for MDS selection (checkboxes)
    output$mds_selection_ui <- renderUI({
        req(pca_loadings_filtered())
        # Get unique variables from filtered loadings
        all_vars <- unique(pca_loadings_filtered()$Variable)
        
        if (length(all_vars) == 0) {
            return(p("No significant PCs (Eigenvalue ≥ 1) or variables found. Adjust your data or PCA parameters."))
        }
        
        # Pre-select top variable from each significant PC as a suggestion (as per paper's method)
        # Ensure there are actually significant PCs before attempting to select
        suggested_mds <- character(0)
        if (nrow(pca_loadings_filtered()) > 0) {
            suggested_mds <- pca_loadings_filtered() %>%
                group_by(PC) %>%
                top_n(1, abs(Loading)) %>% # Take variable with highest absolute loading for each PC
                pull(Variable) %>%
                unique()
        }
        
        
        checkboxGroupInput("selected_mds_vars", "Select MDS Indicators:",
                           choices = all_vars,
                           selected = suggested_mds)
    })
    
    # Reactive for selected MDS variables
    mds_vars_selected <- reactive({
        req(input$selected_mds_vars)
        input$selected_mds_vars
    })
    
    # Dynamic UI for scoring parameters based on selected MDS variables
    output$scoring_params_ui <- renderUI({
        req(mds_vars_selected())
        
        lapply(mds_vars_selected(), function(var) {
            wellPanel(
                h4(paste("Scoring for:", var)),
                selectInput(paste0("score_type_", var), "Scoring Function Type:",
                            choices = c("More is Better" = "more_is_better",
                                        "Less is Better" = "less_is_better",
                                        "Optimum Range" = "optimum_range")),
                uiOutput(paste0("params_for_", var)) # UI for specific parameters
            )
        })
    })
    
    # Dynamic UI for parameters of each scoring type
    observe({
        req(mds_vars_selected())
        lapply(mds_vars_selected(), function(var) {
            output[[paste0("params_for_", var)]] <- renderUI({
                score_type <- input[[paste0("score_type_", var)]]
                if (is.null(score_type)) return(NULL)
                
                # Get mean and SD for initial suggestions for parameters
                var_data <- data_raw()[[var]]
                mean_val <- mean(var_data, na.rm = TRUE)
                sd_val <- sd(var_data, na.rm = TRUE)
                
                if (score_type == "more_is_better") {
                    fluidRow(
                        column(6, numericInput(paste0("param_", var, "_x0"), "Inflection Point (x0):", value = round(mean_val, 2), step = 0.1)),
                        column(6, numericInput(paste0("param_", var, "_b"), "Slope (b):", value = 2.5, min = 0.1, step = 0.1)) # Default b to 2.5
                    )
                } else if (score_type == "less_is_better") {
                    fluidRow(
                        column(6, numericInput(paste0("param_", var, "_x0"), "Inflection Point (x0):", value = round(mean_val, 2), step = 0.1)),
                        column(6, numericInput(paste0("param_", var, "_b"), "Slope (b):", value = 2.5, min = 0.1, step = 0.1)) # Default b to 2.5
                    )
                } else if (score_type == "optimum_range") {
                    fluidRow(
                        column(4, numericInput(paste0("param_", var, "_opt_val"), "Optimum Value:", value = round(mean_val, 2), step = 0.1)),
                        column(4, numericInput(paste0("param_", var, "_width"), "Width:", value = round(sd_val / 2, 2), min = 0.01, step = 0.01))
                    )
                }
            })
        })
    })
    
    # Reactive for calculated SQI and individual scores
    calculated_sqi_data <- eventReactive(input$calculate_sqi, {
        req(data_raw(), mds_vars_selected(), pca_results())
        
        df_original <- data_raw()
        mds_vars <- mds_vars_selected()
        
        # Select only numeric columns for initial scores_df, preserving original non-numeric ones
        scores_df_numeric_only <- df_original %>% select(where(is.numeric))
        
        # 1. Calculate Individual Scores (Non-Linear)
        for (var in mds_vars) {
            score_type <- input[[paste0("score_type_", var)]]
            if (is.null(score_type) || !(var %in% names(scores_df_numeric_only))) {
                showNotification(paste("Error: Scoring parameters or numeric column missing for:", var), type = "error")
                return(NULL)
            }
            
            if (score_type == "more_is_better") {
                x0 <- input[[paste0("param_", var, "_x0")]]
                b <- input[[paste0("param_", var, "_b")]]
                scores_df_numeric_only[[paste0(var, "_Score")]] <- score_more_is_better(scores_df_numeric_only[[var]], x0, b)
            } else if (score_type == "less_is_better") {
                x0 <- input[[paste0("param_", var, "_x0")]]
                b <- input[[paste0("param_", var, "_b")]]
                scores_df_numeric_only[[paste0(var, "_Score")]] <- score_less_is_better(scores_df_numeric_only[[var]], x0, b)
            } else if (score_type == "optimum_range") {
                opt_val <- input[[paste0("param_", var, "_opt_val")]]
                width <- input[[paste0("param_", var, "_width")]]
                scores_df_numeric_only[[paste0(var, "_Score")]] <- score_optimum_range(scores_df_numeric_only[[var]], opt_val, width)
            }
        }
        
        # 2. Calculate Weights based on PCA Communalities (as per paper)
        pca_res <- pca_results()
        var_loadings_matrix <- pca_res$var$coord # All loadings
        
        # Filter for PCs with eigenvalue >= 1, as per paper's MDS selection criteria
        eigenvalues <- pca_res$eig[, "eigenvalue"]
        significant_pc_indices <- which(eigenvalues >= 1)
        
        if (length(significant_pc_indices) == 0) {
            showNotification("No significant principal components (Eigenvalue >= 1) found. Cannot calculate weights. Please review PCA results.", type = "error")
            return(NULL)
        }
        
        # Sum of squared loadings (communalities) for selected MDS variables on significant PCs
        communalities <- sapply(mds_vars, function(v) {
            if (v %in% rownames(var_loadings_matrix)) {
                # Check if the variable has loadings on the significant PCs
                if (length(significant_pc_indices) > 0) {
                    sum(var_loadings_matrix[v, significant_pc_indices]^2)
                } else {
                    0
                }
            } else {
                0 # If variable not found in PCA (e.g., non-numeric or removed)
            }
        })
        
        # Normalize weights to sum to 1
        total_communalities <- sum(communalities)
        if (total_communalities == 0) {
            showNotification("Sum of communalities is zero for selected MDS indicators. Cannot calculate weights. Check MDS selection.", type = "error")
            return(NULL)
        }
        weights <- communalities / total_communalities
        
        # Store weights for display
        mds_weights_df <- tibble(
            Indicator = mds_vars,
            Communalities = communalities,
            Weight = weights
        )
        
        # 3. Calculate SQI
        # Initialize SQI column in a temporary dataframe for calculation
        temp_sqi_calc_df <- scores_df_numeric_only %>%
            select(all_of(mds_vars)) # Ensure only original MDS vars are here for multiplication
        
        temp_sqi_calc_df$SQI_temp <- 0 # Initialize SQI column
        
        for (i in seq_along(mds_vars)) {
            var_name <- mds_vars[i]
            weight_val <- weights[i]
            score_col_name <- paste0(var_name, "_Score")
            if (score_col_name %in% names(scores_df_numeric_only)) {
                temp_sqi_calc_df$SQI_temp <- temp_sqi_calc_df$SQI_temp + (scores_df_numeric_only[[score_col_name]] * weight_val)
            } else {
                showNotification(paste("Warning: Score column not found for", var_name, ". Skipping calculation for this indicator."), type = "warning")
            }
        }
        
        # Combine original data (including non-numeric) with the calculated SQI and individual scores
        # >>> MODIFICATION HERE: Scale SQI to 0-100 <<<
        all_final_cols <- df_original %>%
            bind_cols(scores_df_numeric_only %>% select(starts_with(paste0(mds_vars, "_Score")))) %>%
            mutate(SQI = temp_sqi_calc_df$SQI_temp * 100) # Multiply by 100 for 0-100 scale
        
        list(
            sqi_data = all_final_cols, # Now contains original + scores + SQI (0-100)
            mds_weights = mds_weights_df
        )
    })
    
    # Display SQI Results Table
    output$sqi_results_table <- renderDT({
        req(calculated_sqi_data()$sqi_data)
        calculated_sqi_data()$sqi_data
    }, options = list(pageLength = 10))
    
    # Display MDS Weights Table
    output$mds_weights_table <- renderDT({
        req(calculated_sqi_data()$mds_weights)
        calculated_sqi_data()$mds_weights
    }, options = list(dom = 't')) # 't' for table only, no search/pagination
    
    # Render SQI Histogram
    output$sqi_histogram_plot <- renderPlot({
        req(calculated_sqi_data()$sqi_data)
        # >>> MODIFICATION HERE: Adjust binwidth for 0-100 scale <<<
        ggplot(calculated_sqi_data()$sqi_data, aes(x = SQI)) +
            geom_histogram(binwidth = 5, fill = "steelblue", color = "black") + # Changed binwidth from 0.05 to 5
            labs(title = "Distribution of Soil Quality Index (0-100)", x = "Soil Quality Index", y = "Frequency") +
            theme_minimal()
    })
    
    # Dynamic UI for selecting grouping variable
    output$grouping_variable_selector_ui <- renderUI({
        req(data_raw())
        # Get non-numeric column names from the raw data
        non_numeric_cols <- names(data_raw() %>% select(where(~!is.numeric(.))))
        
        if (length(non_numeric_cols) == 0) {
            return(p("No non-numeric columns found in your data to group by."))
        }
        
        selectInput("group_var_for_plot", "Select Grouping Variable for SQI Bar Graph:",
                    choices = c("None" = "", non_numeric_cols),
                    selected = "")
    })
    
    # Render Grouped SQI Bar Plot
    output$sqi_group_bar_plot <- renderPlot({
        req(calculated_sqi_data()$sqi_data)
        
        group_var <- input$group_var_for_plot
        
        if (is.null(group_var) || group_var == "") {
            return(
                ggplot() +
                    geom_text(aes(x=0.5, y=0.5, label="Select a grouping variable to view the bar graph")) +
                    theme_void() +
                    labs(title = "SQI Grouped Bar Graph")
            )
        }
        
        # Ensure the grouping variable is a factor
        plot_data <- calculated_sqi_data()$sqi_data %>%
            mutate(!!sym(group_var) := as.factor(!!sym(group_var))) %>%
            group_by(!!sym(group_var)) %>%
            summarise(Mean_SQI = mean(SQI, na.rm = TRUE),
                      SD_SQI = sd(SQI, na.rm = TRUE)) %>% # Calculate standard deviation for error bars
            ungroup()
        
        # >>> MODIFICATION HERE: Add h-lines and labels for SQI classes <<<
        ggplot(plot_data, aes(x = !!sym(group_var), y = Mean_SQI, fill = !!sym(group_var))) +
            geom_bar(stat = "identity", color = "black") +
            geom_errorbar(aes(ymin = Mean_SQI - SD_SQI, ymax = Mean_SQI + SD_SQI), width = 0.2, position = position_dodge(.9)) + # Add error bars
            labs(title = paste("Mean Soil Quality Index (0-100) by", group_var),
                 x = group_var,
                 y = "Mean Soil Quality Index (0-100)") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels if needed
                  plot.margin = unit(c(1, 4, 1, 1), "lines")) + # Increase right margin for labels
            
            # Add horizontal lines for class boundaries
            geom_hline(yintercept = 80, linetype = "dashed", color = "darkgreen", linewidth = 0.8) +
            geom_hline(yintercept = 60, linetype = "dashed", color = "forestgreen", linewidth = 0.8) +
            geom_hline(yintercept = 40, linetype = "dashed", color = "orange", linewidth = 0.8) +
            geom_hline(yintercept = 20, linetype = "dashed", color = "firebrick", linewidth = 0.8) +
            
            # Add text labels for soil quality classes (positioned slightly right of plot area)
            annotate("text", x = Inf, y = 80, label = "Q1: Extremely High (80-100)",
                     hjust = 1.1, vjust = -0.5, size = 3, color = "darkgreen") +
            annotate("text", x = Inf, y = 60, label = "Q2: High (60-80)",
                     hjust = 1.1, vjust = -0.5, size = 3, color = "forestgreen") +
            annotate("text", x = Inf, y = 40, label = "Q3: Medium (40-60)",
                     hjust = 1.1, vjust = -0.5, size = 3, color = "orange") +
            annotate("text", x = Inf, y = 20, label = "Q4: Poor (20-40)",
                     hjust = 1.1, vjust = -0.5, size = 3, color = "firebrick") +
            annotate("text", x = Inf, y = 0, label = "Q5: Extremely Poor (0-20)",
                     hjust = 1.1, vjust = -0.5, size = 3, color = "darkred") + # Added Q5 label
            
            # Set y-axis limits to 0-100 and allow text annotations to extend beyond plot area
            coord_cartesian(ylim = c(0, 100), clip = "off")
    })
    
    # Download Handler for SQI Results
    output$download_sqi <- downloadHandler(
        filename = function() {
            paste("sqi_results_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write_csv(calculated_sqi_data()$sqi_data, file)
        }
    )
    
}

# Run the app
shinyApp(ui, server)