# two-way MANOVA

# Install and load necessary packages
# if (!require("shiny")) install.packages("shiny")
# if (!require("readr")) install.packages("readr")
# if (!require("readxl")) install.packages("readxl")
# if (!require("DT")) install.packages("DT")
# if (!require("dplyr")) install.packages("dplyr")
# if (!require("tidyr")) install.packages("tidyr") # For pivot_longer if needed for some plots
# if (!require("ggplot2")) install.packages("ggplot2")
# if (!require("car")) install.packages("car") # For Manova function (optional, can use base manova)
# if (!require("emmeans")) install.packages("emmeans")
# if (!require("multcomp")) install.packages("multcomp") # For glht, though cld uses multcompView
# if (!require("multcompView")) install.packages("multcompView") # Dependency for cld letters
# if (!require("tools")) install.packages("tools") # For file_ext
# if (!require("writexl")) install.packages("writexl") # For Excel download

library(shiny)
library(readr)
library(readxl)
library(DT)
library(dplyr)
library(tidyr)
library(ggplot2)
library(car) # Often preferred for Type II/III sums of squares, or stick to base manova()
library(emmeans)
library(multcomp) # Required for some internal emmeans functionality, even if not directly called
library(multcompView) # For compact letter display (cld)
library(tools) # For file_ext
library(writexl) # For writing data frames to Excel

# --- UI (User Interface) ---
ui <- fluidPage(
    titlePanel("Two-Way MANOVA App"),
    
    sidebarLayout(
        sidebarPanel(
            fileInput("file1", "Choose CSV or XLSX File",
                      multiple = FALSE,
                      accept = c(".csv", ".xlsx")),
            tags$hr(),
            uiOutput("var_selection_panel"), # Dynamic UI for variable selection
            tags$hr(),
            actionButton("run_manova", "Run MANOVA & Post-Hoc Analysis", class = "btn-primary"),
            tags$hr(),
            downloadButton("download_manova_results", "Download MANOVA Summary (.txt)"),
            downloadButton("download_posthoc_tables", "Download Post-Hoc Tables (.xlsx)")
        ),
        
        mainPanel(
            tabsetPanel(
                id = "main_tabs",
                tabPanel("Data Preview",
                         h4("First 10 Rows of Data"),
                         DTOutput("data_preview_table")),
                tabPanel("MANOVA Results",
                         h4("Overall Two-Way MANOVA Test Results"),
                         verbatimTextOutput("manova_output")),
                tabPanel("Post-Hoc Tables",
                         h4("Mean Comparisons with Tukey HSD Letters"),
                         uiOutput("posthoc_tables_ui")), # Dynamic UI for tables
                tabPanel("Plots",
                         h4("Group Mean Plots with Tukey HSD Letters"),
                         uiOutput("plot_output_ui")) # Dynamic UI for plots
            )
        )
    )
)

# --- Server (Server Logic) ---
server <- function(input, output, session) {
    
    # 1. Reactive for uploaded data
    data_react <- reactiveVal(NULL)
    
    observeEvent(input$file1, {
        req(input$file1)
        df <- NULL
        ext <- tools::file_ext(input$file1$name)
        tryCatch({
            if (ext == "csv") {
                df <- read_csv(input$file1$datapath, show_col_types = FALSE)
            } else if (ext == "xlsx") {
                df <- read_excel(input$file1$datapath)
            } else {
                stop("Unsupported file type. Please upload a .csv or .xlsx file.")
            }
            data_react(df)
            showNotification("File uploaded successfully!", type = "message")
        }, error = function(e) {
            showNotification(paste("Error reading file:", e$message), type = "error")
            data_react(NULL)
        })
    })
    
    # 2. Data Preview Table
    output$data_preview_table <- renderDT({
        df <- data_react()
        req(df)
        datatable(head(df, 10), options = list(dom = 't')) # Show first 10 rows without search/pagination
    })
    
    # 3. Dynamic UI for Variable Selection
    output$var_selection_panel <- renderUI({
        df <- data_react()
        if (is.null(df)) {
            return("Upload data to select variables.")
        }
        
        numeric_vars <- names(df)[sapply(df, is.numeric)]
        # For IVs, allow any column to be selected, as they will be converted to factors
        all_column_names <- names(df)
        
        tagList(
            selectInput("dep_vars", "Select Dependent Variables (Numeric)",
                        choices = numeric_vars, multiple = TRUE,
                        selected = if (length(numeric_vars) >= 2) numeric_vars[1:2] else NULL), # Pre-select 2 if available
            selectInput("indep_var1", "Select First Independent Variable (will be treated as Factor)",
                        choices = all_column_names,
                        selected = if (length(all_column_names) >= 1) all_column_names[1] else NULL),
            selectInput("indep_var2", "Select Second Independent Variable (will be treated as Factor)",
                        choices = all_column_names,
                        selected = if (length(all_column_names) >= 2) all_column_names[2] else NULL)
        )
    })
    
    # Helper function to safely extract p-values from summary stats
    get_p_value <- function(summary_stats, term_name) {
        if (term_name %in% rownames(summary_stats)) {
            return(summary_stats[term_name, "Pr(>F)"])
        } else {
            return(NA_real_) # Return NA if term not found in summary stats
        }
    }
    
    # 4. Reactive for MANOVA results
    manova_results <- eventReactive(input$run_manova, {
        req(input$dep_vars, input$indep_var1, input$indep_var2, data_react())
        
        df <- data_react()
        dep_vars <- input$dep_vars
        indep_var1 <- input$indep_var1
        indep_var2 <- input$indep_var2
        
        # Ensure selected variables exist in the dataframe
        if (!all(c(dep_vars, indep_var1, indep_var2) %in% names(df))) {
            showNotification("One or more selected variables not found in the data.", type = "error", duration = 8)
            return(NULL)
        }
        
        # Ensure independent variables are factors
        df[[indep_var1]] <- as.factor(df[[indep_var1]])
        df[[indep_var2]] <- as.factor(df[[indep_var2]])
        
        # Ensure dependent variables are numeric (even if initially selected as such, double check)
        for (dv in dep_vars) {
            if (!is.numeric(df[[dv]])) {
                showNotification(paste("Dependent variable '", dv, "' is not numeric. Please check your data.", sep = ""), type = "error", duration = 8)
                return(NULL)
            }
        }
        
        # Check for at least 2 dependent variables
        if (length(dep_vars) < 2) {
            showNotification("Please select at least two dependent variables for MANOVA.", type = "error", duration = 8)
            return(NULL)
        }
        
        # Construct the formula for a two-way MANOVA with interaction
        formula_str <- paste0("cbind(", paste(dep_vars, collapse = ", "), ") ~ ",
                              indep_var1, " * ", indep_var2)
        manova_formula <- as.formula(formula_str)
        
        model <- tryCatch({
            # Using base manova() - Type I SS. For Type II/III, use car::Manova()
            # car::Manova is generally recommended for unbalanced designs or specific SS types.
            # If using car::Manova, ensure you have 'car' library loaded and uncomment below:
            # car::Manova(manova_formula, data = df, type = "III")
            manova(manova_formula, data = df)
        }, error = function(e) {
            showNotification(paste("Error running MANOVA:", e$message), type = "error", duration = 8)
            return(NULL)
        })
        req(model) # Ensure model is not NULL before proceeding
        
        manova_summary_pillai <- summary(model, test = "Pillai")
        
        # Extract p-values using the helper function
        p_interaction_val <- get_p_value(manova_summary_pillai$stats, paste0(indep_var1, ":", indep_var2))
        p_main1_val <- get_p_value(manova_summary_pillai$stats, indep_var1)
        p_main2_val <- get_p_value(manova_summary_pillai$stats, indep_var2)
        
        # Store all p-values as a named vector using the actual variable names for main effects
        p_values_overall <- c(p_main1_val, p_main2_val, p_interaction_val)
        names(p_values_overall) <- c(indep_var1, indep_var2, "interaction") # Correctly assign names
        
        list(model = model, data = df, dep_vars = dep_vars,
             indep_var1 = indep_var1, indep_var2 = indep_var2,
             p_values_overall = p_values_overall)
    })
    
    # 5. MANOVA Output (summary table)
    output$manova_output <- renderPrint({
        res <- manova_results()
        req(res)
        model_manova <- res$model
        p_values_overall <- res$p_values_overall
        
        cat("--- Overall Two-Way MANOVA Test Results (Pillai's Trace) ---\n")
        print(summary(model_manova, test = "Pillai")) # Using Pillai's Trace
        
        cat("\n--- P-values for Main Effects and Interaction ---\n")
        # Ensure p_values_overall is printed nicely, handling potential NAs
        print(as.data.frame(t(as.matrix(p_values_overall)))) # Transpose for better display
        cat("\n")
        cat("\n--- Summary AOV per Response Variable ---\n")
        print(summary.aov(model_manova))
        
        # Extract p-values for notification logic (now robust due to correct naming)
        p_interaction_notify <- p_values_overall["interaction"]
        p_main1_notify <- p_values_overall[res$indep_var1]
        p_main2_notify <- p_values_overall[res$indep_var2]
        
        if (!is.na(p_interaction_notify) && p_interaction_notify <= 0.05) {
            # Changed type = "info" to type = "message"
            showNotification("Interaction effect is significant (p <= 0.05). Post-hoc will focus on simple effects (combinations).", type = "message", duration = 8)
        } else if (any(c(p_main1_notify, p_main2_notify) <= 0.05, na.rm = TRUE)) {
            # Changed type = "info" to type = "message"
            showNotification("Interaction not significant, but significant main effects detected. Post-hoc will focus on significant main effects.", type = "message", duration = 8)
        } else {
            showNotification("No significant main effects or interaction in MANOVA (p > 0.05). Post-hoc tests are not performed.", type = "warning", duration = 8)
        }
    })
    
    # 6. Reactive for Post-Hoc results and plots
    posthoc_data_and_plots <- eventReactive(input$run_manova, {
        res <- manova_results()
        req(res) # Ensure 'res' is not NULL
        
        model_manova <- res$model
        df <- res$data
        dep_vars <- input$dep_vars
        indep_var1 <- input$indep_var1
        indep_var2 <- input$indep_var2
        p_values_overall <- res$p_values_overall
        
        # Extract p-values by name (assuming p_values_overall is a named vector)
        p_interaction <- p_values_overall["interaction"]
        p_main1 <- p_values_overall[indep_var1]
        p_main2 <- p_values_overall[indep_var2]
        
        # --- CRITICAL ROBUSTNESS CHECKS ---
        # 1. Ensure extracted values are not NULL or numeric(0) - convert to NA_real_
        if (length(p_interaction) == 0) p_interaction <- NA_real_
        if (length(p_main1) == 0) p_main1 <- NA_real_
        if (length(p_main2) == 0) p_main2 <- NA_real_
        
        # 2. Ensure they are single numeric values, forcing NA if not.
        # This converts logical, character, or multiple values to NA.
        # It's an aggressive check against unexpected data types.
        p_interaction <- as.numeric(p_interaction)
        # If as.numeric creates a vector of NA (e.g. from c(NA, NA)), ensure it's a single NA
        if (is.na(p_interaction) && length(p_interaction) > 1) p_interaction <- NA_real_
        p_main1 <- as.numeric(p_main1)
        if (is.na(p_main1) && length(p_main1) > 1) p_main1 <- NA_real_
        p_main2 <- as.numeric(p_main2)
        if (is.na(p_main2) && length(p_main2) > 1) p_main2 <- NA_real_
        # --- END CRITICAL ROBUSTNESS CHECKS ---
        
        
        # Determine post-hoc strategy based on interaction significance
        current_effects_to_plot <- c() # Initialize as empty
        if (!is.na(p_interaction) && p_interaction <= 0.05) {
            # Interaction significant: Analyze simple effects (combinations of factors)
            emmeans_term <- paste0(indep_var1, "*", indep_var2)
            current_effects_to_plot <- c(emmeans_term)
        } else if (any(c(p_main1, p_main2) <= 0.05, na.rm = TRUE)) {
            # Interaction not significant, but significant main effects
            if (!is.na(p_main1) && p_main1 <= 0.05) current_effects_to_plot <- c(current_effects_to_plot, indep_var1)
            if (!is.na(p_main2) && p_main2 <= 0.05) current_effects_to_plot <- c(current_effects_to_plot, indep_var2)
        } else {
            # No significant effects, no post-hoc
            return(list(posthoc_dfs = list(), plots = list()))
        }
        
        # Initialize lists to store results
        posthoc_dfs_list <- list()
        plots_list <- list()
        
        # Loop through each dependent variable
        for (dv in dep_vars) {
            # Fit individual LM for each DV to get EMMs
            formula_dv <- as.formula(paste0(dv, " ~ ", indep_var1, " * ", indep_var2))
            
            # --- NEW: Robustly handle lm model fitting ---
            model_dv <- tryCatch({
                lm(formula_dv, data = df)
            }, error = function(e) {
                showNotification(paste0("Error fitting LM for ", dv, ": ", e$message), type = "error", duration = 5)
                NULL # Return NULL if LM fails
            })
            
            if (is.null(model_dv)) {
                next # Skip to next dependent variable if LM failed
            }
            # --- END NEW ---
            
            for (term in current_effects_to_plot) {
                # Calculate EMMs and CLD
                emmeans_dv <- tryCatch({
                    emmeans(model_dv, as.formula(paste0("~ ", term)))
                }, error = function(e) {
                    showNotification(paste0("Error calculating EMMs for ", dv, " by ", term, ": ", e$message), type = "error", duration = 5)
                    NULL # Return NULL if emmeans fails
                })
                
                if (is.null(emmeans_dv)) {
                    next # Skip to next term if EMMs failed
                }
                
                cld_dv <- tryCatch({
                    cld(emmeans_dv, alpha = 0.05, Letters = LETTERS, adjust = "tukey", reversed = TRUE)
                }, error = function(e) {
                    showNotification(paste0("Error calculating CLD for ", dv, " by ", term, ": ", e$message), type = "error", duration = 5)
                    NULL # Return NULL if CLD fails
                })
                
                if (is.null(cld_dv)) {
                    next # Skip to next term if CLD failed
                }
                
                cld_df <- as.data.frame(cld_dv)
                
                # --- NEW: Check if cld_df is empty before processing or plotting ---
                if (nrow(cld_df) == 0) {
                    showNotification(paste0("No estimable means for ", dv, " by ", term, ". Skipping table and plot."), type = "warning", duration = 5)
                    next # Skip to next term if no rows in CLD
                }
                # --- END NEW ---
                
                # Dynamically rename the .group column to "Group"
                group_col_name <- names(cld_df)[grepl("^\\.group", names(cld_df), ignore.case = TRUE)][1]
                if (is.na(group_col_name)) {
                    group_col_name <- term # Fallback in case .group isn't found (e.g. only one group)
                }
                names(cld_df)[names(cld_df) == group_col_name] <- "Group"
                
                # Store the data frame for the table
                table_name <- paste0(dv, "_by_", term)
                posthoc_dfs_list[[table_name]] <- cld_df
                
                # Create the plot
                plot_x_aes <- term
                plot_fill_aes <- term
                
                if (term == paste0(indep_var1, "*", indep_var2)) {
                    # For interaction plots, combine factors for x-axis if needed
                    cld_df$interaction_group <- interaction(cld_df[[indep_var1]], cld_df[[indep_var2]], sep = ":")
                    plot_x_aes <- "interaction_group"
                    plot_fill_aes <- "interaction_group" # or indep_var1 if faceting by indep_var2
                    plot_title_suffix <- paste0(" (Interaction of ", indep_var1, " and ", indep_var2, ")")
                    plot_x_label <- paste0(indep_var1, " x ", indep_var2, " Combinations")
                } else {
                    plot_title_suffix <- paste0(" (Main Effect of ", term, ")")
                    plot_x_label <- term
                }
                
                # --- NEW: Robust plot generation ---
                plot_dv <- tryCatch({
                    ggplot(cld_df, aes_string(x = plot_x_aes, y = "emmean", fill = plot_fill_aes)) +
                        geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
                        geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2,
                                      position = position_dodge(width = 0.9)) +
                        geom_text(aes(label = Group, y = upper.CL + 0.1 * (max(upper.CL, na.rm = TRUE) - min(lower.CL, na.rm = TRUE))),
                                  position = position_dodge(width = 0.9),
                                  vjust = -0.5, size = 4, color = "black",
                                  na.rm = TRUE) + # <--- Add na.rm = TRUE to geom_text
                        labs(title = paste("Mean", dv, plot_title_suffix, "with Tukey HSD Groupings"),
                             y = paste("Mean", dv),
                             x = plot_x_label) +
                        theme_minimal() +
                        theme(legend.position = "none", plot.title = element_text(hjust = 0.5),
                              axis.text.x = element_text(angle = 45, hjust = 1))
                }, error = function(e) {
                    showNotification(paste0("Error generating plot for ", dv, " by ", term, ": ", e$message), type = "error", duration = 5)
                    NULL # Return NULL if plot generation fails
                })
                
                if (!is.null(plot_dv)) {
                    plots_list[[table_name]] <- plot_dv
                }
                # --- END NEW ---
            }
        }
        
        list(posthoc_dfs = posthoc_dfs_list, plots = plots_list)
    })
    
    # 7. Post-Hoc Tables Output UI
    output$posthoc_tables_ui <- renderUI({
        results <- posthoc_data_and_plots()
        req(results)
        if (length(results$posthoc_dfs) == 0) {
            return(tags$p("No post-hoc tables to display. (Check MANOVA significance)"))
        }
        lapply(names(results$posthoc_dfs), function(table_id) {
            tagList(
                h5(paste("Post-Hoc Table for:", gsub("_by_", " by ", gsub("_", " ", table_id)))), # Format title
                DTOutput(paste0("posthoc_table_", table_id))
            )
        })
    })
    
    # 8. Render individual Post-Hoc Tables
    observeEvent(input$run_manova, {
        results <- posthoc_data_and_plots()
        req(results)
        lapply(names(results$posthoc_dfs), function(table_id) {
            output[[paste0("posthoc_table_", table_id)]] <- renderDT({
                req(results$posthoc_dfs[[table_id]])
                datatable(results$posthoc_dfs[[table_id]],
                          options = list(scrollX = TRUE, pageLength = 10),
                          rownames = FALSE)
            })
        })
    })
    
    # 9. Plots Output UI (for plot placeholders)
    output$plot_output_ui <- renderUI({
        results <- posthoc_data_and_plots()
        req(results)
        if (length(results$plots) == 0) {
            return(tags$p("No plots to display. (Check MANOVA significance)"))
        }
        plot_outputs <- lapply(names(results$plots), function(plot_id) {
            tagList(
                h5(paste("Plot for:", gsub("_by_", " by ", gsub("_", " ", plot_id)))), # Format title
                plotOutput(paste0("plot_", plot_id), height = "400px", width = "100%")
            )
        })
        do.call(tagList, plot_outputs)
    })
    
    # 10. Render individual plots
    observeEvent(input$run_manova, {
        results <- posthoc_data_and_plots()
        req(results)
        lapply(names(results$plots), function(plot_id) {
            output[[paste0("plot_", plot_id)]] <- renderPlot({
                req(results$plots[[plot_id]])
                results$plots[[plot_id]]
            })
        })
    })
    
    # 11. Download MANOVA Results
    output$download_manova_results <- downloadHandler(
        filename = function() {
            paste("MANOVA_Summary_", Sys.Date(), ".txt", sep = "")
        },
        content = function(file) {
            res <- manova_results()
            req(res)
            sink(file)
            print(summary(res$model, test = "Pillai"))
            sink()
        }
    )
    
    # 12. Download Post-Hoc Tables
    output$download_posthoc_tables <- downloadHandler(
        filename = function() {
            paste("Post_Hoc_Tables_", Sys.Date(), ".xlsx", sep = "")
        },
        content = function(file) {
            results <- posthoc_data_and_plots()
            req(results$posthoc_dfs)
            
            # Create a list of data frames for writing to Excel
            df_list_for_excel <- lapply(names(results$posthoc_dfs), function(table_id) {
                results$posthoc_dfs[[table_id]]
            })
            names(df_list_for_excel) <- names(results$posthoc_dfs) # Use original names for sheet names
            
            # Ensure writexl is available
            if (!requireNamespace("writexl", quietly = TRUE)) {
                showNotification("The 'writexl' package is required for Excel download. Please install it.", type = "error", duration = 8)
                return()
            }
            writexl::write_xlsx(df_list_for_excel, path = file)
        }
    )
}

# Run the application
shinyApp(ui = ui, server = server)