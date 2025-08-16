7# One way MANOVA app

# Load necessary libraries
library(shiny)
library(tidyverse) # Includes dplyr and ggplot2
library(car)      # For MANOVA summary (e.g., Pillai's Trace)
library(emmeans)  # For post-hoc tests and CLD
library(multcomp) #assurance (dependency for emmeans and cld)
library(multcompView) # For compact letter display (cld)
library(readxl)   # For Excel file upload
library(DT)       # For interactive tables
library(tools)    # For file_ext
library(writexl)  # For Excel file download
# textshaping is usually a dependency for plotting, no need to explicitly load unless troubleshooting

# --- UI (User Interface) ---
ui <- fluidPage(
  titlePanel("One-Way MANOVA App"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV/Excel File",
                multiple = FALSE,
                accept = c(".csv", ".xlsx")),
      tags$hr(),
      uiOutput("var_select_panel"), # Dynamic UI for variable selection
      # --- NEW INPUT FOR REPLICATIONS ---
      numericInput("min_replications", "Minimum Replications per Group:",
                   value = 2, min = 1), # Default to 2, min of 1
      tags$hr(), # Add a separator
      actionButton("run_manova", "Run MANOVA & Post-Hoc", class = "btn-primary"),
      tags$hr(), # Add a separator for download buttons
      downloadButton("download_manova_results", "Download MANOVA Summary (.txt)"),
      downloadButton("download_posthoc_tables", "Download Post-Hoc Tables (.xlsx)")
    ),
    
    mainPanel(
      tabsetPanel(
        id = "main_tabs", # Added ID for potential future tab control
        tabPanel("Data Preview", DTOutput("data_preview")),
        tabPanel("MANOVA Results", verbatimTextOutput("manova_output")),
        tabPanel("Post-Hoc Results",
                 h4("Tukey HSD for Individual Dependent Variables"),
                 uiOutput("posthoc_results_ui") # This will now generate DTOutput
        ),
        tabPanel("Plots",
                 h4("Group Mean Plots with Tukey HSD Letters"),
                 uiOutput("plot_output_ui") # Dynamic UI for plots
        )
      )
    )
  )
)

# --- Server (Server Logic) ---
server <- function(input, output, session) {
  
  # Reactive value for storing the uploaded data
  data_react <- reactiveVal(NULL)
  
  # 1. Data Upload
  observeEvent(input$file1, {
    req(input$file1)
    df <- NULL
    ext <- tools::file_ext(input$file1$name)
    tryCatch({
      if (ext == "csv") {
        df <- read_csv(input$file1$datapath, show_col_types = FALSE) # Added show_col_types = FALSE
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
  
  # Data Preview Table
  output$data_preview <- renderDT({
    req(data_react())
    datatable(data_react(), options = list(pageLength = 10))
  })
  
  # 2. Dynamic UI for Variable Selection
  output$var_select_panel <- renderUI({
    df <- data_react()
    if (is.null(df)) {
      return("Please upload data to select variables.")
    }
    
    numeric_vars <- names(df)[sapply(df, is.numeric)]
    # For IVs, allow any column to be selected, as they will be converted to factors
    all_column_names <- names(df)
    
    tagList(
      selectInput("dep_vars", "Select Dependent Variables (Numeric, multiple)",
                  choices = numeric_vars, multiple = TRUE,
                  selected = if (length(numeric_vars) >= 2) numeric_vars[1:2] else NULL), # Pre-select 2 if available
      selectInput("indep_var", "Select Independent Variable (Factor/Group)",
                  choices = all_column_names,
                  selected = if (length(all_column_names) >= 1) all_column_names[1] else NULL)
    )
  })
  
  # Helper function to safely extract p-values from summary stats (if needed for notifications)
  get_p_value <- function(summary_stats, term_name) {
    if (term_name %in% rownames(summary_stats)) {
      return(summary_stats[term_name, "Pr(>F)"])
    } else {
      return(NA_real_) # Return NA if term not found in summary stats
    }
  }
  
  
  # Reactive for MANOVA results
  manova_results <- eventReactive(input$run_manova, {
    req(input$dep_vars, input$indep_var, data_react())
    
    df <- data_react()
    dep_vars <- input$dep_vars
    indep_var <- input$indep_var
    
    # Ensure selected variables exist in the dataframe
    if (!all(c(dep_vars, indep_var) %in% names(df))) {
      showNotification("One or more selected variables not found in the data.", type = "error", duration = 8)
      return(NULL)
    }
    
    # Ensure independent variable is a factor
    df[[indep_var]] <- as.factor(df[[indep_var]])
    
    # Ensure dependent variables are numeric
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
    
    # --- REPLICATION VALIDATION ---
    group_counts <- table(df[[indep_var]])
    min_reps <- input$min_replications
    
    # Check if any group has fewer than min_replications
    groups_below_min <- group_counts[group_counts < min_reps]
    
    if (length(groups_below_min) > 0) {
      msg <- paste0("Warning: The following groups have fewer than ", min_reps,
                    " replications: ", paste(names(groups_below_min), " (n=", groups_below_min, ")", collapse = ", "),
                    ". MANOVA results may be unreliable. Consider increasing 'Minimum Replications' or checking your data.")
      showNotification(msg, type = "warning", duration = 10) # Show for 10 seconds
      # Do NOT return NULL here, allow analysis to proceed with a warning
    }
    # --- END REPLICATION VALIDATION ---
    
    # Construct the formula
    formula_str <- paste0("cbind(", paste(dep_vars, collapse = ", "), ") ~ ", indep_var)
    manova_formula <- as.formula(formula_str)
    
    # Perform MANOVA
    model <- tryCatch({
      manova(manova_formula, data = df)
    }, error = function(e) {
      # Specific check for the rank deficiency error
      if (grepl("residuals have rank", e$message, ignore.case = TRUE)) {
        showNotification(paste("Error running MANOVA: ", e$message, 
                               ". This often means there's a linear dependency among your dependent variables, or not enough observations per group relative to the number of DVs. Please check your data or reduce the number of selected dependent variables.", sep = ""), 
                         type = "error", duration = 15) # Longer duration for this important error
      } else {
        showNotification(paste("Error running MANOVA:", e$message), type = "error", duration = 8)
      }
      return(NULL)
    })
    req(model) # Ensure model is not NULL
    
    # Get overall MANOVA p-value for notification logic (Pillai's Trace is common)
    manova_summary_pillai <- summary(model, test = "Pillai")
    p_manova_overall <- get_p_value(manova_summary_pillai$stats, indep_var)
    
    list(model = model, data = df, dep_vars = dep_vars,
         indep_var = indep_var, p_manova_overall = p_manova_overall)
  })
  
  # 3. MANOVA Results Output
  output$manova_output <- renderPrint({
    req(manova_results())
    res <- manova_results()
    
    cat("--- Overall One-Way MANOVA Test Results (Pillai's Trace) ---\n")
    print(summary(res$model, test = "Pillai")) # Keep test = "Pillai" or your preferred test
    cat("\n")
    
    cat("--- ANOVA Test Results per Response Variable (for MANOVA significance) ---\n")
    print(summary.aov(res$model))
    cat("\n")
    
    # Notification based on overall MANOVA significance
    if (!is.na(res$p_manova_overall) && res$p_manova_overall <= 0.05) {
      showNotification("Overall MANOVA is significant (p <= 0.05). Proceeding with post-hoc tests for individual DVs.", type = "message", duration = 8)
    } else {
      showNotification("Overall MANOVA is NOT significant (p > 0.05). Post-hoc tests are not typically interpreted or performed.", type = "warning", duration = 8)
    }
    
  })
  
  # Reactive for Post-Hoc results and plots
  posthoc_data_and_plots <- eventReactive(input$run_manova, {
    res <- manova_results()
    req(res)
    model_manova <- res$model
    df <- res$data
    dep_vars <- input$dep_vars
    indep_var <- res$indep_var
    p_manova_overall <- res$p_manova_overall
    
    # Only proceed with post-hoc if overall MANOVA is significant
    if (is.na(p_manova_overall) || p_manova_overall > 0.05) {
      return(list(posthoc_dfs = list(), plots = list())) # Return empty lists
    }
    
    posthoc_dfs_list <- list()
    plots_list <- list() # This will now store ggplot objects, not renderPlot calls
    
    for (dv in dep_vars) {
      # Robustly fit individual LM for each DV
      formula_dv <- as.formula(paste0(dv, " ~ ", indep_var))
      model_dv <- tryCatch({
        lm(formula_dv, data = df)
      }, error = function(e) {
        showNotification(paste0("Error fitting LM for ", dv, ": ", e$message), type = "error", duration = 5)
        NULL
      })
      if (is.null(model_dv)) next
      
      # Robustly calculate EMMs
      emmeans_dv <- tryCatch({
        emmeans(model_dv, as.formula(paste0("~ ", indep_var)))
      }, error = function(e) {
        showNotification(paste0("Error calculating EMMs for ", dv, ": ", e$message), type = "error", duration = 5)
        NULL
      })
      if (is.null(emmeans_dv)) next
      
      # Robustly calculate CLD
      cld_dv <- tryCatch({
        cld(emmeans_dv, alpha = 0.05, Letters = LETTERS, adjust = "tukey", reversed = TRUE)
      }, error = function(e) {
        showNotification(paste0("Error calculating CLD for ", dv, ": ", e$message), type = "error", duration = 5)
        NULL
      })
      if (is.null(cld_dv)) next
      
      cld_df <- as.data.frame(cld_dv)
      
      if (nrow(cld_df) == 0) {
        showNotification(paste0("No estimable means for ", dv, ". Skipping table and plot."), type = "warning", duration = 5)
        next
      }
      
      # Dynamically rename the .group column to "Group"
      group_col_name <- names(cld_df)[grepl("^\\.group", names(cld_df), ignore.case = TRUE)][1]
      if (is.na(group_col_name)) {
        group_col_name <- indep_var # Fallback if .group isn't found
      }
      names(cld_df)[names(cld_df) == group_col_name] <- "Group"
      
      posthoc_dfs_list[[dv]] <- cld_df
      
      # Robustly create ggplot object
      plot_dv <- tryCatch({
        ggplot(cld_df, aes_string(x = indep_var, y = "emmean", fill = indep_var)) +
          geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
          geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2,
                        position = position_dodge(width = 0.9)) +
          geom_text(aes(label = Group, y = upper.CL + 0.1 * (max(upper.CL, na.rm = TRUE) - min(lower.CL, na.rm = TRUE))),
                    position = position_dodge(width = 0.9),
                    vjust = -0.5, size = 4, color = "black", na.rm = TRUE) + # Added na.rm=TRUE
          labs(title = paste("Mean", dv, "by", indep_var, "with Tukey HSD Groupings"),
               y = paste("Mean", dv),
               x = indep_var) +
          theme_minimal() +
          theme(legend.position = "none", plot.title = element_text(hjust = 0.5),
                axis.text.x = element_text(angle = 45, hjust = 1))
      }, error = function(e) {
        showNotification(paste0("Error generating plot for ", dv, ": ", e$message), type = "error", duration = 5)
        NULL
      })
      if (!is.null(plot_dv)) {
        plots_list[[dv]] <- plot_dv
      }
    }
    
    list(posthoc_dfs = posthoc_dfs_list, plots = plots_list)
  })
  
  # 4. Post-Hoc Results Output UI
  output$posthoc_results_ui <- renderUI({
    results <- posthoc_data_and_plots()
    req(results)
    if (length(results$posthoc_dfs) == 0) {
      return(tags$p("No post-hoc results to display. (Overall MANOVA not significant or an error occurred)."))
    }
    
    # Generate a list of UI elements for each table, now using DTOutput
    lapply(names(results$posthoc_dfs), function(dv_name) {
      tagList(
        h5(paste("Dependent Variable:", dv_name)),
        DTOutput(paste0("posthoc_table_", dv_name))
      )
    })
  })
  
  # 5. Render individual Post-Hoc tables
  observeEvent(input$run_manova, {
    results <- posthoc_data_and_plots()
    req(results)
    
    lapply(names(results$posthoc_dfs), function(dv_name) {
      output[[paste0("posthoc_table_", dv_name)]] <- renderDT({
        req(results$posthoc_dfs[[dv_name]])
        datatable(as.data.frame(results$posthoc_dfs[[dv_name]]),
                  options = list(pageLength = 10, dom = 'tip'),
                  rownames = FALSE) # Added rownames = FALSE for cleaner table
      })
    })
  })
  
  # 5. Plots Output UI
  output$plot_output_ui <- renderUI({
    results <- posthoc_data_and_plots()
    req(results)
    if (length(results$plots) == 0) {
      return(tags$p("No plots to display. (Overall MANOVA not significant or an error occurred)."))
    }
    plot_outputs <- lapply(names(results$plots), function(dv) {
      tagList(
        h5(paste("Plot for:", dv)),
        plotOutput(paste0("plot_", dv), height = "400px", width = "100%")
      )
    })
    do.call(tagList, plot_outputs)
  })
  
  # 6. Render individual plots
  observeEvent(input$run_manova, {
    results <- posthoc_data_and_plots()
    req(results)
    
    lapply(names(results$plots), function(dv) {
      output[[paste0("plot_", dv)]] <- renderPlot({
        req(results$plots[[dv]])
        results$plots[[dv]] # This is the ggplot object
      })
    })
  })
    
    ## Download Capabilities
    
    # Here are the new download handlers you requested.
  
  ### Download MANOVA Summary
  
  #This will allow users to download the raw text output of the MANOVA summary.
  

  output$download_manova_results <- downloadHandler(
    filename = function() {
      paste("OneWay_MANOVA_Summary_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      res <- manova_results()
      req(res)
      # Capture the print output into a text file
      sink(file)
      cat("--- Overall One-Way MANOVA Test Results (Pillai's Trace) ---\n")
      print(summary(res$model, test = "Pillai"))
      cat("\n--- ANOVA Test Results per Response Variable ---\n")
      print(summary.aov(res$model))
      sink() # Close the sink connection
    }
  )
  
  ### Download Post-Hoc Tables
  
  #This will create an Excel file with a separate sheet for each dependent variable's post-hoc results.

    output$download_posthoc_tables <- downloadHandler(
        filename = function() {
            paste("OneWay_PostHoc_Tables_", Sys.Date(), ".xlsx", sep = "")
        },
        content = function(file) {
            results <- posthoc_data_and_plots()
            req(results$posthoc_dfs)

            # Ensure writexl is available
            if (!requireNamespace("writexl", quietly = TRUE)) {
                showNotification("The 'writexl' package is required for Excel download. Please install it.", type = "error", duration = 8)
                return()
            }

            # Create a list of data frames for writing to Excel, using DV names as sheet names
            df_list_for_excel <- lapply(names(results$posthoc_dfs), function(dv_name) {
                results$posthoc_dfs[[dv_name]]
            })
            names(df_list_for_excel) <- names(results$posthoc_dfs)

            writexl::write_xlsx(df_list_for_excel, path = file)
        }
    )

}

# Run the app
shinyApp(ui = ui, server = server)
