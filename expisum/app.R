# Load required libraries. Please ensure these are installed:
# install.packages(c("shiny", "dplyr", "rstatix", "gt", "multcompView", "webshot2"))
# NOTE: The 'webshot2' package is required for PDF export via gtsave().
library(shiny)
library(dplyr) # This implicitly loads the 'magrittr' pipe operator (%>%)
library(rstatix)
library(gt)
library(multcompView)
library(webshot2) # Added for PDF export

# ----------------------------------------------------
# A helper function to perform Two-way ANOVA and generate the table data
# ----------------------------------------------------
generate_anova_table <- function(data, response_var, group_var1, group_var2) {
  # Check if the data contains the required variables and has enough observations
  if (!(response_var %in% names(data) && group_var1 %in% names(data) && group_var2 %in% names(data))) {
    return(gt(data.frame(Message = "Error: One or more selected variables are missing from the data.")))
  }
  
  # 1. Convert variables to symbols for tidy evaluation and create interaction factor
  response_sym <- sym(response_var)
  group1_sym <- sym(group_var1)
  group2_sym <- sym(group_var2)
  interaction_factor <- "Interaction_Group"
  
  # Ensure the grouping variables are treated as factors and create the combined interaction factor
  data <- data %>%
    mutate(
      !!group1_sym := as.factor(!!group1_sym),
      !!group2_sym := as.factor(!!group2_sym),
      !!sym(interaction_factor) := as.factor(paste(!!group1_sym, !!group2_sym, sep = " x "))
    )
  
  # Check for enough groups (interaction cells)
  if (n_distinct(data[[interaction_factor]]) < 2) {
    return(gt(data.frame(Message = "Error: Need at least two unique treatment combinations (interaction cells) for Two-way ANOVA.")))
  }
  
  # 2. Calculate Summary Statistics (Mean, SE, CV, N) - Grouped by both factors
  summary_data <- data %>%
    group_by(!!group1_sym, !!group2_sym, !!sym(interaction_factor)) %>%
    get_summary_stats(!!response_sym, type = "mean_sd", show = c("n", "mean", "sd")) %>%
    mutate(
      se = sd / sqrt(n), # Calculate Standard Error
      cv = (sd / mean) * 100, # Calculate Coefficient of Variation (for cell)
    ) %>%
    select(!!group1_sym, !!group2_sym, !!sym(interaction_factor), n, mean, se, cv) %>%
    rename(Factor1 = !!group1_sym, Factor2 = !!group2_sym, Group = !!sym(interaction_factor))
  
  # 3. Perform Two-way ANOVA (with interaction)
  aov_formula <- as.formula(paste(response_var, "~", group_var1, "*", group_var2))
  aov_model <- aov(aov_formula, data = data)
  
  # Calculate overall CV using Residual Standard Error (RSE) from ANOVA
  aov_summary <- summary(aov_model)[[1]]
  mse_row_index <- nrow(aov_summary)
  
  if (mse_row_index < 4) {
    return(gt(data.frame(Message = "Error: Not enough data points to calculate Two-Way ANOVA residuals.")))
  }
  
  mse <- aov_summary[["Mean Sq"]][mse_row_index]  
  rse <- sqrt(mse)
  grand_mean <- mean(data[[response_var]], na.rm = TRUE)
  overall_cv <- (rse / grand_mean) * 100
  
  # 4. Perform Post-hoc Tukey's HSD on the interaction cells
  # We use the combined interaction factor for Tukey to get all pairwise comparisons
  aov_model_for_tukey <- aov(as.formula(paste(response_var, "~", interaction_factor)), data = data)
  
  # --- CRITICAL FIX: Robust HSD Execution with Fallback ---
  tukey_hsd_safe <- tryCatch({
    # Attempt to run Tukey's HSD
    TukeyHSD(aov_model_for_tukey, which = interaction_factor)
  }, error = function(e) {
    # If HSD fails (e.g., singular fit), assign a dummy result for no separation
    warning(paste("TukeyHSD failed for", response_var, ". Assigning 'a' to all groups:", e$message))
    
    # Create dummy p-values where all are above the threshold (p=1)
    levels <- unique(data[[interaction_factor]])
    dummy_p_adj <- rep(1, length(levels) * (length(levels) - 1) / 2)
    
    # Create a dummy structure that multcompLetters can process
    dummy_hsd <- list()
    dummy_hsd[[interaction_factor]] <- data.frame(
      diff = rep(0, length(dummy_p_adj)), 
      lwr = rep(0, length(dummy_p_adj)), 
      upr = rep(0, length(dummy_p_adj)), 
      "p adj" = dummy_p_adj,
      row.names = outer(levels, levels, FUN = paste, sep = "-")[lower.tri(outer(levels, levels, FUN = paste, sep = "-"))]
    )
    return(dummy_hsd)
  })
  
  # 5. Extract Letter Designations using multcompView
  letters <- multcompLetters(
    x = tukey_hsd_safe[[interaction_factor]][, "p adj"],  
    threshold = 0.05,  
    reverse = TRUE
  )
  letter_df <- data.frame(Group = names(letters$Letters), Letter = letters$Letters)
  
  # 6. Merge all results
  final_data <- summary_data %>%
    left_join(letter_df, by = "Group") %>%
    # Create the final formatted Mean ± SE column, now including the \pm symbol directly in the string
    mutate(
      Mean_SE = paste0(
        format(round(mean, 2), nsmall = 2),  
        " $\\pm$ ", # <-- CHANGE HERE: Added \pm symbol string
        format(round(se, 2), nsmall = 2)
      ),
      # CV value only, NO PERCENT SIGN in the cell
      CV_FMT = format(round(cv, 2), nsmall = 2)  
    ) %>%
    # Select the columns for presentation
    select(Factor1, Factor2, Mean_SE, CV_FMT, Letter)
  
  # Extract P-values from the Two-way ANOVA model
  p_val_g1 <- aov_summary$`Pr(>F)`[1]  
  p_val_g2 <- aov_summary$`Pr(>F)`[2]  
  p_val_interaction <- aov_summary$`Pr(>F)`[3]
  
  # 7. Create the GT table
  gt_table <- final_data %>%
    gt(groupname_col = "Factor1") %>% # Group by the first factor for presentation
    
    # Define columns and labels
    cols_label(
      Factor1 = group_var1,
      Factor2 = group_var2,
      # UPDATED HEADER: Removed the redundant \pm symbol since it's now in the cells
      Mean_SE = paste0("Mean ", response_var, " (SE)"),
      # NEW HEADER with percent symbol
      CV_FMT = "Cell CV (\\%)",  
      Letter = "Significance Group"
    ) %>%
    
    # Add a title and subtitle
    tab_header(
      title = md("**Two-way ANOVA Summary Table**"),
      subtitle = md(paste0(
        "Response: **", response_var, "** | Factors: **", group_var1, "** and **", group_var2, "**<br>",
        "Tukey's HSD Post-hoc on Interaction Cells ($\\alpha = 0.05$)"
      ))
    ) %>%
    
    # Add p-values as footnotes
    tab_footnote(
      footnote = md(paste0(group_var1, " P-value: **", format.pval(p_val_g1, digits = 3, na.print = "NA"), "**")),
      locations = cells_title(groups = "subtitle")
    ) %>%
    tab_footnote(
      footnote = md(paste0(group_var2, " P-value: **", format.pval(p_val_g2, digits = 3, na.print = "NA"), "**")),
      locations = cells_title(groups = "subtitle")
    ) %>%
    tab_footnote(
      footnote = md(paste0("Interaction P-value: **", format.pval(p_val_interaction, digits = 3, na.print = "NA"), "**")),
      locations = cells_title(groups = "subtitle")
    ) %>%
    tab_footnote(
      footnote = md(paste0("Overall Experiment CV: ", format(round(overall_cv, 2), nsmall = 2), "\\%")),
      locations = cells_title(groups = "subtitle")
    ) %>%
    
    # 8. Format Mean_SE column as Markdown to render the \pm symbol
    fmt_markdown(columns = Mean_SE) %>%
    
    # Style the table
    cols_align(align = "center", columns = everything()) %>%
    tab_options(
      table.border.top.style = "solid",
      table.border.top.color = "#D3D3D3",
      column_labels.border.bottom.color = "black",
      column_labels.border.bottom.style = "solid"
    )
  
  return(gt_table)
}

# ----------------------------------------------------
# SHINY UI
# ----------------------------------------------------
ui <- fluidPage(
  tags$head(
    # Use a professional font and styling
    tags$link(href="https://fonts.googleapis.com/css2?family=Inter:wght@400;600&display=swap", rel="stylesheet"),
    tags$style(HTML("
        body {
          font-family: 'Inter', sans-serif;
          background-color: #f7f7f7;
        }
        .container-fluid {
          max-width: 1000px;
          margin-top: 30px;
          padding: 30px;
          background-color: white;
          border-radius: 12px;
          box-shadow: 0 6px 15px rgba(0, 0, 0, 0.1);
        }
        h1 {
          color: #1f2937;
          font-weight: 600;
          border-bottom: 2px solid #e5e7eb;
          padding-bottom: 10px;
          margin-bottom: 20px;
        }
        .btn-primary {
          background-color: #4f46e5;
          border-color: #4f46e5;
          color: white; /* Ensure text is white */
          font-weight: 600;
          padding: 8px 15px;
          border-radius: 6px;
        }
        .btn-primary:hover {
          background-color: #4338ca;
          border-color = #4338ca;
        }
      "))
  ),
  
  # Application title
  titlePanel(h1("Two-way ANOVA Presentation Table Generator")),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("1. Upload Data (CSV format)"),
      fileInput("file1",  
                "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Options for reading the file
      checkboxInput("header", "Header", TRUE),
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      hr(),
      h4("2. Select Variables"),
      
      # Inputs will be dynamically populated in the server
      uiOutput("response_var_ui"),
      uiOutput("group_vars_ui"),
      
      hr(),
      # --- NEW: Action Button to trigger calculation ---
      actionButton("calculate", 
                   "Run ANOVA Analysis", 
                   class = "btn-primary", 
                   icon = icon("calculator")),
      # ------------------------------------------------
      
      p(em("Click the button above to generate or update the tables."))
      
    ),
    
    mainPanel(
      width = 8,
      h3("Statistical Summary & Significance Grouping"),
      
      # Global download button for the combined PDF
      downloadButton("download_all_tables",  
                     "Download All Tables as Single PDF",  
                     class = "btn-primary",  
                     style="margin-bottom: 15px;"),
      
      # Output for dynamically rendered tables
      uiOutput("anova_results"), 
      br(),
      h4("Notes on Interpretation (Two-way ANOVA)"),
      tags$ul(
        tags$li(strong("Factor P-values:"), "Displayed in the table subtitle. Check the Interaction P-value first: if significant, the main effects (Factor 1 and Factor 2) must be interpreted through the interaction."),
        tags$li(strong("Mean $\\pm$ SE:"), "The average value of the response variable for each unique combination of Factor 1 and Factor 2 (the interaction cell)."),
        tags$li(strong("Significance Group:"), "Means followed by the same letter are not statistically different (p > 0.05, based on Tukey's HSD test applied to all pairwise cell comparisons."),
        tags$li(strong("Overall Experiment CV:"), "The Coefficient of Variation for the entire experiment, measuring experimental precision.")
      )
    )
  )
)

# ----------------------------------------------------
# SHINY SERVER
# ----------------------------------------------------
server <- function(input, output, session) {
  
  # 1. Reactive expression to read the uploaded data
  uploaded_data <- reactive({
    req(input$file1) # Require that a file has been uploaded
    
    tryCatch({
      df <- read.csv(input$file1$datapath,
                     header = input$header,
                     sep = input$sep,
                     stringsAsFactors = FALSE)
      # Only keep numeric and character/factor columns
      df <- df %>% select_if(function(col) is.numeric(col) || is.character(col) || is.factor(col))
      return(df)
    },
    error = function(e) {
      # Return a warning if the file couldn't be read
      validate(paste("Invalid file input or format. Check header/separator settings. Original error:", e$message))
      return(NULL)
    })
  })
  
  # 2. Dynamic UI for Response Variable selection (must be numeric)
  output$response_var_ui <- renderUI({
    df <- uploaded_data()
    req(df)
    
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    
    if (length(numeric_cols) == 0) {
      return(p("No numeric columns found for the response variable."))
    }
    
    selectInput("response_var",  
                "Select Response Variable(s) (Numeric):",  
                choices = numeric_cols,
                multiple = TRUE)  
  })
  
  # 3. Dynamic UI for Grouping Variable selection (must be character/factor)
  output$group_vars_ui <- renderUI({
    df <- uploaded_data()
    req(df)
    
    # Find suitable grouping columns (categorical or numeric with few unique values)
    grouping_cols <- names(df)[sapply(df, function(col) is.character(col) || is.factor(col) || (is.numeric(col) && length(unique(col)) < 15))]
    
    if (length(grouping_cols) < 2) {
      return(p("Need at least two categorical/grouping columns for Two-way ANOVA."))
    }
    
    tagList(
      selectInput("group_var1",  
                  "Select Factor 1 (Categorical):",  
                  choices = grouping_cols),
      selectInput("group_var2",  
                  "Select Factor 2 (Categorical):",  
                  choices = grouping_cols,
                  selected = grouping_cols[2]) # Default to the second factor
    )
  })
  
  # 4. Reactive List of Tables Generation (Generates one table per response variable)
  # Uses eventReactive to only trigger when the "calculate" button is clicked.
  results_list <- eventReactive(input$calculate, {
    
    df <- uploaded_data()
    
    # Ensure all necessary inputs are selected BEFORE running calculation
    req(df, input$response_var, input$group_var1, input$group_var2)
    
    response_vars <- input$response_var # Vector of variables
    group_var1 <- input$group_var1
    group_var2 <- input$group_var2
    
    # Initial data checks
    if (is.null(df) || nrow(df) == 0) {
      return(list("error" = gt(data.frame(Message = "Please upload a data file."))))
    }
    if (group_var1 == group_var2) {
      return(list("error" = gt(data.frame(Message = "Error: Factor 1 and Factor 2 must be different columns."))))
    }
    if (!all(response_vars %in% names(df)) || !(group_var1 %in% names(df)) || !(group_var2 %in% names(df))) {
      return(list("error" = gt(data.frame(Message = "Selected columns are invalid or missing from the data."))))
    }
    
    # Iterate over all selected response variables and generate a table for each
    results <- lapply(response_vars, function(rv) {
      generate_anova_table(
        data = df,  
        response_var = rv,  
        group_var1 = group_var1,  
        group_var2 = group_var2
      )
    })
    
    # Name the list elements using the response variable names
    names(results) <- response_vars
    
    return(results)
  })
  
  # 5. Dynamic Rendering of Multiple GT Tables
  # This output depends on results_list, so it only updates after the button is pressed.
  output$anova_results <- renderUI({
    results <- results_list()
    
    # Handle error case
    if ("error" %in% names(results)) {
      # Render the error message provided by the results list
      output$error_table <- render_gt({ results[["error"]] })
      return(gt_output("error_table"))
    }
    
    # Use tagList to return multiple HTML elements
    tagList(
      lapply(names(results), function(var_name) {
        
        output_id <- paste0("table_", gsub("[^a-zA-Z0-9]", "_", var_name))
        
        # Assign the render_gt function to the specific output slot
        output[[output_id]] <- render_gt({
          results[[var_name]]
        })
        
        # Return the UI element (gt_output) for this table
        tags$div(
          h4(paste("Results for Response Variable:", var_name),  
             style = "margin-top: 25px; border-bottom: 1px solid #e5e7eb; padding-bottom: 5px;"),
          gt_output(output_id)
        )
      })
    )
  })
  
  # 6. Global Download Handler for all tables as a single PDF
  # This output also depends on results_list, so it only works after the button is pressed.
  output$download_all_tables <- downloadHandler(
    filename = function() {
      paste0("All_ANOVA_Tables_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      results <- results_list()
      req(results)
      
      # Filter out any error results
      results <- results[names(results) != "error"]
      if (length(results) == 0) return(NULL) # Nothing to download
      
      # 1. Generate HTML for each table
      all_html_parts <- lapply(names(results), function(var_name) {
        gt_table <- results[[var_name]]
        
        # Add a wrapper for separation and a title
        title_html <- paste0("<h2>Results for Response Variable: ", var_name, "</h2>")
        table_html <- gt::as_raw_html(gt_table)
        
        # Use a <hr> separator for visual break between tables in the combined PDF
        return(paste0(title_html, table_html, "<hr style='margin: 30px 0; border: 1px solid #ddd; height: 2px;'>"))
      })
      
      # 2. Combine all HTML parts
      combined_html <- paste(all_html_parts, collapse = "\n")
      
      # Add basic HTML structure for proper rendering
      full_html <- paste0(
        "<!DOCTYPE html><html><head>",
        "<meta charset='utf-8'>",
        # Basic styling for the PDF output
        "<style>
            body { font-family: 'Inter', sans-serif; padding: 20px; }
            h2 { color: #1f2937; font-weight: 600; margin-top: 40px; }
            /* Style for gt elements inside the combined PDF */
            .gt_table { margin-bottom: 30px; }
          </style>",
        "</head><body>",
        "<h1>Consolidated ANOVA Results</h1>",
        combined_html,
        "</body></html>"
      )
      
      # 3. Write to a temporary HTML file
      temp_html_file <- tempfile(fileext = ".html")
      writeLines(full_html, temp_html_file)
      
      # 4. Use webshot to convert HTML to PDF
      # Use larger dimensions and zoom for high-quality PDF rendering of gt tables
      webshot2::webshot(url = temp_html_file,  
                        file = file,  
                        vheight = 1000,  
                        vwidth = 1200,  
                        zoom = 1.5  
      )
    },
    contentType = "application/pdf"
  )
}

# 7. Run the application 
shinyApp(ui = ui, server = server)