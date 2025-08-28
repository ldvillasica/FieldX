# app.R

# Load required libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(readr) # For read_csv
library(rlang) # For !!sym() to handle column names with spaces
library(tidyr) # For pivot_longer to reshape data
library(stringr) # For string manipulation
library(DT)    # For interactive data tables

# Define UI for the dashboard
ui <- dashboardPage(
  skin = "blue", # Set the dashboard theme color
  
  # Dashboard Header
  dashboardHeader(title = "Faceted Plot & Descriptive Analysis", titleWidth = 350),
  
  # Dashboard Sidebar with all inputs
  dashboardSidebar(
    width = 350,
    fileInput("file1", "Choose CSV File",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    tags$hr(),
    selectInput("plot_type", "Select Plot Type:",
                choices = c("Bar Plot" = "bar", "Pie Chart" = "pie", "Density Plot" = "density")),
    tags$hr(),
    uiOutput("categorical_var_selector"),
    uiOutput("response_vars_selector"),
    tags$hr(),
    uiOutput("plot_mode_selector"),
    uiOutput("bar_orientation_selector"), # New input for bar plot orientation
    tags$hr(),
    helpText("Upload a CSV file and select your variables and plot type. Density plots and Pie Charts do not use the 'Categorical Variable' selection.")
  ),
  
  # Dashboard Body with the main plot output
  dashboardBody(
    # Custom CSS to improve aesthetics
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f0f2f5;
        }
        .box {
          border-radius: 12px;
          box-shadow: 0 4px 20px rgba(0, 0, 0, 0.08);
        }
        .box-header.with-border {
          border-bottom-color: #e0e0e0;
        }
        .main-header .logo, .main-header .navbar {
          background-color: #3498db;
        }
        .sidebar-menu>li.active>a, .sidebar-menu>li:hover>a {
            border-left-color: #e74c3c;
        }
        .info-box {
            border-radius: 12px;
        }
      "))
    ),
    
    fluidRow(
      box(
        title = "Data Analysis",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        tabBox(
          width = 12,
          # Tab for Plotting
          tabPanel("Faceted Plot Visualization",
                   plotOutput("main_plot", height = "600px")
          ),
          # New Tab for Descriptive Analysis
          tabPanel("Descriptive Analysis",
                   downloadButton("download_descriptive_stats", "Download Analysis as CSV"),
                   tags$br(), # Add a line break for spacing
                   tags$br(),
                   DT::dataTableOutput("descriptive_table")
          )
        )
      )
    )
  )
)

# Define server logic required for the dashboard
server <- function(input, output, session) {
  
  # Reactive value to store the uploaded data
  data_upload <- reactiveVal(NULL)
  
  # Observe file input and read data
  observeEvent(input$file1, {
    req(input$file1) # Ensure a file is uploaded
    tryCatch({
      df <- read_csv(input$file1$datapath)
      data_upload(df)
    },
    error = function(e) {
      # Show a notification if file reading fails
      showNotification(paste("Error reading file:", e$message), type = "error")
      data_upload(NULL) # Clear data on error
    })
  })
  
  # New reactive expression for descriptive statistics with percentages
  descriptive_stats <- reactive({
    df <- data_upload()
    req(df)
    
    # Initialize an empty data frame to store results
    stats_df <- data.frame(
      Variable = character(),
      Type = character(),
      N = numeric(),
      N_NA = numeric(),
      Details = character(),
      stringsAsFactors = FALSE
    )
    
    # Loop through each column to calculate stats
    for (col_name in names(df)) {
      col_data <- df[[col_name]]
      
      n_total <- length(col_data)
      n_na <- sum(is.na(col_data))
      
      details_string <- ""
      
      if (is.numeric(col_data)) {
        # Numeric variable
        min_val <- round(min(col_data, na.rm = TRUE), 2)
        max_val <- round(max(col_data, na.rm = TRUE), 2)
        mean_val <- round(mean(col_data, na.rm = TRUE), 2)
        median_val <- round(median(col_data, na.rm = TRUE), 2)
        sd_val <- round(sd(col_data, na.rm = TRUE), 2)
        
        details_string <- paste0(
          "Min: ", min_val, ", ",
          "Max: ", max_val, ", ",
          "Mean: ", mean_val, ", ",
          "Median: ", median_val, ", ",
          "SD: ", sd_val
        )
        type_str <- "Numeric"
      } else {
        # Categorical or other non-numeric variable
        unique_vals <- length(unique(col_data))
        top_vals_tbl <- sort(table(col_data), decreasing = TRUE)
        
        # Calculate percentages for top categories
        total_non_na <- n_total - n_na
        top_vals_percent <- round((head(top_vals_tbl, 5) / total_non_na) * 100, 2)
        
        top_vals_str <- paste(
          names(head(top_vals_tbl, 5)),
          " (", top_vals_percent, "%)",
          sep = "", collapse = "; "
        )
        
        details_string <- paste0(
          "Unique Values: ", unique_vals, ", ",
          "Top 5: ", top_vals_str
        )
        type_str <- "Categorical"
      }
      
      # Add the row to the data frame
      stats_df <- bind_rows(stats_df, data.frame(
        Variable = col_name,
        Type = type_str,
        N = n_total,
        N_NA = n_na,
        Details = details_string,
        stringsAsFactors = FALSE
      ))
    }
    
    stats_df
  })
  
  # Render the descriptive statistics table
  output$descriptive_table <- DT::renderDataTable({
    descriptive_stats()
  }, options = list(
    pageLength = 10,
    scrollX = TRUE, # Enable horizontal scrolling for wide tables
    columnDefs = list(list(width = '400px', targets = c(4))) # Adjust column width for details
  ))
  
  # New download handler for the descriptive stats
  output$download_descriptive_stats <- downloadHandler(
    filename = function() {
      paste0("descriptive_analysis_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(descriptive_stats()) # Ensure the data is ready
      write.csv(descriptive_stats(), file, row.names = FALSE)
    }
  )
  
  # Render UI for categorical variable selection
  output$categorical_var_selector <- renderUI({
    df <- data_upload()
    if (is.null(df) || input$plot_type %in% c("density", "pie")) {
      return(NULL)
    }
    non_numeric_cols <- names(df)[!sapply(df, is.numeric)]
    
    selectInput("cat_var_col", "Select Categorical Variable (for X-axis):",
                choices = c("None" = "None", non_numeric_cols),
                selected = "None")
  })
  
  # Render UI for plot mode selection for bar plots
  output$plot_mode_selector <- renderUI({
    if (input$plot_type != "bar") {
      return(NULL)
    }
    radioButtons("bar_plot_mode", "Select Bar Plot Mode:",
                 choices = c("Quantitative (Mean)" = "mean", "Qualitative (Count)" = "count"),
                 selected = "mean")
  })
  
  # New: Render UI for bar plot orientation
  output$bar_orientation_selector <- renderUI({
    if (input$plot_type == "bar") {
      radioButtons("bar_orientation", "Select Bar Plot Orientation:",
                   choices = c("Vertical" = "vertical", "Horizontal" = "horizontal"),
                   selected = "vertical")
    } else {
      return(NULL)
    }
  })
  
  # Render UI for multiple response variables selection
  output$response_vars_selector <- renderUI({
    df <- data_upload()
    if (is.null(df)) {
      return(NULL)
    }
    
    # Adjust choices based on selected plot type and bar plot mode
    if (input$plot_type == "bar") {
      if (input$bar_plot_mode == "mean") {
        cols_to_select <- names(df)[sapply(df, is.numeric)]
        label_text <- "Select Quantitative Variables:"
      } else { # bar_plot_mode == "count"
        cols_to_select <- names(df)[!sapply(df, is.numeric)]
        label_text <- "Select Qualitative Variables:"
      }
    } else if (input$plot_type == "density") {
      cols_to_select <- names(df)[sapply(df, is.numeric)]
      label_text <- "Select Quantitative Variables:"
    } else { # plot_type == "pie"
      cols_to_select <- names(df)[!sapply(df, is.numeric)]
      label_text <- "Select Qualitative Variables:"
    }
    
    if(length(cols_to_select) == 0) {
      return(helpText("No suitable columns found for the selected plot type."))
    }
    
    selectInput("response_vars", label_text,
                choices = cols_to_select,
                multiple = TRUE)
  })
  
  # Reactive expression for the plot data, summarized and prepared
  plot_data <- reactive({
    df <- data_upload()
    req(df, input$response_vars, input$plot_type)
    
    # Handle different plot types and their required data types
    if (input$plot_type == "density") {
      if (any(!sapply(df[input$response_vars], is.numeric))) {
        showNotification("Density plots require numeric variables. Please re-select.", type = "error")
        return(NULL)
      }
      df_long <- df %>%
        pivot_longer(
          cols = all_of(input$response_vars),
          names_to = "Variable",
          values_to = "Value"
        )
      return(list(data = df_long, type = "density"))
    }
    
    # For Bar Plot and Pie Chart
    if (input$bar_plot_mode == "mean") {
      if (any(!sapply(df[input$response_vars], is.numeric))) {
        showNotification("Bar plots of means require numeric variables. Please re-select.", type = "error")
        return(NULL)
      }
      df_long <- df %>%
        pivot_longer(
          cols = all_of(input$response_vars),
          names_to = "Variable",
          values_to = "Value"
        )
      cat_var_selected <- input$cat_var_col != "None" && input$cat_var_col %in% names(df)
      if (cat_var_selected) {
        df_summary <- df_long %>%
          group_by(!!sym(input$cat_var_col), Variable) %>%
          summarise(Value_for_Plot = mean(Value, na.rm = TRUE), .groups = 'drop')
      } else {
        df_summary <- df_long %>%
          group_by(Variable) %>%
          summarise(Value_for_Plot = mean(Value, na.rm = TRUE), .groups = 'drop')
      }
      return(list(data = df_summary, type = "mean"))
    } else { # bar_plot_mode == "count" or plot_type == "pie"
      if (any(sapply(df[input$response_vars], is.numeric))) {
        showNotification("This plot type requires categorical variables. Please re-select.", type = "error")
        return(NULL)
      }
      df_long <- df %>%
        pivot_longer(
          cols = all_of(input$response_vars),
          names_to = "Variable",
          values_to = "Value"
        )
      cat_var_selected <- input$cat_var_col != "None" && input$cat_var_col %in% names(df) && input$plot_type == "bar"
      if (cat_var_selected) {
        df_summary <- df_long %>%
          group_by(!!sym(input$cat_var_col), Variable, Value) %>%
          summarise(Value_for_Plot = n(), .groups = 'drop')
      } else {
        df_summary <- df_long %>%
          group_by(Variable, Value) %>%
          summarise(Value_for_Plot = n(), .groups = 'drop')
      }
      
      return(list(data = df_summary, type = "count"))
    }
  })
  
  # Render the main plot
  output$main_plot <- renderPlot({
    plot_info <- plot_data()
    
    validate(
      need(!is.null(plot_info), "Please upload a CSV file and select the appropriate variables."),
      need(nrow(plot_info$data) > 0, "No data available to plot after filtering and summarizing. Check your data and column selections.")
    )
    
    df_plot <- plot_info$data
    
    # A custom labeller function to wrap long titles based on word count
    label_wrap_gen <- function(width) {
      function(labels) {
        lapply(labels, function(x) {
          words <- strsplit(x, " ")[[1]]
          if (length(words) > 20) {
            return(str_wrap(x, width = width))
          } else {
            return(x)
          }
        })
      }
    }
    
    if (input$plot_type == "bar") {
      # BAR PLOT LOGIC
      y_label <- ifelse(input$bar_plot_mode == "mean", "Mean Value", "Count")
      
      # Determine plot orientation and set up aesthetics
      if (input$bar_orientation == "vertical") {
        # Vertical Plot
        p <- ggplot(df_plot, aes(y = Value_for_Plot))
        cat_var_selected <- input$cat_var_col != "None" && input$cat_var_col %in% names(df_plot)
        
        if (input$bar_plot_mode == "mean") {
          if (cat_var_selected) {
            p <- p + geom_bar(aes(x = !!sym(input$cat_var_col), fill = !!sym(input$cat_var_col)), stat = "identity") +
              labs(x = input$cat_var_col, fill = input$cat_var_col)
          } else {
            p <- p + geom_bar(aes(x = Variable), stat = "identity", fill = "#3498db") +
              labs(x = NULL)
          }
        } else {
          if (cat_var_selected) {
            p <- p + geom_bar(aes(x = !!sym(input$cat_var_col), fill = Value), stat = "identity") +
              labs(x = input$cat_var_col, fill = "Category")
          } else {
            p <- p + geom_bar(aes(x = Value, fill = Value), stat = "identity") +
              labs(x = "Category", fill = "Category")
          }
        }
        
        p <- p + facet_wrap(~ Variable, scales = "free_y", ncol = 3, labeller = labeller(Variable = label_wrap_gen(width = 30))) +
          labs(
            title = paste("Faceted Bar Plots -", y_label),
            y = y_label
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(hjust = 0.5, size = 18, face = "bold", margin = margin(b = 15)),
            axis.title.y = element_text(size = 14, margin = margin(r = 10)),
            axis.text.y = element_text(size = 10),
            axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
            strip.text = element_text(size = 12, face = "bold", color = "white"),
            strip.background = element_rect(fill = "#2c3e50", color = NA),
            panel.spacing = unit(2, "lines"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = "bottom",
            legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 10)
          )
      } else { # Horizontal Plot
        p <- ggplot(df_plot, aes(x = Value_for_Plot)) # x and y are swapped here
        cat_var_selected <- input$cat_var_col != "None" && input$cat_var_col %in% names(df_plot)
        
        if (input$bar_plot_mode == "mean") {
          if (cat_var_selected) {
            p <- p + geom_bar(aes(y = !!sym(input$cat_var_col), fill = !!sym(input$cat_var_col)), stat = "identity") + # y and x swapped
              labs(y = input$cat_var_col, fill = input$cat_var_col)
          } else {
            p <- p + geom_bar(aes(y = Variable), stat = "identity", fill = "#3498db") + # y and x swapped
              labs(y = NULL)
          }
        } else {
          if (cat_var_selected) {
            p <- p + geom_bar(aes(y = !!sym(input$cat_var_col), fill = Value), stat = "identity") + # y and x swapped
              labs(y = input$cat_var_col, fill = "Category")
          } else {
            p <- p + geom_bar(aes(y = Value, fill = Value), stat = "identity") + # y and x swapped
              labs(y = "Category", fill = "Category")
          }
        }
        
        p <- p + facet_wrap(~ Variable, scales = "free_x", ncol = 3, labeller = labeller(Variable = label_wrap_gen(width = 30))) + # Use free_x
          labs(
            title = paste("Faceted Bar Plots -", y_label),
            x = y_label # Swap labels
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(hjust = 0.5, size = 18, face = "bold", margin = margin(b = 15)),
            axis.title.x = element_text(size = 14, margin = margin(r = 10)),
            axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10),
            strip.text = element_text(size = 12, face = "bold", color = "white"),
            strip.background = element_rect(fill = "#2c3e50", color = NA),
            panel.spacing = unit(2, "lines"),
            panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = "bottom",
            legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 10)
          )
      }
      
      p
    } else if (input$plot_type == "density") {
      # DENSITY PLOT LOGIC
      p <- ggplot(df_plot, aes(x = Value, fill = Variable)) +
        geom_density(alpha = 0.6) +
        facet_wrap(~ Variable, scales = "free", ncol = 3, labeller = labeller(Variable = label_wrap_gen(width = 30))) +
        labs(
          title = "Faceted Density Plots",
          x = "Value",
          y = "Density"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 18, face = "bold", margin = margin(b = 15)),
          strip.text = element_text(size = 12, face = "bold", color = "white"),
          strip.background = element_rect(fill = "#2c3e50", color = NA),
          panel.spacing = unit(2, "lines"),
          legend.position = "none"
        )
      
      p
    } else if (input$plot_type == "pie") {
      # PIE CHART LOGIC
      p <- ggplot(df_plot, aes(x = "", y = Value_for_Plot, fill = Value)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        facet_wrap(~ Variable, ncol = 3, labeller = labeller(Variable = label_wrap_gen(width = 30))) +
        labs(
          title = "Faceted Pie Charts - Counts",
          fill = "Category"
        ) +
        theme_void() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 18, face = "bold", margin = margin(b = 15)),
          strip.text = element_text(size = 12, face = "bold", color = "white"),
          strip.background = element_rect(fill = "#2c3e50", color = NA),
          panel.spacing = unit(2, "lines"),
          legend.position = "bottom"
        )
      
      p
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)