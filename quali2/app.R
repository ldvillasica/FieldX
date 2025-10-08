# app.R

# Load required libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(readr)
library(rlang)
library(tidyr)
library(stringr)
library(DT)

# Define UI for the dashboard
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Dynamic Plotting and Descriptive Analysis", titleWidth = 350),
  
  # Dashboard Sidebar with all inputs
  dashboardSidebar(
    width = 350,
    fileInput("file1", "Choose CSV File",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    tags$hr(),
    uiOutput("categorical_var_selector"), 
    tags$hr(),
    uiOutput("bar_orientation_selector"), 
    tags$hr(),
    helpText("Upload a CSV file and the app will automatically generate plots for all quantitative and qualitative variables.")
  ),
  
  # Dashboard Body with the main plot output
  dashboardBody(
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
          tabPanel("Descriptive Analysis",
                   downloadButton("download_descriptive_stats", "Download Analysis as CSV"),
                   tags$br(),
                   tags$br(),
                   DT::dataTableOutput("descriptive_table")
          ),
          tabPanel("Quantitative Variables",
                   h4("Quantitative Variables (Means & Density)"),
                   selectInput("quant_plot_type", "Select Plot Type:",
                               choices = c("Bar Plot" = "bar", "Density Plot" = "density")),
                   uiOutput("quantitative_plots")
          ),
          tabPanel("Qualitative Variables",
                   h4("Qualitative Variables (Counts)"),
                   selectInput("qual_plot_type", "Select Plot Type:",
                               choices = c("Bar Plot" = "bar", "Pie Chart" = "pie")),
                   uiOutput("qualitative_plots")
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
  
  observeEvent(input$file1, {
    req(input$file1)
    tryCatch({
      df <- read_csv(input$file1$datapath)
      
      # FIX: Sanitize column names immediately after loading the data
      names(df) <- make.names(names(df))
      
      data_upload(df)
      showNotification("CSV file loaded and columns sanitized!", type = "success")
    },
    error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
      data_upload(NULL)
    })
  })
  
  # Reactive expressions for quantitative and qualitative columns
  quantitative_vars <- reactive({
    df <- data_upload()
    if (is.null(df)) return(NULL)
    names(df)[sapply(df, is.numeric)]
  })
  
  qualitative_vars <- reactive({
    df <- data_upload()
    if (is.null(df)) return(NULL)
    names(df)[!sapply(df, is.numeric)]
  })
  
  descriptive_stats <- reactive({
    df <- data_upload()
    req(df)
    stats_df <- data.frame(
      Variable = character(),
      Type = character(),
      N = numeric(),
      N_NA = numeric(),
      Details = character(),
      stringsAsFactors = FALSE
    )
    for (col_name in names(df)) {
      col_data <- df[[col_name]]
      n_total <- length(col_data)
      n_na <- sum(is.na(col_data))
      details_string <- ""
      if (is.numeric(col_data)) {
        min_val <- round(min(col_data, na.rm = TRUE), 2)
        max_val <- round(max(col_data, na.rm = TRUE), 2)
        mean_val <- round(mean(col_data, na.rm = TRUE), 2)
        media_val <- round(median(col_data, na.rm = TRUE), 2)
        sd_val <- round(sd(col_data, na.rm = TRUE), 2)
        details_string <- paste0(
          "Min: ", min_val, ", ", "Max: ", max_val, ", ",
          "Mean: ", mean_val, ", ", "Median: ", media_val, ", ",
          "SD: ", sd_val
        )
        type_str <- "Numeric"
      } else {
        unique_vals <- length(unique(col_data))
        top_vals_tbl <- sort(table(col_data), decreasing = TRUE)
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
  
  output$descriptive_table <- DT::renderDataTable({
    descriptive_stats()
  }, options = list(
    pageLength = 10,
    scrollX = TRUE,
    columnDefs = list(list(width = '400px', targets = c(4)))
  ))
  
  output$download_descriptive_stats <- downloadHandler(
    filename = function() {
      paste0("descriptive_analysis_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(descriptive_stats())
      write.csv(descriptive_stats(), file, row.names = FALSE)
    }
  )
  
  output$categorical_var_selector <- renderUI({
    vars <- qualitative_vars()
    if (is.null(vars) || length(vars) == 0) return(NULL)
    selectInput("cat_var_col", "Select Categorical Variable (for Bar Plot X-axis):",
                choices = c("None" = "None", vars),
                selected = "None")
  })
  
  output$bar_orientation_selector <- renderUI({
    df <- data_upload()
    if (is.null(df)) return(NULL)
    radioButtons("bar_orientation", "Select Bar Plot Orientation:",
                 choices = c("Vertical" = "vertical", "Horizontal" = "horizontal"),
                 selected = "vertical")
  })
  
  # New: Dynamically generate plots for all quantitative variables
  output$quantitative_plots <- renderUI({
    df <- data_upload()
    vars <- quantitative_vars()
    req(df, vars, input$quant_plot_type)
    
    plot_output_list <- lapply(vars, function(var) {
      plot_id <- paste0("quant_plot_", var)
      
      # Use `local` to properly scope the variable for each plot
      local({
        my_var <- var
        output[[plot_id]] <- renderPlot({
          df_var <- df %>% select(all_of(my_var))
          
          p <- ggplot(df_var)
          
          if (input$quant_plot_type == "bar") {
            df_summary <- df_var %>%
              summarise(Mean_Value = mean(!!sym(my_var), na.rm = TRUE))
            
            p <- ggplot(df_summary, aes(x = my_var, y = Mean_Value)) +
              geom_bar(stat = "identity", fill = "#3498db") +
              labs(x = "Variable", y = "Mean Value") +
              ggtitle(paste("Mean Value for:", my_var)) +
              theme_minimal() +
              theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))
            
            if (input$bar_orientation == "horizontal") {
              p <- p + coord_flip()
            }
            
          } else if (input$quant_plot_type == "density") {
            p <- ggplot(df_var, aes(x = !!sym(my_var))) +
              geom_density(alpha = 0.6, fill = "#3498db") +
              labs(x = "Value", y = "Density") +
              ggtitle(paste("Density Plot for:", my_var)) +
              theme_minimal() +
              theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))
          }
          p
        })
      })
      plotOutput(plot_id, height = "400px")
    })
    
    do.call(tagList, plot_output_list)
  })
  
  # New: Dynamically generate plots for all qualitative variables
  output$qualitative_plots <- renderUI({
    df <- data_upload()
    vars <- qualitative_vars()
    req(df, vars, input$qual_plot_type)
    
    plot_output_list <- lapply(vars, function(var) {
      plot_id <- paste0("qual_plot_", var)
      
      # Use `local` to properly scope the variable for each plot
      local({
        my_var <- var
        output[[plot_id]] <- renderPlot({
          df_var <- df %>% select(all_of(my_var))
          
          df_summary <- df_var %>%
            group_by(Value = !!sym(my_var)) %>%
            summarise(Count = n(), .groups = 'drop')
          
          p <- ggplot(df_summary)
          
          if (input$qual_plot_type == "bar") {
            p <- p +
              geom_bar(aes(x = Value, y = Count, fill = Value), stat = "identity") +
              labs(x = my_var, y = "Count") +
              ggtitle(paste("Bar Plot for:", my_var)) +
              theme_minimal() +
              theme(
                plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
                legend.position = "bottom",
                axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
                axis.title.y = element_text(size = 14, margin = margin(r = 10))
              )
          } else if (input$qual_plot_type == "pie") {
            p <- p +
              geom_bar(aes(x = "", y = Count, fill = Value), stat = "identity", width = 1) +
              coord_polar("y", start = 0) +
              labs(fill = "Category") +
              ggtitle(paste("Pie Chart for:", my_var)) +
              theme_void() +
              theme(
                plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
                legend.position = "bottom",
                axis.text.x = element_blank()
              )
          }
          p
        })
      })
      plotOutput(plot_id, height = "400px")
    })
    
    do.call(tagList, plot_output_list)
  })
}

# Run the application
shinyApp(ui = ui, server = server)