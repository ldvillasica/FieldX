# app.R 

library(shiny) 
library(ggplot2) 
library(dplyr) 
library(readr) # For read_csv 
library(rlang) # For !!sym() to handle column names with spaces 
library(tidyr) # For pivot_longer to reshape data 
library(stringr) # For string manipulation 

# Define UI for application that draws a barplot with facets 
ui <- fluidPage( 
    # Add a meta tag for responsive design 
    tags$head( 
        tags$meta(name="viewport", content="width=device-width, initial-scale=1.0"), 
        # Custom CSS for better aesthetics and responsiveness 
        tags$style(HTML(" 
       body { 
         font-family: 'Inter', sans-serif; 
         background-color: #f0f2f5; 
         color: #333; 
         padding: 20px; 
       } 
       .container-fluid { 
         max-width: 1200px; 
         margin: auto; 
         background-color: #ffffff; 
         border-radius: 12px; 
         box-shadow: 0 4px 20px rgba(0, 0, 0, 0.08); 
         padding: 30px; 
       } 
       h1 { 
         color: #2c3e50; 
         text-align: center; 
         margin-bottom: 30px; 
         font-weight: 700; 
       } 
       .well { 
         background-color: #ecf0f1; 
         border: 1px solid #bdc3c7; 
         border-radius: 8px; 
         padding: 20px; 
         margin-bottom: 20px; 
       } 
       .form-group label { 
         font-weight: 600; 
         margin-bottom: 8px; 
         display: block; 
       } 
       .form-control { 
         border-radius: 6px; 
         border: 1px solid #ced4da; 
         padding: 10px 15px; 
         box-shadow: inset 0 1px 2px rgba(0,0,0,.075); 
       } 
       .btn-primary { 
         background-color: #3498db; 
         border-color: #3498db; 
         border-radius: 8px; 
         padding: 10px 20px; 
         font-weight: 600; 
         transition: background-color 0.3s ease, border-color 0.3s ease; 
       } 
       .btn-primary:hover { 
         background-color: #2980b9; 
         border-color: #2980b9; 
       } 
       .shiny-plot-output { 
         border: 1px solid #e0e0e0; 
         border-radius: 8px; 
         overflow: hidden; 
         background-color: #fdfdfd; 
       } 
       .col-sm-4, .col-sm-8 { 
         padding: 15px; /* Add padding for better spacing on smaller screens */ 
       } 
       @media (max-width: 768px) { 
         .col-sm-4, .col-sm-8 { 
           width: 100%; 
           float: none; 
         } 
         .container-fluid { 
           padding: 15px; 
         } 
       } 
     ")) 
    ), 
    
    titlePanel("Faceted Plot Generator for Multiple Variables"), 
    
    sidebarLayout( 
        sidebarPanel( 
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
            uiOutput("plot_mode_selector"), # New selector for bar plot mode 
            tags$hr(), 
            helpText("Upload a CSV file and select your variables and plot type. Density plots and Pie Charts do not use the 'Categorical Variable' selection.") 
        ), 
        
        mainPanel( 
            plotOutput("main_plot", height = "600px") 
        ) 
    ) 
) 

# Define server logic required to draw a barplot 
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
        
        if (input$plot_type == "bar") { 
            # BAR PLOT LOGIC 
            y_label <- ifelse(input$bar_plot_mode == "mean", "Mean Value", "Count") 
            
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
            
            p + facet_wrap(~ Variable, scales = "free_y", ncol = 3) + 
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
                    panel.spacing = unit(1.5, "lines"), 
                    panel.grid.major.x = element_blank(), 
                    panel.grid.minor = element_blank(), 
                    legend.position = "bottom", 
                    legend.title = element_text(size = 12, face = "bold"), 
                    legend.text = element_text(size = 10) 
                ) 
        } else if (input$plot_type == "density") { 
            # DENSITY PLOT LOGIC 
            p <- ggplot(df_plot, aes(x = Value, fill = Variable)) + 
                geom_density(alpha = 0.6) + 
                facet_wrap(~ Variable, scales = "free", ncol = 3) + 
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
                    panel.spacing = unit(1.5, "lines"), 
                    legend.position = "none" 
                ) 
            
            p 
        } else if (input$plot_type == "pie") { 
            # PIE CHART LOGIC 
            p <- ggplot(df_plot, aes(x = "", y = Value_for_Plot, fill = Value)) + 
                geom_bar(stat = "identity", width = 1) + 
                coord_polar("y", start = 0) + 
                facet_wrap(~ Variable, ncol = 3) + 
                labs( 
                    title = "Faceted Pie Charts - Counts", 
                    fill = "Category" 
                ) + 
                theme_void() + 
                theme( 
                    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", margin = margin(b = 15)), 
                    strip.text = element_text(size = 12, face = "bold", color = "white"), 
                    strip.background = element_rect(fill = "#2c3e50", color = NA), 
                    panel.spacing = unit(1.5, "lines"), 
                    legend.position = "bottom" 
                ) 
            
            p 
        } 
    }) 
} 

# Run the application 
shinyApp(ui = ui, server = server)