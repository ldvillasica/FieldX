library(shiny)
library(bslib)
library(ggplot2)
library(multcomp)     # Loaded before dplyr to avoid masking issues
library(multcompView)
library(emmeans)
library(MASS)
library(dplyr)
library(ggcorrplot)
library(factoextra)

ui <- page_sidebar(
    theme = bs_theme(bootswatch = "flatly"),
    title = "Publication Analytics Portal",
    sidebar = sidebar(
        fileInput("file", "Upload CSV File", accept = ".csv"),
        hr(),
        h5("Export Quality Settings"),
        numericInput("dpi", "Resolution (DPI):", value = 300, min = 100, max = 600),
        numericInput("p_width", "Width (inches):", value = 7, min = 3, max = 15),
        numericInput("p_height", "Height (inches):", value = 5, min = 3, max = 15)
    ),
    navset_card_tab(
        nav_panel("Data View", tableOutput("preview")),
        
        # ANOVA Tab
        nav_panel("ANOVA & Post-Hoc",
                  sidebarLayout(
                      sidebarPanel(
                          selectInput("resp", "Response (Numeric):", choices = NULL),
                          selectInput("fact1", "Factor 1 (Categorical):", choices = NULL),
                          selectInput("fact2", "Factor 2 (Optional):", choices = NULL),
                          actionButton("run_anova", "Run Analysis", class = "btn-primary")
                      ),
                      mainPanel(
                          h4("ANOVA Table"),
                          verbatimTextOutput("anova_table"),
                          h4("Compact Letter Display (Post-Hoc)"),
                          verbatimTextOutput("cld_output"),
                          plotOutput("anova_plot"),
                          downloadButton("dl_anova", "Download ANOVA Plot")
                      )
                  )
        ),
        
        # Correlation Tab
        nav_panel("Correlation",
                  sidebarLayout(
                      sidebarPanel(
                          uiOutput("corr_vars_ui"),
                          actionButton("run_corr", "Compute Correlation", class = "btn-primary")
                      ),
                      mainPanel(
                          h4("Correlation Matrix"),
                          tableOutput("corr_table"),
                          hr(),
                          h4("Correlation Plot"),
                          plotOutput("corr_plot"),
                          downloadButton("dl_corr", "Download Correlation Plot")
                      )
                  )
        ),
        
        # PCA Tab
        nav_panel("PCA",
                  sidebarLayout(
                      sidebarPanel(
                          uiOutput("pca_vars_ui"),
                          actionButton("run_pca", "Run PCA", class = "btn-primary")
                      ),
                      mainPanel(
                          h4("PCA Variance Summary"),
                          verbatimTextOutput("pca_summary"),
                          hr(),
                          h4("PCA Biplot"),
                          plotOutput("pca_plot"),
                          downloadButton("dl_pca", "Download PCA Biplot")
                      )
                  )
        )
    )
)

server <- function(input, output, session) {
    
    df <- reactive({
        req(input$file)
        read.csv(input$file$datapath, stringsAsFactors = TRUE)
    })
    
    output$preview <- renderTable({
        req(input$file)
        head(df(), 10)
    })
    
    observeEvent(df(), {
        data <- df()
        num_cols <- names(data)[sapply(data, is.numeric)]
        cat_cols <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
        
        updateSelectInput(session, "resp", choices = num_cols)
        updateSelectInput(session, "fact1", choices = cat_cols)
        updateSelectInput(session, "fact2", choices = c("None", cat_cols))
        
        output$corr_vars_ui <- renderUI({
            checkboxGroupInput("corr_vars", "Select Variables (Min 2):", choices = num_cols, selected = num_cols[1:min(3, length(num_cols))])
        })
        
        output$pca_vars_ui <- renderUI({
            checkboxGroupInput("pca_vars", "Select Variables (Min 2):", choices = num_cols, selected = num_cols[1:min(3, length(num_cols))])
        })
    })
    
    # --- ANOVA Engine ---
    anova_res <- eventReactive(input$run_anova, {
        req(input$file, input$resp, input$fact1)
        
        if (input$fact2 == "None" || input$fact2 == "") {
            f <- as.formula(paste(input$resp, "~", input$fact1))
            specs <- input$fact1
        } else {
            f <- as.formula(paste(input$resp, "~", input$fact1, "*", input$fact2))
            specs <- c(input$fact1, input$fact2)
        }
        
        fit <- aov(f, data = df())
        emm <- emmeans(fit, specs = specs)
        cld_res <- multcomp::cld(emm, Letters = letters)
        
        list(fit = fit, emm = emm, cld = cld_res, specs = specs)
    })
    
    output$anova_table <- renderPrint({
        req(anova_res())
        summary(anova_res()$fit)
    })
    
    output$cld_output <- renderPrint({
        req(anova_res())
        anova_res()$cld
    })
    
    anova_plot_obj <- reactive({
        req(anova_res())
        res <- anova_res()
        cld_data <- as.data.frame(res$cld)
        cld_data$.group <- trimws(cld_data$.group)
        
        if (length(res$specs) == 1) {
            p <- ggplot(cld_data, aes(x = .data[[res$specs[1]]], y = emmean)) +
                geom_bar(stat = "identity", fill = "#3498db", width = 0.6, alpha = 0.8) +
                geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2) +
                geom_text(aes(label = .group, y = emmean + SE), vjust = -0.5, size = 5) +
                theme_classic(base_size = 14) +
                labs(y = input$resp, x = res$specs[1])
        } else {
            p <- ggplot(cld_data, aes(x = .data[[res$specs[1]]], y = emmean, fill = .data[[res$specs[2]]])) +
                geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
                geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), position = position_dodge(0.8), width = 0.2) +
                geom_text(aes(label = .group, y = emmean + SE), position = position_dodge(0.8), vjust = -0.5, size = 4) +
                theme_classic(base_size = 14) +
                scale_fill_brewer(palette = "Set1") +
                labs(y = input$resp)
        }
        p
    })
    
    output$anova_plot <- renderPlot({ 
        req(anova_plot_obj())
        anova_plot_obj() 
    })
    
    # --- Correlation Engine ---
    corr_mat_data <- eventReactive(input$run_corr, {
        req(input$file, input$corr_vars)
        validate(
            need(length(input$corr_vars) >= 2, "Please select at least 2 numeric variables for correlation.")
        )
        sub_df <- df()[, input$corr_vars, drop = FALSE]
        cor(sub_df, use = "complete.obs")
    })
    
    output$corr_table <- renderTable({
        req(corr_mat_data())
        as.data.frame(corr_mat_data())
    }, rownames = TRUE)
    
    corr_plot_obj <- reactive({
        req(corr_mat_data())
        ggcorrplot(corr_mat_data(), method = "square", type = "lower",
                   lab = TRUE, lab_size = 4, 
                   colors = c("#e74c3c", "white", "#2ecc71"),
                   ggtheme = theme_classic(base_size = 14))
    })
    
    output$corr_plot <- renderPlot({ 
        req(corr_plot_obj())
        corr_plot_obj() 
    })
    
    # --- PCA Engine ---
    pca_res <- eventReactive(input$run_pca, {
        req(input$file, input$pca_vars)
        validate(
            need(length(input$pca_vars) >= 2, "Please select at least 2 numeric variables for PCA.")
        )
        sub_df <- df()[, input$pca_vars, drop = FALSE]
        prcomp(na.omit(sub_df), scale. = TRUE)
    })
    
    output$pca_summary <- renderPrint({
        req(pca_res())
        summary(pca_res())
    })
    
    pca_plot_obj <- reactive({
        req(pca_res())
        fviz_pca_biplot(pca_res(), geom.ind = "point",
                        col.var = "contrib",
                        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                        repel = TRUE,
                        ggtheme = theme_classic(base_size = 14))
    })
    
    output$pca_plot <- renderPlot({ 
        req(pca_plot_obj())
        pca_plot_obj() 
    })
    
    # --- Publication-Ready Download Handlers ---
    output$dl_anova <- downloadHandler(
        filename = function() { paste0("ANOVA_Plot_", Sys.Date(), ".png") },
        content = function(file) {
            ggsave(file, plot = anova_plot_obj(), dpi = input$dpi, 
                   width = input$p_width, height = input$p_height, units = "in")
        }
    )
    
    output$dl_corr <- downloadHandler(
        filename = function() { paste0("Correlation_Plot_", Sys.Date(), ".png") },
        content = function(file) {
            ggsave(file, plot = corr_plot_obj(), dpi = input$dpi, 
                   width = input$p_width, height = input$p_height, units = "in")
        }
    )
    
    output$dl_pca <- downloadHandler(
        filename = function() { paste0("PCA_Biplot_", Sys.Date(), ".png") },
        content = function(file) {
            ggsave(file, plot = pca_plot_obj(), dpi = input$dpi, 
                   width = input$p_width, height = input$p_height, units = "in")
        }
    )
}

shinyApp(ui, server)