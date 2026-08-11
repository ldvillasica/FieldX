library(shiny)
library(bslib)
library(tidyverse)
library(flextable)
library(readxl)
library(multcompView)

# --- Helper Functions ---

# 1. Compute Post-Hoc Compact Letter Display (CLD)
get_cld <- function(df, group_var, num_var, posthoc_type) {
    if (posthoc_type == "none") return(NULL)
    
    tryCatch({
        f_formula <- as.formula(paste0("`", num_var, "` ~ `", group_var, "`"))
        
        if (posthoc_type == "tukey") {
            fit <- aov(f_formula, data = df)
            thsd <- TukeyHSD(fit)
            p_vals <- thsd[[1]][, "p adj"]
            return(multcompLetters(p_vals)$Letters)
        } else if (posthoc_type == "lsd") {
            ptest <- pairwise.t.test(df[[num_var]], df[[group_var]], p.adjust.method = "none")
            p_mat <- ptest$p.value
            p_vec <- c()
            for (r in rownames(p_mat)) {
                for (c in colnames(p_mat)) {
                    if (!is.na(p_mat[r, c])) {
                        p_vec[paste0(r, "-", c)] <- p_mat[r, c]
                    }
                }
            }
            return(multcompLetters(p_vec)$Letters)
        }
    }, error = function(e) {
        return(NULL)
    })
}

# 2. Build Transposed Data Frame with CLD Letters and Mean CV Row
build_transposed_df <- function(df, group_var, num_vars, anova_type, posthoc_type) {
    group_levels <- unique(df[[group_var]]) %>% stats::na.omit()
    result_rows <- list()
    
    # Calculate letters for each variable
    letters_list <- list()
    for (v in num_vars) {
        letters_list[[v]] <- get_cld(df, group_var, v, posthoc_type)
    }
    
    # Calculate Mean ± SD + Letter per group level
    for (g in group_levels) {
        sub_df <- df[df[[group_var]] == g, , drop = FALSE]
        row_data <- list()
        row_data[[group_var]] <- as.character(g)
        
        for (v in num_vars) {
            vals <- na.omit(sub_df[[v]])
            m <- mean(vals)
            s <- sd(vals)
            
            # Retrieve letter for group g
            g_let <- if (!is.null(letters_list[[v]]) && as.character(g) %in% names(letters_list[[v]])) {
                paste0(" ", letters_list[[v]][[as.character(g)]])
            } else {
                ""
            }
            
            row_data[[v]] <- if (is.na(m)) "N/A" else sprintf("%.2f ± %.2f%s", m, ifelse(is.na(s), 0, s), g_let)
        }
        result_rows[[length(result_rows) + 1]] <- as.data.frame(row_data, stringsAsFactors = FALSE)
    }
    
    transposed_df <- bind_rows(result_rows)
    
    # ANOVA p-value Row
    pval_row <- list()
    pval_row[[group_var]] <- "p-value (F-test)"
    
    # Mean CV Row
    cv_row <- list()
    cv_row[[group_var]] <- "Mean CV (%)"
    
    for (v in num_vars) {
        # 1. Compute p-value
        f_formula <- as.formula(paste0("`", v, "` ~ `", group_var, "`"))
        p_val <- tryCatch({
            if (anova_type == "aov") {
                fit <- aov(f_formula, data = df)
                summary(fit)[[1]][["Pr(>F)"]][1]
            } else {
                fit <- oneway.test(f_formula, data = df, var.equal = FALSE)$p.value
            }
        }, error = function(e) NA)
        
        pval_row[[v]] <- if (is.na(p_val)) "N/A" else if (p_val < 0.001) "< 0.001" else sprintf("%.3f", p_val)
        
        # 2. Compute Average CV across grouping variable
        group_cvs <- sapply(group_levels, function(g) {
            vals <- na.omit(df[df[[group_var]] == g, v, drop = TRUE])
            m <- mean(vals)
            s <- sd(vals)
            if (!is.na(m) && m != 0 && !is.na(s)) (s / m) * 100 else NA
        })
        
        avg_cv <- mean(group_cvs, na.rm = TRUE)
        cv_row[[v]] <- if (is.na(avg_cv)) "N/A" else sprintf("%.2f%%", avg_cv)
    }
    
    bind_rows(
        transposed_df, 
        as.data.frame(pval_row, stringsAsFactors = FALSE), 
        as.data.frame(cv_row, stringsAsFactors = FALSE)
    )
}

# 3. Format Flextable for Display & Word Export
build_ft <- function(df_final, group_var, show_cv, posthoc_type) {
    if (!show_cv) {
        df_final <- df_final[df_final[[group_var]] != "Mean CV (%)", , drop = FALSE]
    }
    
    total_rows <- nrow(df_final)
    summary_start <- if (show_cv) total_rows - 1 else total_rows
    
    ft <- flextable(df_final) %>%
        theme_booktabs() %>%
        autofit() %>%
        align(align = "center", part = "all") %>%
        align(j = 1, align = "left", part = "all") %>%
        bold(i = summary_start:total_rows, part = "body") %>%
        bold(part = "header")
    
    if (posthoc_type != "none") {
        test_label <- if (posthoc_type == "tukey") "Tukey's HSD" else "Fisher's LSD"
        ft <- ft %>% add_footer_lines(
            values = paste0("Means sharing the same letter within a column are not significantly different (", test_label, ", p < 0.05).")
        )
    }
    
    ft
}

# --- UI Definition ---
ui <- page_sidebar(
    theme = bs_theme(version = 5, bootswatch = "flatly"),
    title = "Descriptive Analysis & ANOVA App",
    
    sidebar = sidebar(
        title = "Controls",
        
        # 1. Upload File
        fileInput("file", "1. Upload File (.csv or .xlsx)", accept = c(".csv", ".xlsx")),
        hr(),
        
        # 2. Dynamic Inputs
        uiOutput("group_var_ui"),
        uiOutput("num_vars_ui"),
        hr(),
        
        # 3. Test & Metric Settings
        selectInput(
            "anova_type",
            "Statistical Test (p-value):",
            choices = c(
                "One-Way ANOVA F-test (Equal Variance)" = "aov",
                "Welch's ANOVA F-test (Unequal Variance)" = "oneway.test"
            )
        ),
        selectInput(
            "posthoc_type",
            "Post-Hoc Lettering Test:",
            choices = c(
                "Tukey's HSD Test (p < 0.05)" = "tukey",
                "Fisher's LSD Test (p < 0.05)" = "lsd",
                "None" = "none"
            )
        ),
        checkboxInput("show_cv", "Include Mean CV (%) Row", value = TRUE),
        hr(),
        
        # 4. Exports
        h5("Export Options"),
        downloadButton("download_doc", "Download Table (.docx)", class = "btn-primary w-100 mb-2"),
        downloadButton("download_plot", "Download Plot (.png)", class = "btn-secondary w-100")
    ),
    
    navset_card_tab(
        nav_panel("Summary Table", uiOutput("ft_table")),
        nav_panel(
            "Mean Comparison Plot",
            div(style = "width: 300px; margin-bottom: 15px;", uiOutput("plot_var_ui")),
            plotOutput("mean_plot", height = "450px")
        ),
        nav_panel("Data Preview", tableOutput("data_preview"))
    )
)

# --- Server Logic ---
server <- function(input, output, session) {
    
    raw_data <- reactive({
        req(input$file)
        ext <- tools::file_ext(input$file$name)
        switch(ext,
               csv = read.csv(input$file$datapath, stringsAsFactors = FALSE),
               xlsx = readxl::read_excel(input$file$datapath),
               validate("Invalid file type; please upload a .csv or .xlsx file.")
        )
    })
    
    output$group_var_ui <- renderUI({
        req(raw_data())
        selectInput("group_var", "2. Grouping Variable:", choices = names(raw_data()))
    })
    
    output$num_vars_ui <- renderUI({
        req(raw_data())
        num_cols <- names(raw_data())[sapply(raw_data(), is.numeric)]
        selectInput(
            "num_vars", 
            "3. Numeric Variables to Compare:", 
            choices = num_cols, 
            multiple = TRUE, 
            selected = num_cols[1:min(3, length(num_cols))]
        )
    })
    
    output$plot_var_ui <- renderUI({
        req(input$num_vars)
        selectInput("plot_var", "Variable to Visualize:", choices = input$num_vars)
    })
    
    output$data_preview <- renderTable({
        req(raw_data())
        head(raw_data(), 10)
    })
    
    # Reactive Flextable Output
    ft_obj <- reactive({
        req(raw_data(), input$group_var, input$num_vars)
        transposed_df <- build_transposed_df(
            df = raw_data(), 
            group_var = input$group_var, 
            num_vars = input$num_vars, 
            anova_type = input$anova_type,
            posthoc_type = input$posthoc_type
        )
        build_ft(
            df_final = transposed_df, 
            group_var = input$group_var, 
            show_cv = input$show_cv,
            posthoc_type = input$posthoc_type
        )
    })
    
    # Render HTML Flextable in App
    output$ft_table <- renderUI({
        req(ft_obj())
        htmltools_value(ft_obj())
    })
    
    # Render Plot
    output$mean_plot <- renderPlot({
        req(raw_data(), input$group_var, input$plot_var)
        
        ggplot(raw_data(), aes(x = .data[[input$group_var]], y = .data[[input$plot_var]], fill = .data[[input$group_var]])) +
            stat_summary(fun = mean, geom = "col", alpha = 0.8, width = 0.5) +
            stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15, color = "black") +
            labs(
                title = paste("Mean", input$plot_var, "by", input$group_var),
                subtitle = "Error bars represent Standard Error of the Mean (SEM)",
                x = input$group_var,
                y = input$plot_var
            ) +
            theme_minimal(base_size = 14) +
            theme(legend.position = "none")
    })
    
    # Download Handlers
    output$download_doc <- downloadHandler(
        filename = function() { paste0("summary_table_", Sys.Date(), ".docx") },
        content = function(file) {
            req(ft_obj())
            save_as_docx(ft_obj(), path = file)
        }
    )
    
    output$download_plot <- downloadHandler(
        filename = function() { paste0("mean_plot_", input$plot_var, "_", Sys.Date(), ".png") },
        content = function(file) {
            req(raw_data(), input$group_var, input$plot_var)
            p <- ggplot(raw_data(), aes(x = .data[[input$group_var]], y = .data[[input$plot_var]], fill = .data[[input$group_var]])) +
                stat_summary(fun = mean, geom = "col", alpha = 0.8, width = 0.5) +
                stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15, color = "black") +
                labs(
                    title = paste("Mean", input$plot_var, "by", input$group_var),
                    subtitle = "Error bars represent Standard Error of the Mean (SEM)",
                    x = input$group_var,
                    y = input$plot_var
                ) +
                theme_minimal(base_size = 14) +
                theme(legend.position = "none")
            
            ggsave(file, plot = p, width = 7, height = 5, dpi = 300)
        }
    )
}

shinyApp(ui = ui, server = server)