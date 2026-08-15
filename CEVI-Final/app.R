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
    }, error = function(e) NULL)
}

# 2. Build Transposed Data Frame for Descriptive Summary Table
build_transposed_df <- function(df, group_var, num_vars, anova_type, posthoc_type) {
    group_levels <- unique(df[[group_var]]) %>% stats::na.omit()
    result_rows <- list()
    
    letters_list <- list()
    for (v in num_vars) {
        letters_list[[v]] <- get_cld(df, group_var, v, posthoc_type)
    }
    
    for (g in group_levels) {
        sub_df <- df[df[[group_var]] == g, , drop = FALSE]
        row_data <- list()
        row_data[[group_var]] <- as.character(g)
        
        for (v in num_vars) {
            vals <- na.omit(sub_df[[v]])
            m <- mean(vals)
            s <- sd(vals)
            
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
    
    pval_row <- list()
    pval_row[[group_var]] <- "p-value (F-test)"
    
    cv_row <- list()
    cv_row[[group_var]] <- "Mean CV (%)"
    
    for (v in num_vars) {
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

# 3. Dedicated CVI Summary Table Builder (With Vulnerability Risk Remark Column)
build_cvi_summary_df <- function(df, group_var, anova_type, posthoc_type) {
    group_levels <- unique(df[[group_var]]) %>% stats::na.omit()
    letters_vec <- get_cld(df, group_var, "CVI", posthoc_type)
    
    result_rows <- list()
    
    for (g in group_levels) {
        sub_df <- df[df[[group_var]] == g, , drop = FALSE]
        vals <- na.omit(sub_df$CVI)
        m <- mean(vals)
        s <- sd(vals)
        
        g_let <- if (!is.null(letters_vec) && as.character(g) %in% names(letters_vec)) {
            paste0(" ", letters_vec[[as.character(g)]])
        } else {
            ""
        }
        
        # Determine 3-Tier Remark based on Mean CVI
        remark <- if (is.na(m)) "N/A" else if (m <= 33.33) "Low Vulnerability" else if (m <= 66.66) "Moderate Vulnerability" else "High Vulnerability"
        
        row_data <- list()
        row_data[[group_var]] <- as.character(g)
        row_data[["Mean CVI ± SD"]] <- if (is.na(m)) "N/A" else sprintf("%.2f ± %.2f%s", m, ifelse(is.na(s), 0, s), g_let)
        row_data[["Vulnerability Risk Remark"]] <- remark
        
        result_rows[[length(result_rows) + 1]] <- as.data.frame(row_data, stringsAsFactors = FALSE)
    }
    
    transposed_df <- bind_rows(result_rows)
    
    # Calculate ANOVA p-value
    f_formula <- as.formula(paste0("CVI ~ `", group_var, "`"))
    p_val <- tryCatch({
        if (anova_type == "aov") {
            fit <- aov(f_formula, data = df)
            summary(fit)[[1]][["Pr(>F)"]][1]
        } else {
            fit <- oneway.test(f_formula, data = df, var.equal = FALSE)$p.value
        }
    }, error = function(e) NA)
    
    p_str <- if (is.na(p_val)) "N/A" else if (p_val < 0.001) "< 0.001" else sprintf("%.3f", p_val)
    
    # Calculate Mean CV (%)
    group_cvs <- sapply(group_levels, function(g) {
        vals <- na.omit(df[df[[group_var]] == g, "CVI", drop = TRUE])
        m <- mean(vals)
        s <- sd(vals)
        if (!is.na(m) && m != 0 && !is.na(s)) (s / m) * 100 else NA
    })
    avg_cv <- mean(group_cvs, na.rm = TRUE)
    cv_str <- if (is.na(avg_cv)) "N/A" else sprintf("%.2f%%", avg_cv)
    
    summary_rows <- data.frame(
        x1 = c("p-value (F-test)", "Mean CV (%)"),
        x2 = c(p_str, cv_str),
        x3 = c("-", "-"),
        stringsAsFactors = FALSE
    )
    names(summary_rows) <- c(group_var, "Mean CVI ± SD", "Vulnerability Risk Remark")
    
    bind_rows(transposed_df, summary_rows)
}

# 4. CVI Flextable Formatting
build_cvi_ft <- function(df_final, group_var, show_cv, posthoc_type) {
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
        bold(j = "Vulnerability Risk Remark", part = "body") %>%
        bold(i = summary_start:total_rows, part = "body") %>%
        bold(part = "header")
    
    test_label <- if (posthoc_type == "tukey") "Tukey's HSD" else if (posthoc_type == "lsd") "Fisher's LSD" else "ANOVA"
    
    ft <- ft %>% add_footer_lines(
        values = c(
            "CVI scale: Low (0.00 - 33.33), Moderate (33.34 - 66.66), High (66.67 - 100.00).",
            if (posthoc_type != "none") paste0("Means sharing the same letter within a column are not significantly different (", test_label, ", p < 0.05).") else NULL
        )
    )
    
    ft
}

# 5. Descriptive Summary Flextable
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

# 6. Correlation Matrix Data & Flextable
build_cor_df <- function(df, num_vars, method) {
    n <- length(num_vars)
    cor_mat <- matrix("", nrow = n, ncol = n)
    colnames(cor_mat) <- num_vars
    rownames(cor_mat) <- num_vars
    
    for (i in 1:n) {
        for (j in 1:n) {
            if (i == j) {
                cor_mat[i, j] <- "1.00"
            } else {
                v1 <- num_vars[i]
                v2 <- num_vars[j]
                valid_idx <- complete.cases(df[[v1]], df[[v2]])
                x <- df[[v1]][valid_idx]
                y <- df[[v2]][valid_idx]
                
                if (length(x) > 3) {
                    ct <- tryCatch(cor.test(x, y, method = method), error = function(e) NULL)
                    if (!is.null(ct)) {
                        r <- ct$estimate
                        p <- ct$p.value
                        stars <- if (p < 0.001) "***" else if (p < 0.01) "**" else if (p < 0.05) "*" else ""
                        cor_mat[i, j] <- sprintf("%.2f%s", r, stars)
                    } else {
                        cor_mat[i, j] <- "N/A"
                    }
                } else {
                    cor_mat[i, j] <- "N/A"
                }
            }
        }
    }
    
    cor_df <- as.data.frame(cor_mat, stringsAsFactors = FALSE)
    tibble::rownames_to_column(cor_df, var = "Variable")
}

build_cor_ft <- function(cor_df, method) {
    method_label <- if (method == "pearson") "Pearson" else "Spearman"
    
    flextable(cor_df) %>%
        theme_booktabs() %>%
        autofit() %>%
        align(align = "center", part = "all") %>%
        align(j = 1, align = "left", part = "all") %>%
        bold(part = "header") %>%
        bold(j = 1, part = "body") %>%
        add_footer_lines(
            values = paste0(method_label, " correlation coefficients (* p < 0.05, ** p < 0.01, *** p < 0.001).")
        )
}

# 7. Run PCA Analysis
run_pca <- function(df, num_vars, scale_data = TRUE) {
    clean_df <- na.omit(df[, num_vars, drop = FALSE])
    var_check <- apply(clean_df, 2, sd, na.rm = TRUE)
    clean_df <- clean_df[, var_check > 0, drop = FALSE]
    
    if (ncol(clean_df) < 2) return(NULL)
    
    prcomp(clean_df, scale. = scale_data, center = TRUE)
}

# 8. Combined PCA Flextable
build_combined_pca_ft <- function(pca_res) {
    if (is.null(pca_res)) return(NULL)
    
    imp <- summary(pca_res)$importance
    loadings <- as.data.frame(pca_res$rotation)
    
    num_pcs <- min(ncol(loadings), 5)
    pc_cols <- paste0("PC", 1:num_pcs)
    
    loadings_sub <- loadings[, pc_cols, drop = FALSE]
    loadings_df <- as.data.frame(loadings_sub) %>%
        rownames_to_column(var = "Metric / Variable") %>%
        mutate(across(all_of(pc_cols), ~ sprintf("%.3f", .x)))
    
    std_dev <- sprintf("%.3f", imp[1, pc_cols])
    var_pct <- sprintf("%.2f%%", imp[2, pc_cols] * 100)
    cum_pct <- sprintf("%.2f%%", imp[3, pc_cols] * 100)
    
    summary_rows <- data.frame(
        `Metric / Variable` = c("Standard Deviation", "Variance Explained (%)", "Cumulative Variance (%)"),
        stringsAsFactors = FALSE
    )
    
    for (i in 1:num_pcs) {
        col_name <- pc_cols[i]
        summary_rows[[col_name]] <- c(std_dev[i], var_pct[i], cum_pct[i])
    }
    
    combined_df <- bind_rows(loadings_df, summary_rows)
    n_loadings <- nrow(loadings_df)
    total_rows <- nrow(combined_df)
    
    flextable(combined_df) %>%
        theme_booktabs() %>%
        autofit() %>%
        align(align = "center", part = "all") %>%
        align(j = 1, align = "left", part = "all") %>%
        bold(part = "header") %>%
        bold(j = 1, part = "body") %>%
        bold(i = (n_loadings + 1):total_rows, part = "body") %>%
        hline(i = n_loadings, border = officer::fp_border(color = "black", width = 1.5)) %>%
        add_footer_lines(
            values = paste0("Variable loadings and variance components limited to top ", num_pcs, " principal components.")
        )
}

# 9. Custom Robust Ellipse Generator
get_robust_ellipse_df <- function(scores, group_var, pc_x, pc_y, level = 0.95, n_points = 100) {
    scale_factor <- sqrt(qchisq(level, df = 2))
    
    global_x_span <- diff(range(scores[[pc_x]], na.rm = TRUE))
    global_y_span <- diff(range(scores[[pc_y]], na.rm = TRUE))
    if (is.na(global_x_span) || global_x_span == 0) global_x_span <- 1
    if (is.na(global_y_span) || global_y_span == 0) global_y_span <- 1
    
    min_rx <- global_x_span * 0.08
    min_ry <- global_y_span * 0.08
    
    group_levels <- unique(scores[[group_var]]) %>% stats::na.omit()
    ellipse_list <- list()
    
    for (g in group_levels) {
        sub_df <- scores[scores[[group_var]] == g, , drop = FALSE]
        x <- sub_df[[pc_x]]
        y <- sub_df[[pc_y]]
        n <- length(x)
        cx <- mean(x, na.rm = TRUE)
        cy <- mean(y, na.rm = TRUE)
        
        use_fallback <- FALSE
        
        if (n >= 3) {
            cov_mat <- tryCatch(cov(cbind(x, y)), error = function(e) NULL)
            if (is.null(cov_mat) || any(is.na(cov_mat)) || any(is.infinite(cov_mat))) {
                use_fallback <- TRUE
            } else {
                eig <- tryCatch(eigen(cov_mat), error = function(e) NULL)
                if (is.null(eig) || any(eig$values <= 1e-6)) {
                    use_fallback <- TRUE
                } else {
                    theta <- seq(0, 2 * pi, length.out = n_points)
                    circle <- rbind(cos(theta), sin(theta))
                    transform_mat <- eig$vectors %*% diag(sqrt(eig$values))
                    ellipse_coords <- t(c(cx, cy) + scale_factor * (transform_mat %*% circle))
                    
                    ellipse_list[[as.character(g)]] <- data.frame(
                        x_col = ellipse_coords[, 1],
                        y_col = ellipse_coords[, 2],
                        Group = g,
                        stringsAsFactors = FALSE
                    )
                }
            }
        } else {
            use_fallback <- TRUE
        }
        
        if (use_fallback) {
            if (n == 2) {
                dx <- x[2] - x[1]
                dy <- y[2] - y[1]
                dist <- sqrt(dx^2 + dy^2)
                angle <- atan2(dy, dx)
                
                rx <- max(dist / 2 * 1.4, min_rx)
                ry <- max(min_ry, rx * 0.4)
                
                theta <- seq(0, 2 * pi, length.out = n_points)
                raw_circle <- rbind(rx * cos(theta), ry * sin(theta))
                rot_mat <- matrix(c(cos(angle), sin(angle), -sin(angle), cos(angle)), nrow = 2)
                ellipse_coords <- t(c(cx, cy) + rot_mat %*% raw_circle)
                
                ellipse_list[[as.character(g)]] <- data.frame(
                    x_col = ellipse_coords[, 1],
                    y_col = ellipse_coords[, 2],
                    Group = g,
                    stringsAsFactors = FALSE
                )
            } else {
                rx <- min_rx
                ry <- min_ry
                theta <- seq(0, 2 * pi, length.out = n_points)
                ellipse_coords <- t(c(cx, cy) + rbind(rx * cos(theta), ry * sin(theta)))
                
                ellipse_list[[as.character(g)]] <- data.frame(
                    x_col = ellipse_coords[, 1],
                    y_col = ellipse_coords[, 2],
                    Group = g,
                    stringsAsFactors = FALSE
                )
            }
        }
    }
    
    if (length(ellipse_list) == 0) return(NULL)
    
    res_df <- bind_rows(ellipse_list)
    names(res_df) <- c(pc_x, pc_y, group_var)
    res_df
}

# 10. PCA Biplot Visualization
build_pca_biplot <- function(pca_res, df, group_var, pc_x = "PC1", pc_y = "PC2", show_ellipse = TRUE) {
    if (is.null(pca_res)) return(NULL)
    
    valid_rows <- complete.cases(df[, names(pca_res$center)])
    sub_df <- df[valid_rows, ]
    
    scores <- as.data.frame(pca_res$x)
    scores[[group_var]] <- as.factor(sub_df[[group_var]])
    
    imp <- summary(pca_res)$importance
    var_x <- sprintf("%.1f%%", imp[2, pc_x] * 100)
    var_y <- sprintf("%.1f%%", imp[2, pc_y] * 100)
    
    loadings <- as.data.frame(pca_res$rotation)
    mult <- min(
        (max(scores[[pc_x]]) - min(scores[[pc_x]])) / (max(loadings[[pc_x]]) - min(loadings[[pc_x]])),
        (max(scores[[pc_y]]) - min(scores[[pc_y]])) / (max(loadings[[pc_y]]) - min(loadings[[pc_y]]))
    ) * 0.75
    
    loadings_scaled <- loadings %>%
        mutate(
            xend = .data[[pc_x]] * mult,
            yend = .data[[pc_y]] * mult,
            Variable = rownames(loadings)
        )
    
    p <- ggplot() +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray75") +
        geom_vline(xintercept = 0, linetype = "dashed", color = "gray75")
    
    if (show_ellipse) {
        ellipse_df <- get_robust_ellipse_df(scores, group_var, pc_x, pc_y, level = 0.95)
        
        if (!is.null(ellipse_df)) {
            p <- p + 
                geom_polygon(
                    data = ellipse_df,
                    aes(x = .data[[pc_x]], y = .data[[pc_y]], fill = .data[[group_var]], group = .data[[group_var]]),
                    alpha = 0.20,
                    color = NA
                ) +
                geom_path(
                    data = ellipse_df,
                    aes(x = .data[[pc_x]], y = .data[[pc_y]], color = .data[[group_var]], group = .data[[group_var]]),
                    linewidth = 0.9
                )
        }
    }
    
    p <- p +
        geom_point(
            data = scores,
            aes(x = .data[[pc_x]], y = .data[[pc_y]], color = .data[[group_var]], shape = .data[[group_var]]),
            size = 3.5,
            alpha = 0.9
        ) +
        geom_segment(
            data = loadings_scaled,
            aes(x = 0, y = 0, xend = xend, yend = yend),
            arrow = arrow(length = unit(0.25, "cm")),
            color = "#B22222",
            linewidth = 0.9
        ) +
        geom_text(
            data = loadings_scaled,
            aes(x = xend * 1.12, y = yend * 1.12, label = Variable),
            color = "#8B0000",
            fontface = "bold",
            size = 4.5
        ) +
        labs(
            title = "Principal Component Analysis (PCA) Biplot",
            subtitle = paste("Observations grouped by:", group_var),
            x = paste0(pc_x, " (", var_x, " variance)"),
            y = paste0(pc_y, " (", var_y, " variance)"),
            color = group_var,
            fill = group_var,
            shape = group_var
        ) +
        theme_minimal(base_size = 14) +
        theme(
            panel.grid.minor = element_blank(),
            legend.position = "right",
            plot.title = element_text(face = "bold")
        )
    
    return(p)
}

# --- UI Definition ---
ui <- page_sidebar(
    theme = bs_theme(version = 5, bootswatch = "flatly"),
    title = "Descriptive, ANOVA, PCA & CVI Analysis App",
    
    sidebar = sidebar(
        title = "Controls",
        fileInput("file", "1. Upload File (.csv or .xlsx)", accept = c(".csv", ".xlsx")),
        hr(),
        uiOutput("group_var_ui"),
        uiOutput("num_vars_ui"),
        hr(),
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
        selectInput(
            "cor_method",
            "Correlation Method:",
            choices = c(
                "Pearson (Parametric)" = "pearson",
                "Spearman (Non-parametric)" = "spearman"
            )
        ),
        hr(),
        h5("Export Options"),
        downloadButton("download_doc", "Download Summary Table (.docx)", class = "btn-primary w-100 mb-2"),
        downloadButton("download_cor_doc", "Download Correlation Table (.docx)", class = "btn-info w-100 mb-2"),
        downloadButton("download_pca_doc", "Download PCA Summary Table (.docx)", class = "btn-dark w-100 mb-2"),
        downloadButton("download_cvi_doc", "Download CVI Summary Table (.docx)", class = "btn-success w-100 mb-2"),
        downloadButton("download_biplot", "Download Biplot (.png)", class = "btn-warning w-100 mb-2"),
        downloadButton("download_plot", "Download Mean Plot (.png)", class = "btn-secondary w-100")
    ),
    
    navset_card_tab(
        nav_panel("Summary Table", uiOutput("ft_table")),
        nav_panel("Correlation Table", uiOutput("cor_table")),
        nav_panel(
            "PCA Analysis",
            layout_sidebar(
                sidebar = sidebar(
                    title = "Biplot Settings",
                    width = 250,
                    uiOutput("pc_x_ui"),
                    uiOutput("pc_y_ui"),
                    checkboxInput("scale_pca", "Scale Variables for PCA", value = TRUE),
                    checkboxInput("pca_ellipse", "Show 95% Group Ellipses", value = TRUE)
                ),
                navset_card_tab(
                    nav_panel("PCA Biplot", plotOutput("pca_biplot_out", height = "520px")),
                    nav_panel("PCA Summary Table", uiOutput("pca_combined_table"))
                )
            )
        ),
        nav_panel(
            "CVI Calculation",
            layout_sidebar(
                sidebar = sidebar(
                    title = "Vulnerability Indicator Directions",
                    width = 300,
                    helpText("Select indicators where an INCREASE in value INCREASES vulnerability (+). Uncheck for inverse indicators (-)."),
                    uiOutput("vulnerability_directions_ui")
                ),
                navset_card_tab(
                    nav_panel(
                        "CVI Group Summary",
                        br(),
                        uiOutput("cvi_summary_table_ui"),
                        br(),
                        plotOutput("cvi_plot", height = "400px")
                    ),
                    nav_panel(
                        "Calculated CVI Data",
                        br(),
                        downloadButton("download_cvi_csv", "Download CVI Scores (.csv)", class = "btn-primary mb-3"),
                        tableOutput("cvi_data_preview")
                    )
                )
            )
        ),
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
            selected = num_cols[1:min(4, length(num_cols))]
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
    
    # Dynamic Vulnerability Direction Checkboxes
    output$vulnerability_directions_ui <- renderUI({
        req(input$num_vars)
        lapply(input$num_vars, function(v) {
            checkboxInput(paste0("vdir_", v), label = paste("+", v), value = TRUE)
        })
    })
    
    # --- Summary Table Outputs ---
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
    
    output$ft_table <- renderUI({
        req(ft_obj())
        htmltools_value(ft_obj())
    })
    
    # --- Correlation Outputs ---
    cor_ft_obj <- reactive({
        req(raw_data(), input$num_vars)
        validate(need(length(input$num_vars) >= 2, "Please select at least 2 numeric variables for correlation."))
        
        cor_df <- build_cor_df(raw_data(), input$num_vars, input$cor_method)
        build_cor_ft(cor_df, input$cor_method)
    })
    
    output$cor_table <- renderUI({
        req(cor_ft_obj())
        htmltools_value(cor_ft_obj())
    })
    
    # --- PCA Computation & Dynamic Axes UI ---
    pca_obj <- reactive({
        req(raw_data(), input$num_vars)
        validate(need(length(input$num_vars) >= 2, "Please select at least 2 numeric variables for PCA."))
        run_pca(raw_data(), input$num_vars, scale_data = input$scale_pca)
    })
    
    output$pc_x_ui <- renderUI({
        req(pca_obj())
        pcs <- colnames(pca_obj()$x)
        selectInput("pc_x", "Horizontal Axis (X):", choices = pcs, selected = pcs[1])
    })
    
    output$pc_y_ui <- renderUI({
        req(pca_obj())
        pcs <- colnames(pca_obj()$x)
        selectInput("pc_y", "Vertical Axis (Y):", choices = pcs, selected = ifelse(length(pcs) > 1, pcs[2], pcs[1]))
    })
    
    pca_combined_ft <- reactive({
        req(pca_obj())
        build_combined_pca_ft(pca_obj())
    })
    
    output$pca_combined_table <- renderUI({
        req(pca_combined_ft())
        htmltools_value(pca_combined_ft())
    })
    
    pca_biplot_obj <- reactive({
        req(pca_obj(), raw_data(), input$group_var, input$pc_x, input$pc_y)
        build_pca_biplot(
            pca_res = pca_obj(),
            df = raw_data(),
            group_var = input$group_var,
            pc_x = input$pc_x,
            pc_y = input$pc_y,
            show_ellipse = input$pca_ellipse
        )
    })
    
    output$pca_biplot_out <- renderPlot({
        req(pca_biplot_obj())
        pca_biplot_obj()
    })
    
    # --- Composite Vulnerability Index (CVI) Computation ---
    cvi_computed_data <- reactive({
        req(pca_obj(), raw_data(), input$num_vars, input$group_var)
        
        df <- raw_data()
        valid_idx <- complete.cases(df[, input$num_vars, drop = FALSE])
        clean_df <- df[valid_idx, , drop = FALSE]
        
        pca_res <- pca_obj()
        pc1_scores <- pca_res$x[, 1]
        pc1_loadings <- pca_res$rotation[, 1]
        
        # Check directions from checkboxes
        directions <- sapply(input$num_vars, function(v) {
            val <- input[[paste0("vdir_", v)]]
            if (is.null(val)) TRUE else val
        })
        
        # Alignment check
        pos_dir_vars <- names(directions)[directions == TRUE]
        if (length(pos_dir_vars) > 0 && mean(pc1_loadings[pos_dir_vars]) < 0) {
            pc1_scores <- -pc1_scores
        }
        
        # Rescale PC1 to a 0-100 CVI Scale
        cvi_min <- min(pc1_scores, na.rm = TRUE)
        cvi_max <- max(pc1_scores, na.rm = TRUE)
        cvi_scaled <- ((pc1_scores - cvi_min) / (cvi_max - cvi_min)) * 100
        
        # Add continuous CVI and individual observation Remark Tier
        clean_df$CVI <- cvi_scaled
        clean_df$CVI_Tier <- ifelse(cvi_scaled <= 33.33, "Low Vulnerability",
                                    ifelse(cvi_scaled <= 66.66, "Moderate Vulnerability", "High Vulnerability"))
        clean_df
    })
    
    cvi_summary_ft_obj <- reactive({
        req(cvi_computed_data(), input$group_var)
        df <- cvi_computed_data()
        
        summary_df <- build_cvi_summary_df(
            df = df,
            group_var = input$group_var,
            anova_type = input$anova_type,
            posthoc_type = input$posthoc_type
        )
        
        build_cvi_ft(
            df_final = summary_df,
            group_var = input$group_var,
            show_cv = input$show_cv,
            posthoc_type = input$posthoc_type
        )
    })
    
    output$cvi_summary_table_ui <- renderUI({
        req(cvi_summary_ft_obj())
        htmltools_value(cvi_summary_ft_obj())
    })
    
    output$cvi_plot <- renderPlot({
        req(cvi_computed_data(), input$group_var)
        
        ggplot(cvi_computed_data(), aes(x = .data[[input$group_var]], y = CVI, fill = .data[[input$group_var]])) +
            stat_summary(fun = mean, geom = "col", alpha = 0.85, width = 0.5) +
            stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15, color = "black") +
            geom_hline(yintercept = c(33.33, 66.66), linetype = "dashed", color = "gray40", linewidth = 0.8) +
            annotate("text", x = 0.6, y = 16.5, label = "Low Tier", color = "gray30", fontface = "italic") +
            annotate("text", x = 0.6, y = 50.0, label = "Moderate Tier", color = "gray30", fontface = "italic") +
            annotate("text", x = 0.6, y = 83.3, label = "High Tier", color = "gray30", fontface = "italic") +
            labs(
                title = "Composite Vulnerability Index (CVI) by Group",
                subtitle = "Rescaled (0 - 100) based on PC1 Loadings with 3-Tier Threshold Lines",
                x = input$group_var,
                y = "Composite Vulnerability Index (0-100)"
            ) +
            theme_minimal(base_size = 14) +
            theme(legend.position = "none")
    })
    
    output$cvi_data_preview <- renderTable({
        req(cvi_computed_data())
        head(cvi_computed_data(), 15)
    })
    
    # --- Mean Plot Output ---
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
    
    # --- Download Handlers ---
    output$download_doc <- downloadHandler(
        filename = function() { paste0("summary_table_", Sys.Date(), ".docx") },
        content = function(file) {
            req(ft_obj())
            save_as_docx(ft_obj(), path = file)
        }
    )
    
    output$download_cor_doc <- downloadHandler(
        filename = function() { paste0("correlation_matrix_", Sys.Date(), ".docx") },
        content = function(file) {
            req(cor_ft_obj())
            save_as_docx(cor_ft_obj(), path = file)
        }
    )
    
    output$download_pca_doc <- downloadHandler(
        filename = function() { paste0("pca_summary_table_", Sys.Date(), ".docx") },
        content = function(file) {
            req(pca_combined_ft())
            save_as_docx(pca_combined_ft(), path = file)
        }
    )
    
    output$download_cvi_doc <- downloadHandler(
        filename = function() { paste0("cvi_summary_table_", Sys.Date(), ".docx") },
        content = function(file) {
            req(cvi_summary_ft_obj())
            save_as_docx(cvi_summary_ft_obj(), path = file)
        }
    )
    
    output$download_cvi_csv <- downloadHandler(
        filename = function() { paste0("computed_cvi_scores_", Sys.Date(), ".csv") },
        content = function(file) {
            req(cvi_computed_data())
            write.csv(cvi_computed_data(), file, row.names = FALSE)
        }
    )
    
    output$download_biplot <- downloadHandler(
        filename = function() { paste0("pca_biplot_", Sys.Date(), ".png") },
        content = function(file) {
            req(pca_biplot_obj())
            ggsave(file, plot = pca_biplot_obj(), width = 8, height = 6, dpi = 300)
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