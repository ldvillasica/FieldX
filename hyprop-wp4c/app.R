library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(readxl)
library(stringr)
library(purrr)

# -----------------------------------------------------------------------------
# 1. PARSERS (Native R - No Emulators)
# -----------------------------------------------------------------------------

parse_hyprop_xlsx <- function(file_path, sample_id) {
    # Reads exported HYPROP evaluation sheet
    df <- read_excel(file_path, sheet = 1)
    
    data.frame(
        Sample_ID   = sample_id,
        Suction_hPa = abs(as.numeric(df[[1]])),
        VWC         = as.numeric(df[[2]]),
        Source      = "HYPROP"
    )
}

parse_wp4c_xlsx <- function(file_path, sample_id, bulk_density = 1.3) {
    df <- read_excel(file_path)
    
    fix_num <- function(x) {
        as.numeric(str_replace_all(as.character(x), ",", "."))
    }
    
    df %>%
        mutate(
            Sample_ID   = sample_id,
            MPa_abs     = abs(fix_num(`MPa`)),
            Wet_g       = fix_num(`Wet Weight [g]`),
            Tare_g      = fix_num(`Tare Weight [g]`),
            Dry_g       = fix_num(`Dry Weight [g]`),
            
            # Soil physics conversions
            GWC         = (Wet_g - Dry_g) / (Dry_g - Tare_g),
            VWC         = GWC * bulk_density,
            Suction_hPa = MPa_abs * 10197.16, # 1 MPa ≈ 10,197.16 hPa
            Source      = "WP4C"
        ) %>%
        select(Sample_ID, Suction_hPa, VWC, Source)
}

# -----------------------------------------------------------------------------
# 2. UI
# -----------------------------------------------------------------------------

ui <- fluidPage(
    titlePanel("Soil Water Retention Curve (SWRC) Analyzer"),
    sidebarLayout(
        sidebarPanel(
            h4("1. Upload HYPROP Exports (.xlsx)"),
            fileInput("hyprop_files", "Select HYPROP Excel Files", multiple = TRUE, accept = c(".xlsx", ".xls")),
            
            h4("2. Upload WP4C Samples (.xlsx)"),
            fileInput("wp4c_files", "Select WP4C Excel Files", multiple = TRUE, accept = c(".xlsx", ".xls")),
            
            hr(),
            h4("3. Settings & Filters"),
            sliderInput("bulk_density", "Bulk Density (g/cm³):", min = 0.8, max = 1.8, value = 1.3, step = 0.05),
            uiOutput("sample_selector")
        ),
        
        mainPanel(
            plotlyOutput("swrc_plot", height = "550px"),
            hr(),
            h4("Summary Table"),
            tableOutput("summary_table")
        )
    )
)

# -----------------------------------------------------------------------------
# 3. SERVER
# -----------------------------------------------------------------------------

server <- function(input, output, session) {
    
    # Reactive HYPROP reader
    hyprop_data <- reactive({
        req(input$hyprop_files)
        map_dfr(seq_len(nrow(input$hyprop_files)), function(i) {
            fname <- input$hyprop_files$name[i]
            fpath <- input$hyprop_files$datapath[i]
            s_id  <- tools::file_path_sans_ext(fname)
            parse_hyprop_xlsx(fpath, sample_id = s_id)
        })
    })
    
    # Reactive WP4C reader
    wp4c_data <- reactive({
        req(input$wp4c_files)
        map_dfr(seq_len(nrow(input$wp4c_files)), function(i) {
            fname <- input$wp4c_files$name[i]
            fpath <- input$wp4c_files$datapath[i]
            s_id  <- tools::file_path_sans_ext(fname)
            parse_wp4c_xlsx(fpath, sample_id = s_id, bulk_density = input$bulk_density)
        })
    })
    
    # Combine data
    combined_data <- reactive({
        h_df <- tryCatch(hyprop_data(), error = function(e) NULL)
        w_df <- tryCatch(wp4c_data(), error = function(e) NULL)
        bind_rows(h_df, w_df)
    })
    
    # Dynamic sample filter dropdown
    output$sample_selector <- renderUI({
        df <- combined_data()
        req(nrow(df) > 0)
        samples <- unique(df$Sample_ID)
        selectInput("selected_sample", "Filter Sample:", choices = c("All Samples", samples), selected = "All Samples")
    })
    
    # Interactive Plot
    output$swrc_plot <- renderPlotly({
        df <- combined_data()
        req(nrow(df) > 0, input$selected_sample)
        
        if (input$selected_sample != "All Samples") {
            df <- df %>% filter(Sample_ID == input$selected_sample)
        }
        
        p <- ggplot(df, aes(x = Suction_hPa, y = VWC, color = Source, shape = Sample_ID)) +
            geom_point(size = 2.5, alpha = 0.8) +
            scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
            labs(
                title = paste("SWRC -", input$selected_sample),
                x = "Suction Head (hPa, Log Scale)",
                y = "Volumetric Water Content (cm³/cm³)"
            ) +
            theme_minimal() +
            scale_color_manual(values = c("HYPROP" = "#1f77b4", "WP4C" = "#d62728"))
        
        ggplotly(p)
    })
    
    # Summary output
    output$summary_table <- renderTable({
        df <- combined_data()
        req(nrow(df) > 0)
        
        df %>%
            group_by(Sample_ID, Source) %>%
            summarise(
                Count = n(),
                Min_Suction = round(min(Suction_hPa, na.rm = TRUE), 2),
                Max_Suction = round(max(Suction_hPa, na.rm = TRUE), 2),
                Min_VWC = round(min(VWC, na.rm = TRUE), 4),
                Max_VWC = round(max(VWC, na.rm = TRUE), 4),
                .groups = "drop"
            )
    })
}

shinyApp(ui = ui, server = server)