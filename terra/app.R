library(shiny)
library(tidyterra)
library(terra)
library(ggplot2)
library(patchwork)
library(bslib)
library(geodata)
library(ggspatial) # For North Arrow and Scale Bar

ui <- fluidPage(
    theme = bs_theme(bootswatch = "flatly"),
    titlePanel("Sampling Point Analysis - Professional Layout"),
    
    sidebarLayout(
        sidebarPanel(
            tags$b("1. Data Upload"),
            fileInput("file_input", "Upload CSV", accept = ".csv"),
            selectInput("color_col", "Color Points By:", choices = NULL),
            
            hr(),
            tags$b("2. Zoom Controls (Degrees)"),
            sliderInput("zoom_main", "Main Map Overview:", min = 0.05, max = 5, value = 1, step = 0.05),
            sliderInput("zoom_a", "Subset A Tightness:", min = 0.001, max = 0.2, value = 0.02, step = 0.001),
            sliderInput("zoom_b", "Subset B Tightness:", min = 0.001, max = 0.2, value = 0.02, step = 0.001),
            
            hr(),
            tags$b("3. Window Center Points"),
            selectInput("focus_a", "Focus Window A:", choices = NULL),
            selectInput("focus_b", "Focus Window B:", choices = NULL)
        ),
        
        mainPanel(
            plotOutput("multi_map", height = "900px")
        )
    )
)

server <- function(input, output, session) {
    
    # Load boundaries (Level 2 = Municipalities)
    # This stays in the background to provide context for all points
    adm_base <- gadm(country = "PHL", level = 2, path = tempdir())
    
    raw_data <- reactive({
        req(input$file_input)
        df <- read.csv(input$file_input$datapath)
        if(!"id" %in% names(df)) df$id <- paste0("P", seq_len(nrow(df)))
        return(df)
    })
    
    observeEvent(raw_data(), {
        df <- raw_data()
        updateSelectInput(session, "color_col", choices = names(df))
        updateSelectInput(session, "focus_a", choices = df$id, selected = df$id[1])
        updateSelectInput(session, "focus_b", choices = df$id, selected = df$id[nrow(df)])
    })
    
    spatial_data <- reactive({
        vect(raw_data(), geom = c("lon", "lat"), crs = "EPSG:4326")
    })
    
    output$multi_map <- renderPlot({
        req(spatial_data())
        v <- spatial_data()
        df <- raw_data()
        
        pt_a <- df[df$id == input$focus_a, ]
        pt_b <- df[df$id == input$focus_b, ]
        
        # Main function to render each map panel
        render_panel <- function(center_lon, center_lat, zoom_val, title_str, show_arrow = FALSE) {
            ggplot() +
                # Background Land Layer (Alice Blue for Water)
                geom_spatvector(data = adm_base, fill = "#fdfdfd", color = "gray80", linewidth = 0.2) +
                # Municipal labels (cleaner italic look)
                geom_spatvector_text(data = adm_base, aes(label = NAME_2), 
                                     size = 2.8, fontface = "italic", color = "gray40", check_overlap = TRUE) +
                # Sampling Points
                geom_spatvector(data = v, aes(color = .data[[input$color_col]]), size = 3.5, alpha = 0.9) +
                
                # Mapping Extent
                coord_sf(xlim = c(center_lon - zoom_val, center_lon + zoom_val), 
                         ylim = c(center_lat - zoom_val, center_lat + zoom_val),
                         expand = FALSE) +
                
                # GIS Elements (Scale bar and North arrow)
                annotation_scale(location = "bl", width_hint = 0.4, style = "ticks") +
                annotation_north_arrow(location = "tr", which_north = "true", 
                                       style = north_arrow_minimal(text_size = 8)) +
                
                scale_color_viridis_d(option = "mako") +
                labs(title = title_str, x = "Longitude", y = "Latitude") +
                theme_bw() + 
                theme(
                    panel.background = element_rect(fill = "#eef4f7"),
                    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.2),
                    plot.title = element_text(face = "bold", size = 14),
                    axis.text = element_text(size = 8),
                    legend.title = element_text(face = "bold")
                )
        }
        
        # Generate Panels
        p1 <- render_panel(mean(ext(v)[1:2]), mean(ext(v)[3:4]), input$zoom_main, "Overview Map")
        p2 <- render_panel(pt_a$lon, pt_a$lat, input$zoom_a, paste("Window A:", input$focus_a))
        p3 <- render_panel(pt_b$lon, pt_b$lat, input$zoom_b, paste("Window B:", input$focus_b))
        
        # Combine with Patchwork
        (p1) / (p2 | p3) + 
            plot_layout(heights = c(1.4, 1), guides = "collect") +
            plot_annotation(
                title = "Geospatial Sampling Point Distribution",
                subtitle = "Generated via tidyterra & Shiny",
                tag_levels = 'A'
            )
    })
}

shinyApp(ui, server)