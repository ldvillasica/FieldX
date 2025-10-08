# app.R

# Load the required libraries for the Shiny application.
# `shiny` is the core library for building interactive web apps in R.
# `mermaidR` provides the functionality to render Mermaid diagrams within Shiny.
library(shiny)
library(mermaid)

# Define the user interface (UI) of the Shiny app.
# The UI controls the layout and appearance of the app, including all the widgets
# and panels that the user will interact with.
ui <- fluidPage(
    
    # Set the title of the application.
    titlePanel("Graphical Abstract Creator with Mermaid.js"),
    
    # Use a sidebar layout, which creates a sidebar panel for inputs and a main panel for outputs.
    sidebarLayout(
        
        # Define the content of the sidebar panel.
        # This is where the user will enter their Mermaid code.
        sidebarPanel(
            # Add a title for the input section.
            h4("Enter Your Mermaid Code Here"),
            # Provide a text area for the user to type their Mermaid code.
            # The `value` argument pre-populates the text box with a simple example.
            textAreaInput("mermaid_code", 
                          label = "Mermaid Code:", 
                          rows = 15,
                          placeholder = "Enter your Mermaid diagram code here...",
                          value = "graph TD
    A[Start: Research Question] --> B(Literature Review);
    B --> C{Methodology & Data Collection};
    C --> D[Analysis of Results];
    D --> E[Conclusions & Future Work];
    E --> F[End: Publication];

    style A fill:#f9f,stroke:#333,stroke-width:2px;
    style B fill:#bbf,stroke:#333,stroke-width:2px;
    style C fill:#9b9,stroke:#333,stroke-width:2px;
    style D fill:#f66,stroke:#333,stroke-width:2px;
    style E fill:#fc0,stroke:#333,stroke-width:2px;
    style F fill:#9f9,stroke:#333,stroke-width:2px;"),
            
            # Add some helpful instructions for the user.
            h5("Tips:"),
            tags$ul(
                tags$li("`graph TD` for Top-Down flow."),
                tags$li("`graph LR` for Left-to-Right flow."),
                tags$li("`A[Node Label]` for a rectangular node."),
                tags$li("`A(Node Label)` for a rounded node."),
                tags$li("`A-->B` for a simple arrow connection."),
                tags$li("You can add styling with the `style` keyword.")
            )
        ),
        
        # Define the content of the main panel.
        # This is where the Mermaid diagram will be displayed.
        mainPanel(
            # The `mermaidOutput` function creates a placeholder for the diagram.
            # The `outputId` links it to the server-side rendering logic.
            mermaidOutput("mermaid_diagram")
        )
    )
)

# Define the server logic.
# The server handles the reactive processes of the app, taking user input
# and generating output.
server <- function(input, output) {
    
    # Use `renderMermaid` to create the diagram.
    # This function is part of the `mermaidR` package and takes the Mermaid
    # code as input. It will re-render the diagram whenever the `mermaid_code`
    # text input changes.
    output$mermaid_diagram <- renderMermaid({
        # Get the text from the `textAreaInput` and pass it to the rendering function.
        mermaid(input$mermaid_code)
    })
}

# Run the Shiny application.
shinyApp(ui = ui, server = server)