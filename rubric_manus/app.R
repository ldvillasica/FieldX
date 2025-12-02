# Load necessary libraries
library(shiny)
library(dplyr)
library(tidyr)
library(DT) # For displaying the data table
library(R.utils) # Needed for the JS function in DT

# --- 1. Define the Rubric Criteria and Scoring Scale (Now with Descriptions) ---

# Nested list defining the criteria and their descriptive text for scores 4, 3, 2, 1
rubric_details <- list(
    "Introduction" = list(
        "Problem Statement" = list(
            "4" = "Clearly and compellingly defines the research problem, establishing its importance and relevance to the field.",
            "3" = "Defines the problem but could be more compelling. Relevance is generally clear but lacks depth.",
            "2" = "Problem statement is vague or lacks a clear connection to a significant issue.",
            "1" = "Problem is not stated, or the statement is confusing and irrelevant."
        ),
        "Background & Context" = list(
            "4" = "Provides a concise, logical overview of existing literature, smoothly leading the reader to the research gap.",
            "3" = "Presents relevant background information but the flow may be slightly disjointed.",
            "2" = "Background information is present but either too brief or too extensive, and does not clearly lead to the research gap.",
            "1" = "Lacks sufficient background or includes information that is irrelevant to the topic."
        ),
        "Research Gap" = list(
            "4" = "Explicitly and precisely identifies a clear, significant gap in the current literature that the proposed study will address.",
            "3" = "Identifies a gap but could be more specific or better articulated.",
            "2" = "The research gap is implied but not explicitly stated or is unclear.",
            "1" = "Fails to identify a research gap."
        ),
        "Purpose & Objective" = list(
            "4" = "States the study's purpose and objectives with exceptional clarity and precision. The objectives directly align with the research question.",
            "3" = "States the purpose and objectives, but they could be more specific or better aligned.",
            "2" = "The purpose and objectives are vaguely stated or do not fully align with the research question.",
            "1" = "The purpose and objectives are missing or are completely disconnected from the rest of the introduction."
        ),
        "Thesis/Hypothesis" = list(
            "4" = "The hypothesis or central thesis is clearly stated, testable, and directly addresses the research question.",
            "3" = "The hypothesis or thesis is stated but could be more specific or is not fully testable.",
            "2" = "The hypothesis or thesis is not clearly defined, or its connection to the research question is weak.",
            "1" = "Fails to present a clear hypothesis or thesis."
        ),
        "Overall Cohesion & Flow" = list(
            "4" = "The introduction is well-structured and reads as a cohesive narrative. Transitions between paragraphs are seamless and logical.",
            "3" = "The introduction has a logical structure, but some transitions may feel abrupt.",
            "2" = "The introduction's structure is difficult to follow, and the arguments are fragmented.",
            "1" = "The introduction lacks structure and is difficult to understand."
        )
    ),
    "RRL" = list(
        "Comprehensiveness" = list(
            "4" = "Provides a comprehensive and highly relevant overview of the key studies, theories, and concepts related to the research question.",
            "3" = "Covers relevant literature but may have some minor gaps or include a few less-relevant sources.",
            "2" = "The review is notably incomplete or includes too much irrelevant information.",
            "1" = "Lacks a literature review or the sources are not related to the topic."
        ),
        "Synthesis & Organization" = list(
            "4" = "Synthesizes sources into a coherent, well-structured narrative, grouping similar ideas and debates to create a clear argument.",
            "3" = "Synthesizes sources logically, but the flow could be improved. The narrative is generally clear but may contain minor redundancies.",
            "2" = "Presents sources as a list of summaries rather than a cohesive synthesis of ideas.",
            "1" = "Presents an unorganized list of source summaries with no clear narrative or synthesis."
        ),
        "Critical Analysis" = list(
            "4" = "Critically evaluates the reviewed literature, discussing strengths, weaknesses, and controversies. The author's unique perspective is evident.",
            "3" = "Includes some critical evaluation of sources, but the analysis is not as deep or consistent.",
            "2" = "Summarizes sources without providing critical analysis or evaluation.",
            "1" = "Fails to critically engage with the literature."
        ),
        "Identifying the Gap" = list(
            "4" = "The literature review builds a clear and convincing case for the identified research gap, directly leading to the proposed study.",
            "3" = "The review concludes by identifying the gap, but the connection could be more explicit and persuasive.",
            "2" = "The research gap is mentioned but is not adequately supported by the literature discussed.",
            "1" = "The review fails to justify or even mention a research gap."
        ),
        "Citation & Formatting" = list(
            "4" = "Uses the specified citation style consistently and accurately. All in-text citations correspond correctly to the reference list.",
            "3" = "Generally follows the specified citation style, with only a few minor inconsistencies.",
            "2" = "Fails to consistently apply the specified citation style, with multiple errors.",
            "1" = "Citation style is either not used or is completely incorrect and inconsistent."
        )
    ),
    "Methods" = list(
        "Completeness & Reproducibility" = list(
            "4" = "The procedures are described with such a high level of detail that an independent researcher could replicate the study exactly. All materials, equipment, and settings are explicitly stated.",
            "3" = "The procedures are clear but may be missing some minor details needed for perfect replication. Key materials and methods are described.",
            "2" = "The procedures are vague or incomplete. Significant details are missing, making replication difficult.",
            "1" = "The methods are not described, or the description is entirely incomprehensible."
        ),
        "Logical Flow & Organization" = list(
            "4" = "The section is organized logically and chronologically, following the steps of the experiment or data collection. Subheadings are used effectively to guide the reader.",
            "3" = "The organization is generally logical, though there may be some minor jumps or redundancies. Subheadings are used, but could be more precise.",
            "2" = "The order of the procedures is confusing, making the flow difficult to follow. Subheadings are missing or unhelpful.",
            "1" = "The section is a disorganized collection of sentences with no logical structure."
        ),
        "Relevance to Research Question" = list(
            "4" = "Every method and material described is directly relevant to answering the research question. No unnecessary information is included.",
            "3" = "Most methods and materials are relevant, but some could be more closely tied to the research question.",
            "2" = "The section includes methods that are not clearly relevant to the study's purpose.",
            "1" = "The methods described are entirely irrelevant to the research question."
        ),
        "Justification & Rationale" = list(
            "4" = "The rationale for choosing a specific method or statistical analysis is clearly explained and justified based on the research design.",
            "3" = "The rationale is briefly explained, but could be more explicit in justifying the choices made.",
            "2" = "The rationale for the methods is not provided, leaving the reader to guess why certain approaches were taken.",
            "1" = "There is no justification for the methods used."
        ),
        "Ethical Considerations" = list(
            "4" = "All ethical considerations (e.g., IRB approval, informed consent, data privacy) are explicitly addressed and described.",
            "3" = "Ethical considerations are mentioned, but the details are brief or lack clarity.",
            "2" = "Ethical considerations are either not mentioned or are treated superficially.",
            "1" = "Fails to address any ethical considerations."
        )
    ),
    "Overall" = list(
        "Clarity of Purpose" = list(
            "4" = "The central research question and argument are exceptionally clear and maintained consistently throughout the entire proposal. The reader immediately understands the study's aim.",
            "3" = "The research question is clear, and the overall purpose is evident, but a few sections may lack perfect alignment.",
            "2" = "The research question is somewhat unclear, or the study's purpose becomes muddled in some sections.",
            "1" = "The research question is confusing, or the proposal's purpose is not discernible."
        ),
        "Internal Consistency" = list(
            "4" = "All sections (Introduction, Literature Review, Methods) are flawlessly aligned. The literature review provides a robust rationale for the methods, and the methods are perfectly designed to address the research question.",
            "3" = "The sections are logically connected, but a few minor inconsistencies exist between the problem statement, literature gap, and proposed methods.",
            "2" = "There is a noticeable disconnect between key sections. For example, the methods may not fully address the research gap identified.",
            "1" = "The sections are disconnected and do not form a coherent whole. The proposal is a fragmented collection of ideas."
        ),
        "Significance & Contribution" = list(
            "4" = "The proposal makes a compelling and persuasive case for the study's significance and potential contribution to the field. It demonstrates a deep understanding of why the research matters.",
            "3" = "The significance is adequately explained, and the potential contribution is clear, though the argument could be more powerful or detailed.",
            "2" = "The significance of the study is weakly argued or is not clearly articulated. The contribution to the field is minimal or vague.",
            "1" = "Fails to justify the research's importance. The contribution is not addressed or is insignificant."
        ),
        "Feasibility of the Project" = list(
            "4" = "The proposed project is highly feasible. The methods are realistic, the timeline is well-planned, and the resources required are appropriate for the scope.",
            "3" = "The project is largely feasible, but some minor aspects of the timeline or resource management could be more realistic.",
            "2" = "The project's feasibility is questionable. The timeline is unrealistic, or the proposed methods are impractical.",
            "1" = "The project is entirely unfeasible as proposed. The methods, timeline, and resources are completely out of touch with reality."
        ),
        "Writing Quality & Professionalism" = list(
            "4" = "The entire proposal is meticulously written with perfect grammar, spelling, and professional formatting. It is free of all errors and Plagiarism and AI score is below 20%.",
            "3" = "The writing is professional and clear with only a few minor grammatical or spelling errors. Formatting is consistent. Plagiarism and AI score is below 30%.",
            "2" = "The proposal contains multiple writing errors (grammar, spelling) that distract from the content. Formatting is inconsistent. Plagiarism and AI score is below 40%.",
            "1" = "The writing is filled with errors, making the proposal difficult to read and unprofessional. Plagiarism and AI score is below 50%."
        )
    )
)

# Score choices (simplified for display next to the buttons)
score_choices <- c(
    "4 - Excellent" = 4,
    "3 - Good" = 3,
    "2 - Needs Improvement" = 2,
    "1 - Poor" = 1
)

# Helper function to get the criteria names from the detailed list
get_criteria_names <- function(details) {
    lapply(details, names)
}
rubrics <- get_criteria_names(rubric_details)

# Define max scores based on the rubric structure (needed for percentage calculation)
max_scores_df <- data.frame(
    Section = names(rubric_details),
    MaxScore = sapply(rubric_details, length) * 4 # 4 points per criterion
)
total_max_score <- sum(max_scores_df$MaxScore)

# --- 2. User Interface (UI) ---

ui <- fluidPage(
    # Add custom CSS for better layout and styling of the rubric criteria
    tags$head(
        tags$style(HTML("
            .score-description {
                border-top: 1px solid #ddd;
                padding-top: 10px;
                margin-top: 5px;
            }
            .score-col {
                padding: 5px;
                border-right: 1px solid #eee;
                min-height: 100px; /* Ensure columns have minimum height */
                font-size: 0.9em;
            }
            .score-col:last-child {
                border-right: none;
            }
            .score-header {
                font-weight: bold;
                margin-bottom: 5px;
            }
        "))
    ),
    
    # Set the app title
    titlePanel("Research Proposal Rubric Grader"),
    
    # Use navbarPage for multi-tab structure
    navbarPage("",
               # --- Tab 1: Grading Interface ---
               tabPanel("Score Submission",
                        sidebarLayout(
                            # Sidebar for Student and Section Selection
                            sidebarPanel(
                                width = 4,
                                h4("Student & Section Info"),
                                textInput("student_id", "Student ID or Name", placeholder = "Enter Student Name/ID"),
                                selectInput("section_select", "Select Proposal Section",
                                            choices = names(rubric_details), # Use names of the detailed list
                                            selected = "Introduction"),
                                hr(),
                                # Button to save the current scores
                                actionButton("save_score", "Save Score & Clear Form", class = "btn-primary", icon = icon("save")),
                                p(style = "margin-top: 15px; font-size: 0.9em;", "Scores are saved to the 'Data History' and 'Score Summary' tabs.")
                            ),
                            
                            # Main Panel for Dynamic Rubric Display
                            mainPanel(
                                width = 8,
                                h4(textOutput("current_section_title")),
                                # Placeholder for dynamically generated criteria inputs
                                uiOutput("rubric_criteria_inputs")
                            )
                        )
               ),
               
               # --- Tab 2: Score Summary (New tab for calculated grades) ---
               tabPanel("Score Summary",
                        h3("Calculated Partial and Overall Grades"),
                        p("This table summarizes the percentage grades for each section and the final overall score."),
                        
                        # **NEW DOWNLOAD BUTTON**
                        downloadButton("download_summary", "Download Summary CSV", class = "btn-success", icon = icon("download")),
                        hr(), # Add separator for neatness
                        
                        DTOutput("summary_table")
               ),
               
               # --- Tab 3: Data History (Raw data) ---
               tabPanel("Data History",
                        h3("Raw Grading History"),
                        p("This table shows the raw score for every criterion graded."),
                        # Display accumulated data in a table
                        DTOutput("grading_table"),
                        hr(),
                        # Button to download data
                        downloadButton("download_data", "Download Raw Data as CSV", class = "btn-success", icon = icon("download"))
               )
    )
)

# --- 3. Server Logic ---

server <- function(input, output, session) {
    
    # Reactive value to store the accumulated scores (acts as our in-app database)
    scores_rv <- reactiveVal(
        # Initialize with an empty data frame that has the correct column types
        data.frame(
            ID = character(0),
            Section = character(0),
            Criterion = character(0),
            Score = numeric(0),
            Timestamp = character(0),
            stringsAsFactors = FALSE
        )
    )
    
    # Reactive title for the main panel
    output$current_section_title <- renderText({
        paste0("Criteria for ", input$section_select)
    })
    
    # --- Dynamic Rubric Rendering (Unchanged) ---
    output$rubric_criteria_inputs <- renderUI({
        req(input$section_select)
        current_criteria_details <- rubric_details[[input$section_select]]
        
        # Generate a list of UI elements for each criterion
        lapply(names(current_criteria_details), function(criterion) {
            descriptions <- current_criteria_details[[criterion]]
            input_id <- paste0("score_", gsub(" ", "_", criterion))
            
            div(
                class = "mb-4 p-3 border rounded shadow-sm bg-light",
                tags$h5(class = "fw-bold", criterion),
                
                div(
                    radioButtons(
                        inputId = input_id,
                        label = "Select Score:",
                        choices = score_choices,
                        selected = character(0),
                        inline = TRUE
                    )
                ),
                
                div(class = "row score-description",
                    # Score 4 (Excellent)
                    div(class = "col-md-3 score-col",
                        tags$div(class = "score-header text-success", "4 - Excellent"),
                        tags$p(descriptions[["4"]])
                    ),
                    # Score 3 (Good)
                    div(class = "col-md-3 score-col",
                        tags$div(class = "score-header text-primary", "3 - Good"),
                        tags$p(descriptions[["3"]])
                    ),
                    # Score 2 (Needs Improvement)
                    div(class = "col-md-3 score-col",
                        tags$div(class = "score-header text-warning", "2 - Needs Improvement"),
                        tags$p(descriptions[["2"]])
                    ),
                    # Score 1 (Poor)
                    div(class = "col-md-3 score-col",
                        tags$div(class = "score-header text-danger", "1 - Poor"),
                        tags$p(descriptions[["1"]])
                    )
                )
            )
        })
    })
    
    # --- Score Saving Logic (Unchanged) ---
    observeEvent(input$save_score, {
        if (input$student_id == "") {
            showModal(modalDialog(
                title = "Error",
                "Please enter a Student ID or Name before saving.",
                easyClose = TRUE
            ))
            return()
        }
        
        current_section <- input$section_select
        current_criteria <- names(rubric_details[[current_section]])
        new_scores <- data.frame()
        all_scores_present <- TRUE
        
        for (criterion in current_criteria) {
            input_id <- paste0("score_", gsub(" ", "_", criterion))
            score <- input[[input_id]]
            
            if (is.null(score) || score == "") {
                all_scores_present <- FALSE
                break
            }
            
            new_scores <- new_scores %>%
                bind_rows(data.frame(
                    ID = input$student_id,
                    Section = current_section,
                    Criterion = criterion,
                    Score = as.numeric(score),
                    Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
                ))
        }
        
        if (!all_scores_present) {
            showModal(modalDialog(
                title = "Incomplete Form",
                "Please score all criteria in the selected section before saving.",
                easyClose = TRUE
            ))
            return()
        }
        
        scores_rv(rbind(scores_rv(), new_scores))
        
        showNotification(
            paste0("Scores for ", current_section, " saved successfully for ", input$student_id),
            type = "message",
            duration = 3
        )
        
        for (criterion in current_criteria) {
            input_id <- paste0("score_", gsub(" ", "_", criterion))
            updateRadioButtons(session, input_id, selected = character(0))
        }
    })
    
    # --- Calculation Logic: Reactive Summary Table (FIXED) ---
    grading_summary_rv <- reactive({
        # Ensure there is data before proceeding
        if (nrow(scores_rv()) == 0) return(data.frame())
        
        data <- scores_rv()
        
        # 1. Calculate section totals, merge with max scores, and calculate percentage
        section_summary <- data %>%
            # Group by ID and Section to get the sum of scores for that section
            group_by(ID, Section) %>%
            summarise(
                ActualSum = sum(Score, na.rm = TRUE),
                .groups = 'drop'
            ) %>%
            # Merge with max scores to calculate percentage
            left_join(max_scores_df, by = "Section") %>%
            mutate(
                PartialGrade_Percent = round((ActualSum / MaxScore) * 100, 2)
            ) %>%
            # Keep ActualSum for the next overall calculation step
            select(ID, Section, PartialGrade_Percent, MaxScore, ActualSum) 
        
        # 2. Calculate overall total and percentage grade
        overall_summary <- section_summary %>%
            group_by(ID) %>%
            summarise(
                # ActualSum is now correctly visible in this pipeline step
                TotalActualScore = sum(ActualSum), 
                TotalMaxScore = total_max_score, 
                OverallGrade_Percent = round((TotalActualScore / TotalMaxScore) * 100, 2),
                .groups = 'drop'
            ) %>%
            select(ID, OverallGrade_Percent)
        
        # 3. Pivot partial scores wider and combine with overall score
        final_summary <- section_summary %>%
            select(ID, Section, PartialGrade_Percent) %>%
            pivot_wider(
                names_from = Section,
                values_from = PartialGrade_Percent,
                names_prefix = "Grade_"
            ) %>%
            left_join(overall_summary, by = "ID") %>%
            select(
                ID,
                starts_with("Grade_"),
                OverallGrade_Percent
            )
        
        return(final_summary)
    })
    
    # --- Data Summary Display (Tab 2) ---
    output$summary_table <- renderDT({
        req(grading_summary_rv())
        
        # Define the columns that should be formatted as percentages
        summary_data <- grading_summary_rv()
        percent_cols <- grep("Grade_|Overall", names(summary_data))
        
        # IMPORTANT: When displaying data for download, we don't want the '%' sign embedded in the data itself.
        # However, for DT display, we use JavaScript to append the '%'.
        datatable(summary_data,
                  options = list(
                      pageLength = 10,
                      autoWidth = TRUE,
                      # Use JavaScript to add a '%' sign to all calculated columns for display
                      columnDefs = list(list(targets = percent_cols - 1, render = JS("function(data, type, row){return data + '%'}")))
                  ),
                  rownames = FALSE)
    })
    
    # **NEW DOWNLOAD HANDLER for Summary Table**
    output$download_summary <- downloadHandler(
        filename = function() {
            # Use a descriptive filename for the summary data
            paste("proposal_summary_grades-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            # Ensure the percentage values are exported directly as numbers 
            # (which is what grading_summary_rv() provides before DT renders the '%')
            write.csv(grading_summary_rv(), file, row.names = FALSE)
        }
    )
    
    
    # --- Raw Data Display (Tab 3) ---
    
    output$grading_table <- renderDT({
        # Pivot the data to a wider format for easier readability
        data_wide <- scores_rv() %>%
            # Use unite to combine ID and Section into a unique key
            unite("Key", ID, Section, sep = " - ", remove = FALSE) %>%
            # Pivot wider to make criteria into columns
            pivot_wider(
                id_cols = c(ID, Section, Timestamp),
                names_from = Criterion,
                values_from = Score
            ) %>%
            # Arrange by Timestamp to show most recent scores first
            arrange(desc(Timestamp))
        
        # Use DT package for a searchable, sortable, and clean table
        datatable(data_wide,
                  options = list(pageLength = 10, autoWidth = TRUE),
                  rownames = FALSE)
    })
    
    # --- Raw Data Download (Tab 3) ---
    
    output$download_data <- downloadHandler(
        filename = function() {
            paste("proposal_rubric_raw_scores-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            # Ensure the wide-format data is prepared for export
            data_wide <- scores_rv() %>%
                pivot_wider(
                    id_cols = c(ID, Section, Timestamp),
                    names_from = Criterion,
                    values_from = Score
                ) %>%
                arrange(desc(Timestamp))
            
            write.csv(data_wide, file, row.names = FALSE)
        }
    )
}

# Run the application
shinyApp(ui = ui, server = server)