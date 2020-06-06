# ------------------------------------------------------------------------- 
# INITIALISE 
# ------------------------------------------------------------------------- 

source(file.path('R', 'parse_pdf.R'))
source(file.path('R', 'plots.R'))

library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(DT)
library(rhandsontable)
library(tidytext)
options(shiny.maxRequestSize = 10*1024^2) 

# ------------------------------------------------------------------------- 
# UTILITIES
# ------------------------------------------------------------------------- 

# Make a pretty label for form fields.
pretty_label <- function(main, ...) {
    tagList(
        p(main),
        p(...,  class = 'text-body', style = 'font-weight: 400;')
    )
}

# ------------------------------------------------------------------------- 
# USER INTERFACE
# ------------------------------------------------------------------------- 

header <- dashboardHeader(
    title = "Short Circuit",
    titleWidth = '10%'
)

sidebar <- dashboardSidebar(
    width = '10%',
    sidebarMenu(
        menuItem("Load", tabName = 'load'),
        menuItem("Visualise", tabName = 'visualise')
    )
)

# TAB: Load ---------------------------------------------------------------

tab_load <- tabItem(tabName = 'load',
    fluidRow(
        column(width = 4,
            box(
                # SETUP BOX.
                title = "Load Data",
                status = 'primary',
                solidHeader = TRUE,
                width = NULL,
                height = "90vh",
                
                # INPUT: Load file.
                fileInput(
                    "file",
                    label = pretty_label(
                        main = "Select file",
                        "Select PDF file for analysis (up to 5MB)"
                    ),
                    accept = 'application/pdf'
                ),
                
                # INPUT: Last page.
                conditionalPanel(
                    condition = "output.file_uploaded",
                    uiOutput("selector_last_page"), 
                ),
                
                # TRIGGER MODAL: Inspect document.
                conditionalPanel(
                    condition = "output.file_uploaded",
                    actionButton(
                        'action_inspect_doc', 
                        "Inspect Document",
                        style = 'margin-bottom: 1rem;')
                ),
                
                # REACTIVE UI INPUT: Selector - font heights of interest.
                uiOutput("selector_font_heights"), 
                
                # REACTIVE UI INPUT: Selector - height of section headings.
                uiOutput("selector_section_heights"),
                
                # TRIGGER MODAL: Rename sections and inspect corpus.
                conditionalPanel(
                    condition = "input.selected_height",
                    actionButton(
                        'action_rename_section_headings',
                        "Rename Section Headings",
                        style = 'margin-top: 1rem; margin-bottom: 1rem'
                    ),
                    actionButton(
                        'action_inspect_corpus',
                        "Inspect Corpus",
                        style = 'margin-top: 1rem; margin-bottom: 1rem'
                    )
                )
            )
        ),
        column(width = 8,
            box(
                # SETUP BOX.
                title = "TF-IDF",
                status = 'primary',
                solidHeader = TRUE,
                width = NULL,
                height = '90vh',
                
                # OUTPUT: TF-IDF table.
                div(style = 'overflow-y: auto; max-height: 700px', 
                    dataTableOutput("tf_idf_table")
                )
            )
        ),
        
        # MODAL: Inspect document.
        bsModal(
            id = 'modal_inspect_doc',
            title = "Document Inspector",
            trigger = 'action_inspect_doc',
            size = 'large',
            checkboxInput(
                "inspect_doc_limit_chars",
                "Limit Number of character"
            ),
            numericInput(
                "inspect_doc_no_chars",
                "Number of characters",
                min = 100, max = 1000, step = 100, value = 100),
            uiOutput("inspect_doc_selector_heights"),
            dataTableOutput("table_inspect_doc", height = "400px") 
        ),
        
        # MODAL: Rename sections
        bsModal(
            id = 'modal_rename',
            title = "Rename Section Headings",
            trigger = 'action_rename_section_headings',
            size = 'large',
            rHandsontableOutput('rename_sections'),
            tags$head(tags$style(
                "#model_rename.modal-footer{ display:none}"))
        ),
        
        # MODAL: Inspect corpus.
        bsModal(
            id = 'modal_inpect_documents',
            title = "Corpus Inspector",
            trigger = 'action_inspect_corpus',
            size = 'large',
            dataTableOutput('table_inspect_corpus', height = "400px"),
            tags$head(tags$style(
                "#model_inspect_document.modal-footer{ display:none}"))
        )
    )
)

# TAB: Visualise ----------------------------------------------------------

tab_analyse <- tabItem(tabName = 'visualise',
    fluidRow(
        column(width = 2,
            box(
                title = "Visualisation Control",
                status = 'primary',
                solidHeader = TRUE,
                width = NULL,
                height = '90vh',
                collapsible = TRUE,
                
                # REACTIVE UI INPUT: Selctor for visualisation.
                uiOutput("selector_visualise"),
                
                conditionalPanel(
                    "input.selected_sections_visualise",
                    radioButtons(
                        "selected_plot_visualise",
                        label = "Select plot type",
                        choices = c("Bar Plot", "Word Cloud"),
                        selected = "Bar Plot"
                    )
                ) 
            )
        ),
        column(width = 10,
            box(
                title = "Word Frequencies",
                status = 'primary',
                solidHeader = TRUE,
                width = NULL,
                height = '90vh',
                
                # OUTPUT: Visualisation.
                plotOutput("word_freqs", height = '700px')
            )
        )
    )
)

body <- dashboardBody(
    tags$link(rel = 'stylesheet', type = 'text/css', href = 'style.css'),
    useShinyjs(),
    tabItems(tab_load, tab_analyse)
)

ui <- dashboardPage(header, sidebar, body) 

# ------------------------------------------------------------------------- 
# SERVER
# ------------------------------------------------------------------------- 

server <- function(input, output) {
        
    # Reactive values -------------------------------------------------

    # Corpus data.
    values <- reactiveValues(
        section_lookup = tibble(
            page = integer(),
            doc_line = integer(),
            head_id = integer(),
            heading = character(),
            renamed_heading = character()
        )
    )
    
    # Read PDF Data ---------------------------------------------------
    
    df_doc_raw <- reactive({
        validate(
            need(input$file$datapath,
                 "Please load a document to calculate TF-IDF"
            )
        )
        read_pdf_data(input$file$datapath) %>% 
            # Only interested in alpha-numeric words
            filter(str_detect(text, '^[[:alnum:][:punct:]]+$'))
    })
    # The next bit of code validates the file upload.  This is used 
    # to activate certain elements of the UI when a valid upload is
    # complete.
    output$file_uploaded <- reactive({
        !is.null(df_doc_raw())
    })
    outputOptions(output, 'file_uploaded', suspendWhenHidden = FALSE)
    
    # Get font heights.
    font_heights <- reactive({
        df_doc_raw() %>% 
            distinct(height) %>%
            arrange(height) %>%
            pull(height)
    }) 

    # Document Inspector ---------------------------------------------
   
    observeEvent(input$inspect_doc_limit_chars, {
        shinyjs::toggleState(
            "inspect_doc_no_chars",
            input$inspect_doc_limit_chars
        )
    }) 
     
    # REACTIVE UI OUTPUT: Font height selector.
    output$inspect_doc_selector_heights <- renderUI({
        selectizeInput("inspect_doc_selected_height",
            label = "Only show font heights",
            choices = font_heights(),
            multiple = TRUE
        )
    })
    
    # OUTPUT: Document inspector table.
    output$table_inspect_doc <- renderDataTable({
        
        # Show number of words for each height as set by user.
        df <- df_doc_raw() %>%
            select(height, text) %>% 
            group_by(height) %>%
            summarise(words = str_c(text, collapse = " "), .groups = 'drop')
       
        if (input$inspect_doc_limit_chars) {
            df %<>%
                mutate(words = str_trunc(words, input$inspect_doc_no_chars))
        }
        
        # Show font heights set by user.
        if (!is.null(input$inspect_doc_selected_height)) {
            df %<>% filter(height %in% input$inspect_doc_selected_height)
        }
        
        datatable(
            df,
            rownames = FALSE,
            colnames = c("Font Height", "Words"),
            extensions = 'Scroller',
            options = list(
                deferRender = TRUE,
                scrollY = 400,
                scroller = TRUE,
                search = list(regex = TRUE, caseInsensitive = FALSE))) %>% 
            formatStyle(2, whiteSpace = "normal")
    })
    
    # Select words for analysis ---------------------------------------
    
    # REACTIVE UI OUTPUT: User selects heights of fonts for analysis.
    output$selector_font_heights <- renderUI({
        selectizeInput(
        "selected_font_heights",
        label = pretty_label(
        main = "Select font heights for analysis",
               "Select all fonts that make up the body and section",
               "headings for analysis.  The", em("Inspect Document"),
               "button displays a summary of document words versus",
               "height to help you choose."
        ),
        choices = font_heights(),
            multiple = TRUE
        )
    })
    
    # Document filtered by fonts for analysis.
    df_doc_font_filter <- reactive({
        req(input$selected_font_heights)
        df_doc_raw() %>% 
            filter(height %in% input$selected_font_heights)
    }) 
    
    # REACTIVE UI INPUT: Selects height of candidate section names.
    output$selector_section_heights <- renderUI({
        
        # Get font heights for documents ONLY IF there are less
        # than 1000 candidate words.  This prevents the server becoming
        # unresponsive.
        heights <- df_doc_font_filter() %>% 
            count(height) %>%
            filter(n < 1000) %>%
            arrange(height) %>%
            pull(height)
        
        selectInput(
            "selected_height",
            label = pretty_label(
                main = "Choose section heading font height",
                "NB: Only font heights for section name candidates with",
                "1000 or less words are included"
            ),
            choices = heights,
            selected = heights[which.max(heights)]
        )
    })
     
    # Get candidate section names.
    df_section_lookup_raw <- reactive({
        req(input$selected_height)
        get_section_lookup(df_doc_font_filter(), input$selected_height)
    })
    
    observe({
        values$section_lookup <-
            df_section_lookup_raw() %>%
                distinct(heading, .keep_all = TRUE) %>%
                mutate(renamed_heading = heading)
    })

    # Rename section headings -----------------------------------------

    # OUTPUT: User renames sections of interest.
    output$rename_sections <- renderRHandsontable({
        df <- values$section_lookup %>% 
            select(page, heading, renamed_heading)
        
        df %>%
            rhandsontable(
                colHeaders = c('Page', 'Original Name','New Name'),
                rowHeaders = NULL) %>% 
            hot_col('Page', readOnly = TRUE) %>% 
            hot_col('Original Name', readOnly = TRUE) %>% 
            hot_rows(fixedRowsTop = 1)
    })
    observe({
        req(input$rename_sections)
        df <- hot_to_r(input$rename_sections)
        values$section_lookup %<>%
            select(-renamed_heading) %>% 
            left_join(df, by = c('page', 'heading'))
    })
    
    # Create corpus ---------------------------------------------------
    
    # Update corpus when filtered data changes.
    corpus <- reactive({
        df_doc_font_filter() %>% 
            left_join(values$section_lookup,
                      by = c('doc_line', 'page')) %>% 
            select(-heading) %>% 
            rename(heading = renamed_heading) %>% 
            fill(heading, head_id) %>% 
            drop_na() %>% 
            make_corpus()
    })
    
    # OUTPUT: Inspect corpus.
    output$table_inspect_corpus <- renderDataTable({
        req(nrow(corpus()) > 0)
        df <- corpus() %>% 
            mutate(corpus = str_trunc(corpus, 500, 'center'))
        datatable(df, rownames = FALSE, colname = c("Section Name", "Corpus"),
            options = list(
                deferRender = TRUE,
                scrollY = 400,
                scroller = TRUE,
                search = list(regex = TRUE, caseInsensitive = FALSE))) %>% 
            formatStyle(2, whiteSpace = "normal")
    })
    
    # Get TF_IDF.
    tf_idf <- reactive({
        
        if (nrow(corpus()) > 0) {
            no_sections <- corpus() %>% 
                distinct(heading) %>% 
                pull(heading) %>% 
                length()
        } else {
            no_sections <- 0
        }
        
        validate(
            need(no_sections > 1, paste(
                "You must select at least two sections headings to",
                "calculate the TF-IDF")
            )
        )
        corpus() %>%
            unnest_tokens(word, corpus) %>%
            filter(!str_detect(word, '[[:digit:]]')) %>% 
            count(heading, word) %>%
            bind_tf_idf(word, heading, n)
    })
    
    # OUTPUT: Display TF-IDF
    output$tf_idf_table <- renderDataTable({
        datatable(
            tf_idf(),
            colnames = c("Section", "Word", "Term Count", "Term Freq.", 
                         "IDF", "TF-IDF"),
            filter = 'top',
            options = list(search = list(regex = TRUE,
                                         caseInsensitive = FALSE))
        )
    })
    
    # Visualisation ---------------------------------------------------

    # REACTIVE UI OUTPUT: User selects sections to visualise.
    output$selector_visualise <- renderUI({
        req(nrow(corpus()) > 0)
        choices = corpus()$heading
        checkboxGroupInput("selected_sections_visualise",
                    label = "Choose sections",
                    choices = choices,
                    selected = choices[c(1, 2)]
        )
    })
    
    # OUTPUT: Display plot of word frequencies.
    output$word_freqs <- renderPlot({
        req(tf_idf())
       
        validate(
            need(length(input$selected_sections_visualise) > 0,
                "You must select one or more sections to visualise",
            )
        )
        
        # Filter visualisation by user specified section headings.
        df_plt <- tf_idf() %>%
            filter(heading %in% input$selected_sections_visualise)
        
        # Plot graph.
        if (input$selected_plot_visualise == "Bar Plot") {
            g <- plot_word_freq(df_plt)
        } else {
            g <- plot_word_cloud(df_plt)
        }
        g
    })
}

# ------------------------------------------------------------------------- 
# RUN APP
# ------------------------------------------------------------------------- 

shinyApp(ui, server)
