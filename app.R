library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(tidytext)
library(pdftools)
library(reactable)

options(shiny.maxRequestSize=50*1024^2) 

nrc <- read_rds("data/nrc.rds")

# Define UI for application
ui <- navbarPage(
    title = "The Emotionizer",
    fluid = TRUE,
    windowTitle = "The Emotionizer",
    collapsible = TRUE,
    theme = shinytheme(theme = "flatly"),
    
    header = list(tags$head(HTML('<link href="https://fonts.googleapis.com/css?family=Raleway" rel="stylesheet">')),
                  tags$head(HTML('<style>* {font-size: 100%; font-family: Raleway;}</style>'))),
    
    tabPanel(title = "PDF Upload",
             
             fluidPage(
                 
                 useShinydashboard(),
                 
                 fluidRow(
                     column(width = 1),
                     column(width = 3,
                            tags$p("This application allows you to upload a PDF document and start
                                   exploring the text contained within. Once you upload a file, a 
                                   searchable data table will render for you on this page. Upload 
                                   a PDF document below to start exploring!"),
                            tags$p(fileInput(inputId = "file", label = "Choose PDF File",
                                             accept = ".pdf")),
                            tags$br(),
                            tags$p("Move over to the Word Grouping tab to start exploring
                                   your document's text by n-grams.",),
                            tags$br(),
                            tags$a(href = "https://en.wikipedia.org/wiki/N-gram",
                                          target="_blank",
                                   "Click here to learn more about n-grams")),
                     
                     column(width = 7,
                            fluidRow(
                                tags$p(valueBoxOutput("length", width = 6)),
                                tags$p(valueBoxOutput("tokenized", width = 6))
                                ),
                            fluidRow(
                                tags$p(reactableOutput("contents"))
                            )
                            ),
                     column(width = 1)
                     )
                 )
             ),
    
    tabPanel(title = "Word Grouping",
             fluidPage(
               fluidRow(
                   column(width = 1),
                   column(width = 3,
                          tags$p(sliderInput(inputId = "n", label = "Please select
                                              your grouping size", value = 2, min = 2,
                                              max = 5, step = 1))
                          ),
                   column(width = 7,
                          tags$p(reactableOutput("ngramstable"))
                          ),
                   column(width = 1)
               )  
             )
             ),
    
    tabPanel(title = "Search All",
             fluidPage(
                 fluidRow(
                     column(width = 1),
                     column(width = 3,
                            tags$p("If you do not see a table, it is because 
                            you have not uploaded a PDF yet. Jump back over to the PDF Upload
                            tab and take care of that first! This is a simple table intended
                            for you to be able to search all text in the document by page.")),
                     
                     column(width = 7,
                            tags$p(reactableOutput("doctable"))),
                     column(width = 1)
                     )
                 )
             ),
    
    tabPanel(title = "Download Data",
             fluidPage(
                 fluidRow(
                     column(width = 2),
                     column(width = 8,
                            tags$p("Use the download button below to export your text data as
                                   a .csv file. There are two options provided for you to download your data.
                                   The Emotion Data will have the emotion and sentiment joins with associated counts
                                   just as you see on the PDF Upload tab after uploading data. The Original Data will 
                                   just be the cleaned text data by page from your original upload with no other
                                   processing."),
                            tags$p(downloadButton("downloademotiondata",
                                                  label = "Download Emotion Data")),
                            tags$p(downloadButton("downloadoriginal",
                                                  label = "Download Original Data"))),
                     column(width = 2)
                 )
             ))
    )

# Define server logic
server <- function(input, output) {
    
    file <- reactive({
        req(input$file)
        
        pdf_text(input$file$datapath) %>%
            tibble(text = .) %>%
            mutate(page_number = row_number())
    })
    
    downloaddat <- reactive({
        req(input$file)
        
        file() %>%
            mutate(text = str_remove_all(text, "[:punct:]")) %>%
            mutate(text = str_remove_all(text, "[:digit:]")) %>%
            unnest_tokens("word", "text")
    })
    
    output$length <- renderValueBox({
        req(input$file)
        
        valueBox(
            value = prettyNum(nrow(file()), big.mark = ","),
            subtitle = "Document Pages",
            icon = icon("book"),
            color = "green"
        )
    })
    
    original <- reactive({
        req(input$file)
        
        file() %>%
            unnest_tokens("word", "text") %>%
            anti_join(get_stopwords()) %>%
            inner_join(nrc) %>%
            group_by(page_number, sentiment, word) %>%
            summarise(sent_count = n()) %>%
            ungroup() %>%
            arrange(desc(sent_count)) %>%
            mutate(category = case_when(sentiment == "negative" | sentiment == "positive" ~ "Sentiment",
                                        sentiment != "negative" & sentiment != "positive" ~ "Emotion")) %>%
            mutate(sentiment = str_to_title(sentiment)) %>%
            mutate(word = str_to_title(word)) %>%
            select(page_number, word, sent_count, sentiment, category)
    })
    
    output$tokenized <- renderValueBox({
        req(input$file)
        
        valueBox(
            value = prettyNum(sum(original()$sent_count, na.rm = TRUE), big.mark = ","),
            subtitle = "Tokenized Words",
            icon = icon("bar-chart"),
            color = "purple"
        )
    })
    
    ngram <- reactive({
        req(input$file)
        
        file() %>%
            mutate(text = str_remove_all(text, "[:punct:]")) %>%
            mutate(text = str_remove_all(text, "[:digit:]")) %>%
            unnest_tokens("group", "text", token = "ngrams", n = input$n) %>%
            group_by(group) %>%
            summarise(group_count = n()) %>%
            ungroup() %>%
            arrange(desc(group_count))
    })
    
    output$contents <- renderReactable({
        req(input$file)
        
        reactable(data = original(), rownames = FALSE,
                  defaultColDef = colDef(align = "center"), columns = list(
                      page_number = colDef(name = "Page Number"),
                      word = colDef(name = "Word"),
                      sent_count = colDef(name = "Word Count"),
                      sentiment = colDef(name = "Association"),
                      category = colDef(name = "Category")
                      ))
    })
    
    output$ngramstable <- renderReactable({
        req(input$file)
        
        reactable(data = ngram(), rownames = FALSE, 
                  defaultColDef = colDef(align = "center"), columns = list(
                      group = colDef(name = "Word Group", align = "left"),
                      group_count = colDef(name = "Group Count")
                      ))
    })
    
    output$doctable <- renderReactable({
        req(input$file)
        
        reactable(data = downloaddat(), rownames = FALSE, filterable = TRUE,
                  defaultColDef = colDef(align = "center"), columns = list(
                      page_number = colDef(name = "Page Number"),
                      word = colDef(name = "Word")
                  ))
    })
    
    output$downloademotiondata <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(original(), file)
        }
    )
    
    output$downloadoriginal <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(downloaddat(), file)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
