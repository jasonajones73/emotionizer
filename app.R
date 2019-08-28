library(shiny)
library(shinythemes)
library(tidyverse)
library(tidytext)
library(pdftools)
library(DT)
library(ggdark)

options(shiny.maxRequestSize=50*1024^2) 

nrc <- read_rds("data/nrc.rds")

# Define UI for application
ui <- navbarPage(
    title = img(src = "https://storage.googleapis.com/proudcity/elgl/uploads/2019/07/elgl-logo-189x64.png",
                height = "100%"),
    fluid = TRUE,
    windowTitle = "The Emotionizer",
    collapsible = TRUE,
    theme = shinytheme(theme = "flatly"),
    
    header = list(tags$head(HTML('<link href="https://fonts.googleapis.com/css?family=Roboto+Mono" rel="stylesheet">')),
                  tags$head(HTML('<style>* {font-size: 100%; font-family: Roboto Mono !important;}</style>'))),
    
    tabPanel(title = "PDF Upload",
             
             fluidPage(
                 
                 fluidRow(
                     column(width = 1),
                     column(width = 3,
                            tags$p("This application allows you to upload any PDF document and start
                                   exploring the text contained within. Once you upload a file, a 
                                   searchable data table will render for you. Upload a PDF document 
                                   below to start exploring!"),
                            tags$p(fileInput(inputId = "file", label = "Choose PDF File",
                                             accept = ".pdf"))),
                     
                     column(width = 7,
                            tags$p(DTOutput("contents"))
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
                          tags$p(numericInput(inputId = "n", label = "Please select
                                              your grouping size", value = 2, min = 2,
                                              max = 5, step = 1))
                          ),
                   column(width = 7,
                          tags$p(DTOutput("ngramstable"))
                          ),
                   column(width = 1)
               )  
             )
             ),
    
    tabPanel(title = "Visualize",
             fluidPage(
                 fluidRow(
                     column(width = 3,
                            tags$p("If you do not see a visualization, it is because 
                            you have not uploaded a PDF yet. Jump back over to the other
                            tab and take care of that first! This is a sample visualization 
                            for testing. If a use case can be determined, more customization 
                            and visualization will be made available")),
                     
                     column(width = 9,
                            tags$p(plotOutput("plot", height = "500px")))
                     )
                 )
             )
    )

# Define server logic
server <- function(input, output) {
    
    file <- reactive({
        req(input$file)
        
        pdf_text(input$file$datapath) %>%
            tibble(text = .) %>%
            mutate(page_number = row_number())
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
    
    output$contents <- renderDT({
        req(input$file)
        
        datatable(data = original(), rownames = FALSE, options = list(
            columnDefs = list(list(className = 'dt-left', targets = 0:3))
        ), colnames = c("Page Number", "Word", "Word Count", "Association", "Category"))
    })
    
    output$ngramstable <- renderDT({
        req(input$file)
        
        datatable(data = ngram(), rownames = FALSE, options = list(
            columnDefs = list(list(className = 'dt-left', targets = 0:1))
        ), colnames = c("Word Group", "Group Count"))
    })
    
    output$plot <- renderPlot({
        req(input$file)
        
        file() %>%
            ggplot(aes(page_number, sent_count)) +
            geom_line(aes(color = sentiment)) +
            geom_point(aes(color = sentiment)) +
            geom_vline(xintercept = median(file()$page_number)) +
            dark_mode() +
            scale_colour_viridis_d() +
            labs(title = "Emotion Flow",
                 subtitle = "Horizontal line indicates document midpoint",
                 caption = "Author: Jason Jones \n Twitter: @packpridejones",
                 x = "Page Number") +
            theme(panel.background = element_blank(),
                  panel.grid.major.y = element_line(color = "light grey"),
                  axis.title.y = element_blank(),
                  legend.position = "none") +
            facet_wrap(~sentiment)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
