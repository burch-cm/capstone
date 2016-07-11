# Kitt Burch
# version = 0.5

library(shiny)
suppressMessages(library(data.table))
library(ngram)
library(magrittr)
library(readr)
library(qdap)

# UI
ui <- shinyUI(fluidPage(
   titlePanel("Next Word Prediction"),
   fluidRow(
       column(6,
              h3("Input"),
              textInput("inText", label = "Input text"),
              numericInput("num_guesses", label = "Number of guesses", value = 1, min = 1, max = 10, step = 1),
              actionButton("goButton", label = "Predict Next Word")),
       column(6,
              h3("Prediction"),
              tableOutput("outText"))
   )
))

# Server
server <- shinyServer(function(input, output) {
    # load data
    n1 <- read_rds("../data/unigram_count.rds")
    setorder(n1, -prop)
    n2 <- read_rds("../data/bigram_count.rds")
    setorder(n2, -prop)
    n3 <- read_rds("../data/trigram_count.rds")
    setorder(n3, -prop)
    n4 <- read_rds("../data/tetragram_count.rds")
    setorder(n4, -prop)
    
    # crude count-based prediction function with backoff
    get.nw <- function(phrase, n = 1){
        require(data.table)
        get.ng <- function(phrase){
            require(qdap)
            require(ngram)
            phrase <- tolower(phrase)
            phrase <- gsub("[[:punct:]]", "", phrase)
            phrase <- gsub("[0-9]", "", phrase)
            phrase <- Trim(clean(phrase))
            ng <- ngram_asweka(phrase, min = 1, max = 1) # get ngrams
            return(ng)
        }
        ng <- get.ng(phrase)
        k <- length(ng)
        if(k < 1) return("Phrase length of less than one.")
        w.last <- ng[k] # last word in phrase
        w.ntl <- ifelse(k >= 2, ng[k-1], NA) # next-to-last word
        w.2tl <- ifelse(k >= 3, ng[k-2], NA) # second-to-last word
        if(k >= 4){
            ans <- n4[term1 == w.2tl & term2 == w.ntl & term3 == w.last, term4][1:n] %>%
                .[complete.cases(.)]
            if(length(ans) >= 1){
                return(ans)
            } else {
                get.nw(paste(ng[2:length(ng)], collapse = " "), n)
            }
        } else if(k == 3){
            ans <- n3[term1 == w.ntl & term2 == w.last, term3][1:n] %>%
                .[complete.cases(.)]
            if(length(ans) >= 1){
                return(ans)
            } else {
                get.nw(paste(ng[2:length(ng)], collapse = " "), n)
            }
        } else if(k == 2){
            ans <- n2[term1 == w.last, term2][1:n] %>%
                .[complete.cases(.)]
            if(length(ans) >= 1){
                return(ans)
            } else {
                get.nw(paste(ng[2:length(ng)], collapse = " "), n)
            }
        } else {
            return(n1[1:n, term1])
        }
    }
    
    # load the data
    prediction <- eventReactive(input$goButton, {
        get.nw(input$inText, n = input$num_guesses) %>% .[complete.cases(.)]
    })
    # 
    output$outText <- renderTable({
        prediction() %>% as.data.frame
    })
})

# Run the application 
shinyApp(ui = ui, server = server)

