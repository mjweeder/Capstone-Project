library(shiny)
library(stringr)
library(tm)

ui <- fluidPage(
    titlePanel("Word Prediction Model for Capstone project"),
    h4("Enter one to five words."), 
    h4("The predicted following word will be displayed in the Markov Table."),
    h4("Enter text inputbox and then press the enter text input."),
    h4("If another prediction is desired repeat proceeding steps "),
    h4("Note: Be patient the Model is designed for high probability of success. Can take up to 10 seconds"), 
    sidebarLayout(
        headerPanel(""),
        sidebarPanel(
            #helpText("Enter One, Two, Three or more words"),
            hr(),
            p(""),
            textInput("txt", "Type the word or phrase here:",value = ""),
            
            actionButton("doBtn", "Enter text.")
            
        )),
    mainPanel(
        h3("Results of predictive model"),
        h4("Markov Table for Probability for Following Word."),
        dataTableOutput('mytable')
    )
)

## Function predicting the next word
predictWord <- function(txt) {
    Ngram_4 <- inputFunc(ngram_4a.rds)
    
    txt1 <- txt
    
    txt1<- gsub("[^\x20-\x7E]", "", txt1)
    
    # remove all non alphanumeric characters
    txt1<- gsub("[[:punct:]]", "", txt1, perl=TRUE)
    
    # remove numbers
    txt1 <- gsub('[0-9]+', '', txt1)
    
    # remove quotation 
    txt1<- gsub("[\"]", "", txt1)
    
    # change characters to lower
    txt1<-  tolower(txt1)
    # trim leading and trailing white spaces
    trim <- function (x) gsub("^\\s+|\\s+$", "", x)
    txt1 <- trim(txt1)
    
    # Seperate phrase into individual components
    newVec <- (unlist(sapply(txt1, strsplit, "\\s+", USE.NAMES = FALSE)))
    # count number of words in input text
    ngramWords <- length(newVec ) 
    # check to see if there is invalid word
    vector.is.empty <- function(word) return(length(newVec) == 0 ) 
    bad <- vector.is.empty(newVec)
    if (bad == TRUE) { 
        wordDfT <- data.frame(Word = "Invalid Word",Freq =0 , Probability = 0)}   # bigram prediction
    else if (ngramWords == 1) { Ngram2Func(newVec, Ngram_4)}   # bigram prediction
    else if (ngramWords == 2) {Ngram3Func(newVec,Ngram_4 )}   # trigram prediction
    else if (ngramWords >= 3) {Ngram4Func(newVec, Ngram_4)} # 4 or more-gram prediction
   
}

# input ngram input function
inputFunc <- function(ngram_4a.rds){
    Ngram_4 <- readRDS("ngram_4a.rds")
    return(Ngram_4)
}

# Ngram 2 function
Ngram2Func <- function(newVec,Ngram_4 ) {
    print("first")
    a <- 2; b <- 3; c <-4
    # match last two words of the string and find potential next words.
    Ngram_4Seconda <- Ngram_4[,a] == newVec[1]
    Ngram_4Secondb <- Ngram_4[,b] == newVec[1] 
    Ngram_4Secondc <- Ngram_4[,c] == newVec[1]
    print("second")
    
    vector_4a <- Ngram_4[Ngram_4Seconda,]
    vector_4b <- Ngram_4[Ngram_4Secondb,]
    vector_4c <- Ngram_4[Ngram_4Secondc,]
    print("third")
    
    fourthWorda <- as.character(vector_4a$V4)
    fourthWordb <- as.character(vector_4b$V5)
    fourthWordc <- as.character(vector_4b$V6)
    print("fourth")
    
    # Combine character vectors
    word <- c(fourthWorda, fourthWordb,fourthWordc)
    # note if no observations
    vector.is.empty <- function(word) return(length(word) ==0 )    # if word is present then continue processing
    bad <- vector.is.empty(word)
    if(bad == FALSE ) {
        # sort vector
        word <- sort(word)
        #twitterValidity <- invalid(word)
        wordTable <- table(word)
        wordDf <- as.data.frame(wordTable)
        
        wordOrder <- order(wordDf$Freq, decreasing=TRUE)
        
        wordDfT <- wordDf[wordOrder,]   
        totalFreq <- sum(wordDfT[,2])
        wordDfT$Probability <- wordDfT[,2] / totalFreq
        return(wordDfT)}
    # If word is not present return non-word
    else {
        wordDfT <- data.frame(Word = "No predicted word", Freq =0 , Probability = 0)
        return(wordDfT)
    }
}
# Ngram 3 function
Ngram3Func <- function(newVec, Ngram_4) {
    
    a <- 2; b <- 3; c <- 4
    # match last two words of the string and find potential next words.
    Ngram_4Seconda <- Ngram_4[,a] == newVec[1] &  Ngram_4[,b] == newVec[2]
    Ngram_4Secondb <- Ngram_4[,b] == newVec[1] &  Ngram_4[,c] == newVec[2]
    
    vector_4a <- Ngram_4[Ngram_4Seconda,]
    vector_4b <- Ngram_4[Ngram_4Secondb,]
    
    fourthWorda <- as.character(vector_4a$V5)
    fourthWordb <- as.character(vector_4b$V6)
    
    # Combine character vectors
    word <- c(fourthWorda, fourthWordb)
    # note if no observations
    vector.is.empty <- function(word) return(length(word) ==0 ) 
    good <- vector.is.empty(word)
    
    # if word is present then continue processing
    if(good == FALSE) {
        # sort vector
        word <- sort(word)
        #twitterValidity <- invalid(word)
        wordTable <- table(word)
        wordDf <- as.data.frame(wordTable)
        
        wordOrder <- order(wordDf$Freq, decreasing=TRUE)
        
        wordDfT <- wordDf[wordOrder,]   
        totalFreq <- sum(wordDfT[,2])
        wordDfT$Probability <- wordDfT[,2] / totalFreq
        return(wordDfT)}
    # If word is not present return non-word
    else {
        wordDfT <- data.frame(Word = "No predicted word",Freq =0 , Probability = 0)
        return(wordDfT)
    }
}

# Ngram 4 function
Ngram4Func <- function(newVec,Ngram_4 ) {
    # calculate number of words in input text
    wordNumber <- length(newVec)
    # assign which newVec to use
    if (wordNumber == 3) {d <- 2; e <- 3}   # 3-gram
    else if (wordNumber == 4) {d <- 3; e <- 4} # 4-gram
    else if (wordNumber == 5) {d <- 4; e <- 5} # 5-gram
    
    a <- 2; b <- 3; c <- 4
    # match last two words of the string and find potential next words.
    Ngram_4Seconda <- Ngram_4[,a] == newVec[d] &  Ngram_4[,b] == newVec[e]
    Ngram_4Secondb <- Ngram_4[,b] == newVec[d] &  Ngram_4[,c] == newVec[e]
    
    vector_4a <- Ngram_4[Ngram_4Seconda,]
    vector_4b <- Ngram_4[Ngram_4Secondb,]
    
    fourthWorda <- as.character(vector_4a$V5)
    fourthWordb <- as.character(vector_4b$V6)
    
    # Combine character vectors
    word <- c(fourthWorda, fourthWordb)
    # note if no observations
    vector.is.empty <- function(word) return(length(word) ==0 )
    good <- vector.is.empty(word)
    
    # if word is present then continue processing
    if(good == FALSE) {
    # sort vector
    word <- sort(word)
    #twitterValidity <- invalid(word)
    wordTable <- table(word)
    wordDf <- as.data.frame(wordTable)
    wordOrder <- order(wordDf$Freq, decreasing=TRUE)
    wordDfT <- wordDf[wordOrder,]   
    totalFreq <- sum(wordDfT[,2])
    wordDfT$Probability <- wordDfT[,2] / totalFreq
    return(wordDfT)}
    # If word is not present return non-word
    else {
        wordDfT <- data.frame(Word = "No predicted word",Freq =0 , Probability = 0)
        return(wordDfT)
    }
}

server = function(input, output,session){
    # action to call predictWord function     
    observeEvent(input$doBtn, {
        wordDfT <- predictWord(input$txt)
        output$mytable = renderDataTable({wordDfT})
    })
    session$onSessionEnded(stopApp) # close session when web tab is closed 
}
shinyApp(ui = ui, server = server, enableBookmarking = "disable")
