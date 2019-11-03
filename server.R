options(shiny.maxRequestSize=100*1024^2)
library(shiny)
library(wordcloud)
library(qdap)
library("Rgraphviz")
library(rWordCloud)
library(htmlwidgets)
library(shinyjs)
library(rd3BarChart)
library(reshape)
library(data.table)
library(rhandsontable)
library(stringi)
library(stringr)
library(DT)
library(purrr)
library(rd3Cluster)
library(readr)
library(XLConnect)
library(openxlsx)
library(dplyr)
library(tidyr)
Vars <- reactiveValues(words = NULL)

shinyServer(function(input, output,session) {
  session$onSessionEnded(stopApp)
  sessionVars <- reactiveValues(dfInput = data.frame(),dtm=NULL,dfFilter=NULL,dfWC_2=NULL,
                                dfWC_input=NULL,ngramdfWC1 = NULL, ngramdfWC2 = NULL)
  updateSelect <- reactive({
    
    if (is.null(input$file1) )
      return(NULL)
    
    inFile <- input$file1
    if( tools::file_ext(input$file1$name)=="csv"){
      inp <- read.csv(inFile$datapath, header = T, sep = ",",stringsAsFactors = F)
      inp <- as.data.frame(inp)
      inp$DocNo <- 1:nrow(inp)
      sessionVars$dfInput <- inp[,c(ncol(inp),1:(ncol(inp)-1))]
    }
    else{
      # XLConnect::xlcFreeMemory()
      # wb <- loadWorkbook(inFile$datapath)
      # inp <- readWorksheet(wb,sheet = 1)
      # inp <- as.data.frame(inp)
      # inp$DocNo <- 1:nrow(inp)
      # sessionVars$dfInput <- inp[,c(ncol(inp),1:(ncol(inp)-1))]
      #   #xlsx::read.xlsx(inFile$datapath,sheetIndex = 1,stringsAsFactors=FALSE)
      # XLConnect::xlcFreeMemory()
      inp <- read.xlsx(inFile$datapath,sheet = 1)
      inp <- as.data.frame(inp)
      inp$DocNo <- 1:nrow(inp)
      sessionVars$dfInput <- inp[,c(ncol(inp),1:(ncol(inp)-1))]
    }
    df1 <- sessionVars$dfInput
    
  })
  observe({
    
    #inFile <- "D:/R/TextMining/CCD-messages.csv"
    if (is.null(updateSelect()))
      return(NULL)
    
    coln <- colnames(updateSelect())
    colcount <- 1:length(coln)
    
    updateSelectInput(session,"intCols", choices = colcount)
    
  })
  observe({
    if (is.null(updateSelect()))
      return(NULL)
    coln <- colnames(updateSelect())
    colcount <- 1:length(coln)
    updateSelectizeInput(
      session, 'cols',server = T,choices = coln,
      options = list(placeholder = 'select column names',maxItems = input$intCols)
    )
  })
  output$download_help <- downloadHandler(
    filename = function() {
      paste("TextAnalytics_Tool_helpV1_0.pptx", sep="")
    },
    content = function(file) {
      src <- normalizePath('TextAnalytics_Tool_helpV1_0.pptx')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, file, overwrite = TRUE)
      
      
      #file.rename(out, file)
    }
  )
  
  output$download_stpwds <- downloadHandler(
    filename = function() {
      paste("sysStopWords.csv", sep="")
    },
    content = function(file) {
      src <- normalizePath('stopwords.txt')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, file, overwrite = TRUE)
      
      
      #file.rename(out, file)
    }
  )
  
  output$plot <- renderd3Cloud({
    #createwordcloud()
    freq1 <- data()
    if(is.null(freq1))
      return(NULL)
    if(as.numeric(input$wordLimit) > length(freq1)){
      freq1 <- freq1[1:length(freq1)]
    } else {
      freq1 <- freq1[1:as.numeric(input$wordLimit)]
    }
    colr.idx <- function(x,max,len) {
      return ((x / max) * len)
    }
    brew.col <- brewer.pal(8,"Dark2")
    colr <-
      brew.col[ceiling(sapply(freq1,colr.idx,max(freq1),length(brew.col)))]
    #d3TextCloud(names(freq1), freq1,height = "850",color = "lightgray",textColor = colr)
    sessionVars$flg=1
    gc()
    return(d3Cloud(
      names(freq1), freq1,height = "850",color = "lightgray",textColor = colr
    ))
    #     return(d3Cloud(
    #       names(freq1), freq1,height = "850",color = "lightgray",textColor = colr
    #     ,txtinputid = "wc_1_word"))
    #wordcloud(names(freq1),freq1,scale = c(5,2),min.freq = 2,max.words = 20,colors=brewer.pal(8, "Dark2"))
    
  })
  
  data <- eventReactive(input$submit,{
    tryCatch( expr = {
      sessionVars$dfWC_2=NULL
      if (is.null(input$cols))
        return(NULL)
      df1 <-
        as.data.frame(sessionVars$dfInput)
      
      shinyjs::disable("submit")
      sw <- NULL
      if(!is.null(input$swFile)){
        inFile1 <- input$swFile
        sw <- readLines(inFile1$datapath)
      }
      
      df1 = as.data.frame(df1[,c(colnames(df1)[1],input$cols)],stringsAsFactors = F)
      # if (input$txtFilter !="" | is.null(input$txtFilter)) {
      #   spltFilter <- unlist(strsplit(input$txtFilter,","))
      #   idx <- 1:nrow(df1)
      #   if(length(input$cols)>1){
      #     dat <- as.data.frame(lapply((lapply(df1[,input$cols],iconv,from="WINDOWS-1252",to="UTF-8")),
      #                                 tolower),stringsAsFactors=F)
      #     Filter <- lapply(spltFilter, function(splt)
      #                                 which(t(apply(dat,1,
      #                                  function(row) grepl(tolower(splt),row))),arr.ind = T)[,1])
      #     idx <- unique(unlist(Filter))
      #   }else{
      #     Filter <- lapply(spltFilter,grepl,tolower(iconv(df1[,input$cols],"WINDOWS-1252","UTF-8")))
      #     idx <- which(Reduce(`|`,Filter),arr.ind = T)
      #   }
      # which(grepl(tolower(input$txtFilter), tolower(iconv(df1[,input$cols],"WINDOWS-1252","UTF-8"))),arr.in = TRUE)
      #   df1 = as.data.frame(df1[idx,input$cols],stringsAsFactors = F)
      # } else {
      #   df1 = as.data.frame(df1[,input$cols],stringsAsFactors = F)
      # }
      # 
      if (ncol(df1) == 1) {
        colnames(df1) <- "col1"#input$cols
      }else{
        colnames(df1) <- paste("col",1:ncol(df1),sep="")
      }
      # if(!is.null(input$grid)){
      #   for(i in 1:length(input$grid$data)){
      #     chr <- unlist(input$grid$data[i])
      #     if(chr[1] !="" & chr[2] !="" )
      #     df1 <- data.frame(col1=apply(df1,1,stri_replace_all_fixed
      #                                  ,unlist(strsplit(chr[2],split = ","))
      #                                  ,chr[1], vectorize_all=F),stringsAsFactors = F)
      #   }
      # }
      mapWords <- NULL
      if(!is.null(input$grid)){
        mapWords <- input$grid
        
      }
      rmWords <- NULL
      if(input$rmWrds == TRUE){
        if(!is.null(input$words))
          if(input$words !="")
            rmWords <- unlist(strsplit(input$words,split = ","))#input$words
      }
      withProgress(message = "Loading...", value = 0,{
        # Number of times we'll go through the loop
        n <- 10
        
        for (i in 1:n) {
          # Each time through the loop, add another row of data. This is
          # a stand-in for a long-running computation.
          
          
          # Increment the progress bar, and update the detail text.
          incProgress(1/n, detail = paste( i*10,"%"))
          
          # Pause for 0.1 seconds to simulate a long computation.
          Sys.sleep(0.1)
        }
        setProgress(message = 'Processing Wordcloud',detail = '')
        if(is.na(unique(df1$col1))){
          shinyjs::js$errorPanel("Document doesn't have data")
          shinyjs::enable("submit")
          return(NULL)
        } else {
          
          #dtm1 <- getWordcloud(df1,sw,input$tf,as.numeric(input$ngram),input$rmNum,rmWords,mapWords)
          srcdf <- df1
          dfout = NULL
          dtm.combine <- NULL
          if(nrow(srcdf)>1000){
            
            # print('start')
            t = 1000
            rem = nrow(srcdf) %% t
            
            for(i in 1:((nrow(srcdf)-rem)/t)){
              #print(paste0('loop',i))
              j=((i-1)*t)+1
              
              t1 = t*i
              dff=srcdf[j:t1,]
              dtm1 <- getWordcloud(dff,sw,input$tf,as.numeric(input$ngram),input$rmNum,rmWords,mapWords)
              if(is.null(dtm.combine))
                dtm.combine <- dtm1
              else
                dtm.combine <- c(dtm.combine,dtm1)
              lstop1 <- as.data.frame(as.table(dtm1),stringsAsFactors = F)
              if(is.null(dfout))
                dfout <- lstop1[lstop1$Freq>0,]
              else
                dfout <- rbind(dfout,lstop1[lstop1$Freq>0,])
            }
            #print('end loop')
            #print('rem data')
            if(rem != 0){
              j <- (nrow(srcdf)-rem)+1
              t1 <- nrow(srcdf)
              dff=srcdf[j:t1,]
              dtm1 <- getWordcloud(dff,sw,input$tf,as.numeric(input$ngram),input$rmNum,rmWords,mapWords)
              dtm.combine <- c(dtm.combine,dtm1)
              lstop1 <- as.data.frame(as.table(dtm1),stringsAsFactors = F)
              dfout <- rbind(dfout,lstop1[lstop1$Freq>0,])
            }
            #print('end')
          } else {
            dff = srcdf
            dtm1 <- getWordcloud(dff,sw,input$tf,as.numeric(input$ngram),input$rmNum,rmWords,mapWords)
            dtm.combine <- dtm1
            lstop1 <- as.data.frame(as.table(dtm1),stringsAsFactors = F)
            dfout <- lstop1[lstop1$Freq>0,]
          }
          #en = Sys.time()
          
          sessionVars$dtm <- dtm.combine
          
          setDT(dfout)
          output1 <- dfout[, sum(Freq),by="Terms"]
          indx <- which(output1$V1 >=2,arr.ind = T)
          if(nrow(output1)>=200)
            indx <- which(output1$V1 >=(output1[order(-V1)])[200,V1],arr.ind = T)
          output <- dfout[Terms %in% output1[indx,Terms]]
          
          sessionVars$ngramdfWC1 <- output
          output[,TotFreq := sum(Freq),by = c("Terms")]
          #ds <- unique(dataset[,-c(1,2,3)])
          if(nrow(output)==0){
            freq1 <- 45
            names(freq1) <-"No Terms"
          }
          Vars$words <- input$words
          output_unique <- (unique(output[,c(1,4)]))[order(-TotFreq)]
          freq1 <- output_unique$TotFreq
          names(freq1) <- output_unique$Terms
          return(freq1)
          # Vars$words <- input$words
          # freq1 <- (sort(apply(dtm1,2,sum), decreasing = T))
          # freq1 <- freq1[freq1>1]
        }
      });
      
      #return(freq1)
    }
    ,error = function(e) {
      shinyjs::js$errorPanel("Error occured while generating wordcloud. Please contact Admin.")
      return(NULL)
    }
    ,finally = shinyjs::enable("submit"))
  })
  observeEvent(input$ngram,{
    if(any(names(input) == 'rdrmWords')){
      if(input$rdrmWords=="n-gram Specific" ){
        Vars$words <- ''
        updateSelectizeInput(
          session, 'words',server = T,choices = NULL,selected = Vars$words
        )
      }
    }
  })
  
  #     observeEvent(input$rdrmWords,{
  #      
  #           
  #           updateSelectizeInput(
  #             session, 'words',server = T,choices = NULL,selected = Vars$words
  #           )
  #         
  #       
  #     })
  
  # notify <- eventReactive(input$submit,{
  #   if (length(input$words) > 0) {
  #     notificationItem(text = sprintf("You have removed %s", input$words), icon("remove"))
  #   }
  #   else {
  #     notificationItem(text = ("You haven't removed"), icon("remove"))
  #   }
  #   
  # })
  # #   observe({
  # #     toggle("messages",!is.null(input$words) && input$words != '')
  # #   })
  # msg <- eventReactive(input$submit,{
  #   #show("message")
  #   
  #   if (length(input$words) > 0 & input$rmWrds == 'words') {
  #     dropdownMenu(type = "messages", icon = icon("bell"),
  #                  messageItem(from = "Removed word(s)",
  #                              message = input$words,icon = icon("close"),
  #                              time = Sys.time()
  #                  )
  #     )
  #     
  #   }else if (length(input$words) == 0 | input$rmWrds != 'words') {
  #     
  #     return(NULL)
  #   }
  #   
  # })
  # output$notifyMenu <- renderMenu({
  #   if (any(names(input) == 'd3word') ) {
  #     dropdownMenu(type = "notifications", notificationItem(text = sprintf("You have clicked %s", input$d3word), icon("users")))
  #   }
  #   #     else if(input$rmWrds == 'words'){
  #   #         dropdownMenu(type = "notifications", notificationItem(text = sprintf("You have clicked %s", input$d3word), icon("users")),
  #   #         notify())
  #   #       }
  # })
  # output$messages <- renderMenu({
  #   msg()
  # })
  # output$progressBox <- renderValueBox({
  #   if (any(names(input) == 'd3word')) {
  #     valueBox(
  #       input$d3word, sprintf("You have clicked %s", input$d3word), icon = icon("remove"),
  #       color = "purple"
  #     )
  #     
  #   }
  #   else{
  #     return(valueBox(NULL, NULL, icon = NULL))
  #   }
  # })
  observe({
    toggleState("submit", !is.null(input$cols) )
    
  })
  observe({
    toggle("words", input$rmWrds==TRUE )
    toggle("rdrmWords", input$rmWrds==TRUE )
    
  })
  
  onclick("submit",shinyjs::show("wcContainer",time = 1, anim = TRUE, animType = "slide"))
  
  output$plot1 <- renderPlot({corPlot()})
  corPlot <- eventReactive(input$getCor,{                             
    #dtm1 <- getWordcloud(sprintf("%s.csv",input$select))
    if(is.null(sessionVars$dtm))
      return(NULL)
    
    tdm_temp <- (if(input$flg==0)
      t(removeSparseTerms(sessionVars$dtm,.995))
      else 
        t(removeSparseTerms(sessionVars$dtm1,.995)))
    wordcor=NULL
    
    if (input$txtFilter !="" | is.null(input$txtFilter)) {
      wordcor = input$txtFilter
      #       } else if(any(names(input) == 'wc_1_word')){
      #         wordcor = input$wc_1_word
    } 
    if(any(names(input) == 'd3word')){
      wordcor = input$d3word
    } 
    withProgress(message = "Loading...", value = 0,{
      # Number of times we'll go through the loop
      n <- 10
      
      for (i in 1:n) {
        # Each time through the loop, add another row of data. This is
        # a stand-in for a long-running computation.
        
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste( i*10,"%"))
        
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.1)
      }
      setProgress(message = 'Processing Correlation...',detail = '')
      assoc_len <- length(apply_as_tm(tdm_temp, tm:::findAssocs, wordcor,input$corLimit)[[1]])
      
      vecWordcor <- unlist(stri_extract_all_words(wordcor))
      assocTerms <- names(apply_as_tm(tdm_temp, tm:::findAssocs, wordcor, input$corLimit)[[1]])
      nTerm <- length(assocTerms)
      if(length(assocTerms)>10)
        nTerm <- 10
      if( assoc_len > 0 & !is.null(wordcor)){
        #nA = makeNodeAttrs(g, fillcolor="#ccc")
        
        plot(tdm_temp,assocTerms[1:nTerm],corThreshold=0.12,
             weighting = T,cex=2,main = list(sprintf("Correlation for the word: %s",wordcor), cex = 1.5,
                                             col = colorRampPalette(brewer.pal(8,"Dark2"))(1)),
             attrs = list(node=list(fillcolor = "lightblue",shape ="rectangle",fixedsize=FALSE,fontsize = 20,width=7,height=7)
                          ,graph=list(size = "8,8"), edge = list(color="black",weight=.5)))
        
        # } else if(vecWordcor > 1){
        #   vecAssoc_names <- lapply(vecWordcor,function(x) 
        #     names(apply_as_tm(tdm_temp, tm:::findAssocs, x,input$corLimit)[[1]]))
        #   vecAssoc_names <- unlist(vecAssoc_names)
        #   plot(tdm_temp,vecAssoc_names,corThreshold=.12,
        #        weighting = T,cex=1,main = list(sprintf("Correlation for the word: %s",wordcor), cex = 1.5,
        #                                        col = colorRampPalette(brewer.pal(8,"Dark2"))(1)),
        #        attrs = list(node=list(fillcolor = "lightblue",shape ="rectangle",fixedsize=FALSE,fontsize = 16,width=5,height=5)
        #                     ,graph=list(size = "10,10")))
      } else {
        return(NULL)
      }
    });
    
  })
  #   output$downloadWA <- downloadHandler(
  #     filename = function(){
  #       paste("Shinyplot","png",sep = ".")
  #       },
  #     content = function(file) {
  #       png(file)
  #       tdm_temp <- t(sessionVars$dtm)
  #       
  #       if(any(names(input) == 'd3word')){
  #         if(length(apply_as_tm(tdm_temp, tm:::findAssocs, input$d3word, .5)[[1]])>0){
  #           #nA = makeNodeAttrs(g, fillcolor="#ccc")
  #           
  #           plot(tdm_temp,names(apply_as_tm(tdm_temp, tm:::findAssocs, input$d3word, 0.5)[[1]]),corThreshold=.12,
  #                weighting = T,cex=1,main = list(sprintf("Correlation for the word: %s",input$d3word), cex = 1.5,
  #                                                col = colorRampPalette(brewer.pal(8,"Dark2"))(1)),
  #                attrs = list(node=list(fillcolor = "lightblue",shape ="rectangle",fixedsize=FALSE,fontsize = 16)
  #                ))
  #           
  #         }
  #       }
  #       
  #       
  #       dev.off()
  #     })
  #   observe({
  #     if (any(names(input) =='d3word')) {
  #       if(is.null(sessionVars$d3word)){
  #         sessionVars$d3word <- input$d3word
  #       } else {
  #       sessionVars$d3word <- c(sessionVars$d3word,input$d3word)
  #       }
  #     }
  #   })
  
  output$tab1 <-
    renderDataTable(
      extensions = c( 'Buttons'),
      options = list(
        scrollX = TRUE
        ,initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#003A60 !important', 'color': '#fff'});",
          "}")
        ,autoWidth = T,
        columnDefs = list(list(width = '100px', targets = "_all")),
        paging = T,
        deferRender = TRUE,
        scrollY = 350,
        scroller = TRUE,
        pageLength = 15, lengthMenu = list(c(10,50,100,-1), list('10','50','100', 'All')),
        searching = F,
        dom = 'BRfrltip',
        #dom = 'Rfrtlip',
        buttons = list(list(
          extend = 'collection',
          buttons = list(list(extend='copy')
                         , list(extend='csv',filename="Wordcloud")
                         , list(extend='excel',filename="Wordcloudxl")
                         , list(extend='pdf',filename="Wordcloudpdf")),
          text = 'Download'), I('colvis'))
        #buttons = c('copy', 'csv', 'excel', 'pdf', 'print', I('colvis'))
      ),
      rownames = F,
      filter = "none",
      class = "cell-border stripe",
      {
        # isolate(if (input$flg == 1) {
        #   #messages <- as.data.frame(sessionVars$dfFilter)
        #   messages <- as.data.frame(sessionVars$dfWC_2)
        # } else {
        #   if (!is.null(sessionVars$dfWC_2)& input$flg==2) {
        #     messages <- as.data.frame(sessionVars$dfWC_2)
        #   } else{
        #     messages <-
        #       as.data.frame(sessionVars$dfInput)
        #   }
        #   
        # })
        if(input$flg==1 ){
          messages <- as.data.frame(sessionVars$dfWC_2)
        }else {
          if(!is.null(sessionVars$dfWC_2) & input$flg==2){
            messages <- as.data.frame(sessionVars$dfWC_2)
          }else{
            messages <-
              as.data.frame(sessionVars$dfInput)
          }
        }
        idx <- 1:nrow(messages)
        if (input$flg != 1) {
          if (input$txtFilter != "" | is.null(input$txtFilter)) {
            spltFilter <- unlist(strsplit(input$txtFilter, ","))
            
            if (length(input$cols) > 1) {
              dat <-
                as.data.frame(lapply((
                  lapply(messages[, input$cols], iconv, from = "WINDOWS-1252", to = "UTF-8")
                ),
                tolower), stringsAsFactors = F)
              Filter <- lapply(spltFilter, function(splt)
                which(t(apply(dat, 1,
                              function(row)
                                grepl(tolower(splt), row))), arr.ind = T)[, 1])
              idx <- unique(unlist(Filter))
            } else{
              Filter <-
                lapply(spltFilter, grepl, tolower(iconv(
                  messages[, input$cols], "WINDOWS-1252", "UTF-8"
                )))
              idx <- which(Reduce(`|`, Filter), arr.ind = T)
              #idx <-
              # which(grepl(tolower(input$txtFilter), tolower(iconv(messages[,input$cols],"WINDOWS-1252","UTF-8"))),arr.in = TRUE)
            }
            
          }
        }
        if (any(names(input) == 'd3word')) {
          if (length(input$cols) > 1) {
            dat <-
              as.data.frame(lapply((
                lapply(messages[idx, input$cols], iconv, from = "WINDOWS-1252", to = "UTF-8")
              ), tolower), stringsAsFactors = F)
            idx <-
              which(t(apply(dat, 1, function(row)
                grepl(tolower(input$d3word), row))), arr.ind = T)[, 1]
            #which(grepl(tolower(input$d3word), tolower(iconv(messages[,input$cols],"WINDOWS-1252","UTF-8"))),arr.in = TRUE)
          } else {
            idx <-
              which(grepl(tolower(input$d3word), tolower(
                iconv(messages[idx, input$cols], "WINDOWS-1252", "UTF-8")
              )), arr.in = TRUE)
          }
          
        }
        #sessionVars$dfFilter <- messages[idx,]
        sessionVars$dfWC_2 <- messages[idx, ]
        messages[idx, ]
      }
    )
  
  
  
  dataWC <- eventReactive(input$searchButton,{
    
    
    if (is.null(input$cols))
      return(NULL)
    
    
    if(input$flg==1 ){
      df1 <- as.data.frame(sessionVars$dfWC_2)
      filterDF <- sessionVars$ngramdfWC2_dum
      if(unique(filterDF$ngram)!=stringi::stri_count_words(input$d3word))
        filterDF <- sessionVars$ngramdfWC1
    }else {
      if(!is.null(sessionVars$dfWC_2) & input$flg==2){
        df1 <- as.data.frame(sessionVars$dfWC_2)
        filterDF <- sessionVars$ngramdfWC2_dum
        searchWord <- input$d3word 
        if ((input$txtFilter !="" | is.null(input$txtFilter)) ) 
          searchWord <- input$txtFilter
        docno <- as.character(filterDF[Terms==searchWord,Docs])
        output <- filterDF[Docs %in% docno]
        sessionVars$ngramdfWC2_dum <- output
        output[,TotFreq := sum(Freq),by = c("Terms")]
        #ds <- unique(dataset[,-c(1,2,3)])
        if(nrow(output)==0){
          freq1 <- 45
          names(freq1) <-"No Terms"
        }
        Vars$words <- input$words
        output_unique <- (unique(output[,c(1,4,5)]))[order(-TotFreq)]
        freq1 <- output_unique$TotFreq
        names(freq1) <- output_unique$Terms
        return(freq1)  
      }else{
        df1 <- as.data.frame(sessionVars$dfInput)
        filterDF <- sessionVars$ngramdfWC1
      }
    }
    idx <- 1:nrow(df1)
    if(input$flg!=1 ){
      if ((input$txtFilter !="" | is.null(input$txtFilter)) ) {
        session$sendCustomMessage(type = 'testmessage',
                                  message = input$txtFilter)
        spltFilter <- unlist(strsplit(input$txtFilter,","))
        docno <- unique(as.character(filterDF[Terms %in% spltFilter,Docs]))
        idx <- which(df1$DocNo %in% docno,arr.ind = T)
        
      }
    }
    else{
      session$sendCustomMessage(type = 'testmessage',
                                message = "")
    }
    if (any(names(input) ==
            'd3word')) {
      df1<-as.data.frame(df1[idx,])
      
      dfwc_words=data.frame(words=input$d3word)
      if(is.null(sessionVars$dfWC_input)){
        sessionVars$dfWC_input <- dfwc_words
      }else{
        sessionVars$dfWC_input <- rbind(sessionVars$dfWC_input,dfwc_words)
      }
      docno <- as.character(filterDF[Terms==input$d3word,Docs])
      sessionVars$dfWC_2 <-df1[df1$DocNo %in% docno,] #df1[idx,]
      df1 = as.data.frame(df1[df1$DocNo %in% docno,c(colnames(df1)[1],input$cols)],stringsAsFactors = F)#df1[idx,input$cols]
    } 
    else{
      #return(NULL)
      sessionVars$dfWC_2 <- df1[idx,]
      df1 = as.data.frame(df1[idx,c(colnames(df1)[1],input$cols)],stringsAsFactors = F)#df1[idx,input$cols]
    }
    ngramVal = as.numeric(input$ngram_2)
    if (any(names(input) =='ngram_2dum')) {
      ngramVal = as.numeric(input$ngram_2dum)
    }
    sw <- NULL
    if(!is.null(input$swFile)){
      inFile1 <- input$swFile
      sw <- readLines(inFile1$datapath)
    }
    
    mapWords <- NULL
    if(!is.null(input$grid)){
      mapWords <- input$grid
      
    }
    
    if (ncol(df1) == 1) {
      colnames(df1) <- "col1"#input$cols
    }else{
      
      colnames(df1) <- paste("col",1:ncol(df1),sep="")
    }
    #dtm1 <- getWordcloud(df1,NULL,"tf",1,input$rmNum)
    #       if(!is.null(input$grid)){
    #         for(i in 1:length(input$grid$data)){
    #           chr <- unlist(input$grid$data[i])
    #           df1 <- data.frame(col1=apply(df1,1,stri_replace_all_fixed
    #                                        ,unlist(strsplit(chr[2],split = ","))
    #                                        ,chr[1], vectorize_all=F),stringsAsFactors = F)
    #         }
    #       }
    withProgress(message = "Loading...", value = 0,{
      # Number of times we'll go through the loop
      n <- 10
      
      for (i in 1:n) {
        # Each time through the loop, add another row of data. This is
        # a stand-in for a long-running computation.
        
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste( i*10,"%"))
        
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.1)
      }
      setProgress(message = 'Processing Wordcloud...',detail = '')
      if(nrow(df1)==0){
        session$sendCustomMessage(type = 'testmessage',
                                  message = 'Unable to process wordcloud for selected text')
        freq1 <- NULL
        # freq1<-c(20)
        # names(freq1)<-"NA"
      }
      else{
        #dtm1 <- getWordcloud(df1 = df1,sw = sw,tf = "tf",ngramValue = ngramVal,rmNum = TRUE)
        ##sessionVars$dtm <- dtm1
        #Vars$words <- input$words
        #freq1 <- (sort(apply(dtm1,2,sum), decreasing = T))
        srcdf <- df1
        dfout = NULL
        dtm.combine <- NULL
        if(nrow(srcdf)>1000){
          
          # print('start')
          t = 1000
          rem = nrow(srcdf) %% t
          for(i in 1:((nrow(srcdf)-rem)/t)){
            #print(paste0('loop',i))
            j=((i-1)*t)+1
            
            t1 = t*i
            dff=srcdf[j:t1,]
            dtm1 <- getWordcloud(dff,sw = sw,tf = "tf",ngramValue = ngramVal,rmNum = TRUE,mapWords = mapWords)
            if(is.null(dtm.combine))
              dtm.combine <- dtm1
            else
              dtm.combine <- c(dtm.combine,dtm1)
            lstop1 <- as.data.frame(as.table(dtm1),stringsAsFactors = F)
            if(is.null(dfout))
              dfout <- lstop1[lstop1$Freq>0,]
            else
              dfout <- rbind(dfout,lstop1[lstop1$Freq>0,])
          }
          #print('end loop')
          #print('rem data')
          if(rem != 0){
            j <- (nrow(srcdf)-rem)+1
            t1 <- nrow(srcdf)
            dff=srcdf[j:t1,]
            dtm1 <- getWordcloud(dff,sw = sw,tf = "tf",ngramValue = ngramVal,rmNum = TRUE,mapWords = mapWords)
            dtm.combine <- c(dtm.combine,dtm1)
            lstop1 <- as.data.frame(as.table(dtm1),stringsAsFactors = F)
            dfout <- rbind(dfout,lstop1[lstop1$Freq>0,])
          }
          #print('end')
        } else {
          dff = srcdf
          dtm1 <- getWordcloud(dff,sw = sw,tf = "tf",ngramValue = ngramVal,rmNum = TRUE,mapWords = mapWords)
          dtm.combine <- dtm1
          lstop1 <- as.data.frame(as.table(dtm1),stringsAsFactors = F)
          dfout <- lstop1[lstop1$Freq>0,]
        }
        #en = Sys.time()
        sessionVars$dtm1 <- dtm.combine
        setDT(dfout)
        output1 <- dfout[, sum(Freq),by="Terms"]
        indx <- which(output1$V1 >=(output1[order(-V1)])[ifelse(nrow(output1)>=200,200,nrow(output1)),V1],arr.ind = T)
        output <- dfout[Terms %in% output1[indx,Terms]]
        output$ngram <- ngramVal
        sessionVars$ngramdfWC2 <- output
        sessionVars$ngramdfWC2_dum <- output
        output[,TotFreq := sum(Freq),by = c("Terms")]
        #ds <- unique(dataset[,-c(1,2,3)])
        if(nrow(output)==0){
          freq1 <- 45
          names(freq1) <-"No Terms"
        }
        Vars$words <- input$words
        output_unique <- (unique(output[,c(1,4,5)]))[order(-TotFreq)]
        freq1 <- output_unique$TotFreq
        names(freq1) <- output_unique$Terms
        return(freq1)  
      }
    });
    # pb <-
    #   winProgressBar(
    #     title = "Example progress bar", label = "0% done", min = 0, max = 100, initial =
    #       0
    #   )
    # for (i in 1:10) {
    #   Sys.sleep(0.1)
    #   info <- sprintf("%d%% done", i * 10)
    #   setWinProgressBar(pb, i * 10, label = info)
    # }
    # close(pb)
    
    return(freq1)
    #});
  })
  
  
  
  
  output$selWord <- renderText({
    if (any(names(input) ==
            'd3word')) {
      input$d3word
    } else {
      return(NULL)
    }
  })
  onclick("searchButton",shinyjs::show("wc2Container",time = 1, anim = TRUE, animType = "slide"))
  output$errtext <- renderText({"No data for generating WordCloud"})
  output$plot2 <- renderd3Cloud({#renderPlot({#
    tryCatch({
      
      freq1 <- dataWC()
      
      if(is.null(freq1)){
        # hide("plot2")
        # enable("errtext")
        return(NULL)
      } 
      # else {
      #   hide("errtext")
      #   show("plot2")
      # }
      #dtm1 <- data()
      #freq1 <- (sort(apply(dtm1,2,sum), decreasing =T))
      #freq1[1:50]
      #     freq <- input$freq
      #     word <- input$word
      
      #wordcloud(names(freq1), freq1, random.order=FALSE, scale=c(6,1),min.freq=2, max.words=50,colors=brewer.pal(8, "Dark2"),rot.per=0) # Plot results in a word cloud 
      #        title(main = list("Term Frequency - Wordcloud",cex=1.5,col = colorRampPalette(brewer.pal(8,"Dark2"))(1)))
      colr.idx <- function(x,max,len) {
        return ((x / max) * len)
      }
      brew.col <- brewer.pal(8,"Dark2")
      colr <-
        brew.col[ceiling(sapply(freq1,colr.idx,max(freq1),length(brew.col)))]
      #d3TextCloud(names(freq1), freq1,height = "850",color = "lightgray",textColor = colr)
      gc()
      return(d3Cloud(
        names(freq1), freq1,height = "850",color = "lightgray",textColor = colr
      ))
    }, error=function(warn){
      
      
      return(NULL)
    })
    
  })
  # }
  # })
  
  
  
  output$text <- renderText({
    if (any(names(input) == 'd3barvalues') ) {
      input$d3barvalues
    }
  })
  output$grid <- renderRHandsontable({
    #   dtm1 <- sessionVars$dtm
    #   
    #   freq1 <- (sort(apply(dtm1,2,sum), decreasing = T))
    df <- data.frame("Mapping Keyword"="","Actual Word"="")
    for(i in 1:4) {
      df <- rbind(df,c("",""))
    }
    rhandsontable(df, width = 600, height = 300) 
  })
  
  ##########################Sentiment#########################################
  observe({
    if (is.null(updateSelect()))
      return(NULL)
    coln <- c("Default",colnames(updateSelect()))
    
    updateSelectizeInput(
      session, 'groupby',server = T,choices = coln[!(coln %in% input$cols)],
      options = list(placeholder = 'group by',maxItems = 1)
    )
  })
  
  dfcol <- reactive({
    v <- dput(as.character(input$cols))
  }  )
  
  SentimentData <- eventReactive(input$btnGetSentiment,{
    
    df1 <-  as.data.frame(sessionVars$dfInput)
    if (is.null(input$cols))
      return(NULL)
    
    else {
      #(input$intCols >= 1){
      
      df1$Text <- do.call(paste, c(df1[c(dfcol())], sep="."))
      # df1 <- as.data.frame(df1[,"Text"])
      #colnames(df1) <- c("Text")
      
      #for (co in c(dfcol())) df1[co] <- NULL
    }
    shinyjs::disable("btnGetSentiment")
    withProgress(message = "Loading...", value = 0,{
      # Number of times we'll go through the loop
      n <- 10
      
      for (i in 1:n) {
        # Each time through the loop, add another row of data. This is
        # a stand-in for a long-running computation.
        
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste( i*10,"%"))
        
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.1)
      }
      setProgress(message = 'Processing Sentiments...',detail = '')
      #df1 <- cbind(df1,SentenceCount(df1, extract = T))
      #df1 <- merge(as.data.table(df1)[,row_id:=.N],Sentiment_Sentimentr(df1),by="row_id")
      #colnames(df1)[which(colnames(df1)=='Text.x',arr.ind = T)] <- "Text"
      df1 <- df1[,c("DocNo","Text")]
      df1$Text <- iconv(df1$Text,from = 'WINDOWs-1252',to='UTF-8')
      #df1 <- data.table(Text=iconv(df1$Text,from = 'WINDOWs-1252',to='UTF-8'))
      dfSentiment <- setDT(df1)
      df1 <- sentiment_op(df1)
      #df1 <- cbind(data.frame(letter="Sentiment"),Text = dfSentiment[as.vector(df1[,"Record"]),1],df1)
      df1 <- merge(dfSentiment,df1,by.x="DocNo",by.y="Record",all.x=T,incomparables=0)
      df1 <- cbind(data.frame(letter="Sentiment"),df1)
      #setProgress(message = 'Processing Emotion cards...',detail = '')
      #df2 <- Classify_nrc_Emotions(as.vector(dfSentiment$Text,mode = "character"))
      #rm("dfSentiment")
      df2 <- NULL
      df3 <- merge(df1,as.data.frame(sessionVars$dfInput),by="DocNo",all.x=T,incomparables=0)
      lstSentimenttab <- list(df1,df2,dfSentiment,df3)
    });
    shinyjs::enable("btnGetSentiment")
    return(lstSentimenttab)
  })
  
  
  ##############################################################
  
  taggedData <- eventReactive(input$btnGetTags,{
    
    df1 <-  as.data.frame(sessionVars$dfInput)
    if (is.null(input$cols))
      return(NULL)
    
    else {
      #(input$intCols >= 1){
      
      df1$Text <- do.call(paste, c(df1[c(dfcol())], sep="."))
      # df1 <- as.data.frame(df1[,"Text"])
      #colnames(df1) <- c("Text")
      
      #for (co in c(dfcol())) df1[co] <- NULL
    }
    shinyjs::disable("btnGetTags")
    withProgress(message = "Loading...", value = 0,{
      # Number of times we'll go through the loop
      n <- 10
      
      for (i in 1:n) {
        # Each time through the loop, add another row of data. This is
        # a stand-in for a long-running computation.
        
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste( i*10,"%"))
        
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.1)
      }
      setProgress(message = 'Processing Keyword Tagging...',detail = '')
      
      df1 <- df1[,c("DocNo","Text")]
      df1$Text <- iconv(df1$Text,from = 'WINDOWs-1252',to='UTF-8')
      df1 <- setDT(df1)
      df1[,Record:=1:.N]
      dfTag <- phrase(df1[!is.na(Text)])
      df1 <- merge(df1,unique(dfTag),by="Record",all.x = T)
      df1 <- unique(merge(as.data.frame(df1),tokenDF, by="Keyword", all.x = T))
      #comments2[which(is.na(comments2$Keyword)),c(6,7)] <- "Others"
      setDT(df1)
      df1[is.na(Main.Category),Main.Category :="Others"][is.na(Sub.Category),Sub.Category :="Others"]
      setkey(df1,"Record")
      #key(comments2)
      df1 <- unique(df1[,-c(1,2,3)])
      setcolorder(df1,c(3,2,1))
      sessionVars$dfKeywordTagging <- df1
      b <- df1[,.N,by=c("Main.Category","Sub.Category")]
      b[,Total:=sum(N)][,Distribution := N/Total]#[,Distribution:=round(Distribution,digits = 2),]
      setDT(b,key="Main.Category")
    });
    shinyjs::enable("btnGetTags")
    return(b)
  })
  
  ####################################################
  
  output$keyword <- DT::renderDataTable(
    datatable(
      data = ({
        if(is.null(taggedData()))
          return(NULL)
        keyword <- as.data.frame(taggedData())
      })
      #,extensions = c('Buttons','Responsive')
      ,selection = list(target="none")
      ,options = list(
        
        #scrollX = TRUE
        autoWidth = F
        ,initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#003A60 !important', 'color': '#fff'});",
          "}")
        ,columnDefs = list(
          list(visible=FALSE, targets=c(2,3)),
          list( className = 'distribution', targets = 4
          ))
        ,order = list(4,'desc')
        ,paging = T
        ,deferRender = TRUE
        #,scrollY = 350
        ,scroller = TRUE
        ,pageLength = 50
        ,lengthMenu = list(c(10,50,100,-1), list('10','50','100', 'All'))
        ,searching = F
        ,dom = 'BRfrltip'
        # ,buttons = list(list(
        #                       extend = 'collection',
        #                       buttons = list(list(extend='copy'),list(extend='csv',filename="keywords"), list(extend='excel',filename="keywordsxl")),
        #                       text = 'Download')
        #                 , I('colvis'))
        # 
      )
      
      ,rownames = F
      ,filter = "none"
      ,class = "cell-border stripe"
      ,escape=FALSE
      ,callback = JS(
        "table.on('click.dt', 'td.distribution', function() {
        var td = $(this), row = table.row(td.closest('tr'));
        var d= row.data();
        
        Shiny.onInputChange('txtMaincat',d[0])
        Shiny.onInputChange('txtSubcat',d[1])
        $('#btnHidden').click();
});")
  ) %>% formatPercentage('Distribution', 2) 
  
    )
  seltaggedData <- eventReactive(input$btnHidden,{
    
    if(any(names(input) == 'txtMaincat') & any(names(input) == 'txtSubcat')){
      df1 <-  as.data.frame(sessionVars$dfKeywordTagging)
      
      withProgress(message = "Loading...", value = 0,{
        # Number of times we'll go through the loop
        n <- 10
        
        for (i in 1:n) {
          # Each time through the loop, add another row of data. This is
          # a stand-in for a long-running computation.
          
          
          # Increment the progress bar, and update the detail text.
          incProgress(1/n, detail = paste( i*10,"%"))
          
          # Pause for 0.1 seconds to simulate a long computation.
          Sys.sleep(0.1)
        }
        setProgress(message = 'Loading...',detail = '')
        setDT(df1)
        return(df1[tolower(Main.Category)==tolower(input$txtMaincat) & tolower(Sub.Category)==tolower(input$txtSubcat)])
      });
    } else {
      return(NULL)
    }
    
  })
  
  output$filterkeyword <- DT::renderDataTable(
    datatable(options = list(
      
      #scrollX = TRUE,
      autoWidth = F
      ,initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#003A60 !important', 'color': '#fff'});",
        "}")
      ,paging = T
      ,deferRender = TRUE
      ,scrollY = 350
      ,scroller = TRUE
      ,pageLength = 50, lengthMenu = list(c(10,50,100,-1), list('10','50','100', 'All'))
      ,searching = F
    )
    ,rownames = F
    ,filter = "none"
    ,class = "cell-border stripe"
    ,data={
      selKeyword <- seltaggedData()
      if(is.null(selKeyword))
        return(NULL)
      colnames(selKeyword)[3]<-"Comments"
      keyword <- selKeyword[,3]
      #df <- sentiment[,c("Text","Strong Positive Words","Strong Negative Words","Weak Positive Words","Weak Negative Words","Positive Sentences","Neutral Sentences","Negative Sentences")]
    }) )
  
  #  output$download <- downloadHandler(
  #    filename = function(){"thename.csv"}, 
  #    content = function(fname){
  #      write.csv(seltaggedData(), fname)
  #    }
  #  )
  
  output$downloadTags <- downloadHandler(filename = function(){
    paste0("data-",Sys.Date(),".csv")
  },
  content = function(file){
    write.csv(seltaggedData(),file)
  })
  
  output$downloadAllTags <- downloadHandler(filename = function(){
    paste0("TaggedData-",Sys.Date(),".csv")
  },
  content = function(file){
    write.csv(sessionVars$dfKeywordTagging,file)
  })
  #############################################################
  
  output$sentiment <- DT::renderDataTable(extensions = c('Buttons'),#'Scroller'),
                                          options = list(
                                            scrollX = TRUE
                                            ,initComplete = JS(
                                              "function(settings, json) {",
                                              "$(this.api().table().header()).css({'background-color': '#003A60 !important', 'color': '#fff'});",
                                              "}")
                                            ,autoWidth = T,
                                            columnDefs = list(list(width = '202.5px', targets = "_all")),
                                            paging = T,
                                            deferRender = TRUE,
                                            scrollY = 350,
                                            scroller = TRUE,
                                            pageLength = 10, lengthMenu = list(c(10,50,100,-1), list('10','50','100', 'All')),
                                            searching = F,
                                            dom = 'BRfrltip',
                                            #dom = 'Rfrtlip',
                                            buttons = list(list(
                                              extend = 'collection',
                                              buttons = list(list(extend='copy'),list(extend='csv',filename="Sentiments"), list(extend='excel',filename="Sentimentsxl")),
                                              text = 'Download'), I('colvis'))
                                          ),
                                          rownames = F,
                                          filter = "none",
                                          class = "cell-border stripe",
                                          
                                          {
                                            if(is.null(SentimentData()[[1]]))
                                              return(NULL)
                                            sentiment <- as.data.frame(SentimentData()[[1]])
                                            #df <- sentiment[,c("Text","Strong Positive Words","Strong Negative Words","Weak Positive Words","Weak Negative Words","Positive Sentences","Neutral Sentences","Negative Sentences")]
                                            df <- sentiment[,c("Text","Positive Sentences","Neutral Sentences","Negative Sentences")]
                                            df <- df[!df$Text=="NA",]
                                            df[!apply(df, 1, function(x) all(x=="" )),]
                                            
                                          })
  
  
  
  
  
  output$barchart <- renderd3BarChart({
    if (is.null(SentimentData()[[4]]))
      return(NULL)
    
    Data <- as.data.table(SentimentData()[[4]])
    if(input$groupby %in% c("","Default")){
      Data <- Data[,.(Positive = sum(as.numeric(as.character(Positive)),na.rm = T),
                      Neutral = sum(as.numeric(as.character(Neutral)),na.rm = T)
                      ,Negative = sum(as.numeric(as.character(Negative)),na.rm = T)), by = "letter"]
      #Data <- cbind(data.frame(def.col="sentiment"),Data)
    }
    else
      Data <- Data[,.(Positive = sum(as.numeric(as.character(Positive))),Neutral = sum(as.numeric(as.character(Neutral))),Negative = sum(as.numeric(as.character(Negative)))),by = eval(input$groupby)]
    #Data <- Data[,data.table(t(.SD), keep.rownames=F),]
    #a <- colnames(Data)
    
    #Data <- as.data.frame(Data, row.names = T)
    #Data[,2] <- Data[,1]
    #Data[,1] <- a
    colnames(Data)[1]<-"letter"
    gc()
    d3BarChart(Data,width = 500, height = 600) 
  })  
  
  output$sentimentcloud <- renderd3Cloud({
    tryCatch(expr = {
      if(any(names(input) == 'd3barvalues')){
        sent <- word(input$d3barvalues,-1,sep=",")
        dfSent <- as.data.frame(SentimentData()[[4]])
        df1 <- as.data.frame(dfSent[,c("DocNo",paste(sent,"Sentences"))])
        if(eval(input$groupby) != "Default" & eval(input$groupby) != ""){
          filterval = word(input$d3barvalues, 1, sep = ",") 
          df1 <- as.data.frame(dfSent[dfSent[[eval(input$groupby)]]== filterval, c("DocNo", paste(sent,"Sentences"))])
        }
        #df1 <- as.data.frame(SentimentData())[,word(input$d3barvalues,-1,sep=",")]
        colnames(df1)[2] <- "col1"
        withProgress(message = "Loading...", value = 0,{
          # Number of times we'll go through the loop
          n <- 10
          
          for (i in 1:n) {
            # Each time through the loop, add another row of data. This is
            # a stand-in for a long-running computation.
            
            
            # Increment the progress bar, and update the detail text.
            incProgress(1/n, detail = paste( i*10,"%"))
            
            # Pause for 0.1 seconds to simulate a long computation.
            Sys.sleep(0.1)
          }
          setProgress(message = 'Processing Wordcloud...',detail = '')
          #dtm1 <- getWordcloud(df1,sw=NULL,"tf",2,F,rmWords=NULL,rmPunctuations=TRUE)
          if(nrow(df1)==0){
            session$sendCustomMessage(type = 'testmessage',
                                      message = 'Unable to process wordcloud for selected text')
            freq1 <- NULL
            # freq1<-c(20)
            # names(freq1)<-"NA"
          }
          else{
            #dtm1 <- getWordcloud(df1 = df1,sw = sw,tf = "tf",ngramValue = ngramVal,rmNum = TRUE)
            ##sessionVars$dtm <- dtm1
            #Vars$words <- input$words
            #freq1 <- (sort(apply(dtm1,2,sum), decreasing = T))
            srcdf <- df1
            dfout = NULL
            dtm.combine <- NULL
            if(nrow(srcdf)>1000){
              
              # print('start')
              t = 1000
              rem = nrow(srcdf) %% t
              for(i in 1:((nrow(srcdf)-rem)/t)){
                #print(paste0('loop',i))
                j=((i-1)*t)+1
                
                t1 = t*i
                dff=srcdf[j:t1,]
                dtm1 <- getWordcloud(dff,sw=NULL,"tf",2,F,rmWords=NULL,rmPunctuations=TRUE)
                if(is.null(dtm.combine))
                  dtm.combine <- dtm1
                else
                  dtm.combine <- c(dtm.combine,dtm1)
                lstop1 <- as.data.frame(as.table(dtm1),stringsAsFactors = F)
                if(is.null(dfout))
                  dfout <- lstop1[lstop1$Freq>0,]
                else
                  dfout <- rbind(dfout,lstop1[lstop1$Freq>0,])
              }
              #print('end loop')
              #print('rem data')
              if(rem != 0){
                j <- (nrow(srcdf)-rem)+1
                t1 <- nrow(srcdf)
                dff=srcdf[j:t1,]
                dtm1 <- getWordcloud(dff,sw=NULL,"tf",2,F,rmWords=NULL,rmPunctuations=TRUE)
                dtm.combine <- c(dtm.combine,dtm1)
                lstop1 <- as.data.frame(as.table(dtm1),stringsAsFactors = F)
                dfout <- rbind(dfout,lstop1[lstop1$Freq>0,])
              }
              #print('end')
            } else {
              dff = srcdf
              dtm1 <- getWordcloud(dff,sw=NULL,"tf",2,F,rmWords=NULL,rmPunctuations=TRUE)
              dtm.combine <- dtm1
              lstop1 <- as.data.frame(as.table(dtm1),stringsAsFactors = F)
              dfout <- lstop1[lstop1$Freq>0,]
            }
            #en = Sys.time()
            #sessionVars$dtm1 <- dtm.combine
            setDT(dfout)
            output1 <- dfout[, sum(Freq),by="Terms"]
            indx <- which(output1$V1 >=(output1[order(-V1)])[ifelse(nrow(output1)>=200,200,nrow(output1)),V1],arr.ind = T)
            output <- dfout[Terms %in% output1[indx,Terms]]
            #output$ngram <- ngramVal
            #sessionVars$ngramdfWC2 <- output
            #sessionVars$ngramdfWC2_dum <- output
            output[,TotFreq := sum(Freq),by = c("Terms")]
            #ds <- unique(dataset[,-c(1,2,3)])
            if(nrow(output)==0){
              freq1 <- 45
              names(freq1) <-"No Terms"
            }
            #Vars$words <- input$words
            output_unique <- (unique(output[,c(1,4)]))[order(-TotFreq)]
            freq1 <- output_unique$TotFreq
            names(freq1) <- output_unique$Terms
            
          } 
        });
        # sessionVars$dtm <- dtm1
        # Vars$words <- input$words
        #freq <- (sort(apply(dtm1,1,sum), decreasing = T))
        colr <- ifelse(sent=="Positive","#006633",ifelse(sent=="Neutral","#007FFF","#8B0000"))
        
        #freq1 <- freq
        if(is.null(freq1))
          return(NULL)
        # if(as.numeric(input$wordLimit) > length(freq1)){
        freq1 <- freq1[1:35]
        # } else {
        #   freq1 <- freq1[1:as.numeric(input$wordLimit)]
        # }
        # colr.idx <- function(x,max,len) {
        #   return ((x / max) * len)
        # }
        # brew.col <- brewer.pal(8,"Dark2")
        # colr <-
        #   brew.col[ceiling(sapply(freq1,colr.idx,max(freq1),length(brew.col)))]
        #d3TextCloud(names(freq1), freq1,height = "850",color = "lightgray",textColor = colr)
        sessionVars$flg=1
        gc()
        return(d3Cloud(
          names(freq1), freq1,height = "850",color = "white",textColor = colr
        ))
        
      } 
      
    }
    ,error = function(e) {
      shinyjs::js$errorPanel("Error occured while generating wordcloud. Please contact Admin.")
      return(NULL)
    })
  })
  
  
  
  output$emotion <- renderUI({
    
    if(is.null(SentimentData()[[2]]))
      return(NULL)
    
    pTextSize <- function(x, value) tags$p(x, style=paste0("font-size: ", value, "%;"))
    
    # data
    set.seed(1)
    #x <- rnorm(1000, 100, 10)
    val <- SentimentData()[[2]]#round(c(mean(x), sd(x), min(x), max(x), median(x)))
    Data <- val[,1:8]
    # Data <- as.data.table(val)[,.(Count = .N)]
    # df <- data.frame(BEST_FIT=c("anger","disgust","fear","joy","sadness","surprise")
    #                  ,Count=c(0,0,0,0,0,0))
    # Data[, 1][is.na(Data[, 1])] <- "noemotion"
    # dfMerge <- merge(Data,df,by="BEST_FIT",all=T)
    # dfMerge[is.na(dfMerge)]<-0
    # val <- as.data.table(dfMerge)[,.(Count=sum(Count.x,Count.y)),by=BEST_FIT]
    #val <- colSums(Data)/sum(Data)
    val <- round((colSums(Data)/sum(Data))*100,digits = 2)
    # val$l <- sapply(as.character(val[[1]]),function(x)substr(x,1,2))
    # val <- as.data.table(val)[order(l)]
    # val$l <- NULL
    val <- data.table(emotion=names(val),value=paste(val,"%"))
    #clrs <- c("red","light-blue", "purple","yellow", "green", "blue", "orange","aqua")
    ##dd4b39 anger
    ##ffa854 anticipation
    ##ff54ff disgust
    ##009600 fear
    ##f39c12 joy
    ##5151ff sadness
    ##59bdff surprise
    ##54ff54 trust
    clrs <- c("#dd4b39","#ffa854","#ff54ff","#009600","#f39c12","#5151ff","#59bdff","#54ff54")
    
    # image files
    
    files_icon <- paste0("icon_", as.character(val[[1]]), ".png")
    
    #val <- c(val, paste(round(quantile(x, probs = c(0.25, 0.75))), collapse=" - "))
    text <- map(as.character(stri_trans_totitle(val[[1]])), ~pTextSize(.x, 150))
    val <- map2(as.character(val[[2]]), c(rep(100, nrow(val))), ~pTextSize(.x, .y))
    
    # vb <- map(1:length(val), ~valueBox(
    #  # href= downloadLink(paste0("downloadData_","anger"), "Download"),
    #   val[[.x]], text[[.x]],
    #   icon=icon(list(src=files_icon[.x], width="80px"), lib="local"),
    #   color=clrs[.x], width=NULL)
    # )
    vb <- map(1:length(val), function(x){ box1<-valueBox(
      # href= downloadLink(paste0("downloadData_","anger"), "Download"),
      val[[x]], text[[x]],
      icon=icon(list(src=files_icon[x], width="80px"), lib="local"),
      color=clrs[x], width=NULL)
    box1$children[[1]]$attribs$class<-paste0("action-button small-box")
    box1$children[[1]]$attribs$id<-paste0("button_box_0",x)
    box1})
    gc()
    fluidRow(
      tags$head(tags$style(HTML(".small-box {height: 100px}"))),
      map(1:length(vb),~column(width = 3,vb[[.x]]))
      # column(width=2, vb[[1]]), column(width=2,vb[[2]]), 
      # column(width=2, vb[[3]]), column(width=2,vb[[4]]), 
      # column(width=2, vb[[5]]), column(width=2,vb[[6]]),
      # column(width=2,vb[[7]])
    )
  })
  observe({
    
    lapply(c("Anger","Anticipation","Disgust","Fear","Joy","Sadness","Surprise","Trust"), function(i) {
      val = i
      output[[paste0("download_", i)]] <- downloadHandler(
        filename = function() {
          paste("data-",i,"-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          val <- SentimentData()
          #idx <- #which(val[[2]]$BEST_FIT==tolower(i),arr.ind = T)
          xx <- cbind(as.data.frame(val[[3]])[,"Text"],val[[2]])
          xx<-xx[xx[,which(colnames(xx)==tolower(i))]>0,c(1,grep(pattern = paste0(tolower(i),"_"),x = colnames(xx)))]
          if(nrow(xx)>0){
            Data <- data.frame(Text=xx[,1],ExtractedWords=xx[,2])#data.frame(Text = val[[1]][idx,"Text"])
            write.csv(Data, file)
          }
          else
            stop("No Data.")
        }
      )
    })
  })
  
  output$download_Lexicon <- downloadHandler(
    filename = function() {
      paste("sysLexicon.csv", sep="")
    },
    content = function(file) {
      src <- normalizePath('lex3.csv')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, file, overwrite = TRUE)
      
      
      #file.rename(out, file)
    }
  )
  
  output$download_emotions <- downloadHandler(
    filename = function() {
      paste("sysEmotionLexicon.csv", sep="")
    },
    content = function(file) {
      src <- normalizePath('NRC-Emotion-Lexicons.csv')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, file, overwrite = TRUE)
      
      
      #file.rename(out, file)
    }
  )
  
  # override shinydashboard function
  valueBox <- function (value, subtitle, icon = NULL, color = "aqua", width = 4, href = NULL){
    #shinydashboard:::validateColor(color)
    if (!is.null(icon)) 
      shinydashboard:::tagAssert(icon, type = icon$name)
    if(!is.null(icon)){
      if(!icon$name %in% c("i", "img")) stop("'icon$name' must be 'i' or 'img'.")
      iconClass <- if(icon$name=="i") "icon-large" else "img"
    }
    boxContent <- div(class = "small-box", #paste0("small-box bg-",color)
                      style=paste0("cursor:pointer;color:#fff;background-color:",color,"!important"), 
                      if(gsub("<.*?>", "",value)==0){
                        downloadButton(outputId = paste0("download_",gsub("<.*?>", "",str_trim(subtitle))),
                                       href=paste0("session/",
                                                   unlist(strsplit(session$getTestSnapshotUrl(),"/"))[2],
                                                   "/download/download_",gsub("<.*?>", "",str_trim(subtitle)),
                                                   "?w="),label = "", 
                                       style=
                                         "border-color: transparent; padding: 0px 5px 0px 0px; 
                                       color: #000; float: right; background-color: transparent;",
                                       disabled="disabled")
                      }else{
                        downloadButton(outputId = paste0("download_",gsub("<.*?>", "",str_trim(subtitle))),
                                       href=paste0("session/",
                                                   unlist(strsplit(session$getTestSnapshotUrl(),"/"))[2],
                                                   "/download/download_",gsub("<.*?>", "",str_trim(subtitle)),
                                                   "?w="),label = "", 
                                       style=
                                         "border-color: transparent; padding: 0px 5px 0px 0px; 
                                       color: #000; float: right; background-color: transparent;")
                      },
                      
                      div(class = "inner",style="float:left", h3(value), p(subtitle)), if (!is.null(icon)) 
                        div(class = iconClass, icon))
    
    
    if (!is.null(href)) 
      boxContent <- a(href = href, boxContent)
    div(class = if (!is.null(width)) 
      paste0("col-sm-", width), boxContent)
  }
  
  # override shiny function
  icon <- function (name, class = NULL, lib = "font-awesome"){
    
    if(lib=="local"){
      if(is.null(name$src))
        stop("If lib='local', 'name' must be a named list with a 'src' element
             and optionally 'width' (defaults to 100%).")
      if(is.null(name$width)) name$width <- "100%"
      
      return(tags$img(class="img img-local", src=name$src, width=name$width))
    }
    prefixes <- list(`font-awesome` = "fa", glyphicon = "glyphicon")
    prefix <- prefixes[[lib]]
    if (is.null(prefix)) {
      stop("Unknown font library '", lib, "' specified. Must be one of ", 
           paste0("\"", names(prefixes), "\"", collapse = ", "))
    }
    iconClass <- ""
    if (!is.null(name)) 
      iconClass <- paste0(prefix, " ", prefix, "-", name)
    if (!is.null(class)) 
      iconClass <- paste(iconClass, class)
    iconTag <- tags$i(class = iconClass)
    if (lib == "font-awesome") {
      htmltools::htmlDependencies(iconTag) <- htmltools::htmlDependency("font-awesome", 
                                                                        "4.6.3", c(href = "shared/font-awesome"), stylesheet = "css/font-awesome.min.css")
    }
    iconTag
  }
  
  ##########################Clustering#########################################
  data_Cluster <- eventReactive(input$getCluster,{
    if (is.null(input$cols))
      return(NULL)
    df1 <-
      as.data.frame(sessionVars$dfInput)
    
    shinyjs::disable("getCluster")
    sw <- NULL
    if(!is.null(input$swFile)){
      inFile1 <- input$swFile
      sw <- readLines(inFile1$datapath)
    }
    
    df1 = as.data.frame(df1[,input$cols],stringsAsFactors = F)
    
    if (ncol(df1) == 1) {
      colnames(df1) <- "col1"#input$cols
    }else{
      colnames(df1) <- paste("col",1:ncol(df1),sep="")
    }
    
    rmWords <- NULL
    if(input$rmWrds == TRUE){
      if(!is.null(input$words))
        if(input$words !="")
          rmWords <- unlist(strsplit(input$words,split = " "))#input$words
    }
    withProgress(message = "Loading...", value = 0,{
      # Number of times we'll go through the loop
      n <- 10
      
      for (i in 1:n) {
        # Each time through the loop, add another row of data. This is
        # a stand-in for a long-running computation.
        
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste( i*10,"%"))
        
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.1)
      }
      setProgress(message = 'Processing Cluster...',detail = '')
      
      freqper <- as.numeric(input$freq_percent)/100
      if(freqper ==0)
        freq = .10
      dist_func <- getClusters(df1,sw,ngramValue=as.numeric(input$ngram),freqpercent = freqper)
      
      
    });
    shinyjs::enable("getCluster")
    gc()
    sessionVars$distfn <- dist_func
    return(dist_func)
    
  })
  output$cluster <- renderPlot({
    dist<- data_Cluster()
    if(is.null(dist[[3]]))
      return(NULL)
    dist_func <- dist[[3]]
    # 
    #       disttable=dist(mattable,method="euclidean")
    # 
    #       clustgrps=hclust(disttable,method="average")
    # 
    #       clustertree=cutree(clustgrps,k=3)
    # 
    #       clust_matrix= as.matrix(clustertree)
    # 
    #       data_mclust=data.frame(groups=clustertree)
    # 
    #       data_mclust$text=rownames(data_mclust)
    # 
    #       data_mcust$size=sample(1:ncol(data_mclust$groups))
    # 
    
    clustgrps=hclust(dist_func,method=input$rbcluster_method)
    hcd = as.dendrogram(clustgrps)
    colors = c("#7A99AC", "#E4002B","#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02")
    
    clus4 = cutree(clustgrps, k=5)
    colLab <- function(n) {
      if (is.leaf(n)) {
        a <- attributes(n)
        labCol <- colors[clus4[which(names(clus4) == a$label)]]
        attr(n, "nodePar") <- c(a$nodePar, lab.col = labCol)
      }
      n
    }
    # using dendrapply
    clusDendro = dendrapply(hcd, colLab)
    plot(clusDendro,cex=1.5,font.lab=2 )
    #plot(clustgrps,cex=1.5)
    #require(ape)
    
    #plot(as.phylo(clustgrps), type = "fan", tip.color = colors[clus4],
    #    label.offset = 1, cex = 1.7)
    
    #rect.hclust(clustgrps,k=5,border=colors[clus4])
    
    # hcut_grp <- cutree(dist_func, k = 2, stand = F)
    # 
    # # Visualize
    # fviz_dend(hcut_grp, rect = TRUE, cex = 0.5,
    #           k_colors = c("#7A99AC", "#E4002B","#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02"))
  })
  clust <- reactive({
    
    #df1 <- read.csv("D:/Hackathons/R Noddy Pkgs/scaffWidget/rd3Cluster/word_groups.csv",header = T)
    #df1<- read.csv("D:/Hackathons/TextAnalytics-Dashboard-integrated/YamOnlyText.csv")
    # class(df1)
    # df2
    
    dist<- data_Cluster()
    if(is.null(dist[[1]]))
      return(NULL)
    dist_function <- dist[[1]]
    if(is.null(dist_function))
      return(NULL)
    #disttable=dist(mattable,method="euclidean")
    
    clustgrps=hclust(dist_function,method=input$rbcluster_method)
    
    clustertree=cutree(clustgrps,k=5)
    
    clust_matrix= as.matrix(clustertree)
    
    data_mclust=data.frame(group=clustertree)
    
    data_mclust$text=rownames(data_mclust)
    
    data_mclust$size=20
    
    
    
    return(data_mclust)
  })
  output$d3cluster <- renderd3Cluster({
    a <- clust()
    d3Cluster(a,height = 400)
  })
  
  # output$mytable <- DT::renderDataTable(extensions = c('Buttons','Scroller','Responsive'),
  #                                       options = list(dom='BRrltpi',
  #                                                      buttons = list(list(
  #                                                        extend = 'collection',
  #                                                        buttons = list(list(extend='copy'),list(extend='csv',filename="Cluster"), list(extend='excel',filename="Cluster")),
  #                                                        text = 'Download')),
  #                                                      deferRender = F,
  #                                                      scrollY = 350,
  #                                                      scroller = TRUE,
  #                                                      #autoWidth=T, 
  #                                                      columnDefs = list(list(width = '100px', className = 'dt-left', targets = '_all')),
  #                                                      paging=T, scrollX = T, searching =F, pageLength = 5),
  #                                    rownames=F,
  #                                    filter = "none",
  #                                    class = "display",{
  #                                      if(is.null(clust()))
  #                                        return(NULL)
  #                                      cluster <- as.data.frame(clust())
  #                                      df <- cluster[,c("text","size","group")]
  #                                      df[!apply(df, 1, function(x) all(x=="")),]
  #                                      
  #                                    })
  output$textCluster <- renderText({
    if(any(names(input) == 'txtD3Cluster')){
      dist<-clust()
      selected_word <- input$txtD3Cluster
      grp <- dist[dist$text == selected_word, 1]
      ClusterWords<-paste(dist[dist$group == grp, "text"],collapse = ",")
      return(ClusterWords)
    }
    else {
      return(NULL)
    }
  })
  clusterTab <- reactive({
    if(is.null(sessionVars$distfn))
      return(NULL)
    b <- sessionVars$distfn
    
    c <- b[[2]]
    a <- c[,-4]
    setDT(a)
    if(any(names(input) == 'txtD3Cluster')){
      selected_word <- input$txtD3Cluster
      selected_data <- a[a$Terms==selected_word]
      colnames(selected_data)[3]<-"Comments"
      return(selected_data[,3])
    }
    else{
      
      return(NULL)
    }
  })
  output$MappedTable <- DT::renderDataTable(
    datatable(
      data=clusterTab()
      
      #,selection = list(target="none")
      ,options = list(
        
        #scrollX = TRUE
        autoWidth = F
        ,initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#003A60 !important', 'color': '#fff'});",
          "}")
        
        ,paging = T
        ,deferRender = TRUE
        ,scrollY = 350
        ,scroller = TRUE
        ,pageLength = 50
        ,lengthMenu = list(c(10,50,100,-1), list('10','50','100', 'All'))
        ,searching = F
        
      )
      
      ,rownames = F
      ,filter = "none"
      ,class = "cell-border stripe"
      ,escape=FALSE
    )
  )
  
  })
