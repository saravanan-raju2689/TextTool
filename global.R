###############################################################
#             Invoke required library                         #
###############################################################
library("tm")
library("wordcloud")
library(memoise)
library("RWeka")
library("textir")
library(sentiment)
library(sentimentr)
require(data.table)
require(qdap)
require(tm)
require(stringi)
library(textclean)
library(textstem)
source("text_functions.R")
getWordcloud<- 
  function(df1,sw=NULL,tf,ngramValue,rmNum,rmWords=NULL,mapWords=NULL,rmPunctuations = FALSE){
    
    #############################################################
    #         Attach userdefined functions                      #
    #############################################################
    #setwd("D:/R/TextAnalytics/TextAnalytics/") 
    #    Select text_functions.R
    stpw = readLines("stopwords.txt")      # Select stopwords.txt file
    if(!is.null(sw)){
      stpw = unique(c(stpw,sw))
    }
    stpw1 = stopwords('english')         # tm package stop word list
    comn  = unique(c(stpw, stpw1))       # Union of two list
    stopwords = unique(c(gsub("'","",comn),comn)) # final stop word lsit after removing punctuation
    #head (stopwords)
    
    ############################################################
    #           Read the Text Data in R                        # 
    ############################################################
    #messages <- df1#read.csv(file.choose(), head=T)
    messages <- data.frame(DocNo=df1[,1],col1=do.call(paste, c(as.data.frame(df1[,-1], sep=". ",stringsAsFactors = F))))
    #data.frame(col1=do.call(paste, c(df1, sep=". ")))
    #   attach(messages)
    #   head(messages$body)
    #str(messages$body1)
    #Encoding(as.character(messages$col1)) <- 'UTF-8'
    messages$body1 <- iconv(messages$col1, from="WINDOWS-1252", to = "UTF-8", sub="")
    
    #messages$body1 <- (messages$body1[!is.na(messages$body1)])
    #head(messages$body1)
    
    
    #############################################################
    #                        Text Cleaning                      #
    #############################################################
    
    
    messages$body1 = text.clean(messages$body1,rmNum)                         # basic HTML Cleaning etc
    
    messages$body1  =  removeWords(messages$body1,stopwords)            # removing stopwords created above
    
    ######lemmetization of words ######
    messages$body1 = lemmatize_strings(messages$body1)
    
    #######words replacement#######
    if(!is.null(mapWords)){
      for(i in 1:length(mapWords$data)){
        chr <- unlist(mapWords$data[i])
        if(chr[1] !="" & chr[2] !="" ){
          lstWrds <- unlist(strsplit(chr[2],","))
          messages$body1 <- replace_tokens(messages$body1,tokens = lstWrds,replacement = tolower(chr[1]),ignore.case = T)
        }
      }
    }
    
    if(rmPunctuations)
      messages$body1  =  removePunctuation(messages$body1)
    
    messages$body1 = apply(as.data.frame(messages$body1),1,function(x)gsub('\\s+', ' ',x))#remove extra spaces
    #head(messages$body1)                                      # print top documents
    #remove user selected words
    if(!is.null(rmWords) ){
      messages$body1  =  removeWords(messages$body1,rmWords)
      messages$body1 = apply(as.data.frame(messages$body1),1,function(x)gsub('\\s+', ' ',x))#remove extra spaces
    }
    
    #############################################################
    #                        Mapping                      #
    #############################################################
    # if(!is.null(mapWords)){
    #   for(i in 1:length(mapWords$data)){
    #     chr <- unlist(mapWords$data[i])
    #     if(chr[1] !="" & chr[2] !="" )
    #       messages$body1 <- apply(as.data.frame(messages$body1),1,stri_replace_all_fixed
    #                                  ,unlist(strsplit(chr[2],split = ","))
    #                                  ,chr[1], vectorize_all=F)
    #   }
    # }
    ########################################################
    #             Create Document Term Matrix              #
    ########################################################
    
    #x1 = Corpus(VectorSource(messages$body1))          # Create the corpus
    myReader <- readTabular(mapping=list(content="body1", id="DocNo"))
    x1 <- Corpus(DataframeSource(messages[,c(1,3)]), readerControl=list(reader=myReader))
    #x1 = tm_map(x1, str_replace_all,"[^[:alnum:]]", " ")
    
    # myCorpusCopy <- x1
    # x1 <- tm_map(x1, stemDocument,language = "english")
    # #
    # #
    # # #corpus <- tm_map(corpus, stemCompletion, dictionary=myCorpusCopy, type="shortest")
    # # #corpus <- tm_map(corpus, stemCompletion, dictionary=myCorpusCopy)
    # #
    # #
    # # ##### OOB stemCompletion is not working fine. So below custom function.
    # #
    # stemCompletion2 <- function(x, dictionary) {
    #   x <- unlist(strsplit(as.character(x), " "))
    #   # Unexpectedly, stemCompletion completes an empty string to
    #   # a word in dictionary. Remove empty string to avoid above issue.
    #   x <- x[x != ""]
    #   x <- stemCompletion(x, dictionary=dictionary)
    #   x <- paste(x, sep="", collapse=" ")
    #   PlainTextDocument(stripWhitespace(x))
    # }
    # x1 <- lapply(x1, stemCompletion2, dictionary=myCorpusCopy)
    # x1 <- Corpus(VectorSource(x1))
    
    #   if(rmNum){
    #     x1 <- tm_map(x1, removeNumbers)
    #   }
    #x1 = n.gram(x1,"bi",3)                   # Encoding bi-gram with atleast frequency 3 as uni-gram
    
    ngram <- function(x) NGramTokenizer(x, Weka_control(min = ngramValue, max = ngramValue,delimiters = "\\s"))
    options(mc.cores=1)
    #x1 <- TermDocumentMatrix(x1, control = list(tokenize = ngram,removePunctuation = FALSE))
    #x1 <- removeSparseTerms(x1,sparse=.995)
    #x1 <- n.gram(x1,"bi",2)
    #require(quanteda)
    #x1 <- dfm(quanteda::corpus(x = messages$body1),ngram=ngramValue:ngramValue,concatenator = " ")
    #x1 <- t(x1)
    
    if (tf == 'tf') {
      x1 <- TermDocumentMatrix(x1, control = list(tokenize = ngram,removePunctuation = F,weighting=weightTf))
      #x1 <- removeSparseTerms(x1,sparse=.995)
      #dtm1 = DocumentTermMatrix(x1, control = list(tokenize = ngram))
      dtm1 = x1#custom.dtm(x1 = x1,scheme = "tf",mapWords = mapWords)               # Document Term Frequency
    } else{
      #     dtm1 = DocumentTermMatrix(Corpus(VectorSource(messages$body1)) ,control = list(
      #       tokenize = ngram,weighting = function(x) weightTfIdf(x, normalize = FALSE)))
      # dtm1 = custom.dtm(x1 = x1,scheme = "tfidf",mapWords = mapWords)            # Term Frequency Inverse Document Frequency Scheme
      #dtm1 = as.TermDocumentMatrix(dtm1,weighting=weightTf)
      x1 <- TermDocumentMatrix(x1, control = list(tokenize = ngram,removePunctuation = FALSE,weighting=weightTfIdf))
      #x1 <- removeSparseTerms(x1,sparse=.995)
      dtm1 =x1
    }
    return(dtm1)
    
  }
# ############### Sentiment Analysis variable cleaning ##############
# negation_words <- readLines("negation-words.txt")
# negation_words <- text.trim(negation_words)
# 
# lex_emo <- read.csv("emotions.csv")
# lex_emo$V1 <- as.character(lex_emo$V1)
# lex_emo$V2 <- as.character(lex_emo$V2)
# 
# stopwords_neg <- read.csv("stopwords without negation.csv", header=FALSE)
# stopwords_neg$V1 = as.character(stopwords_neg$V1)
# 
# #################### CLASSIFY LEX FUNCTION #######################
# 
# lex <- read.csv("lex3.csv")
# lex$V1 = as.character(lex$V1)
# lex$V2 = as.character(lex$V2)
# lex$V3 = as.character(lex$V3)
# 
# classify_lex <- function(subj,pol){
#   lex_class = lex[lex$V2==subj,]
#   lex_class = lex_class[lex_class$V3==pol,]
#   lex_class = unique(lex_class[,c(1,3)])
#   lex_class<-as.data.table(lex_class)
#   setkey(lex_class,V1)
#   return(lex_class)
#   #colnames(lex_class) <- "V1"
#   #lex_class
# }
# 
# lex_strong_pos <- classify_lex("strongsubj","positive")
# lex_strong_neg <- classify_lex("strongsubj","negative")
# lex_weak_pos <- classify_lex("weaksubj","positive")
# lex_weak_neg <- classify_lex("weaksubj","negative")
# 
# lex_anger <- lex_emo[lex_emo$V2=="anger",]
# lex_disgust <- lex_emo[lex_emo$V2=="disgust",]
# lex_fear <- lex_emo[lex_emo$V2=="fear",]
# lex_joy <-lex_emo[lex_emo$V2=="joy",]
# lex_sadness <- lex_emo[lex_emo$V2=="sadness",]
# lex_surprise <- lex_emo[lex_emo$V2=="surprise",]
# 
# rm("lex","lex_emo")
# 
# counts_emo <- list(anger = nrow(lex_anger),disgust = nrow(lex_disgust), fear = nrow(lex_fear),joy = nrow(lex_joy), sadness = nrow(lex_sadness),surprise = nrow(lex_surprise))
# list_emo <- list(anger = lex_anger,disgust = lex_disgust, fear = lex_fear,joy = lex_joy, sadness = lex_sadness ,surprise = lex_surprise)
# list_emo$anger[,1]
# counts_emo$anger
# ###################################################################
# ###################FUNCTION negation positions
# 
# negation_position <- function (x) {
#   op = c(which(x %in% negation_words),which(x %in% negation_words)+1)
#   op <- op[order(op, decreasing=F)]
#   x[op]
# }
# 
# 
# ################### MATCHING WORDS FUNCTION #######################
# 
# matching_words <- function(x,lexicon,negation = F){
#   if(negation==T){
#     a <- which(x %in% lexicon$V1)
#     op <- paste(x[a-1],x[a])
#   }
#   else{
#     op = lexicon[na.omit(match(x,lexicon$V1)),1]
#   }
#   return(op)
# }

###################Sentiment (DT) ###############################

sentiment_op <- function(data){
  #data <- data.table(Body=iconv(finaldataframe$Body,from = 'WINDOWs-1252',to='UTF-8'))
  #write(paste("Processing starts",Sys.time()),file = "D:/log.txt",append = T)
  data <- data[, Record := 1:.N]
  setkey(data,Record)
  data <- data[,.(Process = preprocess(Text)),by=Record]
  
  data$Process <- lemmatize_strings(data$Process)
  
  data <- data[, SentenceNo := seq_along(Process), by =  Record]
  data <- cbind(data,Text = data$Process)
  #cleandata1 = data
  
  #data <- data[, Process := text.trim(Process)]
  #cleandata <- cleandata[, Process := clear(Process), by =  1:nrow(cleandata)]
  data <- data[, Process := stringi::stri_trans_tolower(Process)]
  #data <- data[, Words := as.character(stri_extract_all_words(Process))]
  data <- data[, Words := stri_extract_all_words(Process)]
  #cleandata <- data
  #write(paste("Preprocess ",Sys.time()),file = "D:/log.txt",append = T)
  data <- data[,  pos := lapply(Words,negation_position)]
  #write(paste("negation_position",Sys.time()),file = "D:/log.txt",append = T)
  
  words <- data[,.(Words1 = unlist(pos)),by=c("Record","SentenceNo")]
  words[,"strong_positive"]<-ifelse(is.na(lex_strong_neg[words[["Words1"]]][[2]]),0,
                                    unlist(lapply(lex_strong_neg[words[["Words1"]]][[1]],length)))
  words[,"strong_negative"]<-ifelse(is.na(lex_strong_pos[words[["Words1"]]][[2]]),0,
                                    unlist(lapply(lex_strong_pos[words[["Words1"]]][[1]],length)))
  words[,"weak_negative"]<-ifelse(is.na(lex_weak_pos[words[["Words1"]]][[2]]),0,
                                  unlist(lapply(lex_weak_pos[words[["Words1"]]][[1]],length)))
  words[,"weak_positive"]<-ifelse(is.na(lex_weak_neg[words[["Words1"]]][[2]]),0,
                                  unlist(lapply(lex_weak_neg[words[["Words1"]]][[1]],length)))
  words <- words[,.(sum(strong_positive),sum(strong_negative),
                    sum(weak_negative),sum(weak_positive)),by=c("Record","SentenceNo")]
  colnames(words)<-c("Record","SentenceNo","strong_positive","strong_negative",
                     "weak_negative","weak_positive")
  data <- merge(data,words,by=c("Record","SentenceNo"),all.x = T)
  #data[words,`:=`(strong_positive1:=V1,strong_negative1:=V2
  #              ,weak_negative1:=V3,weak_positive1:=V4),on=c("Record","SentenceNo")]
  data[is.na(strong_positive),strong_positive:=0]
  data[is.na(strong_negative),strong_negative:=0]
  data[is.na(weak_negative),weak_negative:=0]
  data[is.na(weak_positive),weak_positive:=0]
  
  rm(words)
  
  # data <- data[,  `:=`(strong_positive= negation_loop(pos,lex_strong_neg,T),
  #                            strong_negative= negation_loop(pos,lex_strong_pos,T),
  #                            weak_negative= negation_loop(pos,lex_weak_pos,T),
  #                            weak_positive= negation_loop(pos,lex_weak_neg,T))]
  # data <- data[,`:=`(strong_positive= lapply(pos,function(x){if(length(x)!=0)length(unique(na.omit(lex_strong_neg[unlist(x)])[[1]]))}),
  #                   strong_negative= lapply(pos,function(x){if(length(x)!=0)length(unique(na.omit(lex_strong_pos[unlist(x)])[[1]]))}),
  #                   weak_negative= lapply(pos,function(x){if(length(x)!=0)length(unique(na.omit(lex_weak_pos[unlist(x)])[[1]]))}),
  #                   weak_positive= lapply(pos,function(x){if(length(x)!=0)length(unique(na.omit(lex_weak_neg[unlist(x)])[[1]]))}))
  #              ,by=Record]
  #write(paste("negation_loop",Sys.time()),file = "D:/log.txt",append = T)
  data[,Words1 := Map(exclude,Words,pos)]
  #write(paste("negation_loop-exclude",Sys.time()),file = "D:/log.txt",append = T)
  # data[,  `:=`(strong_positive1= negation_loop(Words1,lex_strong_pos,F),
  #             strong_negative1= negation_loop(Words1,lex_strong_neg,F),
  #             weak_negative1= negation_loop(Words1,lex_weak_neg,F),
  #             weak_positive1= negation_loop(Words1,lex_weak_pos,F))
  #      ]
  words <- data[,.(Words1 = unlist(Words1)),by=c("Record","SentenceNo")]
  #words[,"p"] <- loop_pos[words[["Words1"]]][[2]]
  words[,"strong_positive1"]<-ifelse(is.na(lex_strong_pos[words[["Words1"]]][[2]]),0,
                                     unlist(lapply(lex_strong_pos[words[["Words1"]]][[1]],length)))
  words[,"strong_negative1"]<-ifelse(is.na(lex_strong_neg[words[["Words1"]]][[2]]),0,
                                     unlist(lapply(lex_strong_neg[words[["Words1"]]][[1]],length)))
  words[,"weak_negative1"]<-ifelse(is.na(lex_weak_neg[words[["Words1"]]][[2]]),0,
                                   unlist(lapply(lex_weak_neg[words[["Words1"]]][[1]],length)))
  words[,"weak_positive1"]<-ifelse(is.na(lex_weak_pos[words[["Words1"]]][[2]]),0,
                                   unlist(lapply(lex_weak_pos[words[["Words1"]]][[1]],length)))
  words <- words[,.(sum(strong_positive1),sum(strong_negative1),
                    sum(weak_negative1),sum(weak_positive1)),by=c("Record","SentenceNo")]
  colnames(words)<-c("Record","SentenceNo","strong_positive1","strong_negative1",
                     "weak_negative1","weak_positive1")
  data <- merge(data,words,by=c("Record","SentenceNo"),all.x = T)
  #data[words,`:=`(strong_positive1:=V1,strong_negative1:=V2
  #              ,weak_negative1:=V3,weak_positive1:=V4),on=c("Record","SentenceNo")]
  data[is.na(strong_positive1),strong_positive1:=0]
  data[is.na(strong_negative1),strong_negative1:=0]
  data[is.na(weak_negative1),weak_negative1:=0]
  data[is.na(weak_positive1),weak_positive1:=0]
  #merge(data,words,by=c("Record","SentenceNo"),all.x = T)
  # data <- data[,`:=`(strong_positive1= lapply(Words1,function(x)length(unique(na.omit(lex_strong_pos[unlist(x)])[[1]]))),
  #                    strong_negative1= lapply(Words1,function(x)length(unique(na.omit(lex_strong_neg[unlist(x)])[[1]]))),
  #                    weak_negative1= lapply(Words1,function(x)length(unique(na.omit(lex_weak_neg[unlist(x)])[[1]]))),
  #                    weak_positive1= lapply(Words1,function(x)length(unique(na.omit(lex_weak_pos[unlist(x)])[[1]]))))]
  #write(paste("negation_loop-after-exclude",Sys.time()),file = "D:/log.txt",append = T)
  data[,  `:=`(negative= -1*((1*(weak_negative+weak_negative1))+(1.5*(strong_negative+strong_negative1))),positive= 1*((1*(weak_positive+weak_positive1))+(1.5*(strong_positive+strong_positive1))))]
  
  #write(paste("negative&positive",Sys.time()),file = "D:/log.txt",append = T)
  data[,  scores := 1*(positive+negative)]
  
  data[,  sentiment := ifelse(scores>0,"Positive",ifelse(scores<0,"Negative","Neutral"))]
  #write(paste("sentiment",Sys.time()),file = "D:/log.txt",append = T)
  cols <- c('Negative','Neutral','Positive')
  data1=reshape2::dcast(data,Record~sentiment,fun.aggregate = length)
  if(ncol(data1)<4){
    missingcols <- cols[which(!cols %in% colnames(data1)[-1])]
    data1[,missingcols] <- 0
    setcolorder(setDT(data1),c("Record","Negative","Neutral","Positive"))
  }
  data2 <- reshape2::dcast(data,Record~sentiment,value.var = "Text",fun.aggregate = function(x)
    paste(x,collapse = "."))
  
  if(ncol(data2)<4){
    missingcols <- cols[which(!cols %in% colnames(data2)[-1])]
    data2[,missingcols] <- ""
    setcolorder(setDT(data2),c("Record","Negative","Neutral","Positive"))
  }
  colnames(data2) <- c('Record','Negative Sentences','Neutral Sentences','Positive Sentences')
  data2 <- merge(data1,data2,by='Record')
  
  #write(paste("final output",Sys.time()),file = "D:/log.txt",append = T)
  return(data2)
}



# ######################### Keyword Tag###########################
# tokenDF <- read.csv("keywords.csv")
# tokenDF$Keyword <- iconv(tokenDF$Keyword,from = 'WINDOWs-1252',to='UTF-8')
# tokenDF$Keyword <- lemmatize_strings(tokenDF$Keyword)
# tokenDF$Keyword <- tolower(as.character(tokenDF$Keyword))
# tokenDF <- unique(tokenDF)
# setDT(tokenDF,key="Keyword")
# 
# phrase <- function(data){
#   setkey(data,Record)
#   data$Text <- lemmatize_strings(data$Text)
#   data <- data[, .(Keyword = getKeywords(Text)),by=Record]
# }
# 
# getKeywords <- function(x){
#   x <- as.character(x)
#   keywords <- unlist(str_extract_all(str_trim(tolower(as.character(x))), regex(paste0("\\b",tokenDF$Keyword,"\\b"),ignore_case = T)))
#   #cat(x,file = "d:/log.txt",sep = "\n",append = T)
#   return(keywords)
# }


################### Function for Text Cleaning and Clustering Logic#########

getClusters <- function(df1,sw,ngramValue,rmWords=NULL,rmNum=F,freqpercent){
  
  #df1=read.csv("D:/Hackathons/TextAnalytics-Dashboard-integrated/CCD-messages.csv")
  
  messages <- data.frame(col1=do.call(paste, c(df1, sep=". ")))
  messages$body1 <- iconv(messages$col1, from="WINDOWS-1252", to = "utf-8", sub="")
  
  messages$body1=tolower(messages$body1)
  
  messages$uniqueid <- 1:nrow(messages)
  
  #sw <- readLines("D:/Text Analytics/TextAnalytics-tool/Text_POC/stopwords.txt")
  
  stpw = readLines("stopwords.txt")      # Select stopwords.txt file
  if(!is.null(sw)){
    stpw = unique(c(stpw,sw))
  }
  stpw1 = stopwords('english')         # tm package stop word list
  comn  = unique(c(stpw, stpw1))       # Union of two list
  stopwords = unique(c(gsub("'","",comn),comn)) # final stop word lsit after removing punctuation
  #head (stopwords)
  
  #############################################################
  #                        Text Cleaning                      #
  #############################################################
  
  messages$body1 = text.clean(messages$body1,rmnum = T)                         # basic HTML Cleaning etc
  
  messages$body1  =  removeWords(messages$body1,stopwords)            # removing stopwords created above
  
  ######lemmetization of words ######
  messages$body1 = lemmatize_strings(messages$body1)
  
  
  messages$body1 = apply(as.data.frame(messages$body1),1,function(x)gsub('\\s+', ' ',x))#remove extra spaces
  #head(messages$body1)                                      # print top documents
  #remove user selected words
  if(!is.null(rmWords) ){
    messages$body1  =  removeWords(messages$body1,rmWords)
    messages$body1 = apply(as.data.frame(messages$body1),1,function(x)gsub('\\s+', ' ',x))#remove extra spaces
  }
  
  # messages$body1=removeNumbers(messages$body1)
  # messages$body1=removePunctuation(messages$body1)
  # messages$body1=removeWords(messages$body1,stopwords)
  
  
  
  myReader <- readTabular(mapping=list(content="body1", id="uniqueid"))
  x1 <- Corpus(DataframeSource(messages[,c(3,2)]), readerControl=list(reader=myReader))
  
  #
  # myCorpusCopy <- x1
  # x1 <- tm_map(x1, stemDocument,language = "english")
  # #
  # #
  # # #corpus <- tm_map(corpus, stemCompletion, dictionary=myCorpusCopy, type="shortest")
  # # #corpus <- tm_map(corpus, stemCompletion, dictionary=myCorpusCopy)
  # #
  # #
  # # ##### OOB stemCompletion is not working fine. So below custom function.
  # #
  # stemCompletion2 <- function(x, dictionary) {
  #   x <- unlist(strsplit(as.character(x), " "))
  #   # Unexpectedly, stemCompletion completes an empty string to
  #   # a word in dictionary. Remove empty string to avoid above issue.
  #   x <- x[x != ""]
  #   x <- stemCompletion(x, dictionary=dictionary)
  #   x <- paste(x, sep="", collapse=" ")
  #   PlainTextDocument(stripWhitespace(x))
  # }
  # #x1 <- lapply(x1, stemCompletion2, dictionary=myCorpusCopy)
  # x1 <- Corpus(VectorSource(x1))
  # 
  ngram <- function(x) NGramTokenizer(x, Weka_control(min = ngramValue, max = ngramValue,delimiters = "\\s"))
  options(mc.cores=1)
  
  x1 <- TermDocumentMatrix(x1, control = list(tokenize = ngram,removePunctuation = F,weighting=weightTf))
  x1=removeSparseTerms(x1,sparse=.99)
  #names(freqpercent) <- "clustering"
  #dtm=x1#t(custom.dtm(x1=x1,scheme="tf"))
  
  
  a1 = apply(x1, 1, sum)
  a2 <- (a1>=2)
  dtm = x1[a2,]
  if (length(a1)>100 ){
    
    freq <- ceiling(freqpercent * (max(a1)))
    if(freq >= 2)
      a2<- (a1>=freq)
  }
  dtm1 = x1[a2, ]
  
  # a0 = NULL; 
  # for (i1 in 1:ncol(dtm1)){ if (sum(dtm1[, i1]) == 0) {a0 = c(a0, i1)} }
  # length(a0)    # no. of empty docs in the corpus
  # if (length(a0) >0) { dtm1.new = dtm1[, -a0]} else {dtm1.new = dtm1};
  # dtm1 = dtm1.new
  
  disttable=dist(dtm,method="euclidean")
  disttable1=dist(dtm1,method="euclidean")
  
  dtm_matrix <- as.matrix(dtm)
  map_df<- as.data.frame(as.table(dtm_matrix))
  map_df <- map_df[map_df$Freq>0,-3]
  
  colnames(map_df)<- c("Terms","uniqueid")
  #map_agg <- aggregate(text~uniqueid,map_dataframe,paste,collapse=", ")
  
  final_df <- merge(map_df,messages,by="uniqueid",sort=T)
  
  
  ret_list <- list(disttable,final_df,disttable1)
  #clustgrps=hclust(disttable,method="average")
  
  #plot(clustgrps)
  return(ret_list)
  # 
  # hcut_grp <- hcut(dist_func, k = 3, stand = F)
  # 
  # # Visualize
  # fviz_dend(hcut_grp, rect = TRUE, cex = 0.5,
  #           k_colors = c("#00AFBB","#2E9FDF", "#E7B800"))
}