###################################################################
#               User defined Functions                            #
###################################################################

# 1. Text cleaning

# text.clean = function(x)                          # text data
# {
#         x  =  gsub("<.*?>", "", x)                  # regex for removing HTML tags
#         x  =  gsub("[^[:alnum:]///' ]", " ", x)     # keep only alpha numeric 
#         x  =  iconv(x, "latin1", "ASCII", sub="")   # Keep only ASCII characters
#         x  =  tolower(x)                          # convert to lower case characters
#         x  =  removePunctuation(x)                # removing punctuation marks
#         x  =  removeNumbers(x)                    # removing numbers
#         x  =  stripWhitespace(x)                  # removing white space
#         x  =  gsub("^\\s+|\\s+$", "", x)          # remove leading and trailing white space
#         return(x)
# }
text.trim = function(x)                          # text data
{
  x  =  gsub("<.*?>", "", x)                  # regex for removing HTML tags
  x  =  gsub("[^[:alnum:]///' ]", " ", x)     # keep only alpha numeric
  x  =  iconv(x, "latin1", "ASCII", sub="")   # Keep only ASCII characters
  x  =  tolower(x)                          # convert to lower case characters
  x  =  removePunctuation(x)                # removing punctuation marks
  x  =  removeNumbers(x)                    # removing numbers
  x  =  stripWhitespace(x)                  # removing white space
  x  =  gsub("^\\s+|\\s+$", "", x)          # remove leading and trailing white space
  return(x)
}
text.clean = function(x,rmnum)                          # text data
{
  x= paste(' ',x,' ')
  x = gsub("http[^[:space:]]*", "", x)
  x  =  gsub("<.*?>", "", x)                  # regex for removing HTML tags
  x =  gsub("\\[+[^\\[]*\\]"," ",x)#remove [[user:1544392569]] 
  #x =   gsub('( |^)-+|-+( |$)', '\\1', gsub("[^ [:alnum:]'\\-]", " ", x))  #
  #x =  gsub("^[.,\\/#!$%\\^&\\*;:{}=\\-_`~()]+|[.,\\/#!$%\\^&\\*;:{}=\\-_`~()]+$"," ",x) #remove punct at start and end of a term
  regex_special ="^[.,\\/#!$%\\^&\\*;:{}=\\-_`~()]+|[.,\\/#!$%\\^&\\*;:{}=\\-_`~()]+$"
  if(rmnum)
    regex_special ="^[.,\\/#!$%\\^&\\*;:{}=\\-_`~()]+|[.,\\/#!$%\\^&\\*;:{}=\\-_`~()]+$|^\\d+$"
  val = lapply(stringi::stri_extract_all_words(x),
               stringi::stri_replace_all_regex,
               regex_special," ")
  #if(length(val)>1){
  x= lapply(val,paste,collapse = " ")
  #}
  # else{
  #   x = paste(val,collapse = " ")
  # }
  #x = gsub("[^ [:alnum:]'\\-]", " ", x)
  #x = gsub(':',' ',gsub('\\]+','',gsub('\\[+','',x)))
  #x = gsub("\\s(\\d+)\\s"," ",x)
  x = gsub("\\s(\\d+)-(\\d+)\\s","",x) #TO REMOVE 200-300 KIND OF PATTERN
  x  =  iconv(x, "latin1", "ASCII", sub="")   # Keep only ASCII characters
  x  =  tolower(x)                          # convert to lower case characters
  x  =  stripWhitespace(x)                  # removing white space
  x  =  gsub("^\\s+|\\s+$", "", x)          # remove leading and trailing white space
  return(x)
}

############################################################################
# Encoding bi/tri gram as unigram

n.gram = function(x1,                       # Text Corpus
                  ngram,                    # encoding scheme "bi" or "tri"
                  f )                       # Mininmum frequency for bi/tri gram pattern matching
  
{ if ( ngram =="bi")
  
{ 
  
  ##################################################################
  # Weka_control(min = 2, max = 2)
  ##################################################################
  
  ngram <- function(x1) NGramTokenizer(x1, Weka_control(min = 2, max = 2))  
  
  tdm0 <- TermDocumentMatrix(x1, control = list(tokenize = ngram,
                                                tolower = TRUE, 
                                                removePunctuation = F,
                                                removeNumbers = TRUE))     # patience. Takes a minute.
  
  
  tdm = tdm0; rm('tdm0')
  
  a1 = apply(tdm, 1, sum)  
  a2 = ((a1 >= f))
  tdm.new = tdm[a2, ]
  rm('a1','a2','tdm')
  
  # remove blank documents (i.e. columns with zero sums)
  a0 = NULL; 
  for (i1 in 1:ncol(tdm.new)){ if (sum(tdm.new[, i1]) == 0) {a0 = c(a0, i1)} }
  length(a0)    # no. of empty docs in the corpus
  if (length(a0) >0) { tdm.new1 = tdm.new[, -a0]} else {tdm.new1 = tdm.new};
  
  rm('a0','tdm.new')
  dim(tdm.new1)    # reduced tdm
  x1mat = t(tdm.new1)    # don't do tfidf, not mentioned anywhere for topic modeling.
  dim(x1mat);    # store[i1, 5] = ncol(x2mat);
  
  
  test = colnames(x1mat); 
  test1 = gsub(" ",".", test);  # replace spaces with dots
  colnames(x1mat) = test1
  
  a11 = apply(x1mat, 2, sum)
  a12 = order(a11, decreasing = T)
  a13 = as.matrix(a11[a12])
  
  x1 = unlist(lapply(x1, content))
  x1 = paste("",x1,"")
  
  
  for (i in 1:nrow(a13)){    
    
    focal.term = gsub("\\.", " ", rownames(a13)[i])
    replacement.term = gsub(" ", "-", focal.term)
    replacement.term=paste("",replacement.term,"")
    x1 = gsub(paste("",focal.term,""), replacement.term, x1)  
    
  }  # now, our x corpus has the top 400 bigrams encoded as unigrams
  
  x1 = Corpus(VectorSource(x1))    # Constructs a source for a vector as input
  
  return(x1)
}
  
  if ( ngram =="tri") 
    
  { 
    
    ##################################################################
    # Weka_control(min = 3, max = 3)
    ##################################################################
    
    ngram <- function(x1) NGramTokenizer(x1, Weka_control(min = 3, max = 3))  
    
    tdm0 <- TermDocumentMatrix(x1, control = list(tokenize = ngram,
                                                  tolower = TRUE, 
                                                  removePunctuation = TRUE,
                                                  removeNumbers = TRUE,
                                                  stopwords = TRUE  ))     # patience. Takes a minute.
    
    tdm = tdm0; rm('tdm0')
    
    a1 = apply(tdm, 1, sum)  
    a2 = ((a1 >= f))
    tdm.new = tdm[a2, ]
    rm('a1','a2','tdm')
    
    # remove blank documents (i.e. columns with zero sums)
    a0 = NULL; 
    for (i1 in 1:ncol(tdm.new)){ if (sum(tdm.new[, i1]) == 0) {a0 = c(a0, i1)} }
    length(a0)    # no. of empty docs in the corpus
    if (length(a0) >0) { tdm.new1 = tdm.new[, -a0]} else {tdm.new1 = tdm.new};
    
    rm('a0','tdm.new')
    dim(tdm.new1)    # reduced tdm
    x1mat = t(tdm.new1)    # don't do tfidf, not mentioned anywhere for topic modeling.
    dim(x1mat);    # store[i1, 5] = ncol(x2mat);
    
    
    test = colnames(x1mat); 
    test1 = gsub(" ",".", test);  # replace spaces with dots
    colnames(x1mat) = test1
    
    a11 = apply(x1mat, 2, sum)
    a12 = order(a11, decreasing = T)
    a13 = as.matrix(a11[a12])
    
    x1 = unlist(lapply(x1, content))
    x1 = paste("",x1,"")
    
    for (i in 1:nrow(a13)){    
      
      focal.term = gsub("\\.", " ", rownames(a13)[i])
      replacement.term = gsub(" ", "-", focal.term)
      replacement.term=paste("",replacement.term,"")
      x1 = gsub(paste("",focal.term,""), replacement.term, x1)   
      
    }  # now, our x corpus has the top 400 bigrams encoded as unigrams
    
    x1 = Corpus(VectorSource(x1))    # Constructs a source for a vector as input
    
    
    ##################################################################
    # Weka_control(min = 2, max = 2)
    ##################################################################
    
    ngram <- function(x1) NGramTokenizer(x1, Weka_control(min = 2, max = 2))  
    
    tdm0 <- TermDocumentMatrix(x1, control = list(tokenize = ngram,
                                                  tolower = TRUE, 
                                                  removePunctuation = TRUE,
                                                  removeNumbers = TRUE,
                                                  stopwords = TRUE,
                                                  stemDocument = TRUE))     # patience. Takes a minute.
    
    tdm = tdm0; rm('tdm0')
    
    a1 = apply(tdm, 1, sum)  
    a2 = ((a1 >= f))
    tdm.new = tdm[a2, ]
    rm('a1','a2','tdm')
    
    # remove blank documents (i.e. columns with zero sums)
    a0 = NULL; 
    for (i1 in 1:ncol(tdm.new)){ if (sum(tdm.new[, i1]) == 0) {a0 = c(a0, i1)} }
    length(a0)    # no. of empty docs in the corpus
    if (length(a0) >0) { tdm.new1 = tdm.new[, -a0]} else {tdm.new1 = tdm.new};
    
    rm('a0','tdm.new')
    dim(tdm.new1)    # reduced tdm
    x1mat = t(tdm.new1)    # don't do tfidf, not mentioned anywhere for topic modeling.
    dim(x1mat);    # store[i1, 5] = ncol(x2mat);
    
    
    test = colnames(x1mat); 
    test1 = gsub(" ",".", test);  # replace spaces with dots
    colnames(x1mat) = test1
    
    a11 = apply(x1mat, 2, sum)
    a12 = order(a11, decreasing = T)
    a13 = as.matrix(a11[a12])
    
    x1 = unlist(lapply(x1, content))
    x1 = paste("",x1,"")
    
    
    for (i in 1:nrow(a13)){    
      
      focal.term = gsub("\\.", " ", rownames(a13)[i])
      replacement.term = gsub(" ", "-", focal.term)
      replacement.term=paste("",replacement.term,"")
      x1 = gsub(paste("",focal.term,""), replacement.term, x1)  
      
    }  # now, our x corpus has the top 400 bigrams encoded as unigrams
    
    x1 = Corpus(VectorSource(x1))    # Constructs a source for a vector as input
    return(x1)
  }
  
}

###########################################################
# Customize Document Term Matrix

custom.dtm  = function(x1,                               # Text Corpus
                       scheme,mapWords=NULL,freq=2)                           # tf or tfidf
{
  
  tdm = x1#TermDocumentMatrix(x1)
  
  a1 = apply(tdm, 1, sum)
  #############################################################
  #                        Mapping                      #
  #############################################################
  if(!is.null(mapWords)){
    for(i in 1:length(mapWords$data)){
      chr <- unlist(mapWords$data[i])
      if(chr[1] !="" & chr[2] !="" ){
        lstWrds <- unlist(strsplit(chr[2],","))
        mappingWord <- chr[1]
        mapTDM <- structure(rep(mappingWord,length(lstWrds)),.Names=lstWrds)
        a1 <- rename(a1,replace=mapTDM)
        rownames(tdm)[rownames(tdm) %in% lstWrds]<-mappingWord
      }
    }
    a1 <- tapply(unlist(a1), names(unlist(a1)), sum)
    
  }
  # if (scheme == "tfidf") {
  #   a2<- (a1>=1)
  # }else{
  #   a2 =((a1 >= 2)) 
  # }
  a2<- (a1>=freq)
  # if (nrow(as.matrix(a1))>100 && names(freq)=="clustering"){
  #   
  #   freq <- ceiling(freq * (max(a1)))
  #   a2<- (a1>=freq)
  # }
  
  
  tdm.new = tdm[a2, ]
  mat1<-rowsum(inspect(tdm.new),group = rownames(inspect(tdm.new)))
  tdm.new <- as.TermDocumentMatrix(mat1,weighting=weightTf)
  # remove blank documents (i.e. columns with zero sums)
  a0 = NULL; 
  for (i1 in 1:ncol(tdm.new)){ if (sum(tdm.new[, i1]) == 0) {a0 = c(a0, i1)} }
  length(a0)    # no. of empty docs in the corpus
  if (length(a0) >0) { tdm.new1 = tdm.new[, -a0]} else {tdm.new1 = tdm.new};
  
  #dim(tdm.new1)    # reduced tdm
  if (scheme == "tfidf") {
    x2mat = t(tfidf(tdm.new1))
  }
  else {x2mat = t((tdm.new1))}
  return(x2mat)
  
}


trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

preprocess = function(x)
{
  x  =  gsub("<.*?>", "", x)                  # regex for removing HTML tags
  x  = gsub("\\[+[^\\[]*\\]"," ",x)		 #remove [[user:1544392569]] 
  x  = iconv(x, to = "utf-8", sub = "")	
  x  = gsub(" ?(f|ht)tp(s?)://(.*)[.][a-z]+", "", x) #regex for removing websites
  x  = removeWords(x, c("cc","e.g","e.g.","etc","i.e.","i.e"))
  x  =  removeNumbers(x)                    # removing numbers
  x  = strsplit(as.character(x), "[!.?]")[[1]]
  x  =  gsub("[^[:alnum:]///' ]", " ", x)     # keep only alpha numeric 
  x  =  iconv(x, "latin1", "ASCII", sub="")   # Keep only ASCII characters 
  #x  =  removeNumbers(x)                    # removing numbers
  x  =  stripWhitespace(x)                  # removing white space
  x  =  gsub("^\\s+|\\s+$", "", x)          # remove leading and trailing white space
  x  = x[x!=""]
  return(x)
}

