#Ebert/Rider
#Updated 23 March 2016


# Storage directory for loading R objects
#setwd("/home/david/Desktop/Documents/GitRepos/LAR")
setwd("C:/Users/000678922/Desktop/LAR")
storage.directory = "~/Desktop/Huang Research/LAR/"
github.directory = "~/Desktop/Documents/GitRepos/LAR"

# Load libraries for other scripts
library(stringr) #library for str_count function
library(ggplot2) #for graphs
library(caret) #for confusionMatrix

#library(tm) # for building term frequency matrix from corpus
#library(cldr) # for detecting tweet language
#library(beepr) # for beeping, just use beepr::beep(3)

#library(e1071) # for naive bayes model
#library(pROC) #ROC curves
#library(randomForest)
#library(rpart)


#AFINN_lexicon
  AFINN_lexicon = read.delim(file = "Lexicons/AFINN/AFINN-111.txt", stringsAsFactors = FALSE, header = F, quote = '')
  names(AFINN_lexicon) <- c('word','score')
  AFINN_lexicon = rbind(AFINN_lexicon, c("happytoken", 5), c("sadtoken", -5))
  AFINN_lexicon$word.clean <- gsub('-',' ' , AFINN_lexicon$word)  #Replacing Hyphens with Spaces
  AFINN_lexicon$word.clean <- gsub("[[:punct:]]", '', AFINN_lexicon$word.clean)  #Removing punctuation


  
  classify.polarity = function(documents, lexicon = AFINN_lexicon){ # This is for the lexicon approach
    require(plyr)
    require(dplyr)
    sentscorevec = laply(documents, function(documents, lex = lexicon)
    {
      words = unlist(strsplit(documents, " ")) #access words
      #eventually add words to term-document matrix here?
      indices = match(words, lexicon[,1], nomatch = 0)
      vals = as.numeric(lexicon[indices,2])
      #print(c(words, indices, vals))
      
      #fix negation
      if(length(words)>1){
        for(i in 2:length(words)){
          #print(i)
          #print(words[i-1])
          if(words[i-1] %in% negations & words[i] != words[i-1]){
            #print(words[(i-1):i])
            #print("There's a negation here")
            vals[length(vals)+1] = (-2)*as.numeric(lexicon[pmatch(words[i], lexicon[,1], nomatch = NA),2])
          }
        }   
      }
      
      #return sum
      return(sum(na.omit(vals)))
    }, .progress = "text")
    return(sentscorevec)
  }
  






AFINN_lexicon.frequencies=function(x){
  str_count(x,AFINN_lexicon$word.clean)
}



ndsi.frequencies=function(x){
  str_count(x,freq.all$word[1:nrow(freq.all)])
}



bin.maker = function(binsize, max){
  nbins = ceiling(max/binsize)
  result = as.list(1:nbins)
  for(i in 1:(nbins-1)){
    result[[i]]=((binsize*(i-1)+1):(binsize*(i)))
  }
  result[[nbins]] = ((binsize*(nbins-1)+1):max)
  return(result)
}



classify.polarity.machine = function(documents, chunk.size = 5000, model = rf.model){
  require(plyr)
  require(dplyr)
  require(randomForest)
  load(file = paste(storage.directory, "rf.model.RData", sep = ""))
  load(paste(storage.directory,"freq.all.RData", sep = "")) # load freq.all lexicon into memory as freq.all
  ndsi_lexicon = freq.all[1:1024,]
  
  if(length(documents)<chunk.size){stop("chunk.size must be less than length(documents). Also, length(documents) must be at least 2.")}
  
  column.names = paste("X", 1:1024, sep = "")
  result = NULL
  
  chunks = bin.maker(chunk.size, length(documents))
  
  for(i in 1:length(chunks)){
    print(paste((i-1)*chunk.size, "out of", length(documents), "rows analyzed:", 
                round((i-1)*100*chunk.size/length(documents), digits = 1), "percent complete"))
    
    term.freq <- t(apply(t(documents[chunks[[i]]]), 2, #MAY TAKE TIME!
                         ndsi.frequencies))
    
    colnames(term.freq) = column.names
    pred.sentiment = predict(model, newdata = term.freq, type = "prob")
    result = c(result, pred.sentiment[,1])
  }
  
  return(result)
}





#Optimize cutoff value for classifier scores
optimize.cutoff = function(score.vec, polarity.vec, min = -10, max = 10, step = 1){
  require(caret)
  require(pROC)
  cutoffs = seq(min, max, by = step)
  accuracy.vec=1:length(cutoffs)
  for(i in 1:length(cutoffs)){
    accuracy.vec[i] = confusionMatrix(as.numeric(score.vec>=(cutoffs[i])),polarity.vec)$overall[1]
  }
  print(plot(roc(polarity.vec, score.vec)))
  print(accuracy.vec)
  optimal.cutoff = cutoffs[which.max(accuracy.vec)]
  print(confusionMatrix(as.numeric(score.vec>=(optimal.cutoff)),polarity.vec)$overall[1])
  return(optimal.cutoff)
}








#############################################################
###### Remove Unnecessary Raw columns
#############################################################

remove_raw_columns = function(tweet_data_frame){
  
  #Keep only necessary columns from among the 42 provided by raw data
  tweet_data_frame = tweet_data_frame[,c("text", "screen_name", "id_str", "place_lat", "place_lon", "created_at")]
  
  #Rename place_lat to lat
  tweet_data_frame$lat = tweet_data_frame$place_lat
  tweet_data_frame$place_lat = NULL
  
  #Rename place_lon to lon
  tweet_data_frame$lon = tweet_data_frame$place_lon
  tweet_data_frame$place_lon = NULL
  
  return(tweet_data_frame)
}



#############################################################
###### Remove Spanish Tweets
#############################################################

remove_spanish_tweets = function(tweet_data_frame){
  require(cldr)
  
  #Remove rows with very Spanish-sounding tweets.
  tweet_data_frame[,c("language", "isReliable")] = detectLanguage(tweet_data_frame$text)[c("detectedLanguage", "isReliable")]
  tweet_data_frame = tweet_data_frame[tweet_data_frame$language != "SPANISH" | tweet_data_frame$isReliable == FALSE,]
  tweet_data_frame$language = NULL
  tweet_data_frame$isReliable = NULL 
  
  return(tweet_data_frame)
}


#############################################################
###### Identify emoticon tweets within a data frame
#############################################################


identify_emoticons = function(tweet_data_frame){
  
  #initialize polarity column
  tweet_data_frame$polarity = NA

  # Emoticons to use
  sad_emoticons = c("\\:\\(", "\\:-\\(", "\\)\\:", "\\)-\\:", ":\\[", ":\\{", "\\}:","=\\(", "\\)=", "☹")
  happy_emoticons = c("\\:\\)" , "\\(\\:", "\\:-\\)", "\\(-\\:", "\\:D", "\\:-D", "=\\)", "\\(=", "☺", "☻")
  
  # grep everything all at once
  sad_indices = grep(paste(sad_emoticons, collapse = "|"), tweet_data_frame$text, value = FALSE)
  happy_indices = grep(paste(happy_emoticons, collapse = "|"),tweet_data_frame$text, value = FALSE)
  
  # Mark sad tweets FALSE, happy tweets TRUE
  tweet_data_frame$polarity[sad_indices] = FALSE
  tweet_data_frame$polarity[happy_indices] = TRUE
  
  return(tweet_data_frame)
}



#############################################################
###### Clean Tweets
#############################################################

clean.tweets = function(documents, 
                        usernameToken = "usernametoken", 
                        hashToken = " hashtoken ",
                        happyToken = " happytoken ", 
                        sadToken = " sadtoken "){
  happy_emoticons = c("\\:\\)" , "\\(\\:", "\\:-\\)", "\\(-\\:", "\\:D", "\\:-D", "=\\)", "\\(=", "☺", "☻")
  sad_emoticons = c("\\:\\(", "\\:-\\(", "\\)\\:", "\\)-\\:", ":\\[", "\\]:", ":\\{", "\\}:","=\\(", "\\)=", "☹")
  require(plyr)
  require(dplyr)
  require(qdapRegex)
  cleantext = laply(documents, function(documents)
  {
    #documents = gsub("RT", "retweet", documents) # tokenize retweets. Ignore this since tweets aren't retweeets
    documents = rm_url(documents) #tokenize urls
    documents = gsub("@\\w+", usernameToken, documents) #tokenize @
    documents = gsub("\\#", hashToken, documents) #tokenize #. Not necessary for tweets that haven't been classified yet.
    documents = gsub(paste(happy_emoticons, collapse = "|"), happyToken, documents) #tokenize happy emoticons
    documents = gsub(paste(sad_emoticons, collapse = "|"), sadToken, documents) #tokenize sad emoticons
    documents = gsub("[[:punct:]]", "", documents) #remove punctuation
    documents = gsub("[[:digit:]]", "", documents) #remove numbers
    documents = gsub("[^a-zA-Z]", " ", documents) #remove everything that isn't a letter
    documents = tolower(documents) #set lower case
    documents<-gsub('([[:alpha:]])\\1+', '\\1\\1', documents) # limit character repeats to maximum 2
    documents<-trimws(documents) #remove leading and trailing whitespace
  }, .progress = "text")
  return(cleantext)
}



#############################################################
###### Import tweets from .json files
#############################################################


lexicon_sentiment_score = function(documents, lexicon = AFINN_lexicon){
  require(plyr)
  require(dplyr)
  negations = c("no", "not","none","nobody","nothing","neither","never","doesnt","isnt","wasnt","shouldnt","wouldnt", "couldnt","wont","cant","dont")
  sentscorevec = laply(documents, function(documents, lex = lexicon)
  {
    words = unlist(strsplit(documents, " ")) #access words
    #eventually add words to term-document matrix here?
    indices = match(words, lexicon[,1], nomatch = 0)
    vals = as.numeric(lexicon[indices,2])
    #print(c(words, indices, vals))
    
    #fix negation
    if(length(words)>1){
      for(i in 2:length(words)){
        #print(i)
        #print(words[i-1])
        if(words[i-1] %in% negations & words[i] != words[i-1]){
          #print(words[(i-1):i])
          #print("There's a negation here")
          vals[length(vals)+1] = (-2)*as.numeric(lexicon[pmatch(words[i], lexicon[,1], nomatch = NA),2])
        }
      }   
    }

    #return sum
    return(sum(na.omit(vals)))
  }, .progress = "text")
  return(sentscorevec)
}




#############################################################
###### Import tweets from .json files
#############################################################

#raw_file_path = "~/Desktop/Huang Research/September_LaPY_data/raw_data/"
#feather_file_path = "~/Desktop/Huang Research/September_LaPY_data/feather_data/"
#csv_file_path = "~/Desktop/Huang Research/September_LaPY_data/csv_data/"
#max_tweets_per_file = 1000000  # make sure this number is significantly above the number of tweets per file, about 200k

import_tweets_from_json = function(base_file_path = "C:/Users/000678922/Desktop/LAR_data",
                                   raw_file_path = "raw_data/",
                                   feather_file_path = "feather_data/",
                                   csv_file_path = "csv_data/",
                                   max_tweets_per_file = 1000000){
  require(streamR)
  require(feather)
  
  keep_df = data.frame()
  j=1
  setwd(base_file_path)
  
  for(i in list.files(path = raw_file_path)){
    print(paste("Reading ", i, "...", sep = ""))
    
    #Import one .json file into R as data frame
    temp_df = parseTweets(tweets = paste(raw_file_path, i, sep = ""))
    
    #Remove unnecessary raw columns
    temp_df = remove_raw_columns(temp_df)
    
    #Remove very spanish sounding tweets
    temp_df = remove_spanish_tweets(temp_df)
    
    #Identify happy and sad emoticons in tweets before cleaning.
    #Happy tweets have polarity marked as TRUE; Sad tweets have polarity marked as FALSE
    print(paste("Identifying happy and sad emoticons in ", i, "...", sep = ""))
    temp_df = identify_emoticons(temp_df)
    
    #Clean, lowercase, remove url's and tokenize text
    print(paste("Cleaning tweets from ", i, "...", sep = ""))
    temp_df$text = clean.tweets(documents = temp_df$text, 
                                happyToken = " ", 
                                sadToken = " ")
    
    #Remove rows with no text
    temp_df = temp_df[temp_df$text!="",]
    
    #Find AFINN score for new tweets
    print(paste("Finding AFINN scores for ", i, "...", sep = ""))
    temp_df$afinn_score = lexicon_sentiment_score(temp_df$text)
    
    
    #Combine temp_df and keep_df
    keep_df = rbind(keep_df, temp_df)
    
    #If keep_df is too big OR if last file then write max_tweets_per_file tweets from keep_df to file
    if(nrow(keep_df) >= max_tweets_per_file | i==tail(list.files(path = raw_file_path),1)){
      
      print("Writing to file...")
      if (nrow(keep_df)>max_tweets_per_file) {
        write_df = keep_df[1:max_tweets_per_file,]
        keep_df = keep_df[(max_tweets_per_file+1):nrow(keep_df),]
      }
      else {
        print("Wow! Either this is really unusual, or it's almost time to be done!")
        write_df = keep_df
        keep_df = data.frame()
      }
      
      # Now write max_tweets_per_file # of tweets to file from write_df and let keep_df keep collecting batches of tweets
      print(paste("Writing", nrow(write_df), "Tweets to file", sep = " "))
      write_feather(x = write_df, path = paste(feather_file_path, j, ".feather", sep=""))
      write.csv(x = write_df, file = paste(csv_file_path, j, ".csv", sep=""))
      write_df = NULL
      j = j+1
    }
  }
}




#############################################################
###### Build emoticon semi-supervised data set
#############################################################


extract_emoticon_tweets = function(tweet_data_frame){
  happy_tweets = subset(tweet_data_frame, polarity == TRUE)
  sad_tweets = subset(tweet_data_frame, polarity == FALSE)
  
  #Undersample to create emoticon data frame
  sample_size = min(nrow(happy_tweets),nrow(sad_tweets))
  happy_indices = sample(nrow(happy_tweets), sample_size)
  sad_indices = sample(nrow(sad_tweets), sample_size)
  
  emoticon_tweets = rbind(happy_tweets[happy_indices,],sad_tweets[sad_indices,])
  return(emoticon_tweets)
}




#############################################################
###### Find word frequency in a list of documents
#############################################################


word_frequency <- function(document.vector, sparsity = .99){
  require(tm)
  # construct corpus
  temp.corpus <- Corpus(VectorSource(document.vector))
  
  # construct tf matrix and remove sparse terms
  temp.tf <- DocumentTermMatrix(temp.corpus,
                                control = list(stopwords = stopwords('english'),
                                               removeNumbers = T))
  temp.tf <- removeSparseTerms(temp.tf, sparsity)
  temp.tf <- as.matrix(temp.tf)
  
  # construct word frequency df
  freq.df <- colSums(temp.tf)
  freq.df <- data.frame(word = names(freq.df), freq = freq.df)
  rownames(freq.df) <- NULL
  return(freq.df)
}





#############################################################
###### Build ndsi_lexicon words from emoticon
#############################################################


emoticon_to_rf_classifier = function(emoticon_tweets, word_sparsity = 0.9999, smoothing_alpha = 2^7){
  require(tm)
  
  #Collect word frequencies in happy and sad tweets
  word.freq.happy = word_frequency(emoticon_tweets$text[emoticon_tweets$polarity == TRUE],
                            sparsity=word_sparsity) #terms must occur in at least 1 out of 1000 tweets
  word.freq.sad = word_frequency(emoticon_tweets$text[emoticon_tweets$polarity == FALSE],
                            sparsity=word_sparsity)
  
  #Merge by word into ndsi_lexicon
  ndsi_lexicon = merge(word.freq.happy, word.freq.sad, by = 'word', all = T)
  colnames(ndsi_lexicon) = c("word", "freq.happy", "freq.sad")
  
  #Set NA's to 0
  ndsi_lexicon$freq.happy[is.na(ndsi_lexicon$freq.happy)] = 0
  ndsi_lexicon$freq.sad[is.na(ndsi_lexicon$freq.sad)] = 0
  
  #Differences between Positive and Negative Frequencies
  ndsi_lexicon$diff = abs(ndsi_lexicon$freq.happy - ndsi_lexicon$freq.sad)

  
  #NDSI - note the presence of smoothing_alpha term
  ndsi_lexicon$ndsi = abs(ndsi_lexicon$diff)/
    (ndsi_lexicon$freq.happy + ndsi_lexicon$freq.sad + 2 * smoothing_alpha) 
                                
  
  #Sorting by NDSI, then scale NDSI score to 1.
  ndsi_lexicon = ndsi_lexicon[order(-ndsi_lexicon$ndsi), ]
  ndsi_lexicon$ndsi = ndsi_lexicon$ndsi/max(ndsi_lexicon$ndsi)
  
  #Convert word to a string
  ndsi_lexicon$word = as.character(ndsi_lexicon$word)
  ndsi_lexicon = ndsi_lexicon[ndsi_lexicon$ndsi>0,] # restrict to words with a nonzero ndsi score.

  #Write to feather? If so add this feature later
  #
  #
  
  return(ndsi_lexicon)
}








#############################################################
###### Build a Bag of Words random forest classifier from emoticon_tweets
#############################################################


make_term_freq = function(emoticon_tweets, ndsi_lexicon, max_words = 98, min_ndsi_score = 0.01){
  
  # Begin by trimming ndsi_lexicon in case it's too long
  # Min ndsi score
  ndsi_lexicon = subset(ndsi_lexicon, ndsi>=min_ndsi_score)
  
  # Max words
  if (nrow(ndsi_lexicon)>max_words){
    ndsi_lexicon = ndsi_lexicon[1:max_words,]
  }

  # Function for apply statement
  ndsi.frequencies=function(x){
    str_count(x,ndsi_lexicon$word[1:nrow(ndsi_lexicon)])
  }
  
  # Create emoticon term frequency matrix. This may take a long time!
  emoticon_term_freq <- t(apply(t(emoticon_tweets[,"text"]), 2,    #TAKES TIME; 10 minutes for 100 000 tweets and 1276 terms 
                                ndsi.frequencies))
  
  # Append target variable and afinn scores
  emoticon_term_freq = data.frame(polarity=emoticon_tweets$polarity, AFINN = emoticon_tweets$afinn_score, emoticon_term_freq)

  return(emoticon_term_freq)
}


#############################################################
###### Make and test a random forest classifier from emoticon_term_freq
#############################################################


make_rf_classifier = function(emoticon_term_freq, train_frac = 0.03, test_frac = 0.03){
  require(ggplot2)
  require(randomForest)
  require(caret)
  require(pROC)
  
  train_indices = sample(nrow(emoticon_term_freq), train_frac*nrow(emoticon_term_freq))
  test_indices = sample(nrow(emoticon_term_freq), test_frac*nrow(emoticon_term_freq))
  
  emoticon_term_freq$polarity = as.factor(emoticon_term_freq$polarity) 
  
  rf_model=randomForest(polarity~.,data = emoticon_term_freq[train_indices,])
  
  #train accuracy
  train_conf_matrix = rf_model$confusion
  train_accuracy = sum(diag(train_conf_matrix))/sum(train_conf_matrix)
  
  #train AUC
  train_phat = predict(rf_model, newdata = emoticon_term_freq[train_indices,], type = "prob")
  train_auc = roc(emoticon_term_freq$polarity[train_indices],train_phat[,2])$auc
  
  #test accuracy
  test_pred = predict(rf_model, newdata = emoticon_term_freq[test_indices,])
  test_conf_matrix = confusionMatrix(emoticon_term_freq$polarity[test_indices],test_pred)$table
  test_accuracy = sum(diag(test_conf_matrix)/sum(test_conf_matrix))
  
  #test AUC
  test_phat = predict(rf_model, newdata = emoticon_term_freq[test_indices,], type = "prob")
  test_auc = roc(emoticon_term_freq$polarity[test_indices],test_phat[,2])$auc
  
  #Sent140
  sent140_term_freq = make_term_freq(get_sent140(), ndsi_lexicon)
  sent140_pred = as.factor(predict(rf_model, newdata = sent140_term_freq))
  sent140_phat = predict(rf_model, newdata = sent140_term_freq, type = "prob")
  
  # Accuracy and AUC
  sent140_conf_matrix = confusionMatrix(sent140$polarity,sent140_pred)
  sent140_auc = roc(emoticon_term_freq$polarity[train_indices],train_phat[,2])$auc

}

 





#############################################################
###### Apply rf_model to tweets
#############################################################





get_sent140 = function(){
  
  #Import .csv file
  setwd(github.directory)
  sent140 = read.csv("testdata.manual.2009.06.14.csv", header = FALSE, colClasses = 
                       c("character", "character", "character", "character", "character", "character"))
  colnames(sent140) = c("polarity", "not_sure", "created_at", "search_query", "username", "raw_text")
  
  #initialize polarity
  sent140[sent140$polarity == 0,]$polarity = FALSE
  sent140[sent140$polarity == 4,]$polarity = TRUE
  
  #clean text
  sent140 = sent140[sent140$polarity !=2, c("polarity", "raw_text")]
  sent140$polarity = as.factor(sent140$polarity)
  sent140$text = clean.tweets(sent140$raw_text)
  
  #afinn_score
  sent140$afinn_score = lexicon_sentiment_score(sent140$text)
  
  return(sent140)
}











