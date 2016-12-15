# Ebert/Rider
# 23 March 2016

#############################################################
### Load libraries and set storage directories
#############################################################

# Load libraries for other scripts
library(stringr) #library for str_count function
library(ggplot2) #for graphs
library(caret) #for confusionMatrix
library(feather) # for importing data
library(tm)

setwd("/home/david/Desktop/Documents/GitRepos/LAR")
storage.directory = "~/Desktop/Huang Research/LAR/"
github.directory = "~/Desktop/Documents/GitRepos/LAR"
    


#############################################################
### Small Functions
#############################################################

AFINN_lexicon.frequencies=function(x){
  str_count(x,AFINN_lexicon$word.clean)
}

ndsi.frequencies=function(x){
  str_count(x,ndsi_lexicon_df$word[1:nrow(ndsi_lexicon_df)])
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



#############################################################
### Classify tweet polarity using model
#############################################################

classify.polarity.machine = function(tweet_df,
                                     chunk.size = 5000,
                                     ndsi_lexicon,
                                     model = rf.model) {
  require(plyr)
  require(dplyr)
  require(randomForest)
  require(doMC)
  registerDoMC(2)
  #load(file = paste(storage.directory, "rf.model.RData", sep = ""))
  #load(paste(storage.directory,"freq.all.RData", sep = "")) # load freq.all lexicon into memory as freq.all
  #ndsi_lexicon = freq_all[1:1024,]
  
  documents = tweet_df$text
  
  if (length(documents) < chunk.size) {
    stop(
      "chunk.size must be less than length(documents). Also, length(documents) must be at least 2."
    )
  }
  
  column.names = paste("X", 1:500, sep = "")
  result = NULL
  
  chunks = bin.maker(chunk.size, length(documents))
  
  for (i in 1:length(chunks)) {
    print(
      paste((i - 1) * chunk.size,
            "out of",
            length(documents),
            "rows analyzed:",
            round((i - 1) * 100 * chunk.size / length(documents), digits = 1),
            "percent complete"
      )
    )
    
    term.freq <-
      t(apply(t(documents[chunks[[i]]]), 2, #MAY TAKE TIME!
              ndsi.frequencies))
    
    colnames(term.freq) = column.names
    term.freq = cbind(tweet_df[i, c("afinn_score",
                                    "count_hashtags",
                                    "count_usernames",
                                    "count_url",
                                    "is_reply")], term.freq)
    
    pred.sentiment = predict(model, newdata = term.freq, type = "prob")
    result = c(result, pred.sentiment[, 2]) # Column 2 gives the probability of 'sad' tweets
  }
  return(result)
}



#############################################################
### Remove Unnecessary Raw columns
#############################################################

remove_raw_columns = function(tweet_data_frame) {
  require(stringr)
  
  #Keep only necessary columns from among the 42 provided by raw data
  tweet_data_frame = tweet_data_frame[, c(
    "text",
    "screen_name",
    "id_str",
    "place_lat",
    "place_lon",
    "created_at",
    "in_reply_to_status_id_str"
  )]
  
  #Rename place_lat to lat
  tweet_data_frame$lat = tweet_data_frame$place_lat
  tweet_data_frame$place_lat = NULL
  
  #Rename place_lon to lon
  tweet_data_frame$lon = tweet_data_frame$place_lon
  tweet_data_frame$place_lon = NULL
  
  #Rename in_reply_to_status_id
  tweet_data_frame$is_reply = 1 - as.numeric(is.na(tweet_data_frame$in_reply_to_status_id_str))
  tweet_data_frame$in_reply_to_status_id_str = NULL
  
  #Add columns for hashtags, usernames, and url
  dict <- c("#", "@", "https")
  counts = lapply(dict, str_count, string = tweet_data_frame$text)
  
  tweet_data_frame$count_hashtags = counts[[1]]
  tweet_data_frame$count_usernames = counts[[2]]
  tweet_data_frame$count_url = counts[[3]]
  
  return(tweet_data_frame)
}



#############################################################
### Remove Spanish Tweets
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
### Identify emoticon tweets within a data frame
#############################################################

identify_emoticons = function(tweet_data_frame){
  
  #initialize polarity column
  tweet_data_frame$polarity = NA
  
  #Rewrite text for identifying emoticons:
  tweet_data_frame$text = iconv(tweet_data_frame$text, "latin1", "ASCII", "byte")

  # Emoticons to use
  sad_emoticons = c("\\:\\(", "\\:-\\(", "\\)-\\:", ":\\[", ":\\{", "\\}:","=\\(", "\\)=", "☹",
                    "<ed><a0><bd><ed><b8><a0>", #Angry face
                    "<ed><a0><bd><ed><b8><a7>", #Anguished face
                    #"<ed><a0><bd><ed><b6><95><ed><a0><bc><ed><bf><bd>", #Middle finger, removed because it expresses anger, not sadness.
                    "<ed><a0><bd><ed><b2><a2>", #Anger symbol
                    "<ed><a0><bd><ed><b8><ad>", #Loudly crying face  <- This one is MASSIVE! #5 on emojitracker
                    "<ed><a0><bd><ed><b8><92>", #Unamused face
                    "<ed><a0><bd><ed><b8><b0>", #FACE WITH OPEN MOUTH AND COLD SWEAT
                    "<ed><a0><bd><ed><b8><a9>", #WEARY FACE
                    "<ed><a0><bd><ed><b2><94>", #BROKEN HEART
                    "<ed><a0><bd><ed><b8><91>", #EXPRESSIONLESS FACE
                    "<ed><a0><bd><ed><b8><ab>", #TIRED FACE
                    "<ed><a0><bd><ed><b8><9e>"  #DISAPPOINTED FACE
                    # Note that ): and ]: are removed because they attracted rubbish, e.g. #freestuffoc
  )
  
  happy_emoticons = c("\\:\\)" , "\\(\\:", "\\:-\\)", "\\(-\\:", "\\:D", "\\:-D", "=\\)", "\\(=", "☺", "☻",
                      #"<ed><a0><bd><ed><b8><82>", #Face with tears of joy #1 on emojitracker! Removed because it's noisy
                      "<e2><98><ba><ef><b8><8f>", #Smiling face
                      "<e2><9d><a4>", #Heavy black heart
                      "<e2><99><a5>", #Black hearts suit
                      "<ed><a0><bd><ed><b8><8d>", #Smiling face with heart-shaped eyes
                      "<ed><a0><bd><ed><b8><8a>", #Smiling face with smiling eyes
                      "<ed><a0><bd><ed><b1><8e>", #THUMBS DOWN SIGN
                      "<ed><a0><bd><ed><b8><98>", #FACE THROWING A KISS
                      "<ed><a0><bd><ed><b2><95>", #TWO HEARTS
                      "<ed><a0><bd><ed><b1><8d>", #THUMBS UP SIGN
                      "<ed><a0><bd><ed><b8><86>"  #SMILING FACE WITH OPEN MOUTH AND TIGHTLY-CLOSED EYES
  )
  
                    
  # grep everything all at once
  sad_indices = grep(paste(sad_emoticons, collapse = "|"), tweet_data_frame$text, value = FALSE)
  happy_indices = grep(paste(happy_emoticons, collapse = "|"),tweet_data_frame$text, value = FALSE)
  conflicts = intersect(happy_indices, sad_indices)

  sad_indices = setdiff(sad_indices, conflicts)
  happy_indices = setdiff(happy_indices, conflicts)  
  
  
  # Mark sad tweets 0, happy tweets 1
  tweet_data_frame$polarity[sad_indices] = 0
  tweet_data_frame$polarity[happy_indices] = 1
  
  return(tweet_data_frame)
}



#############################################################
### Clean Tweets
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
    documents = gsub("<.*>", "", documents) #remove unicode stuff in angle brackets
    documents = gsub("[[:punct:]]", "", documents) #remove punctuation
    #documents = gsub("[[:digit:]]", "", documents) #remove numbers; turned off
    #documents = gsub("[^a-zA-Z]", " ", documents) #remove everything that isn't a letter; turned off
    documents = tolower(documents) #set lower case
    documents<-gsub('([[:alpha:]])\\1+', '\\1\\1', documents) # limit character repeats to maximum 2

    documents<-trimws(documents) #remove leading and trailing whitespace
  }, .progress = "text")
  return(cleantext)
}



#############################################################
### Stem Tweets
#############################################################

stem.tweets = function(documents){
  require(plyr)
  require(dplyr)
  require(qdapRegex)
  require(tm)
  require(SnowballC)
  
  cleantext = laply(documents, function(documents)
  {
    documents = paste(stemDocument(unlist(strsplit(documents, split = " "))), collapse = " ") 
    #Word stemming from tm package; this is moved to after AFINN scores.
    
  }, .progress = "text")
  return(cleantext)
}



#############################################################
### Look up word in tweets
#############################################################

word_lookup = function(documents, word){ #look up a word in list of documents. Return indices where that word occurs
  require(plyr)
  require(dplyr)
  tf_vector = laply(documents, function(documents)
  {
    documents = word %in% unlist(strsplit(documents, split = ' ')) 
  }, .progress = "text")
  return(tf_vector)
}


#############################################################
### Import tweets from .json files
#############################################################

lexicon_sentiment_score = function(documents, lexicon = AFINN_lexicon){
  require(plyr)
  require(dplyr)
  negations = c("no", "not","none","nobody","nothing","neither","never","doesnt",
                "isnt","wasnt","shouldnt","wouldnt", "couldnt","wont","cant","dont")
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
### Import tweets from .json files
#############################################################

#raw_file_path = "~/Desktop/Huang Research/LAR_Data/raw_data/"
#feather_file_path = "~/Desktop/Huang Research/LAR_Data/feather_data/"
#csv_file_path = "~/Desktop/Huang Research/LAR_Data/csv_data/"
#max_tweets_per_file = 1000000  # make sure this number is significantly above the number of tweets per file, about 200k

import_tweets_from_json = function(months_to_import,
                                   base_file_path = "~/Desktop/Huang Research/LAR_Data/",
                                   raw_file_path = "raw_data/",
                                   feather_file_path = "feather_data/",
                                   csv_file_path = "csv_data/",
                                   max_tweets_per_file = 2000000,
                                   all_four_lexicons = FALSE,
                                   stem_tweets = TRUE){
  require(streamR)
  require(feather)
  
  keep_df = data.frame()
  setwd(base_file_path)
  for(month in months_to_import){
    j=1
    files_to_import = list.files(path = paste(raw_file_path, month, sep = ""))
    print(paste("These are the files to be imported from ", month,':', sep = ""))
    print(files_to_import)
    
    for(i in files_to_import){
      print(paste("Reading ", raw_file_path, month, i, "...", sep = ""))
      
      #Import one .json file into R as data frame
      temp_df = parseTweets(tweets = paste(raw_file_path, month, i, sep = ""))
      
      #Remove unnecessary raw columns
      temp_df = remove_raw_columns(temp_df)
      
      #Remove very spanish sounding tweets
      temp_df = remove_spanish_tweets(temp_df)
      
      #Remove columns without lat or lon
        temp_df = temp_df[!is.na(temp_df$lat),]
        temp_df = temp_df[!is.na(temp_df$lon),]
      
      #Remove columns from outside proper lat or lon
        temp_df = temp_df[temp_df$lat >= 33,]
        temp_df = temp_df[temp_df$lat <= 35,]
        temp_df = temp_df[temp_df$lon >= -119,]
        temp_df = temp_df[temp_df$lon <= -117,]
      
      #Identify happy and sad emoticons in tweets before cleaning.
      #Happy tweets have polarity marked as TRUE; Sad tweets have polarity marked as FALSE
        print(paste("Identifying happy and sad tweets in ", i, "...", sep = ""))
        temp_df = identify_emoticons(temp_df)
      
      #Clean, lowercase, remove url's and tokenize text
        print(paste("Cleaning tweets from ", i, "...", sep = ""))
        temp_df$text = clean.tweets(documents = temp_df$text, 
                                    happyToken = " happytoken ", 
                                    sadToken = " sadtoken ")
      
      #Remove rows with no text
        temp_df = temp_df[temp_df$text!="",]
      
      #Find AFINN score for new tweets
        print(paste("Finding AFINN scores for ", i, "...", sep = ""))
        temp_df$afinn_score = lexicon_sentiment_score(temp_df$text)
      
      #Find other scores for tweets:
        if(all_four_lexicons == TRUE){
          print(paste("Finding OpinionFinder_lexicon scores for ", i, "...", sep = ""))
          temp_df$opinionfinder_score = lexicon_sentiment_score(temp_df$text, lexicon = OpinionFinder_lexicon)
          print(paste("Finding NRC_lexicon scores for ", i, "...", sep = ""))
          temp_df$nrc_score = lexicon_sentiment_score(temp_df$text, lexicon = NRC_lexicon)
          print(paste("Finding ANEW_lexicon scores for ", i, "...", sep = ""))
          temp_df$anew_score = lexicon_sentiment_score(temp_df$text, lexicon = ANEW_lexicon)
        }

      
      #word stemming
      if(stem_tweets == TRUE){
        print(paste("Stemming tweets from ", i, "...", sep = ""))
        temp_df$text = stem.tweets(temp_df$text)
      }
      
      #Combine temp_df and keep_df
      keep_df = rbind(keep_df, temp_df)
      
      #If keep_df is too big, then write max_tweets_per_file tweets from keep_df to file
      if(nrow(keep_df) > max_tweets_per_file){
        
        #Rewrite data frames
        write_df = keep_df[1:max_tweets_per_file,]
        keep_df = keep_df[(max_tweets_per_file+1):nrow(keep_df),]

        # Now write max_tweets_per_file # of tweets to file from write_df and let keep_df keep collecting batches of tweets
        print(paste("Writing", nrow(write_df), "Tweets to file", sep = " "))
        write_feather(x = write_df, path = paste(feather_file_path, month, j, ".feather", sep=""))
        write.csv(x = write_df, file = paste(csv_file_path, month, j, ".feather", sep=""), row.names = FALSE)
        write_df = NULL
        j = j+1
      }
      
      #If in last , then write max_tweets_per_file tweets from keep_df to file
      if(i==files_to_import[length(files_to_import)]){
        print(paste("Writing final tweets from ", month, " to file...", sep = ""))
        write_df = keep_df
        keep_df = data.frame()
        
        # Write tweets to file
        print(paste("Writing", nrow(write_df), "Tweets to file", sep = " "))
        write_feather(x = write_df, path = paste(feather_file_path, month, j, ".feather", sep=""))
        write.csv(x = write_df, file = paste(csv_file_path, month, j, ".csv", sep=""), row.names = FALSE)
        write_df = NULL
        j = 1
      }
    }
  }
}



#############################################################
### Build emoticon semi-supervised data set
#############################################################

extract_emoticon_tweets = function(tweet_data_frame){
  happy_tweets = subset(tweet_data_frame, polarity == TRUE)
  sad_tweets = subset(tweet_data_frame, polarity == FALSE)
  
  #Undersample to create emoticon data frame
  sample_size = min(nrow(happy_tweets),nrow(sad_tweets))
  happy_indices = sample(nrow(happy_tweets), sample_size)
  sad_indices = sample(nrow(sad_tweets), sample_size)
  print(paste("Creating emoticon_tweets from", nrow(happy_indices), 
              "happy tweets and", nrow(sad_indices), "sad tweets..."))
  
  emoticon_tweets = rbind(happy_tweets[happy_indices,],sad_tweets[sad_indices,])
  return(emoticon_tweets)
}



#############################################################
### Find word frequency in a list of documents
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
### Build ndsi_lexicon words from emoticon
#############################################################

make_ndsi_lexicon = function(emoticon_tweets, 
                                     word_sparsity = 0.9999, 
                                     smoothing_alpha = 2^7, 
                                     max_words = 500, 
                                     min_ndsi_score = 0.01){
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
  
  #NDSI - note the presence of smoothing_alpha term: Version 1
  ndsi_lexicon$ndsi = abs(ndsi_lexicon$diff)/
    (ndsi_lexicon$freq.happy + ndsi_lexicon$freq.sad + 2 * smoothing_alpha) 
  
  #NDSI - note the presence of smoothing_alpha term: Version 2
  #ndsi_lexicon$ndsi = abs(ndsi_lexicon$diff + ndsi_lexicon$freq.happy + ndsi_lexicon$freq.sad)/
  #  (ndsi_lexicon$freq.happy + ndsi_lexicon$freq.sad + 2 * smoothing_alpha) 
  
  #Optional: Remove hashtoken and usernametoken
  ndsi_lexicon = ndsi_lexicon[ndsi_lexicon$word != 'hashtoken',]
  ndsi_lexicon = ndsi_lexicon[ndsi_lexicon$word != 'usernametoken',]
  
  #Sorting by NDSI, then scale NDSI score to 1.
  ndsi_lexicon = ndsi_lexicon[order(-ndsi_lexicon$ndsi), ]
  ndsi_lexicon$ndsi = ndsi_lexicon$ndsi/max(ndsi_lexicon$ndsi)
  
  #Convert word to a string
  ndsi_lexicon$word = as.character(ndsi_lexicon$word)
  ndsi_lexicon = ndsi_lexicon[ndsi_lexicon$ndsi>0,] # restrict to words with a nonzero ndsi score.

  # Finally, trim ndsi_lexicon in case it's too long
  # Min ndsi score
  ndsi_lexicon = subset(ndsi_lexicon, ndsi>=min_ndsi_score)
  
  # Max words
  if (nrow(ndsi_lexicon)>max_words){
    ndsi_lexicon = ndsi_lexicon[1:max_words,]
  }
  
  return(ndsi_lexicon)
}



#############################################################
### Build a Bag of Words random forest classifier from emoticon_tweets
#############################################################

make_term_freq = function(emoticon_tweets, ndsi_lexicon){
  
  # Function for apply statement
  ndsi.frequencies=function(x){
    str_count(x,ndsi_lexicon$word[1:nrow(ndsi_lexicon)])
  }
  
  # Create emoticon term frequency matrix. This may take a long time!
  emoticon_term_freq <- t(apply(t(emoticon_tweets[,"text"]), 2,    #TAKES TIME; 10 minutes for 100 000 tweets and 1276 terms 
                                ndsi.frequencies))
  
  # Append target variable and afinn scores
  emoticon_term_freq = data.frame(polarity=emoticon_tweets$polarity, 
                                  afinn_score = emoticon_tweets$afinn_score, 
                                  count_hashtags = emoticon_tweets$count_hashtags,
                                  count_usernames = emoticon_tweets$count_usernames,
                                  count_url = emoticon_tweets$count_url,
                                  is_reply = emoticon_tweets$is_reply,
                                  emoticon_term_freq)
  return(emoticon_term_freq)
}



#############################################################
### Make and test a random forest classifier from emoticon_term_freq
#############################################################

make_rf_classifier = function(emoticon_term_freq, ndsi_lexicon, ntrain = 100, ntest = 100){
  require(ggplot2)
  require(randomForest)
  require(caret)
  require(pROC)
  
  train_indices = sample(nrow(emoticon_term_freq), min(ntrain, nrow(emoticon_term_freq)))
  test_indices = setdiff(1:nrow(emoticon_term_freq), train_indices)
  #test_indices = sample(nrow(emoticon_term_freq), min(ntest, nrow(emoticon_term_freq)))
  
  emoticon_term_freq$polarity = as.factor(emoticon_term_freq$polarity) 
  
  rf_model = randomForest(polarity~.,data = emoticon_term_freq[train_indices,])
  #rf_model = rf_model$model
  
  print("Optimizing cutoff value")
  
  #train accuracy
  train_phat = predict(rf_model, newdata = emoticon_term_freq[train_indices,], type = "prob")
  train_cutoff_info = optimize_rf_cutoff(score.vec = train_phat[,2], polarity.vec = emoticon_term_freq[train_indices,"polarity"], 
                                        min = 0.3, max = 0.7, step = 0.001)
  train_cutoff = train_cutoff_info$optimal_cutoff
  train_accuracy = train_cutoff_info$accuracy
  
  #train AUC
  train_auc = roc(emoticon_term_freq$polarity[train_indices],train_phat[,2])$auc

  #test accuracy
  test_phat = predict(rf_model, newdata = emoticon_term_freq[test_indices,], type = "prob")
  test_cutoff_info = optimize_rf_cutoff(score.vec = test_phat[,2], polarity.vec = emoticon_term_freq[test_indices,"polarity"], 
                                      min = 0.3, max = 0.7, step = 0.001)
  test_cutoff = test_cutoff_info$optimal_cutoff
  test_accuracy = test_cutoff_info$accuracy
  
  #test AUC
  test_auc = roc(emoticon_term_freq$polarity[test_indices],test_phat[,2])$auc
  
  #Sent140
  sent140 = get_sent140()
  sent140_term_freq = make_term_freq(sent140, ndsi_lexicon)
  sent140_phat = predict(rf_model, newdata = sent140_term_freq, type = "prob")
  sent140_cutoff_info = optimize_rf_cutoff(score.vec = sent140_phat[,2], polarity.vec = as.numeric(sent140$polarity)-1, 
                                      min = 0.3, max = 0.7, step = 0.001)
  
  # Accuracy and AUC
  sent140_cutoff = sent140_cutoff_info$optimal_cutoff
  sent140_accuracy = sent140_cutoff_info$accuracy
  sent140_auc = roc(sent140_term_freq$polarity,sent140_phat[,2])$auc
  
  return(list(
    "ntrain" = length(train_indices),
    "train_accuracy" = train_accuracy,
    "train_auc" = train_auc,
    "train_optimal_cutoff" = train_cutoff,
    "train_phat" = train_phat,
    
    "ntest" = length(test_indices),
    "test_accuracy" = test_accuracy,
    "test_auc" = test_auc, 
    "test_optimal_cutoff" = test_cutoff,
    "test_phat" = test_phat,
    
    "sent140_accuracy" = sent140_accuracy,
    "sent140_auc" = sent140_auc,
    "sent140_cutoff" = sent140_cutoff,
    "sent140_phat" = sent140_phat,
    
    "model" = rf_model))
}



#############################################################
### Optimize cutoff value for classifier scores
#############################################################
optimize_rf_cutoff = function(score.vec, polarity.vec, min = 0, max = 10, step = 1){
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
  accuracy = confusionMatrix(as.numeric(score.vec>=(optimal.cutoff)),polarity.vec)$overall[1]
  return(list(
    "optimal_cutoff" = optimal.cutoff,
    "accuracy" = accuracy))
}



#############################################################
###### Get sent140 testing data
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
  
  #Add columns for hashtags, usernames, and url
  dict <- c("#", "@", "https")
  counts = lapply(dict, str_count, string=sent140$raw_text)
  sent140$count_hashtags = counts[[1]]
  sent140$count_usernames = counts[[2]]
  sent140$count_url = counts[[3]]
  sent140$is_reply = 0  # This would be so much better if we could look up these tweets and see if they are retweets. 
                        # But they're most likely not responses?
  
  #clean text
  sent140 = sent140[sent140$polarity !=2, c("polarity", "raw_text", "count_hashtags", "count_usernames", "count_url", "is_reply")]
  sent140$polarity = as.factor(sent140$polarity)
  sent140$text = clean.tweets(sent140$raw_text)
  
  #lexicon scores
  sent140$afinn_score = lexicon_sentiment_score(sent140$text)
  sent140$nrc_score = lexicon_sentiment_score(sent140$text, lexicon = NRC_lexicon)
  sent140$anew_score = lexicon_sentiment_score(sent140$text, lexicon = ANEW_lexicon)
  sent140$opinionfinder_score = lexicon_sentiment_score(sent140$text, lexicon = OpinionFinder_lexicon)
  
  
  #word stemming
  sent140$text = stem.tweets(sent140$text)
  
  #stem words
  sent140$text = stem.tweets(sent140$text)
  
  return(sent140)
}



#############################################################
### Build classifier from feather file 
#############################################################

build_classifier_from_tweets = function(tweet_df, ntrain = 10000, ntest = 5000, ...){
  require(feather)
  require(tools)
  
  # Extract emoticon tweets from tweet_df
  emoticon_df = extract_emoticon_tweets(tweet_df)
  print(paste("Finished extracting", nrow(emoticon_df),"emoticon tweets and building NDSI lexicon using tweets from tweet_df..."))

  # Make ndsi lexicon
  ndsi_lexicon_df = make_ndsi_lexicon(emoticon_df, ...)
  
  # Make term-frequency data frame
  emoticon_term_freq = make_term_freq(emoticon_df, ndsi_lexicon_df, ...)
  print(paste("Finished making term-frequency data frame from tweet_df with", ncol(emoticon_term_freq)-5, "NDSI words..."))
  
  # Build Random Forest Classifier
  print("Building random forest classifier...")
  result = make_rf_classifier(emoticon_term_freq, ndsi_lexicon_df, ntrain = ntrain, ntest = ntest)
  return(result)
}



#############################################################
### Import lexicons
#############################################################

#lexicons MUST be formatted so that the first column lists words and the second column gives the sentiment score of that word.

# AFINN lexicon
    AFINN_lexicon = read.delim(file = "Lexicons/AFINN/AFINN-111.txt", stringsAsFactors = FALSE, header = F, quote = '')
    names(AFINN_lexicon) <- c('word','score')
    #AFINN_lexicon = rbind(AFINN_lexicon, c("happytoken", 5), c("sadtoken", -5))
    AFINN_lexicon$word.clean <- gsub('-',' ' , AFINN_lexicon$word)  #Replacing Hyphens with Spaces
    AFINN_lexicon$word.clean <- gsub("[[:punct:]]", '', AFINN_lexicon$word.clean)  #Removing punctuation
  
#ANEW_lexicon
    ANEW_lexicon = read.csv(file = "Lexicons/ANEW.csv", header = FALSE)
    colnames(ANEW_lexicon) = c("word", "score")
    ANEW_lexicon$score = ANEW_lexicon$score-(mean(ANEW_lexicon$score)+1)
    #ANEW_lexicon$score = ANEW_lexicon$score - 6 # This may work better?


#NRC_lexicon Word-Emotion Association Lexicon (formerly known as EmoLex)
    NRC_lexicon = read.csv(file = "Lexicons/EmoLex/NRC-emotion-lexicon-wordlevel-alphabetized-v0.92.txt",
                   sep = "\t", header = FALSE)
    colnames(NRC_lexicon) = c("word", "emotion", "indicator")
    NRC_lexicon = NRC_lexicon[NRC_lexicon$emotion == "negative"|NRC_lexicon$emotion == "positive",]
    NRC_lexicon = NRC_lexicon[NRC_lexicon$indicator == 1,]
    NRC_lexicon[NRC_lexicon$emotion == "negative",]$indicator = -1
    NRC_lexicon = NRC_lexicon[c("word", "indicator")]
    colnames(NRC_lexicon) = c("word", "score")

#OpinionFinder_lexicon (Formerly known as Wiebe)
    OpinionFinder_lexicon = read.csv(file = "Lexicons/opinionFinder.csv", header = TRUE, stringsAsFactors = FALSE)
    OpinionFinder_lexicon = as.data.frame(cbind(as.character(OpinionFinder_lexicon$V1), 
                                                as.integer(2*(OpinionFinder_lexicon$V3 == "positive")-1)))
    colnames(OpinionFinder_lexicon) = c("word", "score")
    OpinionFinder_lexicon$score = as.integer(OpinionFinder_lexicon$score)
    OpinionFinder_lexicon$score = (((OpinionFinder_lexicon$score-1)*2)-1)*-1
