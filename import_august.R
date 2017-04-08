# David Ebert
# September 13, 2016

source("functions.R")


#############################################################
###### Import one raw data frame
#############################################################

#library(streamR)
#temp_df = parseTweets(tweets = "~/Desktop/Huang Research/LAR_Data/raw_data/2016-09/2016-09-06.json")
#temp_df = temp_df[1:1000,]


#############################################################
###### Import August 2016 tweets. Then analyze them.
#############################################################

# use import_tweets_from_json() from other functions.R to read .json files into cleaned .feather files
a = Sys.time()
# This takes a long time!
import_tweets_from_json(months_to_import = "2016-08/", 
                        base_file_path = "~/Desktop/Huang Research/LAR_Data")
Sys.time()-a # takes about 30 minutes for a week's worth of tweets


# Read (currently one) cleaned feather file back into R
library(feather)
#tweet_df = read_feather(path = "~/Desktop/Huang Research/LAR_Data/feather_data/2016-08/1.feather")
# This runs fast... sometimes...




################################
#### Make august data frame with 5.6 million tweets; write all august tweets to feather file
################################
a = Sys.time()
load(file = "~/Desktop/Huang Research/LAR_Data/feather_data/2016-08/1.RData")
all_august_tweets = write_df
rm(write_df)
load(file = "~/Desktop/Huang Research/LAR_Data/feather_data/2016-08/2.RData")
all_august_tweets = rbind(all_august_tweets, write_df)
rm(write_df)
load(file = "~/Desktop/Huang Research/LAR_Data/feather_data/2016-08/3.RData")
all_august_tweets = rbind(all_august_tweets, write_df)
rm(write_df)
dim(all_august_tweets)

#save(all_august_tweets, file = "~/Desktop/Huang Research/LAR_Data/feather_data/2016-08/all_august_tweets.RData")
load(file = "~/Desktop/Huang Research/LAR_Data/feather_data/2016-08/all_august_tweets.RData")
#write_feather(all_august_tweets, path = "~/Desktop/Huang Research/LAR_Data/feather_data/2016-08/all_august_tweets.feather")
all_august_tweets = read_feather(path = "~/Desktop/Huang Research/LAR_Data/feather_data/2016-08/all_august_tweets.feather")
# 4651429 tweets from August
Sys.time()-a




################################
#### Build classifier from all_august_tweets
################################

#Extract emoticon tweets from all_august_tweets
emoticon_df = extract_emoticon_tweets(all_august_tweets)
print(paste("Finished extracting", nrow(emoticon_df),"emoticon tweets and building NDSI lexicon using tweets from tweet_df..."))
# 352078 emoticon tweets, evenly balanced
# sad     happy 
# 176039  214077 

# Remove all_august_tweets from memory
rm(all_august_tweets)

# Make emoticon_df using max 40000 rows of emoticon_df, then save to feather file
indices = as.integer(c(sample(1:nrow(emoticon_df)/2,20000), sample(((nrow(emoticon_df)+2)/2):nrow(emoticon_df),20000)))
emoticon_df = emoticon_df[indices,]
                                                                        # table(emoticon_df$polarity) # Check for balance

#write.csv(x = emoticon_df, file = "~/Desktop/Huang Research/LAR_Data/feather_data/2016-08/emoticon_df.csv", row.names = FALSE)
#emoticon_df2 = read.csv(file = "~/Desktop/Huang Research/LAR_Data/feather_data/2016-08/emoticon_df.csv") #NOT THE SAME!
#write_feather(emoticon_df, path = "~/Desktop/Huang Research/LAR_Data/feather_data/2016-08/emoticon_df.feather")
#emoticon_df = read_feather(path = "~/Desktop/Huang Research/LAR_Data/feather_data/2016-08/emoticon_df.feather") # WHY THIS ERROR??


# Make ndsi lexicon and save to feather 
a = Sys.time()
ndsi_lexicon_df = make_ndsi_lexicon(emoticon_df, max_words = 2000, smoothing_alpha = 2^12)
Sys.time()-a

#write.csv(x = ndsi_lexicon_df, file = "~/Desktop/Huang Research/LAR_Data/feather_data/2016-08/ndsi_lexicon_df.csv", row.names = FALSE)
#ndsi_lexicon_df2 = read.csv(file = "~/Desktop/Huang Research/LAR_Data/feather_data/2016-08/ndsi_lexicon_df.csv")
#write_feather(ndsi_lexicon_df, path = "~/Desktop/Huang Research/LAR_Data/feather_data/2016-08/ndsi_lexicon_df.feather")
#ndsi_lexicon_df = read_feather(path = "~/Desktop/Huang Research/LAR_Data/feather_data/2016-08/ndsi_lexicon_df.feather")


# Make term-frequency data frame 
emoticon_term_freq = make_term_freq(emoticon_df, ndsi_lexicon_df)

#write.csv(x = emoticon_term_freq, file = "~/Desktop/Huang Research/LAR_Data/feather_data/2016-08/emoticon_term_freq.csv", row.names = FALSE)
#emoticon_term_freq2 = write.csv(file = "~/Desktop/Huang Research/LAR_Data/feather_data/2016-08/emoticon_term_freq.csv") # NOT THE SAME!!!
#write_feather(emoticon_term_freq, path = "~/Desktop/Huang Research/LAR_Data/feather_data/2016-08/emoticon_term_freq.feather")
#emoticon_term_freq = read_feather(path = "~/Desktop/Huang Research/LAR_Data/feather_data/2016-08/emoticon_term_freq.feather")
#print(paste("Finished making term-frequency data frame from tweet_df with", ncol(emoticon_term_freq)-6, "NDSI words..."))


# Build Random Forest Classifier and write to file <- This takes some time...
a = Sys.time()
print("Building random forest classifier...")
rf_model = make_rf_classifier(emoticon_term_freq, ndsi_lexicon_df, ntrain = 28000)
# try to get ntrain to 40000*.07 = 28000. 
# Takes 5 mins with ntrain = 5000
# Takes 13 mins with ntrain = 10000
# Takes 35 mins with ntrain = 20000
# Takes 1.17 hours with ntrain = 28000
save(rf_model, file = "~/Desktop/Huang Research/LAR_Data/feather_data/2016-08/rf_model.RData")
# load(file = "~/Desktop/Huang Research/LAR_Data/feather_data/2016-08/rf_model.RData")
Sys.time()-a
beepr::beep(3)
rf_model$train_accuracy
rf_model$test_accuracy
rf_model$sent140_accuracy

################################
#### Apply rf_model to all_august_tweets
################################

# Get all_august_tweets from above

# then classify them!
all_august_tweets$pred_polarity = classify.polarity.machine(tweet_df = all_august_tweets, 
                                                            chunk.size = 10000, 
                                                            ndsi_lexicon = max_imbalance_lexicon, 
                                                            model = final_model_emoji)

write.csv(x = all_august_tweets, file = "~/Desktop/Huang Research/LAR_Data/all_august_pred.csv", row.names = FALSE)
write_feather(all_august_tweets, path = "~/Desktop/Huang Research/LAR_Data/all_august_pred.feather")
save(all_august_tweets, file = "~/Desktop/Huang Research/LAR_Data/all_august_pred.feather")
beepr::beep(3)


all_august_pred_import = read.csv(file = "~/Desktop/Huang Research/LAR_Data/all_august_pred.csv", row.names = FALSE)
all_august_pred_import = read_feather(path = "~/Desktop/Huang Research/LAR_Data/all_august_pred.feather")


# Write all_august_pred to 2 shortened files
library(feather)
august_pred_import_1 = all_august_pred_import[1:2000000,c("screen_name", "id_str", "lat", "lon", "afinn_score", "pred_polarity")]
august_pred_import_2 = all_august_pred_import[2000001:nrow(all_august_pred_import),c("screen_name", "id_str", "lat", "lon", "afinn_score", "pred_polarity")]
write.csv(x = august_pred_import_1, file = "~/Desktop/Huang Research/LAR_Data/august_pred_import_1.csv", row.names = FALSE)
write.csv(x = august_pred_import_1, file = "~/Desktop/Huang Research/LAR_Data/august_pred_import_2.csv", row.names = FALSE)



#a = Sys.time()
#tweet_df = make_tweet_df(1:4)
#model_result = build_classifier_from_tweets(tweet_df, ntrain = 100000, ntest = 1000)
#Sys.time()-a
#model_result

#optimize_rf_cutoff(score.vec = model_result$sent140_phat[,2], 
#                   polarity.vec = as.numeric(model_result$sent140_actual)-1, 
#                   min = 0.25, 
#                   max = 0.7, 
#                   step = .01)



#Check high and low afinn scores:
all_august_pred_import[all_august_pred_import$pred_polarity < 0.2, c("text", "afinn_score","pred_polarity", "polarity", "id_str")]
all_august_pred_import[all_august_pred_import$afinn_score < (-10), c("text", "afinn_score","pred_polarity", "polarity", "id_str")]



# Check rows with individual words.
all_august_pred_import[word_lookup(all_august_pred_import$text,'it'), c("text", "afinn_score","pred_polarity", "polarity", "id_str")]



# Check pred_polarity for tweets with known polarity
known_polarity_tweets = all_august_pred_import[!is.na(all_august_pred_import$polarity),]
mean(as.data.frame(known_polarity_tweets[known_polarity_tweets$polarity == 0, "pred_polarity"])[,1])
mean(as.data.frame(known_polarity_tweets[known_polarity_tweets$polarity == 1, "pred_polarity"])[,1])
# WHY DO SAD TWEETS HAVE HIGHER PREDICTED POLARITY???


# Check the average pred_polarity of each word in the lexicon over ALL tweets.
ndsi_lexicon_df$avg_polarity = NA
for(i in 1:nrow(ndsi_lexicon_df)){
  lookup_word = ndsi_lexicon_df$word[i]
  ndsi_lexicon_df$avg_polarity[i]=
    mean(as.data.frame(all_august_pred_import[word_lookup(all_august_pred_import$text,lookup_word),"pred_polarity"])[,1])
  print(i)
}