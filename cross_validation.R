# David Ebert
# 3 March 2017

# Perform 10-fold cross validation with the e-senti model
# Make sure the NDSI lexicon is made WITHOUT the test data
# Aim for 36,000 training, 4,000 test

source("functions.R")

################################
#### Import all_august_tweets
################################
# load(file = "~/Desktop/Huang Research/LAR_Data/feather_data/2016-08/all_august_tweets.RData")
all_august_tweets = read_feather(path = "~/Desktop/Huang Research/LAR_Data/feather_data/2016-08/all_august_tweets.feather")



################################
#### Make balanced subset of 40,000 emoji tweets, with 10 folds, then save to file
################################

emoji_df_num_rows = 40000  # <--- this should get up to 40000

### Extract emoticon tweets from all_august_tweets
emoji_df = extract_emoticon_tweets(all_august_tweets)
rm(all_august_tweets)

### Limit emoji_df to balanced 40000 rows, then save to file
set.seed(10)
indices = as.integer(c(sample(1:nrow(emoji_df)/2,emoji_df_num_rows/2), sample(((nrow(emoji_df)+2)/2):nrow(emoji_df),emoji_df_num_rows/2)))
emoji_df = emoji_df[indices,]


### Make folds
set.seed(11)
shuffle_indices = sample(1:nrow(emoji_df),nrow(emoji_df), replace = FALSE)
emoji_df$fold = ceiling(shuffle_indices*10/emoji_df_num_rows)
table(emoji_df$polarity, emoji_df$fold) #looks good


# Write to file
# write_feather(emoji_df, path = "~/Desktop/Huang Research/LAR_Data/feather_data/2016-08/emoji_df.feather")
# emoji_df = read_feather(path = "~/Desktop/Huang Research/LAR_Data/feather_data/2016-08/emoji_df.feather") # WHY THIS ERROR??
# save(emoji_df, file = "~/Desktop/Huang Research/LAR_Data/feather_data/2016-08/emoji_df.RData")
load(file = "~/Desktop/Huang Research/LAR_Data/feather_data/2016-08/emoji_df.RData")



################################
#### Build the classifier with each fold
################################

storage_directory = "~/Desktop/Huang Research/LAR_Data/feather_data/2016-08/"
train_accuracy = rep(NA, 10)
test_accuracy = rep(NA, 10)
train_auc = rep(NA, 10)
test_auc = rep(NA, 10)
time = rep(NA, 10)



for(i in 1:10){  #<-- this should eventually be 10, but for dummy tests less than that is fine!s
  #i=5
  a = Sys.time()
  print(paste("###  Beginning fold number", i, " ###"))
  
  ### Make ndsi_lexicon for training data.
  ndsi_lexicon_df = make_ndsi_lexicon(emoji_df[emoji_df$fold != i,], max_words = 800)
  print("Finished making NDSI lexicon.")
  
  write_feather(ndsi_lexicon_df, path = paste(storage_directory, "ndsi_lexicon_df",i,".feather", sep = ""))
  #ndsi_lexicon_df = read_feather(path = paste(storage_directory, "ndsi_lexicon_df",i,".feather", sep = ""))
  save(ndsi_lexicon_df, file = paste(storage_directory, "ndsi_lexicon_df",i,".RData", sep = ""))
  #load(file = paste(storage_directory, "ndsi_lexicon_df",i,".RData", sep = ""))
  
  
  ### Make term-frequency data frame
  emoji_term_freq = make_term_freq(emoji_df, ndsi_lexicon_df)
  print("Finished building emoji_term_freq")
  
  write_feather(emoji_term_freq, path = paste(storage_directory, "emoji_term_freq",i,".feather", sep = ""))
  #emoji_term_freq = read_feather(path = paste(storage_directory, "emoji_term_freq",i,".feather", sep = ""))
  save(emoji_term_freq, file = paste(storage_directory, "emoji_term_freq",i,".RData", sep = ""))
  #load(file = paste(storage_directory, "emoji_term_freq",i,".RData", sep = ""))
  
  
  ### Build Random Forest Classifier and write to file <- This takes some time!
  print("Building random forest classifier...")
  rf_model = make_rf_classifier(emoji_term_freq, ndsi_lexicon_df, ntrain = 36000)
  
  #save(rf_model, file = paste(storage_directory, "rf_model",i,".RData", sep = ""))
  load(file = paste(storage_directory, "rf_model",i,".RData", sep = ""))
  
  
  ### Record Information
  beepr::beep(3)
  train_accuracy[i] = rf_model$train_accuracy
  test_accuracy[i] = rf_model$test_accuracy
  train_auc[i] = rf_model$train_auc
  test_auc[i] = rf_model$test_auc
  time[i] = difftime(Sys.time(),a, units = "mins")
  
  ### Print information
  print(paste("Average train_accuracy:", mean(train_accuracy, na.rm = TRUE)))
  print(paste(" Average test_accuracy:", mean(test_accuracy, na.rm = TRUE)))
  print(paste("     Average train_auc:", mean(train_auc, na.rm = TRUE)))
  print(paste("      Average test_auc:", mean(test_auc, na.rm = TRUE)))
  print(paste("          Average time:", mean(time, na.rm = TRUE), "minutes"))
}


mean(train_accuracy, na.rm = TRUE)
mean(test_accuracy, na.rm = TRUE)
mean(train_auc, na.rm = TRUE)
mean(test_auc, na.rm = TRUE)
mean(time, na.rm = TRUE)



################################
#### Summary of model timing
################################

#  Number of emoji tweets: 20 000
#   Size of training data:    200 
# Average time in minutes:      1.87

#  Number of emoji tweets: 20 000
#   Size of training data:  1 000
# Average time in minutes:      2.4

#  Number of emoji tweets: 40 000       (3 folds)
#   Size of training data:  1 000
# Average time in minutes:      4.4
#   Average test accuracy:     71.1%
#        Average test AUC:      0.778

#  Number of emoji tweets: 40 000       (3 folds)
#   Size of training data: 10 000
# Average time in minutes:     24.8
#   Average test accuracy:     73.6%
#        Average test AUC:      0.8099

#  Number of emoji tweets: 40 000       (1 fold)
#   Size of training data: 20 000
# Average time in minutes:     59.1
#   Average test accuracy:     74.72%
#        Average test AUC:      0.823


############################################################
############################################################
#######              THE BIG GOAL: 10 Folds
#######    Number of emoji tweets: 40 000       
#######     Size of training data: 36 000
#######   Average time in minutes: ??????
#######     Average test accuracy: ??????
#######          Average test AUC: ??????
############################################################
############################################################



################################
#### Old timing information
################################

# try to get ntrain to 40000*.07 = 28000. 
# Takes 5     mins with ntrain = 5000
# Takes 13    mins with ntrain = 10000
# Takes 35    mins with ntrain = 20000
# Takes 1.17 hours with ntrain = 28000
# Takes ???? hours with ntrain = 36000
