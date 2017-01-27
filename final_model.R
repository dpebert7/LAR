# Ebert
# 12 January 2016


# Setup

source('~/Desktop/Documents/GitRepos/LAR/functions.R')
library(feather)
library(ROCR)
library(pROC)
library(randomForest)
library(doMC)
registerDoMC(2)

# Tweets
load(file = "~/Desktop/Documents/GitRepos/LAR/compare_models/emoji_40k.feather")
load(file = "~/Desktop/Documents/GitRepos/LAR/compare_models/sent140_term_freq.RData")

# Indices for 28k and sentiment140
load(file = "~/Desktop/Documents/GitRepos/LAR/compare_models/indices.RData")
load(file = "~/Desktop/Documents/GitRepos/LAR/compare_models/sent140_indices.RData")

# Lexicons and term-frequency matrices
# load(file = "~/Desktop/Documents/GitRepos/LAR/compare_models/max_imbalance_lexicon.RData")
# load(file = "~/Desktop/Documents/GitRepos/LAR/compare_models/unigram_lexicon.RData")
# emoji_term_freq_max_imbalance = make_term_freq(emoji_40k, max_imbalance_lexicon)
# emoji_term_freq_unigram = make_term_freq(emoji_40k, unigram_lexicon)
# save(emoji_term_freq_max_imbalance, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/emoji_term_freq_max_imbalance.RData")
# save(emoji_term_freq_unigram, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/emoji_term_freq_unigram.RData")
# beepr::beep(3)

# term_freq matrices built with the SAME ndsi lexicon!! (In this case we use )
load(file = "~/Desktop/Documents/GitRepos/LAR/compare_models/emoji_term_freq_max_imbalance.RData")
load(file = "~/Desktop/Documents/GitRepos/LAR/compare_models/sent140_tf_max_imbalance.RData")
#load(file = "~/Desktop/Documents/GitRepos/LAR/compare_models/emoji_term_freq_unigram.RData") #  <- old





load(file = "~/Desktop/Documents/GitRepos/LAR/optimize_alpha/final_model.RData")


build_final_model = function(term_freq_df, tweet_df = emoji_40k, ntweets = 28000,
                             afinn_score = FALSE, tweet_features = FALSE, num_words = 600){
  
  # SELECT APPROPRIATE COLUMNS FOR MODEL
  column_vec = c(1)
  if(afinn_score == TRUE){column_vec = append(column_vec,c(2))}         # Include afinn_score if necessary
  if(tweet_features == TRUE){column_vec = append(column_vec,c(3:6))}    # Include tweet features if necessary
  if(num_words > 0){column_vec = append(column_vec,c(7:(6+num_words)))} # Include NDSI words if necessary
  
  # EMOJI/EMOTICON DATA
  # print(paste("Building model for emoji/emoticon with", ntweets, "tweets ..."))
  # final_model_emoji = randomForest(polarity~.,data = emoji_term_freq_max_imbalance[sample(indices,ntweets),column_vec])
  # save(final_model_emoji, file = "~/Desktop/Documents/GitRepos/LAR/optimize_alpha/final_model_emoji.RData")
  # 
  # test_phat = predict(final_model_emoji, newdata = emoji_term_freq_max_imbalance[-indices,column_vec], type = "prob")
  # auc = roc(emoji_term_freq_max_imbalance$polarity[-indices],test_phat[,2])
  # pred = prediction(test_phat[,2], emoji_term_freq_max_imbalance$polarity[-indices])
  # acc_list = performance(pred,"acc")
  # max_acc = max(unlist(acc_list@y.values))
  # 
  # data_for_parker_emoji = cbind(emoji_term_freq_max_imbalance$polarity[-indices], test_phat[,2])
  # save(data_for_parker_emoji,  file = "~/Desktop/Documents/GitRepos/LAR/compare_models/data_for_parker_emoji.RData")
  # 
  # print(paste("       Emoji AUC:", as.numeric(auc$auc)))
  # print(paste("  Emoji accuracy:", max_acc))
  # emoji_auc = as.numeric(auc$auc)
  # emoji_acc = max_acc
  # print(" ")
  emoji_auc = NULL
  emoji_acc = NULL
  
  # SENTIMENT140
  print("Building model for sentiment140...")
  final_model_sent140 = randomForest(polarity~.,data = sent140_tf_max_imbalance[sent140_indices,column_vec])
  save(final_model_sent140, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/final_model_sent140.RData")
  
  test_phat = predict(final_model_sent140, newdata = sent140_tf_max_imbalance[-sent140_indices,column_vec], type = "prob")
  auc = roc(sent140_tf_max_imbalance$polarity[-sent140_indices],test_phat[,2])
  pred = prediction(test_phat[,2], sent140_tf_max_imbalance$polarity[-sent140_indices])
  acc_list = performance(pred,"acc")
  max_acc = max(unlist(acc_list@y.values))
  print(paste("     Sent140 AUC:", as.numeric(auc$auc)))
  print(paste("Sent140 accuracy:", max_acc))
  sent140_auc = as.numeric(auc$auc)
  sent140_acc = max_acc
  
  return(
    list(
    "emoji_auc" = emoji_auc,
    "emoji_acc" = emoji_acc,
    "sent140_auc" = sent140_auc,
    "sent140_acc" = sent140_acc))
}

for(i in 1:100){
  build_final_model(emoji_term_freq_max_imbalance, ntweets=28000, afinn_score = TRUE,
                    tweet_features = TRUE, num_words = 4648) #4648
  beepr::beep(3)
}


# AUC: 0.9296875
# ACC: 0.8611111
#goal: 86.3





# Final Comparison Models

# EMOJI TWEETS
# AFINN + tweet features + 600 NDSI words		& 0.814		& 74.6\%	\\ \hline
# 600 NDSI words + tweet features			      & 0.806		& 73.5\%	\\ \hline
# AFINN + 600 NDSI words 				            & 0.794		& 73.8\%	\\ \hline
# 600 NDSI words					                  & 0.786		& 72.4\%  \\ \hline  
# AFINN + tweet features 	        		      & 0.744		& 69.0\%	\\ \hline   
# AFINN lexicon 		              		  	  & 0.641		& 64.1\%  \\ \hline
# Tweet features 					                  & 0.675		& 64.7\%	\\ \hline

# SENTIMENT140 TWEETS
# AFINN + tweet features + all unigrams			& 0.926		& 85.2\%	\\ \hline %<- this is in a different beat_sent140.R
# AFINN + tweet features + 600 NDSI words		& 0.909 	& 82.4\%	\\ \hline
# 600 NDSI words + tweet features			      & 0.853 	& 78.7\%	\\ \hline
# AFINN + 600 NDSI words 			            	& 0.908		& 82.4\%	\\ \hline
# 600 NDSI words					                  & 0.861		& 78.7\%	\\ \hline  
# AFINN + tweet features 	               		& 0.889		& 78.7\%	\\ \hline   
# AFINN lexicon 					                  & 0.855		& 76.8\%  \\ \hline
# Tweet features 					                  & 0.574		& 55.6\%	\\ \hline



#1 Tweet Features -- DONE
test1 = build_final_model(emoji_term_freq_max_imbalance, ntweets=28000, afinn_score = FALSE, 
                          tweet_features = TRUE, num_words = 0)


#2 AFINN Lexicon -- DONE
test2 = build_final_model(emoji_term_freq_max_imbalance, ntweets=28000, afinn_score = TRUE, 
                          tweet_features = FALSE, num_words = 0)


#3 AFINN + tweet featuers
test3 = build_final_model(emoji_term_freq_max_imbalance, ntweets=28000, afinn_score = TRUE, 
                          tweet_features = TRUE, num_words = 0)


#4 600 NDSI words -- DONE
test4 = build_final_model(emoji_term_freq_max_imbalance, ntweets=28000, afinn_score = FALSE, 
                          tweet_features = FALSE, num_words = 600)



#5 AFINN + 600 NDSI words -- DONE
test5 = build_final_model(emoji_term_freq_max_imbalance, ntweets=28000, afinn_score = TRUE, 
                          tweet_features = FALSE, num_words = 600)



a = Sys.time()
#6 600 NDSI words + tweet features	
test6 = build_final_model(emoji_term_freq_max_imbalance, ntweets=28000, afinn_score = FALSE, 
                          tweet_features = TRUE, num_words = 600)
Sys.time()-a

a = Sys.time()
#7 AFINN + tweet features + 600 NDSI words -- NEED TO GET DATA TO PARKER!!
test7 = build_final_model(emoji_term_freq_max_imbalance, ntweets=28000, afinn_score = TRUE, 
                          tweet_features = TRUE, num_words = 600)

Sys.time()-a

load(file = "~/Desktop/Documents/GitRepos/LAR/compare_models/data_for_parker_emoji.RData")
dim(data_for_parker_emoji)
beepr::beep(3)









max_imbalance_lexicon
dim(max_imbalance_lexicon)
max_imbalance_lexicon[max_imbalance_lexicon$diff == 0,]
