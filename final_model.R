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

# 28k Indices
load(file = "~/Desktop/Documents/GitRepos/LAR/compare_models/indices.RData")

# Lexicons and term-frequency matrices
# load(file = "~/Desktop/Documents/GitRepos/LAR/compare_models/max_imbalance_lexicon.RData")
# load(file = "~/Desktop/Documents/GitRepos/LAR/compare_models/unigram_lexicon.RData")
# emoji_term_freq_max_imbalance = make_term_freq(emoji_40k, max_imbalance_lexicon)
# emoji_term_freq_unigram = make_term_freq(emoji_40k, unigram_lexicon)
# save(emoji_term_freq_max_imbalance, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/emoji_term_freq_max_imbalance.RData")
# save(emoji_term_freq_unigram, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/emoji_term_freq_unigram.RData")
# beepr::beep(3)

load(file = "~/Desktop/Documents/GitRepos/LAR/compare_models/emoji_term_freq_max_imbalance.RData")
#load(file = "~/Desktop/Documents/GitRepos/LAR/compare_models/emoji_term_freq_unigram.RData")
load(file = "~/Desktop/Documents/GitRepos/LAR/compare_models/sent140_tf_max_imbalance.RData")





# Make sure 70% of data is used
ntest = 28000


load(file = "~/Desktop/Documents/GitRepos/LAR/optimize_alpha/final_model.RData")


build_final_model = function(term_freq_df, tweet_df = emoji_40k, ntweets = 28000,
                             include_sent140 = FALSE, afinn_score = FALSE, tweet_features = FALSE, num_words = 600){
  
  column_vec = c(1)
  
  if(afinn_score == TRUE){
    print("Including AFINN score")
    column_vec = append(column_vec,c(2))
  }
  
  if(tweet_features == TRUE){
    print("Including tweet features")
    column_vec = append(column_vec,c(3:6))
  }
  
  if(num_words > 0){
    print(paste("Including",num_words, "NDSI words"))
    column_vec = append(column_vec,c(7:(6+num_words)))
  }
  
  print(paste("Building model with", ntweets, "tweets ..."))
  final_model_emoji = randomForest(polarity~.,data = emoji_term_freq_max_imbalance[sample(indices,ntweets),column_vec])
  save(final_model_emoji, file = "~/Desktop/Documents/GitRepos/LAR/optimize_alpha/final_model_emoji.RData")
  
  test_phat = predict(final_model_emoji, newdata = emoji_term_freq_max_imbalance[-indices,column_vec], type = "prob")
  auc = roc(emoji_term_freq_max_imbalance$polarity[-indices],test_phat[,2])
  pred = prediction(test_phat[,2], emoji_term_freq_max_imbalance$polarity[-indices])
  acc_list = performance(pred,"acc")
  max_acc = max(unlist(acc_list@y.values))
  print(paste("       Emoji AUC:", as.numeric(auc$auc)))
  print(paste("  Emoji accuracy:", max_acc))
  emoji_auc = as.numeric(auc$auc)
  emoji_acc = max_acc
  print(" ")
  
  load(file = "~/Desktop/Documents/GitRepos/LAR/compare_models/sent140_indices.RData")
  load(file = "~/Desktop/Documents/GitRepos/LAR/compare_models/sent140_tf_max_imbalance.RData")
  
  final_model_sent140 = randomForest(polarity~.,data = sent140_tf_max_imbalance[sent140_indices,column_vec])
  save(final_model_sent140, file = "~/Desktop/Documents/GitRepos/LAR/optimize_alpha/final_model_sent140.RData")
  
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








# Final Comparison Models

# AFINN + tweet features + 600 NDSI words		& 0.xxx     & xx.x\%	  \\ \hline
# 600 NDSI words + tweet features				    & 0.xxx     & xx.x\%	  \\ \hline
# AFINN + 600 NDSI words 						        & 0.xxx     & xx.x\%	  \\ \hline
# 600 NDSI words								            & 0.xxx 	  & xx.x\%  	\\ \hline  
# AFINN + tweet features 	        					& 0.745     & 69.2\%	  \\ \hline   
# AFINN lexicon 					            			& 0.641     & 64.1\%   	\\ \hline
# Tweet features 					            			& 0.678     & 64.7\%	  \\ \hline



#1 Tweet Features
test1 = build_final_model(emoji_term_freq_max_imbalance, ntweets=28000, afinn_score = FALSE, tweet_features = TRUE, 
                  include_sent140 = TRUE, num_words = 0)


#2 AFINN Lexicon -- DONE
test2 = build_final_model(emoji_term_freq_max_imbalance, ntweets=28000, afinn_score = TRUE, tweet_features = FALSE,
                  include_sent140 = TRUE, num_words = 0)


#3 AFINN + tweet featuers
test3 = build_final_model(emoji_term_freq_max_imbalance, ntweets=28000, afinn_score = TRUE, tweet_features = TRUE, 
                  include_sent140 = TRUE, num_words = 0)


#4 600 NDSI words
test4 = build_final_model(emoji_term_freq_max_imbalance, ntweets=28000, afinn_score = FALSE, tweet_features = FALSE, 
                  include_sent140 = TRUE, num_words = 600)



#5 AFINN + 600 NDSI words
test5 = build_final_model(emoji_term_freq_max_imbalance, ntweets=28000, afinn_score = TRUE, tweet_features = FALSE, 
                  include_sent140 = TRUE, num_words = 600)


#6 600 NDSI words + tweet features	
test6 = build_final_model(emoji_term_freq_max_imbalance, ntweets=28000, afinn_score = FALSE, tweet_features = TRUE, 
                  include_sent140 = TRUE, num_words = 600)


#7 AFINN + tweet features + 600 NDSI words
test7 = build_final_model(emoji_term_freq_max_imbalance, ntweets=28000, afinn_score = TRUE, tweet_features = TRUE, 
                  include_sent140 = TRUE, num_words = 600)




