# Ebert
# Compare Models


# Setup ----
# Make balanced set of 40000 non-stemmed emoji tweets
# emoji_df_no_stem = read_feather(path = "~/Desktop/Huang Research/LAR_Data/compare_lexicons/feather_data/2016-08/emoji_df_no_stem.feather")
# indices = as.integer(c(sample(1:nrow(emoji_df_no_stem)/2,20000), sample(((nrow(emoji_df_no_stem)+2)/2):nrow(emoji_df_no_stem),20000)))
# emoji_40k = emoji_df_no_stem[indices,]
# 
# write_feather(emoji_40k, path = "~/Desktop/Documents/GitRepos/LAR/compare_models/emoji_40k.feather")
# read_feather(path = "~/Desktop/Documents/GitRepos/LAR/compare_models/emoji_40k.feather")

# ndsi_lexicon
# ndsi_lexicon_df = make_ndsi_lexicon(emoji_40k, max_words = 1000, smoothing_alpha = 2^12)
# write_feather(ndsi_lexicon_df, path = "~/Desktop/Documents/GitRepos/LAR/compare_models/ndsi_lexicon_df.feather")
# read_feather(path = "~/Desktop/Documents/GitRepos/LAR/compare_models/ndsi_lexicon_df.feather")
#
# emoji_term_freq = make_term_freq(emoji_40k, ndsi_lexicon_df[1:500,])
# write_feather(emoji_term_freq, path = "~/Desktop/Documents/GitRepos/LAR/compare_models/emoji_term_freq.feather")
# read_feather(path = "~/Desktop/Documents/GitRepos/LAR/compare_models/emoji_term_freq.feather")


# training and test data
# set.seed(1234)
# indices = sample(1:40000,0.70*40000)
# traindata = emoji_40k[indices,]
# testdata = emoji_40k[-indices,]
# 
# save(indices, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/indices.RData")
# write_feather(traindata, path = "~/Desktop/Documents/GitRepos/LAR/compare_models/traindata.feather")
# write_feather(testdata, path = "~/Desktop/Documents/GitRepos/LAR/compare_models/testdata.feather")
# load(file = "~/Desktop/Documents/GitRepos/LAR/compare_models/indices.RData")
# read_feather(path = "~/Desktop/Documents/GitRepos/LAR/compare_models/traindata.feather")
# read_feather(path = "~/Desktop/Documents/GitRepos/LAR/compare_models/testdata.feather")



# Import data ----
  library(feather)
  library(ROCR)
  library(pROC)
  library(randomForest)
  
  emoji_40k = read_feather(path = "~/Desktop/Documents/GitRepos/LAR/compare_models/emoji_40k.feather")
  ndsi_lexicon_df = read_feather(path = "~/Desktop/Documents/GitRepos/LAR/compare_models/ndsi_lexicon_df.feather")
  load(file = "~/Desktop/Documents/GitRepos/LAR/compare_models/indices.RData")
  traindata = read_feather(path = "~/Desktop/Documents/GitRepos/LAR/compare_models/traindata.feather")
  testdata = read_feather(path = "~/Desktop/Documents/GitRepos/LAR/compare_models/testdata.feather")
  emoji_term_freq = read_feather(path = "~/Desktop/Documents/GitRepos/LAR/compare_models/emoji_term_freq.feather")
  
  ntest = 28000
  # set number of training tweets. 28000 is maximum value

# MODEL 1: AFINN ----
  auc1 = roc(emoji_40k$polarity, emoji_40k$afinn_score)

  pred = prediction(emoji_40k$afinn_score,emoji_40k$polarity)
  acc_list <- performance(pred,"acc")
  max_acc1 = max(unlist(acc_list@y.values))


# MODEL 2: OpinionFinder ----
  auc2 = roc(emoji_40k$polarity, emoji_40k$opinionfinder_score)
  
  pred = prediction(emoji_40k$opinionfinder_score,emoji_40k$polarity)
  acc_list <- performance(pred,"acc")
  max_acc2 = max(unlist(acc_list@y.values))


# MODEL 3: NRC ----
  auc3 = roc(emoji_40k$polarity, emoji_40k$nrc_score)
  
  pred = prediction(emoji_40k$nrc_score,emoji_40k$polarity)
  acc_list <- performance(pred,"acc")
  max_acc3 = max(unlist(acc_list@y.values))


# MODEL 4: ANEW ----
  auc4 = roc(emoji_40k$polarity, emoji_40k$anew_score)
  
  pred = prediction(emoji_40k$anew_score,emoji_40k$polarity)
  acc_list <- performance(pred,"acc")
  max_acc4 = max(unlist(acc_list@y.values))



# MODEL 5: NDSI ----
  rf_model5 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1,7:ncol(emoji_term_freq))])
  test_phat5 = predict(rf_model5, newdata = emoji_term_freq[-indices,c(1,7:ncol(emoji_term_freq))], type = "prob")
  auc5 = roc(emoji_term_freq$polarity[-indices],test_phat5[,2])
  
  pred5 = prediction(test_phat5[,2], emoji_40k$polarity[-indices])
  acc_list5 = performance(pred5,"acc")
  max_acc5 = max(unlist(acc_list5@y.values))


# Model 6: Tweet Features ----
  rf_model6 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1,3:6)])
  test_phat6 = predict(rf_model6, newdata = emoji_term_freq[-indices,c(1,3:6)], type = "prob")
  auc6 = roc(emoji_term_freq$polarity[-indices],test_phat6[,2])
  
  pred6 = prediction(test_phat6[,2], emoji_40k$polarity[-indices])
  acc_list6 = performance(pred6,"acc")
  max_acc6 = max(unlist(acc_list6@y.values))


# MODEL 7: AFINN + tweet features ----
  rf_model7 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1:6)])
  test_phat7 = predict(rf_model7, newdata = emoji_term_freq[-indices,c(1:6)], type = "prob")
  auc7 = roc(emoji_term_freq$polarity[-indices],test_phat7[,2])
  
  pred7 = prediction(test_phat7[,2], emoji_40k$polarity[-indices])
  acc_list7 = performance(pred7,"acc")
  max_acc7 = max(unlist(acc_list7@y.values))


# MODEL 8: AFINN + NDSI ----
  rf_model8 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1:2,7:ncol(emoji_term_freq))])
  test_phat8 = predict(rf_model8, newdata = emoji_term_freq[-indices,c(1:2,7:ncol(emoji_term_freq))], type = "prob")
  auc8 = roc(emoji_term_freq$polarity[-indices],test_phat8[,2])
  
  pred8 = prediction(test_phat8[,2], emoji_40k$polarity[-indices])
  acc_list8 = performance(pred8,"acc")
  max_acc8 = max(unlist(acc_list8@y.values))


# MODEL 9: tweet features + NDSI ----
  rf_model9 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1,3:ncol(emoji_term_freq))])
  test_phat9 = predict(rf_model9, newdata = emoji_term_freq[-indices,c(1,3:ncol(emoji_term_freq))], type = "prob")
  auc9 = roc(emoji_term_freq$polarity[-indices],test_phat9[,2])
  
  pred9 = prediction(test_phat9[,2], emoji_40k$polarity[-indices])
  acc_list9 = performance(pred9,"acc")
  max_acc9 = max(unlist(acc_list9@y.values))


# MODEL 10: AFINN + tweet features + NDSI ----
  rf_model10 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),])
  test_phat10 = predict(rf_model10, newdata = emoji_term_freq[-indices,], type = "prob")
  auc10 = roc(emoji_term_freq$polarity[-indices],test_phat10[,2])
  
  pred10 = prediction(test_phat10[,2], emoji_40k$polarity[-indices])
  acc_list10 = performance(pred10,"acc")
  max_acc10 = max(unlist(acc_list10@y.values))


# MODEL 11: RTextTools ----



# PRINT RESULTS ----
  as.numeric(auc1$auc); max_acc1
  as.numeric(auc2$auc); max_acc2
  as.numeric(auc3$auc); max_acc3
  as.numeric(auc4$auc); max_acc4
  as.numeric(auc5$auc); max_acc5
  as.numeric(auc6$auc); max_acc6
  as.numeric(auc7$auc); max_acc7
  as.numeric(auc8$auc); max_acc8
  as.numeric(auc9$auc); max_acc9
  as.numeric(auc10$auc); max_acc10
  #as.numeric(auc11$auc); max_acc11
  #as.numeric(auc12$auc); max_acc12
  

  
# RESULTS ----


  # MODEL   AUC       MAX_ACCURACY    MODEL
  # 1       0.690     64.2            AFINN lexicon
  # 2       0.648     61.2            OpinionFinder
  # 3       0.646     61.1            NRC
  # 4       0.696     64.0            ANEW
  # 5       0.830     75.5            NDSI
  # 6       0.678     64.7            Tweet Features
  # 7       0.737     68.3            AFINN + tweet features
  # 8       0.837     76.1            AFINN + NDSI
  # 9       0.837     76.6            tweet features + NDSI
  # 10      0.842     77.3            AFINN + tweet features + NDSI
  # 11      
  # 12      

