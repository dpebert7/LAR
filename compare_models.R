# Ebert
# Compare Models
# All models are intended for the emoji data.
# To se models built for Sentiment140, see classify_sent140.R


# Setup ----
  # Make balanced set of 40000 non-stemmed emoji tweets
  # emoji_df_no_stem = read_feather(path = "~/Desktop/Huang Research/LAR_Data/compare_lexicons/feather_data/2016-08/emoji_df_no_stem.feather")
  # set.seed(123)
  # indices_40k = as.integer(c(sample(1:nrow(emoji_df_no_stem)/2,20000), sample(((nrow(emoji_df_no_stem)+2)/2):nrow(emoji_df_no_stem),20000)))
  # save(indices_40k, file = "~/Desktop/Huang Research/LAR_Data/compare_lexicons/feather_data/2016-08/indices_40k.RData")
  # load(file = "~/Desktop/Huang Research/LAR_Data/compare_lexicons/feather_data/2016-08/indices_40k.RData")
  # emoji_40k = emoji_df_no_stem[indices_40k,]
  # emoji_40k$polarity = as.factor(emoji_40k$polarity)
  
  # save(emoji_40k, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/emoji_40k.RData")
  # load(file = "~/Desktop/Documents/GitRepos/LAR/compare_models/emoji_40k.RData")
  
  # ndsi_lexicon
  # ndsi_lexicon_df = make_ndsi_lexicon(emoticon_df, max_words = 2000, smoothing_alpha = 2^16)
  # write_feather(ndsi_lexicon_df, path = "~/Desktop/Documents/GitRepos/LAR/compare_models/ndsi_lexicon_df.feather")
  # read_feather(path = "~/Desktop/Documents/GitRepos/LAR/compare_models/ndsi_lexicon_df.feather")
  
  # emoji_term_freq = make_term_freq(emoji_40k, ndsi_lexicon_df)
  # save(emoji_term_freq, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/emoji_term_freq.RData")
  # load(file = "~/Desktop/Documents/GitRepos/LAR/compare_models/emoji_term_freq.RData")

  # emoji_term_freq = make_term_freq(emoji_40k, ndsi_lexicon_df)
  # save(emoji_term_freq, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/emoji_term_freq.RData")
  # load(file = "~/Desktop/Documents/GitRepos/LAR/compare_models/emoji_term_freq.RData")
  
  
  # training and test data
  # set.seed(1234)
  # indices = sample(1:40000,0.70*40000)
  # indices = sort(indices)
  # save(indices, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/indices.RData")
  # load(file = "~/Desktop/Documents/GitRepos/LAR/compare_models/indices.RData")
  # 
  # set.seed(1234)
  # small_indices = sample(1:40000,0.1*40000)
  # small_indices = sort(small_indices)
  # save(small_indices, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/small_indices.RData")
  # load(file = "~/Desktop/Documents/GitRepos/LAR/compare_models/small_indices.RData")


# Import data ----
  source('~/Desktop/Documents/GitRepos/LAR/functions.R')
  library(feather)
  library(ROCR)
  library(pROC)
  library(randomForest)

  
  ndsi_lexicon_df = read_feather(path = "~/Desktop/Documents/GitRepos/LAR/compare_models/ndsi_lexicon_df.feather")
  
  load(file = "~/Desktop/Documents/GitRepos/LAR/compare_models/emoji_40k.feather")
  load(file = "~/Desktop/Huang Research/LAR_Data/compare_lexicons/feather_data/2016-08/indices_40k.RData")
  load(file = "~/Desktop/Documents/GitRepos/LAR/compare_models/emoji_term_freq.RData")
  
  load(file = "~/Desktop/Documents/GitRepos/LAR/compare_models/indices.RData")
  load(file = "~/Desktop/Documents/GitRepos/LAR/compare_models/small_indices.RData")
  
  ################################## = get_##################################()
  load(file = "~/Desktop/Documents/GitRepos/LAR/compare_models/##################################_term_freq.RData")
  
  ntest = 28000 # Should be set to 28000 for paper tests; 4000 for testing NDSI 
  # indices = small_indices # This is just in case ntest = 4000
  
  
  # set number of training tweets. 28000 is maximum value



# MODEL 1: AFINN ----
  auc1a = roc(emoji_40k$polarity, emoji_40k$afinn_score)
  pred1a = prediction(emoji_40k$afinn_score,emoji_40k$polarity)
  acc_list1a <- performance(pred1a,"acc")
  max_acc1a = max(unlist(acc_list1a@y.values))


# MODEL 2: OpinionFinder ----
  auc2a = roc(emoji_40k$polarity, emoji_40k$opinionfinder_score)
  pred2a = prediction(emoji_40k$opinionfinder_score,emoji_40k$polarity)
  acc_list2a <- performance(pred2a,"acc")
  max_acc2a = max(unlist(acc_list2a@y.values))


# MODEL 3: NRC ----
  auc3a = roc(emoji_40k$polarity, emoji_40k$nrc_score)
  pred3a = prediction(emoji_40k$nrc_score,emoji_40k$polarity)
  acc_list3a <- performance(pred3a,"acc")
  max_acc3a = max(unlist(acc_list3a@y.values))


# MODEL 4: ANEW ----
  auc4a = roc(emoji_40k$polarity, emoji_40k$anew_score)
  pred4a = prediction(emoji_40k$anew_score,emoji_40k$polarity)
  acc_list4a <- performance(pred4a,"acc")
  max_acc4a = max(unlist(acc_list4a@y.values))


# Model 5: AFINN + Random Forest Features ----
  rf_model5 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1,2)])
  save(rf_model5, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/rf_model6.RData")
  
  test_phat5a = predict(rf_model5, newdata = emoji_term_freq[-indices,c(1,2)], type = "prob")
  auc5a = roc(emoji_term_freq$polarity[-indices],test_phat5a[,2])
  pred5a = prediction(test_phat5a[,2], emoji_40k$polarity[-indices])
  acc_list5a = performance(pred5a,"acc")
  max_acc5a = max(unlist(acc_list5a@y.values))
  




  

# MODEL 11: NDSI 100 ----
  rf_model11 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1,7:106)])
  save(rf_model11, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/rf_model11.RData")
  
  test_phat11a = predict(rf_model11, newdata = emoji_term_freq[-indices,c(1,7:106)], type = "prob")
  auc11a = roc(emoji_term_freq$polarity[-indices],test_phat11a[,2])
  pred11a = prediction(test_phat11a[,2], emoji_40k$polarity[-indices])
  acc_list11a = performance(pred11a,"acc")
  max_acc11a = max(unlist(acc_list11a@y.values))
  

# MODEL 12: NDSI 250 ----
  rf_model12 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1,7:256)])
  save(rf_model12, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/rf_model12.RData")
  
  test_phat12a = predict(rf_model12, newdata = emoji_term_freq[-indices,c(1,7:256)], type = "prob")
  auc12a = roc(emoji_term_freq$polarity[-indices],test_phat12a[,2])
  pred12a = prediction(test_phat12a[,2], emoji_40k$polarity[-indices])
  acc_list12a = performance(pred12a,"acc")
  max_acc12a = max(unlist(acc_list12a@y.values))
  
  
# MODEL 13: NDSI 500 ----
  rf_model13 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1,7:506)])
  save(rf_model13, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/rf_model13.RData")
  
  test_phat13a = predict(rf_model13, newdata = emoji_term_freq[-indices,c(1,7:506)], type = "prob")
  auc13a = roc(emoji_term_freq$polarity[-indices],test_phat13a[,2])
  pred13a = prediction(test_phat13a[,2], emoji_40k$polarity[-indices])
  acc_list13a = performance(pred13a,"acc")
  max_acc13a = max(unlist(acc_list13a@y.values))
  

  
  
# MODEL 14: NDSI 1000 ----
  rf_model14 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1,7:1006)])
  save(rf_model14, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/rf_model14.RData")
  
  test_phat14a = predict(rf_model14, newdata = emoji_term_freq[-indices,c(1,7:1006)], type = "prob")
  auc14a = roc(emoji_term_freq$polarity[-indices],test_phat14a[,2])
  pred14a = prediction(test_phat14a[,2], emoji_40k$polarity[-indices])
  acc_list14a = performance(pred14a,"acc")
  max_acc14a = max(unlist(acc_list14a@y.values))
  

# MODEL 15: NDSI 1500 ----
  rf_model15 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1,7:1506)])
  save(rf_model15, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/rf_model15.RData")
  
  test_phat15a = predict(rf_model15, newdata = emoji_term_freq[-indices,c(1,7:1506)], type = "prob")
  auc15a = roc(emoji_term_freq$polarity[-indices],test_phat15a[,2])
  pred15a = prediction(test_phat15a[,2], emoji_40k$polarity[-indices])
  acc_list15a = performance(pred15a,"acc")
  max_acc15a = max(unlist(acc_list15a@y.values))

  
# MODEL 16: NDSI 2000 ----
  rf_model16 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1,7:2006)])
  save(rf_model16, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/rf_model16.RData")
  
  test_phat16a = predict(rf_model16, newdata = emoji_term_freq[-indices,c(1,7:2006)], type = "prob")
  auc16a = roc(emoji_term_freq$polarity[-indices],test_phat16a[,2])
  pred16a = prediction(test_phat16a[,2], emoji_40k$polarity[-indices])
  acc_list16a = performance(pred16a,"acc")
  max_acc16a = max(unlist(acc_list16a@y.values))
  




# Model 6: Tweet Features ----
  rf_model6 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1,3:6)])
  save(rf_model6, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/rf_model6.RData")
  
  test_phat6a = predict(rf_model6, newdata = emoji_term_freq[-indices,c(1,3:6)], type = "prob")
  auc6a = roc(emoji_term_freq$polarity[-indices],test_phat6a[,2])
  pred6a = prediction(test_phat6a[,2], emoji_40k$polarity[-indices])
  acc_list6a = performance(pred6a,"acc")
  max_acc6a = max(unlist(acc_list6a@y.values))
  
  
# MODEL 7: AFINN + tweet features ----
  rf_model7 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1:6)])
  save(rf_model7, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/rf_model7.RData")
  
  test_phat7a = predict(rf_model7, newdata = emoji_term_freq[-indices,c(1:6)], type = "prob")
  auc7a = roc(emoji_term_freq$polarity[-indices],test_phat7a[,2])
  pred7a = prediction(test_phat7a[,2], emoji_40k$polarity[-indices])
  acc_list7a = performance(pred7a,"acc")
  max_acc7a = max(unlist(acc_list7a@y.values))
  
  
# MODEL 8: AFINN + 500 NDSI ----
  rf_model8 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1:2,7:506)])
  save(rf_model8, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/rf_model8.RData")
  
  test_phat8a = predict(rf_model8, newdata = emoji_term_freq[-indices,c(1:2,7:506)], type = "prob")
  auc8a = roc(emoji_term_freq$polarity[-indices],test_phat8a[,2])
  pred8a = prediction(test_phat8a[,2], emoji_40k$polarity[-indices])
  acc_list8a = performance(pred8a,"acc")
  max_acc8a = max(unlist(acc_list8a@y.values))
  

# MODEL 9: tweet features + 500 NDSI ----
  rf_model9 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1,3:506)])
  save(rf_model9, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/rf_model9.RData")
  
  test_phat9a = predict(rf_model9, newdata = emoji_term_freq[-indices,c(1,3:506)], type = "prob")
  auc9a = roc(emoji_term_freq$polarity[-indices],test_phat9a[,2])
  pred9a = prediction(test_phat9a[,2], emoji_40k$polarity[-indices])
  acc_list9a = performance(pred9a,"acc")
  max_acc9a = max(unlist(acc_list9a@y.values))
  
  
# MODEL 10: AFINN + tweet features + 500 NDSI ----
  rf_model10 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1:506)])
  save(rf_model10, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/rf_model10.RData")
  
  test_phat10a = predict(rf_model10, newdata = emoji_term_freq[-indices,c(1:506)], type = "prob")
  auc10a = roc(emoji_term_freq$polarity[-indices],test_phat10a[,2])
  pred10a = prediction(test_phat10a[,2], emoji_40k$polarity[-indices])
  acc_list10a = performance(pred10a,"acc")
  max_acc10a = max(unlist(acc_list10a@y.values))
  
  
# MODEL 10.1: AFINN + tweet features + 1000 NDSI ----
  rf_model10.1 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1:1006)])
  save(rf_model10.1, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/rf_model10.1.RData")
  
  test_phat10.1a = predict(rf_model10.1, newdata = emoji_term_freq[-indices,c(1:1006)], type = "prob")
  auc10.1a = roc(emoji_term_freq$polarity[-indices],test_phat10.1a[,2])
  pred10.1a = prediction(test_phat10.1a[,2], emoji_40k$polarity[-indices])
  acc_list10.1a = performance(pred10.1a,"acc")
  max_acc10.1a = max(unlist(acc_list10.1a@y.values))

  
# MODEL 10.2: AFINN + tweet features + 1500 NDSI ----
  rf_model10.2 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1:1506)])
  save(rf_model10.2, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/rf_model10.2.RData")
  
  test_phat10.2a = predict(rf_model10.2, newdata = emoji_term_freq[-indices,c(1:1506)], type = "prob")
  auc10.2a = roc(emoji_term_freq$polarity[-indices],test_phat10.2a[,2])
  pred10.2a = prediction(test_phat10.2a[,2], emoji_40k$polarity[-indices])
  acc_list10.2a = performance(pred10.2a,"acc")
  max_acc10.2a = max(unlist(acc_list10.2a@y.values))

  
# MODEL 10.3: AFINN + tweet features + 2000 NDSI ----
  rf_model10.3 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1:2006)])
  save(rf_model10.3, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/rf_model10.3.RData")
  
  test_phat10.3a = predict(rf_model10.3, newdata = emoji_term_freq[-indices,c(1:2006)], type = "prob")
  auc10.3a = roc(emoji_term_freq$polarity[-indices],test_phat10.3a[,2])
  pred10.3a = prediction(test_phat10.3a[,2], emoji_40k$polarity[-indices])
  acc_list10.3a = performance(pred10.3a,"acc")
  max_acc10.3a = max(unlist(acc_list10.3a@y.values))
  
  
# MODEL 10.4: AFINN + tweet features + 250 NDSI ----
  rf_model10.4 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1:106)])
  save(rf_model10.4, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/rf_model10.4.RData")
  
  test_phat10.4a = predict(rf_model10.4, newdata = emoji_term_freq[-indices,c(1:106)], type = "prob")
  auc10.4a = roc(emoji_term_freq$polarity[-indices],test_phat10.4a[,2])
  pred10.4a = prediction(test_phat10.4a[,2], emoji_40k$polarity[-indices])
  acc_list10.4a = performance(pred10.4a,"acc")
  max_acc10.4a = max(unlist(acc_list10.4a@y.values))


# MODEL 10.5: AFINN + tweet features + 100 NDSI ----
  rf_model10.5 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1:256)])
  save(rf_model10.5, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/rf_model10.5.RData")
  
  test_phat10.5a = predict(rf_model10.5, newdata = emoji_term_freq[-indices,c(1:256)], type = "prob")
  auc10.5a = roc(emoji_term_freq$polarity[-indices],test_phat10.5a[,2])
  pred10.5a = prediction(test_phat10.5a[,2], emoji_40k$polarity[-indices])
  acc_list10.5a = performance(pred10.5a,"acc")
  max_acc10.5a = max(unlist(acc_list10.5a@y.values))

  
  
  
  
  

# PRINT RESULTS ----


### EMOJI RESULTS ###
as.numeric(auc1a$auc); max_acc1a
as.numeric(auc2a$auc); max_acc2a
as.numeric(auc3a$auc); max_acc3a
as.numeric(auc4a$auc); max_acc4a

as.numeric(auc6a$auc); max_acc6a
as.numeric(auc7a$auc); max_acc7a
as.numeric(auc8a$auc); max_acc8a
as.numeric(auc9a$auc); max_acc9a
as.numeric(auc10.4a$auc); max_acc10.4a
as.numeric(auc10.5a$auc); max_acc10.5a
as.numeric(auc10a$auc); max_acc10a
as.numeric(auc10.1a$auc); max_acc10.1a
as.numeric(auc10.2a$auc); max_acc10.2a
as.numeric(auc10.3a$auc); max_acc10.3a

as.numeric(auc11a$auc); max_acc11a
as.numeric(auc12a$auc); max_acc12a
as.numeric(auc13a$auc); max_acc13a
as.numeric(auc14a$auc); max_acc14a
as.numeric(auc15a$auc); max_acc15a
as.numeric(auc16a$auc); max_acc16a





# RESULTS ----


### EMOJI_DF ###

# MODEL   AUC       ACCURACY    MODEL                               TIME TO BUILD
# 1       0.693     & 64.5        AFINN lexicon                       Short
# 2       0.652     & 61.3        OpinionFinder                       Short
# 3       0.646     & 60.8        NRC                                 Short
# 4       0.702     & 64.3        ANEW                                Short
# 5       0.682     & 64.4        AFINN (with random forest)          Short


# 6       0.668     & 64.6        Tweet Features                      Short
# 7       0.742     & 69.1        AFINN + tweet features              Short
# 8       0.813     & 73.9        AFINN + 500 NDSI                    5 mins
# 9       0.817     & 74.0        tweet features + 500 NDSI           4.7 mins
# 10.4    0.817     & 74.4        AFINN + tweet features + 100 NDSI   43 secs
# 10.5    0.826     & 75.0        AFINN + tweet features + 250 NDSI   1.8 mins
# 10      0.829     & 75.2        AFINN + tweet features + 500 NDSI   11.8 mins
# 10.1    0.833     & 75.6        AFINN + tweet features + 1000 NDSI  8 mins    <- Why does this take less time than the model with 500 words?
# 10.2    0.835     & 75.7        AFINN + tweet features + 1500 NDSI  12 mins
# 10.3    0.835     & 75.8        AFINN + tweet features + 2000 NDSI  18.6 mins

# 11      0.756     & 71.5        NDSI 100                            10.33 mins; ntest = 28000
# 12      0.806     & 73.8        NDSI 250                            35.5 mins; ntest = 28000
# 13      0.xxx     & xx.x        NDSI 500                            1.5 hours; ntest = 28000
# 14      0.xxx     & xx.x        NDSI 1000                           3.5 hours; ntest = 28000
# 15      0.xxx     & xx.x        NDSI 1500                           ??; ntest = 28000
# 16      0.xxx     & xx.x        NDSI 2000                           ??; ntest = 28000

# 11      0.785     & 70.9        NDSI 100                            44.4 secs; ntest = 4000
# 12      0.799     & 71.9        NDSI 250                            2.2 mins; ntest = 4000
# 13      0.805     & 72.9        NDSI 500                            4.5 mins; ntest = 4000
# 14      0.810     & 73.2        NDSI 1000                           9.4 mins; ntest = 4000
# 15      0.814     & 73.8        NDSI 1500                           14.2 mins; ntest = 4000
# 16      0.816     & 73.6        NDSI 2000                           19.3 mins; ntest = 4000
