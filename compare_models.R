# Ebert
# Compare Models


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

  # Sentiment 140:
  # sent140 = get_sent140()
  # sent140_term_freq = make_term_freq(sent140, ndsi_lexicon_df)
  # save(sent140_term_freq, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/sent140_term_freq.RData")
  # load(file = "~/Desktop/Documents/GitRepos/LAR/compare_models/sent140_term_freq.RData")


# Import data ----
  source('~/Desktop/Documents/GitRepos/LAR/functions.R')
  library(feather)
  library(ROCR)
  library(pROC)
  library(randomForest)
  library(doMC)
  registerDoMC(2)
  
  ndsi_lexicon_df = read_feather(path = "~/Desktop/Documents/GitRepos/LAR/compare_models/ndsi_lexicon_df.feather")
  
  load(file = "~/Desktop/Documents/GitRepos/LAR/compare_models/emoji_40k.feather")
  load(file = "~/Desktop/Huang Research/LAR_Data/compare_lexicons/feather_data/2016-08/indices_40k.RData")
  load(file = "~/Desktop/Documents/GitRepos/LAR/compare_models/emoji_term_freq.RData")
  
  load(file = "~/Desktop/Documents/GitRepos/LAR/compare_models/indices.RData")
  load(file = "~/Desktop/Documents/GitRepos/LAR/compare_models/small_indices.RData")
  
  sent140 = get_sent140()
  load(file = "~/Desktop/Documents/GitRepos/LAR/compare_models/sent140_term_freq.RData")
  
  ntest = 4000 # Should be set to 28000 for paper tests; 4000 for testing NDSI 
  indices = small_indices # This is just in case ntest = 4000
  
  
  # set number of training tweets. 28000 is maximum value



# MODEL 1: AFINN ----
  auc1a = roc(emoji_40k$polarity, emoji_40k$afinn_score)
  pred1a = prediction(emoji_40k$afinn_score,emoji_40k$polarity)
  acc_list1a <- performance(pred1a,"acc")
  max_acc1a = max(unlist(acc_list1a@y.values))
  
  auc1b = roc(sent140$polarity, sent140$afinn_score)
  pred1b = prediction(sent140$afinn_score,sent140$polarity)
  acc_list1b <- performance(pred1b,"acc")
  max_acc1b = max(unlist(acc_list1b@y.values))
  

# MODEL 2: OpinionFinder ----
  auc2a = roc(emoji_40k$polarity, emoji_40k$opinionfinder_score)
  pred2a = prediction(emoji_40k$opinionfinder_score,emoji_40k$polarity)
  acc_list2a <- performance(pred2a,"acc")
  max_acc2a = max(unlist(acc_list2a@y.values))

  auc2b = roc(sent140$polarity, sent140$opinionfinder_score)
  pred2b = prediction(sent140$opinionfinder_score, sent140$polarity)
  acc_list2b <- performance(pred2b,"acc")
  max_acc2b = max(unlist(acc_list2b@y.values))

# MODEL 3: NRC ----
  auc3a = roc(emoji_40k$polarity, emoji_40k$nrc_score)
  pred3a = prediction(emoji_40k$nrc_score,emoji_40k$polarity)
  acc_list3a <- performance(pred3a,"acc")
  max_acc3a = max(unlist(acc_list3a@y.values))
  
  auc3b = roc(sent140$polarity, sent140$nrc_score)
  pred3b = prediction(sent140$nrc_score, sent140$polarity)
  acc_list3b <- performance(pred3b,"acc")
  max_acc3b = max(unlist(acc_list3b@y.values))

# MODEL 4: ANEW ----
  auc4a = roc(emoji_40k$polarity, emoji_40k$anew_score)
  pred4a = prediction(emoji_40k$anew_score,emoji_40k$polarity)
  acc_list4a <- performance(pred4a,"acc")
  max_acc4a = max(unlist(acc_list4a@y.values))
  
  auc4b = roc(sent140$polarity, sent140$anew_score)
  pred4b = prediction(sent140$anew_score, sent140$polarity)
  acc_list4b <- performance(pred4b,"acc")
  max_acc4b = max(unlist(acc_list4b@y.values))

  


# Model 5: Tweet Features ----
  rf_model5 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1,7:506)])
  save(rf_model5, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/rf_model6.RData")
  
  test_phat5a = predict(rf_model5, newdata = emoji_term_freq[-indices,c(1,7:506)], type = "prob")
  auc5a = roc(emoji_term_freq$polarity[-indices],test_phat5a[,2])
  pred5a = prediction(test_phat5a[,2], emoji_40k$polarity[-indices])
  acc_list5a = performance(pred5a,"acc")
  max_acc5a = max(unlist(acc_list5a@y.values))
  
  test_phat5b = predict(rf_model5, newdata = sent140_term_freq[-indices,c(1,7:506)], type = "prob")
  auc5b = roc(sent140_term_freq$polarity[-indices],test_phat5b[,2])
  pred5b = prediction(test_phat5b[,2], sent140$polarity[-indices])
  acc_list5b = performance(pred5b,"acc")
  max_acc5b = max(unlist(acc_list5b@y.values))



  


# MODEL 11: NDSI 100 ----
  rf_model11 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1,7:106)])
  save(rf_model11, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/rf_model11.RData")
  
  test_phat11a = predict(rf_model11, newdata = emoji_term_freq[-indices,c(1,7:106)], type = "prob")
  auc11a = roc(emoji_term_freq$polarity[-indices],test_phat11a[,2])
  pred11a = prediction(test_phat11a[,2], emoji_40k$polarity[-indices])
  acc_list11a = performance(pred11a,"acc")
  max_acc11a = max(unlist(acc_list11a@y.values))
  
  test_phat11b = predict(rf_model11, newdata = sent140_term_freq[-indices,c(1:106)], type = "prob")
  auc11b = roc(sent140_term_freq$polarity[-indices],test_phat11b[,2])
  pred11b = prediction(test_phat11b[,2], sent140$polarity[-indices])
  acc_list11b = performance(pred11b,"acc")
  max_acc11b = max(unlist(acc_list11b@y.values))

  
# MODEL 12: NDSI 250 ----
  rf_model12 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1,7:256)])
  save(rf_model12, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/rf_model12.RData")
  
  test_phat12a = predict(rf_model12, newdata = emoji_term_freq[-indices,c(1,7:256)], type = "prob")
  auc12a = roc(emoji_term_freq$polarity[-indices],test_phat12a[,2])
  pred12a = prediction(test_phat12a[,2], emoji_40k$polarity[-indices])
  acc_list12a = performance(pred12a,"acc")
  max_acc12a = max(unlist(acc_list12a@y.values))
  
  test_phat12b = predict(rf_model12, newdata = sent140_term_freq[-indices,c(1,7:256)], type = "prob")
  auc12b = roc(sent140_term_freq$polarity[-indices],test_phat12b[,2])
  pred12b = prediction(test_phat12b[,2], sent140$polarity[-indices])
  acc_list12b = performance(pred12b,"acc")
  max_acc12b = max(unlist(acc_list12b@y.values))

  
# MODEL 13: NDSI 500 ----
  rf_model13 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1,7:506)])
  save(rf_model13, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/rf_model13.RData")
  
  test_phat13a = predict(rf_model13, newdata = emoji_term_freq[-indices,c(1,7:506)], type = "prob")
  auc13a = roc(emoji_term_freq$polarity[-indices],test_phat13a[,2])
  pred13a = prediction(test_phat13a[,2], emoji_40k$polarity[-indices])
  acc_list13a = performance(pred13a,"acc")
  max_acc13a = max(unlist(acc_list13a@y.values))
  
  test_phat13b = predict(rf_model13, newdata = sent140_term_freq[-indices,c(1,7:506)], type = "prob")
  auc13b = roc(sent140_term_freq$polarity[-indices],test_phat13b[,2])
  pred13b = prediction(test_phat13b[,2], sent140$polarity[-indices])
  acc_list13b = performance(pred13b,"acc")
  max_acc13b = max(unlist(acc_list13b@y.values))
  
  
# MODEL 14: NDSI 1000 ----
  rf_model14 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1,7:1006)])
  save(rf_model14, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/rf_model14.RData")
  
  test_phat14a = predict(rf_model14, newdata = emoji_term_freq[-indices,c(1,7:1006)], type = "prob")
  auc14a = roc(emoji_term_freq$polarity[-indices],test_phat14a[,2])
  pred14a = prediction(test_phat14a[,2], emoji_40k$polarity[-indices])
  acc_list14a = performance(pred14a,"acc")
  max_acc14a = max(unlist(acc_list14a@y.values))
  
  test_phat14b = predict(rf_model14, newdata = sent140_term_freq[-indices,c(1,7:1006)], type = "prob")
  auc14b = roc(sent140_term_freq$polarity[-indices],test_phat14b[,2])
  pred14b = prediction(test_phat14b[,2], sent140$polarity[-indices])
  acc_list14b = performance(pred14b,"acc")
  max_acc14b = max(unlist(acc_list14b@y.values))
  
  
# MODEL 15: NDSI 1500 ----
  rf_model15 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1,7:1506)])
  save(rf_model15, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/rf_model15.RData")
  
  test_phat15a = predict(rf_model15, newdata = emoji_term_freq[-indices,c(1,7:1506)], type = "prob")
  auc15a = roc(emoji_term_freq$polarity[-indices],test_phat15a[,2])
  pred15a = prediction(test_phat15a[,2], emoji_40k$polarity[-indices])
  acc_list15a = performance(pred15a,"acc")
  max_acc15a = max(unlist(acc_list15a@y.values))
  
  test_phat15b = predict(rf_model15, newdata = sent140_term_freq[-indices,c(1,7:1506)], type = "prob")
  auc15b = roc(sent140_term_freq$polarity[-indices],test_phat15b[,2])
  pred15b = prediction(test_phat15b[,2], sent140$polarity[-indices])
  acc_list15b = performance(pred15b,"acc")
  max_acc15b = max(unlist(acc_list15b@y.values))
  
  
# MODEL 16: NDSI 2000 ----
  rf_model16 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1,7:2006)])
  save(rf_model16, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/rf_model16.RData")
  
  test_phat16a = predict(rf_model16, newdata = emoji_term_freq[-indices,c(1,7:2006)], type = "prob")
  auc16a = roc(emoji_term_freq$polarity[-indices],test_phat16a[,2])
  pred16a = prediction(test_phat16a[,2], emoji_40k$polarity[-indices])
  acc_list16a = performance(pred16a,"acc")
  max_acc16a = max(unlist(acc_list16a@y.values))
  
  test_phat16b = predict(rf_model16, newdata = sent140_term_freq[-indices,c(1,7:2006)], type = "prob")
  auc16b = roc(sent140_term_freq$polarity[-indices],test_phat16b[,2])
  pred16b = prediction(test_phat16b[,2], sent140$polarity[-indices])
  acc_list16b = performance(pred16b,"acc")
  max_acc16b = max(unlist(acc_list16b@y.values))



  
  
  
  
  
a = Sys.time()

# Model 6: Tweet Features ----
  rf_model6 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1,3:6)])
  save(rf_model6, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/rf_model6.RData")
  
  test_phat6a = predict(rf_model6, newdata = emoji_term_freq[-indices,c(1,3:6)], type = "prob")
  auc6a = roc(emoji_term_freq$polarity[-indices],test_phat6a[,2])
  pred6a = prediction(test_phat6a[,2], emoji_40k$polarity[-indices])
  acc_list6a = performance(pred6a,"acc")
  max_acc6a = max(unlist(acc_list6a@y.values))
  
  test_phat6b = predict(rf_model6, newdata = sent140_term_freq[-indices,c(1,3:6)], type = "prob")
  auc6b = roc(sent140_term_freq$polarity[-indices],test_phat6b[,2])
  pred6b = prediction(test_phat6b[,2], sent140$polarity[-indices])
  acc_list6b = performance(pred6b,"acc")
  max_acc6b = max(unlist(acc_list6b@y.values))
  
  print("done with model 6")
  Sys.time()-a
  beepr::beep(3)
  a = Sys.time()
  
  
# MODEL 7: AFINN + tweet features ----
  rf_model7 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1:6)])
  save(rf_model7, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/rf_model7.RData")
  
  test_phat7a = predict(rf_model7, newdata = emoji_term_freq[-indices,c(1:6)], type = "prob")
  auc7a = roc(emoji_term_freq$polarity[-indices],test_phat7a[,2])
  pred7a = prediction(test_phat7a[,2], emoji_40k$polarity[-indices])
  acc_list7a = performance(pred7a,"acc")
  max_acc7a = max(unlist(acc_list7a@y.values))
  
  test_phat7b = predict(rf_model7, newdata = sent140_term_freq[-indices,c(1:6)], type = "prob")
  auc7b = roc(sent140_term_freq$polarity[-indices],test_phat7b[,2])
  pred7b = prediction(test_phat7b[,2], sent140$polarity[-indices])
  acc_list7b = performance(pred7b,"acc")
  max_acc7b = max(unlist(acc_list7b@y.values))
  
  print("done with model 7")
  Sys.time()-a
  beepr::beep(3)
  a = Sys.time()
  
# MODEL 8: AFINN + 500 NDSI ----
  rf_model8 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1:2,7:506)])
  save(rf_model8, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/rf_model8.RData")
  
  test_phat8a = predict(rf_model8, newdata = emoji_term_freq[-indices,c(1:2,7:506)], type = "prob")
  auc8a = roc(emoji_term_freq$polarity[-indices],test_phat8a[,2])
  pred8a = prediction(test_phat8a[,2], emoji_40k$polarity[-indices])
  acc_list8a = performance(pred8a,"acc")
  max_acc8a = max(unlist(acc_list8a@y.values))
  
  test_phat8b = predict(rf_model8, newdata = sent140_term_freq[-indices,c(1:2,7:506)], type = "prob")
  auc8b = roc(sent140_term_freq$polarity[-indices],test_phat8b[,2])
  pred8b = prediction(test_phat8b[,2], sent140$polarity[-indices])
  acc_list8b = performance(pred8b,"acc")
  max_acc8b = max(unlist(acc_list8b@y.values))
  
  print("done with model 8")
  Sys.time()-a
  beepr::beep(3)
  a = Sys.time()
  
# MODEL 9: tweet features + 500 NDSI ----
  rf_model9 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1,3:506)])
  save(rf_model9, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/rf_model9.RData")
  
  test_phat9a = predict(rf_model9, newdata = emoji_term_freq[-indices,c(1,3:506)], type = "prob")
  auc9a = roc(emoji_term_freq$polarity[-indices],test_phat9a[,2])
  pred9a = prediction(test_phat9a[,2], emoji_40k$polarity[-indices])
  acc_list9a = performance(pred9a,"acc")
  max_acc9a = max(unlist(acc_list9a@y.values))
  
  test_phat9b = predict(rf_model9, newdata = sent140_term_freq[-indices,c(1,3:506)], type = "prob")
  auc9b = roc(sent140_term_freq$polarity[-indices],test_phat9b[,2])
  pred9b = prediction(test_phat9b[,2], sent140$polarity[-indices])
  acc_list9b = performance(pred9b,"acc")
  max_acc9b = max(unlist(acc_list9b@y.values))
  
  print("done with model 9")
  Sys.time()-a
  beepr::beep(3)
  a = Sys.time()
  
# MODEL 10: AFINN + tweet features + 500 NDSI ----
  rf_model10 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1:506)])
  save(rf_model10, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/rf_model10.RData")
  
  test_phat10a = predict(rf_model10, newdata = emoji_term_freq[-indices,c(1:506)], type = "prob")
  auc10a = roc(emoji_term_freq$polarity[-indices],test_phat10a[,2])
  pred10a = prediction(test_phat10a[,2], emoji_40k$polarity[-indices])
  acc_list10a = performance(pred10a,"acc")
  max_acc10a = max(unlist(acc_list10a@y.values))
  
  test_phat10b = predict(rf_model10, newdata = sent140_term_freq[-indices,c(1:506)], type = "prob")
  auc10b = roc(sent140_term_freq$polarity[-indices],test_phat10b[,2])
  pred10b = prediction(test_phat10b[,2], sent140$polarity[-indices])
  acc_list10b = performance(pred10b,"acc")
  max_acc10b = max(unlist(acc_list10b@y.values))
  
  print("done with model 10")
  Sys.time()-a
  beepr::beep(3)
  a = Sys.time()
  
  
# MODEL 10.1: AFINN + tweet features + 1000 NDSI ----
  rf_model10.1 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1:1006)])
  save(rf_model10.1, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/rf_model10.1.RData")
  
  test_phat10.1a = predict(rf_model10.1, newdata = emoji_term_freq[-indices,c(1:1006)], type = "prob")
  auc10.1a = roc(emoji_term_freq$polarity[-indices],test_phat10.1a[,2])
  pred10.1a = prediction(test_phat10.1a[,2], emoji_40k$polarity[-indices])
  acc_list10.1a = performance(pred10.1a,"acc")
  max_acc10.1a = max(unlist(acc_list10.1a@y.values))
  
  test_phat10.1b = predict(rf_model10.1, newdata = sent140_term_freq[-indices,c(1:1006)], type = "prob")
  auc10.1b = roc(sent140_term_freq$polarity[-indices],test_phat10.1b[,2])
  pred10.1b = prediction(test_phat10.1b[,2], sent140$polarity[-indices])
  acc_list10.1b = performance(pred10.1b,"acc")
  max_acc10.1b = max(unlist(acc_list10.1b@y.values))
  
  print("done with model 10.1")
  Sys.time()-a
  beepr::beep(3)
  a = Sys.time()
  
  
# MODEL 10.2: AFINN + tweet features + 1500 NDSI ----
  rf_model10.2 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1:1506)])
  save(rf_model10.2, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/rf_model10.2.RData")
  
  test_phat10.2a = predict(rf_model10.2, newdata = emoji_term_freq[-indices,c(1:1506)], type = "prob")
  auc10.2a = roc(emoji_term_freq$polarity[-indices],test_phat10.2a[,2])
  pred10.2a = prediction(test_phat10.2a[,2], emoji_40k$polarity[-indices])
  acc_list10.2a = performance(pred10.2a,"acc")
  max_acc10.2a = max(unlist(acc_list10.2a@y.values))
  
  test_phat10.2b = predict(rf_model10.2, newdata = sent140_term_freq[-indices,c(1:1506)], type = "prob")
  auc10.2b = roc(sent140_term_freq$polarity[-indices],test_phat10.2b[,2])
  pred10.2b = prediction(test_phat10.2b[,2], sent140$polarity[-indices])
  acc_list10.2b = performance(pred10.2b,"acc")
  max_acc10.2b = max(unlist(acc_list10.2b@y.values))
  
  print("done with model 10.2")
  Sys.time()-a
  beepr::beep(3)
  a = Sys.time()
  
  
# MODEL 10.3: AFINN + tweet features + 2000 NDSI ----
  rf_model10.3 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1:2006)])
  save(rf_model10.3, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/rf_model10.3.RData")
  
  test_phat10.3a = predict(rf_model10.3, newdata = emoji_term_freq[-indices,c(1:2006)], type = "prob")
  auc10.3a = roc(emoji_term_freq$polarity[-indices],test_phat10.3a[,2])
  pred10.3a = prediction(test_phat10.3a[,2], emoji_40k$polarity[-indices])
  acc_list10.3a = performance(pred10.3a,"acc")
  max_acc10.3a = max(unlist(acc_list10.3a@y.values))
  
  test_phat10.3b = predict(rf_model10.3, newdata = sent140_term_freq[-indices,c(1:2006)], type = "prob")
  auc10.3b = roc(sent140_term_freq$polarity[-indices],test_phat10.3b[,2])
  pred10.3b = prediction(test_phat10.3b[,2], sent140$polarity[-indices])
  acc_list10.3b = performance(pred10.3b,"acc")
  max_acc10.3b = max(unlist(acc_list10.3b@y.values))
  
  print("done with model 10.3")
  Sys.time()-a
  beepr::beep(3)
  a = Sys.time()
  
  

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
as.numeric(auc10a$auc); max_acc10a
as.numeric(auc10.1a$auc); max_acc10.1a

as.numeric(auc11a$auc); max_acc11a
as.numeric(auc12a$auc); max_acc12a
as.numeric(auc13a$auc); max_acc13a
as.numeric(auc14a$auc); max_acc14a
as.numeric(auc15a$auc); max_acc15a
as.numeric(auc16a$auc); max_acc16a


### SENT140 RESULTS ###
as.numeric(auc1b$auc); max_acc1b
as.numeric(auc2b$auc); max_acc2b
as.numeric(auc3b$auc); max_acc3b
as.numeric(auc4b$auc); max_acc4b

as.numeric(auc6b$auc); max_acc6b
as.numeric(auc7b$auc); max_acc7b
as.numeric(auc8b$auc); max_acc8b
as.numeric(auc9b$auc); max_acc9b
as.numeric(auc10b$auc); max_acc10b
as.numeric(auc10.1b$auc); max_acc10.1b

as.numeric(auc11b$auc); max_acc11b
as.numeric(auc12b$auc); max_acc12b
as.numeric(auc13b$auc); max_acc13b
as.numeric(auc14b$auc); max_acc14b
as.numeric(auc15b$auc); max_acc15b
as.numeric(auc16b$auc); max_acc16b


# RESULTS ----


### EMOJI_DF ###

# MODEL   AUC       ACCURACY    MODEL                               TIME TO BUILD
# 1       0.693     64.5        AFINN lexicon                       Short
# 2       0.652     61.3        OpinionFinder                       Short
# 3       0.646     60.8        NRC                                 Short
# 4       0.702     64.3        ANEW                                Short

# 6       0.668     64.6        Tweet Features                      Short
# 7       0.742     69.2        AFINN + tweet features              Short
# 8       0.xxx     xx.x        AFINN + NDSI                        
# 9       0.xxx     xx.x        tweet features + NDSI               
# 10      0.830     75.3        AFINN + tweet features + 500 NDSI   11.8 mins
# 10.1    0.xxx     xx.x        AFINN + tweet features + 1000 NDSI  8 mins
# 10.2    0.xxx     xx.x        AFINN + tweet features + 1500 NDSI  ??
# 10.3    0.xxx     xx.x        AFINN + tweet features + 2000 NDSI  ??

# 11      0.756     71.5        NDSI 100                            10.33 mins; ntest = 28000
# 12      0.806     73.8        NDSI 250                            35.5 mins; ntest = 28000
# 13      0.xxx     xx.x        NDSI 500                            1.5 hours; ntest = 28000
# 14      0.xxx     xx.x        NDSI 1000                           3.5 hours; ntest = 28000
# 15      0.xxx     xx.x        NDSI 1500                           ??; ntest = 28000
# 16      0.xxx     xx.x        NDSI 2000                           ??; ntest = 28000

# 11      0.785     70.9        NDSI 100                            44.4 secs; ntest = 4000
# 12      0.799     71.9        NDSI 250                            2.2 mins; ntest = 4000
# 13      0.805     72.9        NDSI 500                            4.5 mins; ntest = 4000
# 14      0.810     73.2        NDSI 1000                           9.4 mins; ntest = 4000
# 15      0.814     73.8        NDSI 1500                           14.2 mins; ntest = 4000
# 16      0.816     73.6        NDSI 2000                           19.3; ntest = 4000




### SENTIMENT140 ###

# MODEL   AUC       ACCURACY    MODEL
# 1       0.840     76.3        AFINN lexicon                   
# 2       0.779     72.1        OpinionFinder
# 3       0.743     67.7        NRC          
# 4       0.824     74.9        ANEW

# 6       0.547     54.8        Tweet Features
# 7       0.748     75.0        AFINN + tweet features
# 8       0.xxx     xx.x        AFINN + NDSI
# 9       0.xxx     xx.x        tweet features + NDSI
# 10      0.826     75.3        AFINN + tweet features + 500 NDSI
# 10.1    0.xxx     xx.x        AFINN + tweet features + 1000 NDSI
# 10.2    0.xxx     xx.x        AFINN + tweet features + 1500 NDSI
# 10.3    0.xxx     xx.x        AFINN + tweet features + 2000 NDSI

# 11      0.660     63.2        NDSI 100    ntest = 28000
# 12      0.704     68.4        NDSI 250    ntest = 28000
# 13      0.xxx     xx.x        NDSI 500    ntest = 28000
# 14      0.xxx     xx.x        NDSI 1000   ntest = 28000
# 15      0.xxx     xx.x        NDSI 1500   ntest = 28000
# 16      0.xxx     xx.x        NDSI 2000   ntest = 28000

# 11      0.772     70.4        NDSI 100    ntest = 4000
# 12      0.785     73.8        NDSI 250    ntest = 4000
# 13      0.784     73.2        NDSI 500    ntest = 4000
# 14      0.787     74.7        NDSI 1000   ntest = 4000
# 15      0.788     74.1        NDSI 1500   ntest = 4000
# 16      0.794     74.8        NDSI 2000   ntest = 4000
