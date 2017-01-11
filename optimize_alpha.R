# Ebert


# SETUP ---

  # (Also see setup from compare_models.R)
  
  emoticon_df = read_feather(path = "~/Desktop/Huang Research/LAR_Data/feather_data/2016-08/emoticon_df.feather") # WHY THIS ERROR??
  
  ndsi_lexicon_df_0 = make_ndsi_lexicon(emoticon_df, max_words = 250, smoothing_alpha = 2^0)
  ndsi_lexicon_df_2 = make_ndsi_lexicon(emoticon_df, max_words = 250, smoothing_alpha = 2^2)
  ndsi_lexicon_df_4 = make_ndsi_lexicon(emoticon_df, max_words = 250, smoothing_alpha = 2^4)
  ndsi_lexicon_df_8 = make_ndsi_lexicon(emoticon_df, max_words = 250, smoothing_alpha = 2^8)
  ndsi_lexicon_df_12 = make_ndsi_lexicon(emoticon_df, max_words = 250, smoothing_alpha = 2^12)
  ndsi_lexicon_df_16 = make_ndsi_lexicon(emoticon_df, max_words = 250, smoothing_alpha = 2^16)
  ndsi_lexicon_df_20 = make_ndsi_lexicon(emoticon_df, max_words = 250, smoothing_alpha = 2^20)
  ndsi_lexicon_df_24 = make_ndsi_lexicon(emoticon_df, max_words = 10000, smoothing_alpha = 2^24)
  ndsi_lexicon_df_24$word_freq = ndsi_lexicon_df_24$freq.happy + ndsi_lexicon_df_24$freq.sad
  unigrams_lexicon = arrange(ndsi_lexicon_df_24, desc(word_freq))
  ndsi_lexicon_df_24 = ndsi_lexicon_df_24[1:250,]
  unigrams_lexicon = unigrams_lexicon[1:250,]
  
  # set.seed(1234)
  # ndsi_indices = sample(1:40000,5000) # Aim later for 10000
  # ndsi_indices = sort(indices)
  # save(ndsi_indices, file = "~/Desktop/Documents/GitRepos/LAR/optimize_alpha/ndsi_indices.RData")

  load(file = "~/Desktop/Documents/GitRepos/LAR/optimize_alpha/ndsi_indices.RData")
  ntest = 5000 # Use smaller 12000 for testing alpha



# TEST MODELS ---

  # alpha_0: NDSI 250 ----
  emoji_term_freq = make_term_freq(emoji_40k, ndsi_lexicon_df_0)
  model_alpha_0 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1,7:256)])
  save(model_alpha_0, file = "~/Desktop/Documents/GitRepos/LAR/optimize_alpha/model_alpha_0.RData")
  test_phat0 = predict(model_alpha_0, newdata = emoji_term_freq[-indices,c(1,7:256)], type = "prob")
  auc0 = roc(emoji_term_freq$polarity[-indices],test_phat0[,2])
  pred0 = prediction(test_phat0[,2], emoji_40k$polarity[-indices])
  acc_list0 = performance(pred0,"acc")
  max_acc0 = max(unlist(acc_list0@y.values))
  

  # alpha_2: NDSI 250 ----
  emoji_term_freq = make_term_freq(emoji_40k, ndsi_lexicon_df_2)
  model_alpha_2 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1,7:256)])
  save(model_alpha_2, file = "~/Desktop/Documents/GitRepos/LAR/optimize_alpha/model_alpha_2.RData")
  test_phat2 = predict(model_alpha_2, newdata = emoji_term_freq[-indices,c(1,7:256)], type = "prob")
  auc2 = roc(emoji_term_freq$polarity[-indices],test_phat2[,2])
  pred2 = prediction(test_phat2[,2], emoji_40k$polarity[-indices])
  acc_list2 = performance(pred2,"acc")
  max_acc2 = max(unlist(acc_list2@y.values))
  
  
  
  # alpha_4: NDSI 250 ----
  emoji_term_freq = make_term_freq(emoji_40k, ndsi_lexicon_df_4)
  model_alpha_4 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1,7:256)])
  save(model_alpha_4, file = "~/Desktop/Documents/GitRepos/LAR/optimize_alpha/model_alpha_4.RData")
  test_phat4 = predict(model_alpha_4, newdata = emoji_term_freq[-indices,c(1,7:256)], type = "prob")
  auc4 = roc(emoji_term_freq$polarity[-indices],test_phat4[,2])
  pred4 = prediction(test_phat4[,2], emoji_40k$polarity[-indices])
  acc_list4 = performance(pred4,"acc")
  max_acc4 = max(unlist(acc_list4@y.values))
  
  
  # alpha_8: NDSI 250 ----
  emoji_term_freq = make_term_freq(emoji_40k, ndsi_lexicon_df_8)
  model_alpha_8 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1,7:256)])
  save(model_alpha_8, file = "~/Desktop/Documents/GitRepos/LAR/optimize_alpha/model_alpha_8.RData")
  test_phat8 = predict(model_alpha_8, newdata = emoji_term_freq[-indices,c(1,7:256)], type = "prob")
  auc8 = roc(emoji_term_freq$polarity[-indices],test_phat8[,2])
  pred8 = prediction(test_phat8[,2], emoji_40k$polarity[-indices])
  acc_list8 = performance(pred8,"acc")
  max_acc8 = max(unlist(acc_list8@y.values))
  

  # alpha_12: NDSI 250 ----
  emoji_term_freq = make_term_freq(emoji_40k, ndsi_lexicon_df_12)
  model_alpha_12 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1,7:256)])
  save(model_alpha_12, file = "~/Desktop/Documents/GitRepos/LAR/optimize_alpha/model_alpha_12.RData")
  test_phat12 = predict(model_alpha_12, newdata = emoji_term_freq[-indices,c(1,7:256)], type = "prob")
  auc12 = roc(emoji_term_freq$polarity[-indices],test_phat12[,2])
  pred12 = prediction(test_phat12[,2], emoji_40k$polarity[-indices])
  acc_list12 = performance(pred12,"acc")
  max_acc12 = max(unlist(acc_list12@y.values))
  

  # alpha_16: NDSI 250 ----
  emoji_term_freq = make_term_freq(emoji_40k, ndsi_lexicon_df_16)
  model_alpha_16 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1,7:256)])
  save(model_alpha_16, file = "~/Desktop/Documents/GitRepos/LAR/optimize_alpha/model_alpha_16.RData")
  test_phat16 = predict(model_alpha_16, newdata = emoji_term_freq[-indices,c(1,7:256)], type = "prob")
  auc16 = roc(emoji_term_freq$polarity[-indices],test_phat16[,2])
  pred16 = prediction(test_phat16[,2], emoji_40k$polarity[-indices])
  acc_list16 = performance(pred16,"acc")
  max_acc16 = max(unlist(acc_list16@y.values))
  

  # alpha_20: NDSI 250 ----
  emoji_term_freq = make_term_freq(emoji_40k, ndsi_lexicon_df_20)
  model_alpha_20 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1,7:256)])
  save(model_alpha_20, file = "~/Desktop/Documents/GitRepos/LAR/optimize_alpha/model_alpha_20.RData")
  test_phat20 = predict(model_alpha_20, newdata = emoji_term_freq[-indices,c(1,7:256)], type = "prob")
  auc20 = roc(emoji_term_freq$polarity[-indices],test_phat20[,2])
  pred20 = prediction(test_phat20[,2], emoji_40k$polarity[-indices])
  acc_list20 = performance(pred20,"acc")
  max_acc20 = max(unlist(acc_list20@y.values))
  

  # alpha_24: NDSI 250 ----
  emoji_term_freq = make_term_freq(emoji_40k, ndsi_lexicon_df_24)
  model_alpha_24 = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1,7:256)])
  save(model_alpha_24, file = "~/Desktop/Documents/GitRepos/LAR/optimize_alpha/model_alpha_24.RData")
  test_phat24 = predict(model_alpha_24, newdata = emoji_term_freq[-indices,c(1,7:256)], type = "prob")
  auc24 = roc(emoji_term_freq$polarity[-indices],test_phat24[,2])
  pred24 = prediction(test_phat24[,2], emoji_40k$polarity[-indices])
  acc_list24 = performance(pred24,"acc")
  max_acc24 = max(unlist(acc_list24@y.values))
  

  # alpha_unigram: NDSI 250 ----
  emoji_term_freq = make_term_freq(emoji_40k, unigrams_lexicon)
  model_alpha_unigram = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1,7:256)])
  save(model_alpha_unigram, file = "~/Desktop/Documents/GitRepos/LAR/optimize_alpha/model_alpha_unigram.RData")
  test_phatunigram = predict(model_alpha_unigram, newdata = emoji_term_freq[-indices,c(1,7:256)], type = "prob")
  aucunigram = roc(emoji_term_freq$polarity[-indices],test_phatunigram[,2])
  predunigram = prediction(test_phatunigram[,2], emoji_40k$polarity[-indices])
  acc_listunigram = performance(predunigram,"acc")
  max_accunigram = max(unlist(acc_listunigram@y.values))
  
  
# RESULTS ----
  
  as.numeric(auc0$auc); max_acc0
  as.numeric(auc2$auc); max_acc2
  as.numeric(auc4$auc); max_acc4
  as.numeric(auc8$auc); max_acc8
  as.numeric(auc12$auc); max_acc12
  as.numeric(auc16$auc); max_acc16
  as.numeric(auc20$auc); max_acc20
  as.numeric(auc24$auc); max_acc24
  as.numeric(aucunigram$auc); max_accunigram


  #41 mins for 5000-tweet models:
  
  # 0   0.677   60.5%
  # 2   0.730   66.6%
  # 4   0.711   69.4%
  # 8   0.736   69.9%
  #12   0.761   69.45%
  #16   0.770   69.84%  <- Best alpha is 2^16
  #20   0.764   70.09%
  #24   0.760   69.23%
  #uni  0.755   68.56%
