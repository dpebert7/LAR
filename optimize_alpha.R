# Ebert



# SETUP ---

  # (Also see setup from compare_models.R)
  
  emoticon_df = read_feather(path = "~/Desktop/Huang Research/LAR_Data/feather_data/2016-08/emoticon_df.feather") # WHY THIS ERROR??
  
  
  ndsi_lexicon_df_24 = make_ndsi_lexicon(emoticon_df[indices,], max_words = 10000, smoothing_alpha = 2^24)
  ndsi_lexicon_df_24$word_freq = ndsi_lexicon_df_24$freq.happy + ndsi_lexicon_df_24$freq.sad  # Words must occur at least 2 times.

  max_imbalance_lexicon = arrange(ndsi_lexicon_df_24, desc(diff))  
  unigram_lexicon = arrange(ndsi_lexicon_df_24, desc(word_freq))
  
  save(max_imbalance_lexicon, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/max_imbalance_lexicon.RData")
  save(unigram_lexicon, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/unigram_lexicon.RData")
  

  
  
  # set.seed(1234)
  # ndsi_indices = sample(1:40000,5000) # Aim later for 10000
  # ndsi_indices = sort(indices)
  # save(ndsi_indices, file = "~/Desktop/Documents/GitRepos/LAR/optimize_alpha/ndsi_indices.RData")
  
  # set.seed(1234)
  # ndsi_indices = sample(1:40000,0.25*40000)
  # ndsi_indices = sort(ndsi_indices)
  # save(ndsi_indices, file = "~/Desktop/Documents/GitRepos/LAR/optimize_alpha/ndsi_indices.RData")
  # load(file = "~/Desktop/Documents/GitRepos/LAR/optimize_alpha/ndsi_indices.RData")

  load(file = "~/Desktop/Documents/GitRepos/LAR/optimize_alpha/ndsi_indices.RData")
  ntest = 10000 # Use smaller 12000 for testing alpha
  
  load(file = "~/Desktop/Documents/GitRepos/LAR/compare_models/max_imbalance_lexicon.RData")
  load(file = "~/Desktop/Documents/GitRepos/LAR/compare_models/unigram_lexicon.RData")
  
  


# TEST MODELS ---

    
    test_penalty = function(penalty, tweet_df = emoji_40k, ntweets = 300, num_words = 100){
      print(paste("Building ndsi_lexicon with penalty =", penalty, "..."))
      ndsi_lexicon = make_ndsi_lexicon(emoticon_df[ndsi_indices,], max_words = num_words, smoothing_alpha = penalty*ntweets)
      
      print("Building emoji_term_freq...")
      emoji_term_freq = make_term_freq(tweet_df, ndsi_lexicon)
      
      print("Building model...")
      model = randomForest(polarity~.,data = emoji_term_freq[sample(ndsi_indices,ntweets),c(1,7:(6+num_words))])
      save(model, file = "~/Desktop/Documents/GitRepos/LAR/optimize_alpha/model_alpha_0.RData")
      
      test_phat = predict(model, newdata = emoji_term_freq[-ndsi_indices,c(1,7:(6+num_words))], type = "prob")
      auc = roc(emoji_term_freq$polarity[-ndsi_indices],test_phat[,2])
      pred = prediction(test_phat[,2], emoji_40k$polarity[-ndsi_indices])
      acc_list = performance(pred,"acc")
      max_acc = max(unlist(acc_list@y.values))
      print(paste("     Emoji AUC:", as.numeric(auc$auc)))
      print(paste("Emoji accuracy:", max_acc))
      print(" ")
      return(list(
        "auc" = as.numeric(auc$auc),
        "acc" = max_acc))
    }
    
    example_result = test_penalty(penalty = 0.1)
    
    
    penalty_vec = c(0.0001, 0.0005, 0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 5, 10, 50, 100, 500, 1000, 5000, 10000)
    auc_vec = c()
    acc_vec = c()
    for(i in penalty_vec){
      a = Sys.time()
      temp_result = test_penalty(penalty = i, tweet_df = emoji_40k, ntweets = 10000, num_words = 500)
      auc_vec = append(auc_vec, temp_result$auc)
      acc_vec = append(acc_vec, temp_result$acc)
      Sys.time()-a
      beepr::beep()
    }
    df = as.data.frame(cbind(penalty_vec, auc_vec, acc_vec))
    
    p = ggplot(data = df, aes(penalty_vec))
    p = p + geom_line(aes(y = auc_vec), colour = "red")
    p = p + geom_line(aes(y = acc_vec), colour = "blue")
    p = p + scale_x_log10()
    p
    
    # FIRST TEST SHOWS OPTIMAL PENALTY NEAR 0.1ish (1000 tweets, 100 words)
    # penalty = c(1e-04 1e-03 1e-02 1e-01 1e+00 1e+01 1e+02 1e+03 1e+04)   <- Optimal is penalty value of 0.1 <=> alpha = 2800
    # auc = c(0.5109797 0.5285311 0.7209580 0.7444492 0.7240016 0.7317099 0.7116334 0.7332342 0.7189718)
    # acc = c(0.5088333 0.5188333 0.6393333 0.6785833 0.6558333 0.6685000 0.6440833 0.6599167 0.6515000)


    
    # Second TEST SHOWS OPTIMAL PENALTY NEAR 0.1ish (1000 tweets, 100 words)
    # penalty = c(11e-04 5e-04 1e-03 5e-03 1e-02 5e-02 1e-01 5e-01 1e+00 5e+00 1e+01 5e+01 
    #             1e+02 5e+02 1e+03 5e+03 1e+04)   <- Optimal is penalty value of 0.1 <=> alpha = 2800
    # auc = c(0.7327993, 0.7408514, 0.7506467, 0.7731706, 0.7757714, 0.7771006, 0.7773481, 0.7771934, 
    #         0.7777207, 0.7780344, 0.7772277, 0.7770729, 0.7781652, 0.7779355, 0.7779633,0.7772700, 0.7778815)
    # acc = c(0.6647667, 0.7044000, 0.7071333, 0.7091667, 0.7081000, 0.7089000, 0.7097333, 0.7097333, 
    #         0.7100667, 0.7092000, 0.7091667, 0.7098000, 0.7089667, 0.7090000, 0.7095000, 0.7090333, 0.7086667)
    
    
    # RESULTS FROM UNIGRAM ARE JUST AS GOOD!
    # 0.7776902
    # 0.7053333
    
    # > as.numeric(aucmax_imbalance$auc); max_accmax_imbalance
    # 0.7802656
    # 0.711
    
    
    
    
  # Compare above to unigram and max_difference
    
    # alpha_unigram: NDSI 500 ----
    emoji_term_freq = make_term_freq(emoji_40k, unigram_lexicon[1:500,])
    model_alpha_unigram = randomForest(polarity~.,data = emoji_term_freq[sample(ndsi_indices,ntest),c(1,7:506)])
    save(model_alpha_unigram, file = "~/Desktop/Documents/GitRepos/LAR/optimize_alpha/model_alpha_unigram.RData")
    test_phatunigram = predict(model_alpha_unigram, newdata = emoji_term_freq[-ndsi_indices,c(1,7:506)], type = "prob")
    aucunigram = roc(emoji_term_freq$polarity[-ndsi_indices],test_phatunigram[,2])
    predunigram = prediction(test_phatunigram[,2], emoji_40k$polarity[-ndsi_indices])
    acc_listunigram = performance(predunigram,"acc")
    max_accunigram = max(unlist(acc_listunigram@y.values))
    beepr::beep(3)
    
    # alpha_difference: NDSI 500 ----
    emoji_term_freq = make_term_freq(emoji_40k, max_imbalance_lexicon[1:500,])
    model_alpha_max_imbalance = randomForest(polarity~.,data = emoji_term_freq[sample(ndsi_indices,ntest),c(1,7:506)])
    save(model_alpha_max_imbalance, file = "~/Desktop/Documents/GitRepos/LAR/optimize_alpha/model_alpha_max_imbalance.RData")
    test_phatmax_imbalance = predict(model_alpha_max_imbalance, newdata = emoji_term_freq[-ndsi_indices,c(1,7:506)], type = "prob")
    aucmax_imbalance = roc(emoji_term_freq$polarity[-ndsi_indices],test_phatmax_imbalance[,2])
    predmax_imbalance = prediction(test_phatmax_imbalance[,2], emoji_40k$polarity[-ndsi_indices])
    acc_listmax_imbalance = performance(predmax_imbalance,"acc")
    max_accmax_imbalance = max(unlist(acc_listmax_imbalance@y.values))
    beepr::beep(3)


# TEST MOVING AWAY FROM NDSI SCORE: RESULTS AREN'T THAT GREAT.
  
  # alpha_unigram: NDSI 500 ----
  # emoji_term_freq = make_term_freq(emoji_40k, unigram_lexicon)
  model_alpha_unigram = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1,7:506)])
  save(model_alpha_unigram, file = "~/Desktop/Documents/GitRepos/LAR/optimize_alpha/model_alpha_unigram.RData")
  test_phatunigram = predict(model_alpha_unigram, newdata = emoji_term_freq[-indices,c(1,7:506)], type = "prob")
  aucunigram = roc(emoji_term_freq$polarity[-indices],test_phatunigram[,2])
  predunigram = prediction(test_phatunigram[,2], emoji_40k$polarity[-indices])
  acc_listunigram = performance(predunigram,"acc")
  max_accunigram = max(unlist(acc_listunigram@y.values))
  
  # alpha_difference: NDSI 500 ----
  # emoji_term_freq = make_term_freq(emoji_40k, max_imbalance_lexicon)
  model_alpha_max_imbalance = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1,7:506)])
  save(model_alpha_max_imbalance, file = "~/Desktop/Documents/GitRepos/LAR/optimize_alpha/model_alpha_max_imbalance.RData")
  test_phatmax_imbalance = predict(model_alpha_max_imbalance, newdata = emoji_term_freq[-indices,c(1,7:506)], type = "prob")
  aucmax_imbalance = roc(emoji_term_freq$polarity[-indices],test_phatmax_imbalance[,2])
  predmax_imbalance = prediction(test_phatmax_imbalance[,2], emoji_40k$polarity[-indices])
  acc_listmax_imbalance = performance(predmax_imbalance,"acc")
  max_accmax_imbalance = max(unlist(acc_listmax_imbalance@y.values))
  beepr::beep(3)
  
  as.numeric(aucunigram$auc); max_accunigram
  as.numeric(aucmax_imbalance$auc); max_accmax_imbalance
  
  # RESULTS FROM 500 WORDS, 28000 TWEETS
  
  # > as.numeric(aucunigram$auc); max_accunigram
  # [1] 0.7800845
  # [1] 0.7243333
  # >   as.numeric(aucmax_imbalance$auc); max_accmax_imbalance
  # [1] 0.7803176
  # [1] 0.72375
  
  # NOT A BIG DIFFERENCE BETWEEN THE TWO METHODS! GRAR!

