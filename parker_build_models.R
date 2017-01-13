# Ebert
# 11 January 2017


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
  
  emoji_term_freq = make_term_freq(emoji_40k, max_imbalance_lexicon)
  save(emoji_term_freq, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/emoji_term_freq.RData")
  load(file = "~/Desktop/Documents/GitRepos/LAR/compare_models/emoji_term_freq.RData")
  
  
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
  # save(sent140, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/sent140.RData")
  # sent140_term_freq = make_term_freq(sent140, ndsi_lexicon_df)
  # save(sent140_term_freq, file = "~/Desktop/Documents/GitRepos/LAR/compare_models/sent140_term_freq.RData")
  # load(file = "~/Desktop/Documents/GitRepos/LAR/compare_models/sent140_term_freq.RData")


# Import data ----

  library(feather)
  library(ROCR)
  library(pROC)
  
  base_file_path = "~/Desktop/Documents/GitRepos/LAR/compare_models/"
  
  ndsi_lexicon_df = read_feather(path = paste(base_file_path, "ndsi_lexicon_df.feather", sep = ""))
  
  load(file = paste(base_file_path,"emoji_40k.feather", sep = ""))
  load(file = paste(base_file_path, "indices_40k.RData", sep = ""))
  load(file = paste(base_file_path, "emoji_term_freq.RData", sep = ""))
  
  load(file = paste(base_file_path, "indices.RData", sep = ""))
  
  load(file = paste(base_file_path, "sent140.RData", sep = ""))
  load(file = paste(base_file_path,"sent140_term_freq.RData", sep = ""))
  
  ntest = 28000 # Should be set to 28000 for paper tests; 4000 for testing NDSI
  
# Run Models
  
  build_ndsi_model = function(nwords, ntest = 2800){
    require(randomForest)
    print(paste("Building RF model with", nwords, "NDSI words and", ntest, "training tweets..."))
    rf_model = randomForest(polarity~.,data = emoji_term_freq[sample(indices,ntest),c(1,7:(6+nwords))])
    
    test_phat = predict(rf_model, newdata = emoji_term_freq[-indices,c(1,7:(6+nwords))], type = "prob")
    auc = roc(emoji_term_freq$polarity[-indices],test_phat[,2])
    pred = prediction(test_phat[,2], emoji_40k$polarity[-indices])
    acc_list = performance(pred,"acc")
    max_acc = max(unlist(acc_list@y.values))
    print(paste("       Emoji AUC:", as.numeric(auc$auc)))
    print(paste("  Emoji accuracy:", max_acc))
    
    test_phat = predict(rf_model, newdata = sent140_term_freq[-indices,c(1:(6+nwords))], type = "prob")
    auc = roc(sent140_term_freq$polarity[-indices],test_phat[,2])
    pred = prediction(test_phat[,2], sent140$polarity[-indices])
    acc_list = performance(pred,"acc")
    max_acc = max(unlist(acc_list@y.values))
    print(paste("     Sent140 AUC:", as.numeric(auc$auc)))
    print(paste("Sent140 accuracy:", max_acc))
  }

  build_ndsi_model(nwords = 100)



# Instructions: Run build_ndsi_model function for nwords = 100, 200, 300, ..., 1000. 
# Start with ntest = 300 to make sure it works, then crank it up to 28000








