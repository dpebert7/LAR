# David Ebert
# 6 April 2017
# Use best model to predict polarity of all August 2016 Tweets

source("functions.R")


################################
#### Load data frame of august Tweets with 5.6 million tweets; write all august tweets to feather file
################################

  # Data frame is called all_august_tweets
  load(file = "~/Desktop/Huang Research/LAR_Data/feather_data/2016-08/all_august_tweets.RData")
  
  


################################
# Load the best model from 10-fold Cross Validation
################################
  storage_directory = "~/Desktop/Huang Research/LAR_Data/feather_data/2016-08/"
  load(file = paste(storage_directory, "rf_model",i,".RData", sep = ""))
  
  for(i in 1:10){
    print(i)
    load(file = paste(storage_directory, "rf_model",i,".RData", sep = ""))
    print(rf_model$test_accuracy)
  }
  
  # Highest test accuracy is in model 2; so set i = 2 and call that the "best_model"
  load(file = paste(storage_directory, "rf_model",'2',".RData", sep = ""))
  best_model = rf_model
  
  # also need correct ndsi_lexicon_df
  load(file = paste(storage_directory, "ndsi_lexicon_df","2",".RData", sep = ""))
  

################################
# Classify Tweets
################################

  all_august_tweets$pred_polarity = classify.polarity.machine(tweet_df = all_august_tweets, 
                                                              chunk.size = 10000, 
                                                              ndsi_lexicon = ndsi_lexicon_df, 
                                                              model = best_model$model)
  
  # Write to file
  #write.csv(x = all_august_tweets, file = "~/Desktop/Huang Research/LAR_Data/all_august_pred.csv", row.names = FALSE)
  #write_feather(all_august_tweets, path = "~/Desktop/Huang Research/LAR_Data/all_august_pred.feather")
  #save(all_august_tweets, file = "~/Desktop/Huang Research/LAR_Data/all_august_pred.RData")
  beepr::beep(3)
  
  # Write all_august_pred to 2 shortened files
  #august_pred_import_1 = all_august_tweets[1:2000000,c("screen_name", "id_str", "lat", "lon", "afinn_score", "pred_polarity")]
  #august_pred_import_2 = all_august_tweets[2000001:nrow(all_august_tweets),c("screen_name", "id_str", "lat", "lon", "afinn_score", "pred_polarity")]
  #write.csv(x = august_pred_import_1, file = "~/Desktop/Huang Research/LAR_Data/august_pred_import_1.csv", row.names = FALSE)
  #write.csv(x = august_pred_import_1, file = "~/Desktop/Huang Research/LAR_Data/august_pred_import_2.csv", row.names = FALSE)
  beepr::beep(3)
  
  # re-import 
  all_august_pred_import = read_feather(path = "~/Desktop/Huang Research/LAR_Data/all_august_pred.feather")
  