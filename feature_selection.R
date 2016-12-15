# Feature Selection

library(randomForest)
load(file = "~/Desktop/Huang Research/LAR_Data/feather_data/2016-08/rf_model.RData")
varImpPlot(rf_model$mode)

# Most important features:

# 1)  afinn_score (by a lot!)
# 2)  count_url
# 3)  count_usernames
# 4)  X3: thank
# 5)  X4: love
# 6)  count_hashtags
# 7)  X1: happi
# 8)  X355: use
# 9)  X261: met
# 10) X35: usernametoken

# Roughly speaking, it looks like the most helpful features are those that are common to a lot
# of tweets. The model may benefit by increasing the (alpha constant) penalty for tweets that
# occur infrequently.

library(feather)
ndsi_lexicon_df = read_feather(path = "~/Desktop/Huang Research/LAR_Data/feather_data/2016-08/ndsi_lexicon_df.feather")


rf_model$model$importance
ndsi_lexicon_df$importance = rf_model$model$importance[6:nrow(rf_model$model$importance),]
ndsi_lexicon_df$ndsi_rank = 1:500
  
library(plyr)
arrange(ndsi_lexicon_df,desc(importance))
arrange(ndsi_lexicon_df,importance)
