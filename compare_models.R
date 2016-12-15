# Ebert
# Compare Models


library(feather)

# Import non-stemmed emoji tweets and split into balanced 40 000 tweets
  emoji_df_no_stem = read_feather(path = "~/Desktop/Huang Research/LAR_Data/compare_lexicons/feather_data/2016-08/emoji_df_no_stem.feather")
  indices = as.integer(c(sample(1:nrow(emoji_df_no_stem)/2,20000), sample(((nrow(emoji_df_no_stem)+2)/2):nrow(emoji_df_no_stem),20000)))


  emoji_40k = emoji_df_no_stem[indices,]
  dim(emoji_40k)
  
  write_feather(emoji_40k, path = "~/Desktop/Documents/GitRepos/LAR/compare_models/emoji_40k.feather")
  emoji_40k = read_feather(path = "~/Desktop/Documents/GitRepos/LAR/compare_models/emoji_40k.feather")
  