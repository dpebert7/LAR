# David Ebert
# September 13, 2016

source("functions.R")


# Import NEW 2016 tweets. Then analyze them.

new_data_path = "~/Desktop/Huang Research/September_LaPY_data/"
feather_file_path = "~/Desktop/Huang Research/September_LaPY_data/feather_data/"

# use import_tweets_from_json() from other functions.R to read .json files into cleaned .feather files
a = Sys.time()
import_tweets_from_json()
Sys.time()-a # takes about 30 minutes for a week's worth of tweets

# Read (currently one) cleaned feather file back into R
library(feather)
x = read_feather(path = paste(feather_file_path, "1.feather", sep = "")) # This runs fast... sometimes...
emoticon = extract_emoticon_tweets(x)
ndsi_lexicon = emoticon_to_rf_classifier(emoticon)
emoticon_term_freq = make_term_freq(emoticon, ndsi_lexicon)
