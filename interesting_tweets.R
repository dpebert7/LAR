# David Ebert
# 7 April 2017
# Search for interesting tweets


all_august_pred_import = read_feather(path = "~/Desktop/Huang Research/LAR_Data/all_august_pred.feather")


sad_tweets = as.data.frame(all_august_pred_import[all_august_pred_import$pred_polarity<0.2,])
happy_tweets = as.data.frame(all_august_pred_import[all_august_pred_import$pred_polarity>0.8,])

rm(all_august_pred_import)


happy_tweets[happy_tweets$afinn_score<(-5),c("text", "id_str")]
sad_tweets[sad_tweets$afinn_score>3 & sad_tweets$pred_polarity<0.1, c("text", "id_str")]




interesting_tweet_ids = c(766353525349552128, 769996761700110338, 762708224592060416, 768182827120635904, 
                          768566926746320897, 760085125480652800, 760985416975560704, 
                          770519433391529984, 759980312495476736, 760005240552689664, 760956782248210432, 760590171372859392,
                          760428715557781504, 760428484820738048, 760393903677648896, 760396201787166720, 760391955746402304, 
                          760381882655608832, 760371388678938624, 760046972610879488, 760048702002044930, 760030424324112384)

all_august_pred_import = read_feather(path = "~/Desktop/Huang Research/LAR_Data/all_august_pred.feather")


all_august_pred_import[all_august_pred_import$id_str %in% interesting_tweet_ids,]
write.csv(all_august_pred_import[all_august_pred_import$id_str %in% interesting_tweet_ids,], "interesting_tweets.csv")
