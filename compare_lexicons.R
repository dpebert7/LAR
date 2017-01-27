# 13 January 2017
# Compare 4 lexicons over the entire emoji/emoticon data set

source("functions.R") #get cleaning function, AFINN_lexicon

library(feather)
library(pROC)
library(ROCR)

# Get all of the emoji data (352080 rows) with lexicon scores
emoji_df_no_stem = read_feather(path = "~/Desktop/Huang Research/LAR_Data/compare_lexicons/feather_data/2016-08/emoji_df_no_stem.feather")

# afinn_score
roc(emoji_df_no_stem$polarity, emoji_df_no_stem$afinn_score)$auc
max(unlist(performance(prediction(emoji_df_no_stem$afinn_score, emoji_df_no_stem$polarity),"acc")@y.values))


# opinionfinder_score
roc(emoji_df_no_stem$polarity, emoji_df_no_stem$opinionfinder_score)$auc
max(unlist(performance(prediction(emoji_df_no_stem$opinionfinder_score, emoji_df_no_stem$polarity),"acc")@y.values))


# nrc_score
roc(emoji_df_no_stem$polarity, emoji_df_no_stem$nrc_score)$auc
max(unlist(performance(prediction(emoji_df_no_stem$nrc_score, emoji_df_no_stem$polarity),"acc")@y.values))


# anew_score
roc(emoji_df_no_stem$polarity, emoji_df_no_stem$anew_score)$auc
max(unlist(performance(prediction(emoji_df_no_stem$anew_score, emoji_df_no_stem$polarity),"acc")@y.values))


# RESULTS FROM COMPARING LEXICONS: 352080 EMOJI/EMOTICON TWEETS
# 
# LEXICON		    AUC	    ACCURACY
# AFINN		      0.690	  64.3\%
# OpinionFinder	0.647	  60.9\%
# NRC		        0.644	  60.7\%
# ANEW		      0.696	  63.8\%



















#10 December 2016
# Compare 4 lexicons

# libraries, functions, and directory, cleaning function , AFINN_lexicon, etc----
source("functions.R") #get cleaning function, AFINN_lexicon


# EMOJI DATA: 40000 equally balanced semi-supervised tweets from LA August 2016  
  a = Sys.time()
  import_tweets_from_json(months_to_import = "2016-08/", 
                          base_file_path = "~/Desktop/Huang Research/LAR_Data/compare_lexicons",
                          all_four_lexicons = TRUE)
  Sys.time()-a

# This set is created in import_new script
  a = Sys.time()
  all_august_tweets = read_feather(path = "~/Desktop/Huang Research/LAR_Data/compare_lexicons/feather_data/2016-08/1.feather")
  all_august_tweets = rbind(all_august_tweets, read_feather(path = "~/Desktop/Huang Research/LAR_Data/compare_lexicons/feather_data/2016-08/2.feather"))
  all_august_tweets = rbind(all_august_tweets, read_feather(path = "~/Desktop/Huang Research/LAR_Data/compare_lexicons/feather_data/2016-08/3.feather"))
  
  write_feather(all_august_tweets, path = "~/Desktop/Huang Research/LAR_Data/compare_lexicons/feather_data/2016-08/all_august.feather")
  all_august_tweets = read_feather(path = "~/Desktop/Huang Research/LAR_Data/compare_lexicons/feather_data/2016-08/all_august.feather")
  # 4651427 tweets from August
  Sys.time()-a
  
  emoji_df_no_stem = extract_emoticon_tweets(all_august_tweets)
  write_feather(emoji_df_no_stem, path = "~/Desktop/Huang Research/LAR_Data/compare_lexicons/feather_data/2016-08/emoji_df_no_stem.feather")
  emoji_df_no_stem = read_feather(path = "~/Desktop/Huang Research/LAR_Data/compare_lexicons/feather_data/2016-08/emoji_df_no_stem.feather")




# TEST/VALIDATION DATA: sentiment140
  sent140 = get_sent140()

# Calculate AFINN scores using lexicon_sentiment_score (which includes a negation stopper and uses MATCH instead of PMATCH)
  #sent140 data
    auc(roc(sent140$polarity, sent140$afinn_score))

  #emoji data
    AFINN_lexicon = AFINN_lexicon[AFINN_lexicon$word != "sadtoken" & AFINN_lexicon$word != "happytoken",]
    emoji_df_no_stem$AFINN.score = lexicon_sentiment_score(emoji_df_no_stem$text)
    #optimize.cutoff(score.vec = emoji_df_no_stem$AFINN.score, polarity.vec = emoji_df_no_stem$polarity)
    auc(roc(emoji_df_no_stem$polarity, emoji_df_no_stem$afinn_score))
    

# Calculate OpinionFinder (formerly known as WIEBE) scores for emoji_df_no_stem and sent140 using lexicon_sentiment_score
  #sent140 data
    auc(roc(sent140$polarity, sent140$OpinionFinder))

  #emoji_df_no_stem data
    emoji_df_no_stem$OpinionFinder.score = lexicon_sentiment_score(emoji_df_no_stem$clean, lexicon = OpinionFinder)
    optimize.cutoff(score.vec = emoji_df_no_stem$OpinionFinder.score, polarity.vec = emoji_df_no_stem$polarity)


# Calculate NRC (formerly known as EmoLex) scores for emoji_df_no_stem and sent140 using lexicon_sentiment_score
  #sent140 data
    auc(roc(sent140$polarity, sent140$OpinionFinder_score))
    
  #emoji_df_no_stem data
    emoji_df_no_stem$NRC.score = lexicon_sentiment_score(emoji_df_no_stem$clean, lexicon = NRC)
    optimize.cutoff(score.vec = emoji_df_no_stem$NRC.score, polarity.vec = emoji_df_no_stem$polarity)


# Calculate ANEW scores for emoji_df_no_stem and sent140 using lexicon_sentiment_score
  # Note that ANEW scores range from 1 to 9, so the cutoff needs to be calculated more carefully
  #sent140 data
    auc(roc(sent140$polarity, sent140$ANEW_score))
    
  #emoji_df_no_stem data
    emoji_df_no_stem$ANEW.score = lexicon_sentiment_score(emoji_df_no_stem$clean, lexicon = ANEW)
    optimize.cutoff(score.vec = emoji_df_no_stem$ANEW.score, polarity.vec = emoji_df_no_stem$polarity, min = 1, max = 4, step = 0.05)

    
    
    
    
    
    
    
    

# Calculate rf.polarity scores for emoji_df_no_stem and sent140 using lexicon_sentiment_score
  # Note that rf.polarity scores range from 0 to 1, so the cutoff needs to be calculated more carefully
    #sent140 data
    sent140$rf.score = classify.polarity.machine(sent140$clean, chunk.size = 100, model = rf.model)
    optimize.cutoff(score.vec = sent140$rf.score, polarity.vec = sent140$polarity, min = 0.4, max = 0.8, step = 0.001)
  
  #emoji_df_no_stem data
    emoji_df_no_stem$rf.score = classify.polarity.machine(emoji_df_no_stem$clean, chunk.size = 10000, model = rf.model)
    optimize.cutoff(score.vec = emoji_df_no_stem$rf.score, polarity.vec = emoji_df_no_stem$polarity, min = 0.4, max = 0.7, step = 0.005)


# Save data frames with scores
  save(sent140, file = paste(storage.directory,"sent140.with.lexicon.scores.RData", sep = "")) # save sent140 lexicon into memory
  save(emoji_df_no_stem, file = paste(storage.directory,"emoji_df_no_stem.with.lexicon.scores.RData", sep = "")) # save emoji_df_no_stem lexicon into memory








# Example ggplot showing separation
ggplot(sent140, aes(sent140[,"AFINN.score"], fill = as.factor(polarity))) + geom_density(alpha = .2, adjust = 1) + 
  theme(axis.text = element_text(size = 25),
        axis.title= element_text(size = 35),
        plot.title= element_text(size = 40),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 35),
        legend.position = "bottom") +
  scale_fill_manual(values = c(20,3),
                    labels = c(" negative   ", " positive"),
                    name = "Polarity:  ") +
  labs(x = "AFINN.score", y = "density") + ggtitle("AFINN.score densities over sent140")
ggsave(filename = "afinn.sent140.png", plot = last_plot(), width = 12, height = 10)

ggplot(emoji_df_no_stem, aes(emoji_df_no_stem[,"AFINN.score"], fill = as.factor(polarity))) + geom_density(alpha = .2, adjust = 7) +
  theme(axis.text = element_text(size = 25),
        axis.title= element_text(size = 35),
        plot.title= element_text(size = 40),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 35),
        legend.position = "bottom") +
  scale_fill_manual(values = c(20,3),
                    labels = c(" negative   ", " positive"),
                    name = "Polarity:  ") +
  labs(x = "AFINN.score", y = "density") + ggtitle("AFINN.score densities over emoji_df_no_stem") 
ggsave(filename = "afinn.emoji_df_no_stem.png", plot = last_plot(), width = 12, height = 10)


ggplot(sent140, aes(sent140[,"rf.score"], fill = as.factor(polarity))) + geom_density(alpha = .2) + 
  theme(axis.text = element_text(size = 25),
        axis.title= element_text(size = 35),
        plot.title= element_text(size = 40),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 35),
        legend.position = "bottom") +
  scale_fill_manual(values = c(20,3),
                    labels = c(" negative   ", " positive"),
                    name = "Polarity:  ") +
  labs(x = "rf.score", y = "density") + ggtitle("rf.score densities over sent140") 
ggsave(filename = "rf.sent140.png", plot = last_plot(), width = 12, height = 10)

ggplot(emoji_df_no_stem, aes(emoji_df_no_stem[,"rf.score"], fill = as.factor(polarity))) + geom_density(alpha = .2) + 
  theme(axis.text = element_text(size = 25),
        axis.title= element_text(size = 35),
        plot.title= element_text(size = 40),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 35),
        legend.position = "bottom") +
  scale_fill_manual(values = c(20,3),
                    labels = c(" negative   ", " positive"),
                    name = "Polarity:  ") +
  labs(x = "rf.score", y = "density") + ggtitle("rf.score densities over emoji_df_no_stem")
ggsave(filename = "rf.emoji_df_no_stem.png", plot = last_plot(), width = 12, height = 10)


# for loop iterating through all possible plots
classifiers = c("AFINN", "OpinionFinder", "NRC", "ANEW", "rf")
for(i in classifiers){
  model.name = paste(i, ".score", sep = "")
  print(ggplot(sent140, aes(sent140[,model.name], fill = as.factor(polarity))) + geom_density(alpha = .2) +
          labs(x = model.name, y = "density") + ggtitle(paste(model.name, "Densities over sent140")))
  print(ggplot(emoji_df_no_stem, aes(emoji_df_no_stem[,model.name], fill = as.factor(polarity))) + geom_density(alpha = .2) +
          labs(x = model.name, y = "density") + ggtitle(paste(model.name, "Densities over emoji_df_no_stem")))
}



