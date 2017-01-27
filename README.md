# LAR

Sentiment polarity analysis of tweets from Los Angeles County. These R scripts are used to (1) collect geotagged tweets from Los Angeles county; (2) clean, stem, and process tweets; (3) train and evaluate a semi-supervised random forest classifier; (4) classify the sentiment polarity of tweets from LA county; and (5) plot the tweets on a map of LA county. 


### Primary Scripts

- `animation.R`: Create animated map visualization of LA county. Images for animation are stored in the `images` directory.

- `collectlatweets.R`: Function for continuously collecting tweets from Los Angeles County.

- `functions.R`: Primary functions for import_new.R.

- `import_new.R`: Script for processing, cleaning, and classifying tweets.

- `lamap.R`: Make plots for tweets from August 2015.


### Additional Scripts

- `classify_sent140.R` creates a model for maximum effectiveness in classifying Sentiment140 tweets. Since there are relatively few tweets, this model is not required to take 

- `compare_lexicons.R` compares the results of four publicly-available lexicons. We find that the AFINN lexicon is most effective.

- `compare_models.R` is the main file for extensively comparing models built for the emoji data.

- `feature_selection.R` compares the efficacy of three types of model features: tweet attributes (URLs, hashtags, etc), AFINN lexicon scores, and NDSI word frequencies.

- `final_model.R`

- `ndsi_lexicon_results.txt` Compares the results of varying ways of creating the NDSI lexicon. We gravitated toward a maximum-imbalance lexicon rather than a maximum-frequency lexicon.

- `num_words_plot.R` compares the results of the NDSI lexicon over varying numbers of words. Though more words generally increases accuracy, there are diminishing returns after about 800 words are included in the model.

- `optimize_alpha.txt` is an old file used to optimize the alpha parameter used to create the NDSI lexicon.

- `testdata.manual.2009.06.14.csv` contains ~350 tweets for model vaidataion, also known as Sentiment140.

- The `lexicons` directory contains 4 publicly-available lexicons. It is not included in the GitHub repository.

- The `compare_models` directory stores multiple models used for comparisons. It is not included in the primary GitHub repository.


### Research

Follow tweet collection on Twitter: @tsutweets1. As of January 2017, we've collected over 60 million tweets from the Los Angeles County area over the course of a year.

Check out a <poster> or <beamer> explaining our research. 


### Contact

`david.ebert@go.tarleton.edu`

`parker.rider@go.tarleton.edu`
