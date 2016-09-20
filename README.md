## Rsentiment

Polarity analysis of Tweets from LA county for purpose of studying land use. Tweet collection and analysis completed in R using streamR, twitteR, and randomforest. Validation data from http://help.sentiment1.com/for-students/.

trainedClassifier.R creates trained classifiers for classifying LA tweets. The code is founded on crawfordCode.R

secondR.R is the most current script for the lexicon approach to classifying tweets.

functios.R includes commonly used functions and lexicons

semisuper.R creates a semi-supervised training set from previously collected LACounty tweets. It also creates a word cloud of happy and sad terms from the semi-supervised training set.

supervisedLAtweets.R is a failed attempt to collect old tweets from LA county. It is replaced by semisuper.R

crawfordCode.R is code from Dr. Crawford's Data Mining 2 class. It is the foundation of trainedClassifier.R

keepcapturingLAtweets.R continuously captures geotagged tweets from LA county. The most up-to-date version of this script is running on the shared school computer.

visualizations.R provides visualizes data from LA2014 and the classifiers using ggplot2

