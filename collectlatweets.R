# Ebert/Rider
# collectlatweets.R
# updated 11 May 2016


# This file runs the function keepCapturingTweets() continually on the school computer,
# collecting about 160k-180k tweets every 24 hours and storing it in a .json file 
# that is about 500MB in size.

# Tweet collection began January 2016. As of 11 May 2016, nearly 16 million tweets
# have been collected.

library(ROAuth)
library(streamR)
library(twitteR)



# Authenticate streamR
load("my_oauth.Rdata") # Note to avoid reusing credentials, "my_oauth.Rdata" should only be 
# used on the school computer

# Test streamR
sampleStream(file.name = "junk.json",oauth = my_oauth, timeout = 5)


# Authenticate twitteR
consumerKey <- "fLbX5nkO6ehC0mqt1FqDsZjrC"
consumerSecret <- "usvekPy7GEen6rXdHRhHlXcV9SHTWbAQYDVFqmKIjRviA8kOaj"
accesToken <- "725384302171582464-lD42S9BHMhOTFsVkIu9wvIqaRCQddA2"
accessSecret <- "suAGmcm15fnyciMbneQ6ahTfnoXW9eYCG0NCLGzx2WGN5"
setup_twitter_oauth(consumerKey, consumerSecret, accesToken, accessSecret)
1

# Test twitteR
tweet("Test Tweeeet!")



keepCapturingTweets = function(){
  require(streamR)
  require(rjson)
  i = Sys.Date()
  a = Sys.time()
  sum = 14756972 # estimated number of tweets as of May 3, based on 44.2 GB of tweets
  
  while(1!=0){
    print(Sys.time())
    temp.file.json = paste(Sys.Date(), ".json", sep = "")    
    
    while (Sys.Date()==i){
      filterStream(file.name = temp.file.json, # Save tweets in temporary .json file
                   # Note that this file isn't overwritten after timeout
                   track = c(""), # Collect any tweets from stream; no search term
                   language = "en", # English tweets
                   location = c(-119, 33, -117, 35), # LA county coordinates. 
                   # Note that some tweets just outside the location area may also be collected.
                   timeout = 1800, # Keep connection alive for up to 30 minutes at a time
                   oauth = my_oauth) # Use my_oauth file as the OAuth credentials
      Sys.sleep(1) #pause very briefly before reopening stream
    }
    
    # parse temp.json into R
    print("Posting to Twitter")
    temp.df <- parseTweets(temp.file.json, simplify = FALSE)
    sum = sum + nrow(temp.df)
    tweet_text = paste(nrow(temp.df), " new geotagged tweets collected from LA county. Total: ", 
                       sum,".", sep = "")
    tweet(tweet_text)
    i = Sys.Date() #increment i to new day
  }
}

keepCapturingTweets()