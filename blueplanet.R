# load libraries
library(rtweet)
library(dplyr)
library(tidytext)
library(stringr)
library(janitor)
library(ggplot2)


# Getting tweets
tweetsBlueBig <- search_tweets(q = '#BluePlanet2', n = 18000, 
                            include_rts = FALSE, parse = TRUE)

# Clean tweets
cleanTweetsBlue <- janitor::remove_empty_cols(tweetsBlueBig)

# Tidy data
tidyTweetsBlue <- cleanTweetsBlue %>% 
  select(screen_name, user_id, created_at, status_id, text,
         retweet_count, favorite_count, hashtags, source)

write.csv(tweetsBlueBig, file = "tidyTweetsBlue.csv")

# Basic descriptions
# What is the most retweeted tweet?
mostRetweetedBlue <- tidyTweetsBlue %>% arrange(desc(retweet_count)) %>% 
                      select(text) %>% head(1)
getURLinsideTweet <- gsub(".*(https://)", "https://", mostRetweetedBlue$text)
browseURL(getURLinsideTweet)

mostRetweeted$text

# favourited
mostFavourited <- tidyTweetsBlue %>% arrange(desc(favorite_count)) %>%
  select(text) %>% head(1) 
getURLinsideFavTweet <- gsub(".*(https://)", "https://",mostFavourited$text)
browseURL(getURLinsideFavTweet)


# What are the most used hashtags?
tidy_hashtags <- tidyTweetsBlue %>% unnest(hashtags) 

tidy_hashtags <- tidy_hashtags %>%
  count(hashtags, sort = TRUE) %>% top_n(n = 10, wt = n) 

p <- tidy_hashtags %>%  filter(hashtags != "BluePlanet2")

t <- ggplot(data = p) + 
  geom_bar(aes(x = reorder(hashtags, -n), y = n,
               fill = rainbow(n=length(p$hashtags))), stat = 'identity') +
  ylab("Hashtag Count") + xlab("Hashtags")

t +  theme_bw() + theme(legend.position="none", axis.text.x = element_text(angle = 60, hjust = 1))
