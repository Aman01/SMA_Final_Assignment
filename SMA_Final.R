library(rtweet)
library(twitteR)
library(ggplot2)
library(dplyr)

#AppName
appname <- "celebizz"

#API key(application programming interface key)
key <- "v72kUGWHrDFKBT4XmJrFdlaFF"

#API secret key
secret <- "MBawOsmXVxtJEf086ZGz8Gt2hBIgIQx3yUMVmgY14HL7F0cyg7"

OauthToken = "897605731-927aP8civMPyeZCM7uZeVY8XJ944ZNEHtshAHrl3"
OauthaSecret = "vYBPe2GrfC32QTgbXctkMy8qAlmq9myVVbwj7QRRnKsW9"

setup_twitter_oauth(key, secret, OauthToken, OauthaSecret)

rt <- search_tweets(
  "#Paytm", n = 500, include_rts = FALSE
)
rt
rt$screen_name
rt$text
head(rt$text)
write.csv(rt$text,file = 'C:/R/tweets.csv',row.names = F)

## get user IDs of accounts following Vijay Shekhar
followers <- get_followers("@vijayshekhar", n = 200)
followers
followers_data <- lookup_users(followers$user_id)
followers_data
followers_data$screen_name
followers_data$text


## plot the frequency of tweets over time
tmls <- get_timelines(c("@vijayshekhar"), n = 1000)

tmls %>%
  dplyr::filter(created_at > "2018-01-01") %>%
  dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by Vijay Shekhar",
    subtitle = "Twitter status (tweet) counts aggregated by day from October/November 2017",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

#200 most recently favorited statuses by Vijay Shekhar
favourites <- get_favorites("@vijayshekhar", n = 200)
favourites
favourites$text


#Location Analysis

# how many locations are represented
length(unique(rt$location))

availableTrendLocations()
getTrends(2295411)
tweets= searchTwitter("Paytm",n=1000,geocode = "72.87,19.07,2000km",retryOnRateLimit = 200)
tweetsDF2= twListToDF(tweets)
tweets
head(tweetsDF2)
tweetsDF2$longitude= as.numeric(tweetsDF2$longitude)
tweetsDF2$longitude

#Sentiment Analysis

library("SnowballC")
library("tm")
library("twitteR")
library("syuzhet")

vjtweets <- userTimeline("@vijayshekhar", n=200)

n.tweet <- length(vjtweets)
tweets.df <- twListToDF(vjtweets) 
tweets.df
head(tweets.df)
head(tweets.df$text)

#Removing http, https, # and @
tweets.df2 <- gsub("http.*","",tweets.df$text)

tweets.df2 <- gsub("https.*","",tweets.df2)

tweets.df2 <- gsub("#.*","",tweets.df2)

tweets.df2 <- gsub("@.*","",tweets.df2)

head(tweets.df2)
tweets.df2

#Emotion score for each of the tweets
#'Syuzhet' breaks the emotion into 10 different emotions - 
#anger, anticipation, disgust, fear, joy, sadness, surprise, trust, negative and positive.

word.df <- as.vector(tweets.df2)

emotion.df <- get_nrc_sentiment(word.df)

emotion.df2 <- cbind(tweets.df2, emotion.df) 

head(emotion.df2)


sent.value <- get_sentiment(word.df)

#Most Positive tweets
most.positive <- word.df[sent.value == max(sent.value)]
most.positive

most.negative <- word.df[sent.value <= min(sent.value)] 
most.negative

sent.value


positive.tweets <- word.df[sent.value > 0]

head(positive.tweets)

