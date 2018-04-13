# Glasgow Science Festival 2018 - Fashion, sustainability and the environment

# Research question - do different high street clothing stores discuss sustainability in the environoment to differing degress in their social media marketing?

# Working hypothesis - Different brands will discuss sustainability and the environment to differing degrees

# Follow up question ideas: differences between demographics (price, gender, age)? Differences in how they are talked about?

# Brands chosen - Next, Gap, Zara, Primark, Monsoon





# load required packages

require(dplyr)
require(tidyr)
require(tidytext)
require(ggplot2)
require(twitteR)

# create account lists

twitAcList <- c("@nextofficial", "@UKGap", "@Zara", "@Primark", "@MonsoonUK")

fbAcList <- c("nextofficial", "UKGap", "Zara", "Primark", "MonsoonUK")


# query twitter API using userTimeline funcxtion to create dataframe of brand tweets

tlTweets <- userTimeline("@nextofficial", n = 3200)
timelineFrame <- twListToDF(tlTweets)

for(account in twitAcList) {
  tlTweets <- userTimeline(account, n = 3200)
  tlFrame <- twListToDF(tlTweets)
  timelineFrame <- tlFrame %>%
    bind_rows(timelineFrame)
}

# identify replies to other tweets as 'Reply' and non-replies [marketing messages] as 'Message'

timelineFrame$replyToSN[is.na(timelineFrame$replyToSN)] <- 0
timelineFrame$replyToSN <- ifelse(timelineFrame$replyToSN != 0, "Reply", "Message")

print(table(timelineFrame$replyToSN, timelineFrame$screenName))

# simplify dataframe

timeFrame <- timelineFrame %>%
  select(text, favoriteCount, replyToSN, created, screenName, retweetCount)

# separate date from date plus time

timeFrame$createdChar <- as.character(timeFrame$created)

timeFrame$date <- as.Date(sapply(timeFrame$createdChar, 
                         FUN = function(x) {
                           strsplit(x, " ")[[1]][1]
                           }
                         ))

# social media activity overview

tweetDates <- timeFrame %>%
  group_by(date, screenName) %>%
  summarise(tweets = n()) %>%
  filter(date > "2018-01-01")

ggplot(tweetDates, aes(x = date, y = tweets, colour = screenName)) + geom_line()

tweetDatesMessages <- timeFrame %>%
  filter(replyToSN == "Message") %>%
  group_by(date, screenName) %>%
  summarise(tweets = n()) %>%
  filter(date > "2018-01-01")

ggplot(tweetDatesMessages, aes(x = date, y = tweets, colour = screenName)) + geom_line()
ggplot(tweetDatesMessages, aes(x = screenName, y = tweets)) + geom_boxplot()
