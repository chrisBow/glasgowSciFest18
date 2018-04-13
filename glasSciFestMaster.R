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



