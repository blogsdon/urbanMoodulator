library(twitteR)
library(wordcloud)
library(plyr)
library(ggplot2)
setwd("~/Documents/Seattle/Twitter/")
source("twitterFuns.R")

test <- userTimeline("zachstednick", n = 3200, includeRts = TRUE)

kobe <- userTimeline("kobebryant", n = 3200, includeRts = TRUE)
tweetCloud(kobe, maxwords=160)

mike <- userTimeline("mayormcginn", n = 3200, includeRts = TRUE)
tweetCloud(mike, maxwords = 150)

seattlish <- userTimeline("seattlish", n = 3200, includeRts = TRUE)
tweetCloud(seattlish, maxwords = 160)




mike <- userTimeline("mayormcginn", n = 3200, includeRts = TRUE)
# tweetCloud(mike, "mayormcginn", nmax = 70)
png(file = "mikementions.png", width = 1200, height = 900, pointsize = 30)
mentionCloud(mike, "mayormcginn", nmax = 70)
dev.off()

png(file = "mikehashtags.png", width = 1200, height = 900, pointsize = 30)
hashtagCloud(mike, "mayormcginn", nmax = 100)
dev.off()

hourlyStats(test)
hp <- hourlyStats(mike) + ggtitle("Twitter Activity of Former Seattle Mayor Mike McGinn")
hp
ggsave(file = "hourlymike.png", hp, width = 7, height = 5)


pm <- monthlyStats(mike)
pm <- pm + geom_vline(xintercept = as.numeric(as.POSIXct("2013-11-05")), linetype = "dashed") +
  ggtitle("Twitter Activity of Former Seattle Mayor Mike McGinn")
pm
ggsave(file = "monthlymike.png", pm, width = 7, height = 5)


tweets <- parseTweets(mike)
View(tweets[tweets$date >= "2013-01-01" & tweets$date < "2013-02-01" & !tweets$RT, ])
View(tweets[tweets$date >= "2013-04-01" & tweets$date < "2013-06-01" & !tweets$RT, ])
View(tweets[tweets$date >= "2015-03-01" & tweets$date < "2015-04-01" & !tweets$RT, ])
View(tweets[tweets$date >= "2015-05-01" & tweets$date < "2015-06-01" & !tweets$RT, ])


dset <- parseTweets(mike)
hourlydset <- ddply(dset, .(hour), function(x) {
  c("tweets" = sum(!x$RT),
    "retweets" = sum(x$RT))
})
hourlydset$rtfrac <- hourlydset$retweets / (hourlydset$retweets + hourlydset$tweets)


abc <- tstats("mayormcginn")
tweets <- abc[[1]]
# tweets <- arrange(tweets, -favorites)
tweets$hour <- lubridate::hour(tweets$time)






daily <- abc[[2]]
daily$pop <- daily$favorites + daily$retweets
ggplot(daily) + theme_bw() + 
  geom_point(aes(x = date, y = tweets, col = pop))


hourly <- ddply(tweets, .(hour), function(x) {
  c("tweeted" = sum(!x$RT),
    "retweeted" = sum(x$RT),
    "favorites" = sum(x$favorites[!x$RT]),
    "retweets" = sum(x$retweets[!x$RT]))
})

hourly$tweetFreq <- hourly$tweeted / sum(hourly$tweeted)
hourly$retweetFreq <- hourly$retweeted / sum(hourly$retweeted)


hlong <- reshape2::melt(hourly, id.vars = "hour")

ggplot(hlong[hlong$variable %in% c("tweeted", "retweeted"), ]) + theme_bw() + 
  geom_bar(aes(x = hour, y = value, fill = variable),stat = "identity", position = "dodge")



ggsave(file = "monthlymcginn.png", monthlyplot, width = 7, height = 5)





