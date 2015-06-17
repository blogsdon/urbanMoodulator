library(twitteR)

twitteR::setup_twitter_oauth(consumer_key = "z0voP3jiUq5KLx0FYv8pJiWOO",
                             consumer_secret = "JQrocM0zjboCtBvmGX0buqhrYOmym2oIuS3sVdEXJBpBH1LSWX",
                             access_token = "496598655-HKVW7FO4sudl9w93jQkzSvWcNGV2yKT1c4Oda6gs",
                             access_secret = "fTUp14vSy7jn65PaLHv4nY1V2zj2JpZ4Vvn5qoIcDPupF")




parseTweets <- function(tweets) {
  dset <- do.call('rbind', lapply(tweets, function(x) {
    data.frame("time" = x$created,
               "favorites" = x$favoriteCount,
               "retweets" = x$retweetCount,
               "tweet" = x$text,
               "RT" = x$isRetweet)  
  }))
  dset$time <- lubridate::with_tz(dset$time, "America/Los_Angeles")
  dset$hour <- lubridate::hour(dset$time)
  dset$date <- as.Date(dset$time)
  dset
}


hourlyStats <- function(tweets) {
  dset <- parseTweets(tweets)
  
  hourly <- ddply(dset, .(date, hour), function(x) {
    c("tweets" = sum(!x$RT),
      "retweets" = sum(x$RT))
  })
  hourlySums <- ddply(dset, .(hour), function(x) {
    c("tweets" = sum(!x$RT),
      "retweets" = sum(x$RT))    
  })
  hlong <- reshape2::melt(hourlySums, id.vars = "hour")
  
  ggplot(hlong) + theme_bw() + 
    geom_bar(aes(x = hour, y = value, fill = variable), stat = "identity", position = "dodge") +
    xlab("Hour of Day") + ylab("Count") + 
    ggtitle("Twitter Activity") +
    scale_fill_discrete(name = "Type", labels = c("Tweet", "Retweet")) +
    scale_x_continuous(breaks = c(0, 5, 8, 12, 15, 18, 22), 
                       labels = c("Midnight", "5am", "8am", "Noon", "3pm", "6pm", "10pm"))
}


monthlyStats <- function(tweets) {
  
  tweets <- parseTweets(tweets)
  
  tweets$month <- format(tweets$time, "%B %Y")
  monthly <- ddply(tweets, .(month), function(x) {
    c("tweeted" = sum(!x$RT),
      "retweeted" = sum(x$RT),
      "favorites" = sum(x$favorites[!x$RT]),
      "retweets" = sum(x$retweets[!x$RT]))  
  })
  monthly$date <- lubridate::dmy(paste("1", monthly$month))
  monthly$popularity <- monthly$favorites + monthly$retweets
  monthly$pop2 <- log(monthly$popularity)
  print(monthly)
  mlong <- reshape2::melt(monthly, id.vars = c("month", "date", "popularity", "pop2"))
  
  
  monthlyplot <- ggplot(mlong[mlong$variable %in% c("tweeted", "retweeted"), ]) + theme_bw() + 
    geom_bar(aes(x = date, y = value, fill = variable, alpha = popularity), position = "dodge", stat = "identity") +
    scale_alpha_continuous(name = "Favs + Retweets", range = c(.2, 1), trans = "log", breaks = c(20, 200, 2000), labels = c(20, 200, 2000)) +
    scale_fill_discrete(name = "Type", labels = c("Tweets", "Retweets")) +
    ggtitle("Twitter Activity of Seattle Mayor Mike McGinn") +
    xlab("Date") + ylab("Count")
  monthlyplot
}


extractMentions <- function(tweets) {
  unlist(lapply(tweets, function(x) {
    tmp <- strsplit(x$text, split = " ")
    ats <- tmp[[1]][grep("^@", tmp[[1]])]
    ats <- gsub("^([^:]+):*$", "\\1", ats)
    ats <- gsub("[^A-Za-z0-9@]", "", ats)
  }))
}

extractHashtags <- function(tweets) {
  unlist(lapply(tweets, function(x) {
    tmp <- strsplit(x$text, split = " ")
    hs <- tmp[[1]][grep("^#[^ ]", tmp[[1]])]
    hs <- gsub("^([^:]+):*$", "\\1", hs)
    hs <- gsub("[^A-Za-z0-9#]", "", hs)
    if(length(grep("^#$", hs))) print(x$text)
    hs
  }))  
}

tweetCloud <- function(tweets, ntweets = 3200, maxwords = 160) {
  texts <- unlist(lapply(tweets, function(x) x$text))
  allText <- paste(texts, collapse = " ")
  allText <- iconv(allText, "ASCII", "UTF-8", sub="")
  allText <- gsub("&amp", "", allText)
  allText <- gsub("\\^[^ ]+", "", allText)
  wordcloud(allText, use.r.layout = TRUE, scale = c(2, .8),
            colors = brewer.pal(8, "Dark2"), max.words = maxwords, random.order = FALSE)
  
}

# tweetCloud <- function(tweets, user, nmax = 100) {
#   mentions <- extractMentions(tweets)
#   hashtags <- extractHashtags(tweets)
#   all <- c(mentions, hashtags)
#   self <- grep(user, all)
#   if(length(self)) {
#     all <- all[-self]
#   }
#   alltab <- sort(table(all), decreasing = TRUE)
#   
#   all <- all[all %in% names(alltab)[1:nmax]]
#   
#   wordcloud(all, use.r.layout = TRUE, scale = c(2, .8),
#             colors = brewer.pal(8, "Dark2"), min.freq = 1)  
# }


mentionCloud <- function(tweets, user, nmax = 100) {
  mentions <- extractMentions(tweets)
  all <- c(mentions)
  self <- grep(user, all, ignore.case = TRUE)
  if(length(self)) {
    all <- all[-self]
  }
  alltab <- sort(table(all), decreasing = TRUE)
  
  all <- all[all %in% names(alltab)[1:nmax]]
  
  wordcloud(all, use.r.layout = TRUE, scale = c(2, .8),
            colors = brewer.pal(8, "Dark2"), min.freq = 1, random.order = FALSE)  
}

hashtagCloud <- function(tweets, user, nmax = 100) {
  hashtags <- extractHashtags(tweets)
  all <- c(hashtags)
  self <- grep(user, all)
  if(length(self)) {
    all <- all[-self]
  }
  alltab <- sort(table(all), decreasing = TRUE)
  
  all <- all[all %in% names(alltab)[1:nmax]]
  
  wordcloud(all, use.r.layout = TRUE, scale = c(2, .8),
            colors = brewer.pal(8, "Dark2"), min.freq = 1, random.order = FALSE)  
}



# Customized wordcloud to leave @s and hashtags
wordcloud <- function (words, freq, scale = c(4, 0.5), min.freq = 3, max.words = Inf, 
                       random.order = TRUE, random.color = FALSE, rot.per = 0.1, 
                       colors = "black", ordered.colors = FALSE, use.r.layout = FALSE, 
                       fixed.asp = TRUE, ...) 
{
  if (!fixed.asp && rot.per > 0) 
    stop("Variable aspect ratio not supported for rotated words. Set rot.per=0.")
  tails <- "g|j|p|q|y"
  last <- 1
  nc <- length(colors)
  if (missing(freq)) {
    if (!require("tm")) 
      stop("freq must either be non-missing, or the tm package must be available")
    if (is.character(words) || is.factor(words)) {
      corpus <- Corpus(VectorSource(words))
      #corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, function(x) removeWords(x, 
                                                       stopwords()))
    }
    else corpus <- words
    tdm <- TermDocumentMatrix(corpus)
    freq <- slam::row_sums(tdm)
    words <- names(freq)
  }
  if (ordered.colors) {
    if (length(colors) != 1 && length(colors) != length(words)) {
      stop(paste("Length of colors does not match length of words", 
                 "vector"))
    }
  }
  if (min.freq > max(freq)) 
    min.freq <- 0
  overlap <- function(x1, y1, sw1, sh1) {
    #if (!use.r.layout) 
    #  return(.overlap(x1, y1, sw1, sh1, boxes))
    s <- 0
    if (length(boxes) == 0) 
      return(FALSE)
    for (i in c(last, 1:length(boxes))) {
      bnds <- boxes[[i]]
      x2 <- bnds[1]
      y2 <- bnds[2]
      sw2 <- bnds[3]
      sh2 <- bnds[4]
      if (x1 < x2) 
        overlap <- x1 + sw1 > x2 - s
      else overlap <- x2 + sw2 > x1 - s
      if (y1 < y2) 
        overlap <- overlap && (y1 + sh1 > y2 - s)
      else overlap <- overlap && (y2 + sh2 > y1 - s)
      if (overlap) {
        last <<- i
        return(TRUE)
      }
    }
    FALSE
  }
  ord <- rank(-freq, ties.method = "random")
  words <- words[ord <= max.words]
  freq <- freq[ord <= max.words]
  if (ordered.colors) {
    colors <- colors[ord <= max.words]
  }
  if (random.order) 
    ord <- sample.int(length(words))
  else ord <- order(freq, decreasing = TRUE)
  words <- words[ord]
  freq <- freq[ord]
  words <- words[freq >= min.freq]
  freq <- freq[freq >= min.freq]
  if (ordered.colors) {
    colors <- colors[ord][freq >= min.freq]
  }
  thetaStep <- 0.1
  rStep <- 0.05
  plot.new()
  op <- par("mar")
  par(mar = c(0, 0, 0, 0))
  if (fixed.asp) 
    plot.window(c(0, 1), c(0, 1), asp = 1)
  else plot.window(c(0, 1), c(0, 1))
  normedFreq <- freq/max(freq)
  size <- (scale[1] - scale[2]) * normedFreq + scale[2]
  boxes <- list()
  for (i in 1:length(words)) {
    rotWord <- runif(1) < rot.per
    r <- 0
    theta <- runif(1, 0, 2 * pi)
    x1 <- 0.5
    y1 <- 0.5
    wid <- strwidth(words[i], cex = size[i], ...)
    ht <- strheight(words[i], cex = size[i], ...)
    if (grepl(tails, words[i])) 
      ht <- ht + ht * 0.2
    if (rotWord) {
      tmp <- ht
      ht <- wid
      wid <- tmp
    }
    isOverlaped <- TRUE
    while (isOverlaped) {
      if (!overlap(x1 - 0.5 * wid, y1 - 0.5 * ht, wid, 
                   ht) && x1 - 0.5 * wid > 0 && y1 - 0.5 * ht > 
            0 && x1 + 0.5 * wid < 1 && y1 + 0.5 * ht < 1) {
        if (!random.color) {
          if (ordered.colors) {
            cc <- colors[i]
          }
          else {
            cc <- ceiling(nc * normedFreq[i])
            cc <- colors[cc]
          }
        }
        else {
          cc <- colors[sample(1:nc, 1)]
        }
        text(x1, y1, words[i], cex = size[i], offset = 0, 
             srt = rotWord * 90, col = cc, ...)
        boxes[[length(boxes) + 1]] <- c(x1 - 0.5 * wid, 
                                        y1 - 0.5 * ht, wid, ht)
        isOverlaped <- FALSE
      }
      else {
        if (r > sqrt(0.5)) {
          warning(paste(words[i], "could not be fit on page. It will not be plotted."))
          isOverlaped <- FALSE
        }
        theta <- theta + thetaStep
        r <- r + rStep * thetaStep/(2 * pi)
        x1 <- 0.5 + r * cos(theta)
        y1 <- 0.5 + r * sin(theta)
      }
    }
  }
  par(mar = op)
  invisible()
}
