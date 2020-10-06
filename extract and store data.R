library(twitteR)
library(ROAuth)
library(base64enc)
library(openssl)
library(httpuv)

api.key <- "OoKWJblfHg5AKLlhnHqwOOMh5"
api.secret <- "oRPbMSYHO6VcBERzUci5NH8rQbVkk7RDgSAszu8e1mUhpT3Snl"
acc.token <- "1307139322479554560-AtXS8nLHqNAOqqcJtYkotrv40tLesg"
acc.serect <- "AyFQmTf0fErd6jjIWS1OJP145bsKH7u9G65XoC2eZrUG5"
setup_twitter_oauth(api.key, api.secret, acc.token, acc.serect)

# extract data from Twitter APIS
date <- Sys.Date()
setwd("data collection")
for(i in 0:6){
  d <- date - i
  d2 <- d - 1
  d <- as.character(d)
  d2 <- as.character(d2)
  tweet1 <- searchTwitter("#Election2020 -filter:retweets", n = 500, lang = "en", since = d2, until = d)
  tweet2 <- searchTwitter("#TrumpPence2020 -filter:retweets", n =500, lang = "en", since = d2, until = d)
  tweet3 <- searchTwitter("#BidenHarris2020 -filter:retweets", n = 500, lang = "en", since = d2, until = d)
  name1 <- paste("election data", d2, ".csv")
  name2 <- paste("trump data", d2, ".csv")
  name3 <- paste("biden data", d2, ".csv")
  df1 <- twListToDF(tweet1)
  df2 <- twListToDF(tweet2)
  df3 <- twListToDF(tweet3)
  write.csv(df1,file = name1)
  write.csv(df2,file = name2)
  write.csv(df3,file = name3)
}

setwd("..")

rm(list = ls())