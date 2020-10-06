library("dplyr")
library("plyr")
library("readr")
library("tm")
library("stringr")
library("tidyr")

# load data
setwd("data collection")
data.all <- list.files(pattern = "election data") %>% 
  lapply(read_csv) %>% 
  bind_rows

# data clean
data.raw <- data.all %>% 
  select(text, created) %>% 
  sapply(iconv, from = "latin1", to = "ASCII", sub = "") %>% # convert coding method and remove emojis
  as.data.frame()

data.clean <- VCorpus(VectorSource(data.raw$text)) %>% # set VCorpus
  tm_map(gsub, pattern = "(s?)(f|ht)tp(s?)://\\S+\\b", replacement = "") %>% # remove all URL links
  tm_map(gsub, pattern = "#[A-Za-z0-9]+|@[A-Za-z0-9]+", replacement = "") %>% #remove all hashtags and @users
  tm_map(gsub, pattern = "<(.*)>", replacement = "") %>% #remove "<...>" code
  tm_map(removePunctuation) %>% # remove all punctuation
  tm_map(PlainTextDocument) %>% # convert to text document
  tm_map(content_transformer(tolower)) # convert to lower letters 
  
data.clean <- data.frame(text=unlist(sapply(data.clean, `[`, "content")),
                         stringsAsFactors=FALSE) # convert to data frame

data.clean <- cbind(data.clean, created = data.raw$created) %>% 
  separate(created, c("date", "time"), " ") 


# set filter rules
data.trump <- data.clean %>% filter(sapply(data.clean$text, FUN = grepl, pattern = "trump"))
data.biden <- data.clean %>% filter(sapply(data.clean$text, FUN = grepl, pattern = "biden"))
