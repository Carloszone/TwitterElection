library("dplyr")
library("readr")
library("tm")
library("stringr")
library("tidyr")
library("lubridate")

# load data
setwd("data collection")
data.raw <- list.files(pattern = "election data") %>% 
  lapply(read_csv) %>% 
  bind_rows

# data clean
data.raw <- data.raw %>% 
  select(text, created) %>% 
  sapply(iconv, from = "latin1", to = "ASCII", sub = "") %>% # convert coding method and remove emojis
  as.data.frame()

data.clean <- VCorpus(VectorSource(data.raw$text)) %>% # set VCorpus
  tm_map(gsub, pattern = "(s?)(f|ht)tp(s?)://\\S+\\b", replacement = "") %>% # remove all URL links
  tm_map(gsub, pattern = "#[A-Za-z0-9]+|@[A-Za-z0-9]+\\b", replacement = "") %>% #remove all hashtags and @users
  tm_map(gsub, pattern = "<(.*)>", replacement = "") %>% #remove "<...>" code
  tm_map(removePunctuation) %>% # remove all punctuation
  tm_map(removeNumbers) %>% # remove numbers
  tm_map(stripWhitespace) %>% # remove multiple space
  tm_map(PlainTextDocument) %>% # convert to text document
  tm_map(content_transformer(tolower)) # convert to lower letters 

# this part is used to create a text for words frequency analysis
meaninglesswords <- c("joe","biden", "donald", "trump", "trumps")

data.fre <- VCorpus(VectorSource(data.clean))
data.fre <- tm_map(data.fre, removeWords, stopwords("english")) %>%
  tm_map(removeWords,meaninglesswords) %>%
  tm_map(stripWhitespace)
  
data.fre <- data.frame(text=unlist(sapply(data.fre, `[`, "content")),
                       stringsAsFactors=FALSE) # convert to data frame

# bind result
data.new <- data.frame(text=unlist(sapply(data.clean, `[`, "content")),
                       stringsAsFactors=FALSE) # convert to data frame

data.new <- cbind(data.new, created = data.raw$created, textfre = data.fre$text) %>% 
  separate(created, c("date", "time"), " ") %>%
  mutate(date = ymd(date)) # add date information

# set filter rules
data.trump <- data.new %>% 
  filter(sapply(data.new$text, FUN = grepl, pattern = "trump"))

data.biden <- data.new %>% 
  filter(sapply(data.new$text, FUN = grepl, pattern = "biden"))

