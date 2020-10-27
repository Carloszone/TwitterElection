library(RSentiment)
library(rJava)

options(java.parameters = "- Xmx8000m") # setting the memory size
Sys.setenv(JAVA_HOME='') # if meet the "rJava" error, please set you Java path correctly


#result classification function
res.classify <- function(x){
  if(x >0 & x != 99){ res <- "postive"}
  if(x == 99){ res <- " sarcasm"}
  if(x == 0){ res <- "neutral"}
  if(x < 0 ){ res <- "negative"}
  return(res)
  }

#calculate result and add the result to original data set
data.trump <- data.trump %>% 
  mutate(score = sapply(data.trump$text, calculate_score)) 
data.trump <- data.trump %>%
  mutate(result = sapply(data.trump$score,res.classify)) %>%
  mutate(name = "trump")

data.biden <- data.biden %>% 
  mutate(score = sapply(data.biden$text, calculate_score)) 
data.biden <- data.biden %>%
  mutate(result = sapply(data.biden$score,res.classify)) %>%
  mutate(name = "biden")

# save result
write.csv(data.trump, "trump.csv")
write.csv(data.biden, "biden.csv")

data.tb <- rbind(data.trump, data.biden) 
write.csv(data.tb,"all.csv")

