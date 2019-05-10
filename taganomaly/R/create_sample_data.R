raw_content <- c("Error","Warn","Debug","Attach","Limit","No clue","Check with author")

rawdf <- data.frame(
  date = rep(seq(as.POSIXct('2018-01-01 12:00:00'),as.POSIXct('2018-01-05 14:00:00'),by='20 min'),30),
  category = sample(c('A','B','C'),replace = T,size = 8850),
  content = sample(raw_content,replace = T,size = 8850)
)
library(dplyr)

## create 1 h windows per category
df_1h <- rawdf %>% mutate(date = as.POSIXct(date,format = '%d-%m-%Y %H')) %>%
  group_by(date,category) %>% summarize(value = n())

catA <- df_1h[df_1h$category=='A',]
plot(catA$date,catA$value)



write.csv(df_1h,"data/sample-1H.csv")
write.csv(rawdf,"data/sample-raw.csv")
