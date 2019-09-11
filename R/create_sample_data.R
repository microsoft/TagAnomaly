raw_content <- c("Error","Warn","Debug","Attach","Limit","No clue","Check with author")

rawdf <- data.frame(
  date = rep(seq(as.POSIXct('2019-09-01 12:00:00'),as.POSIXct('2019-10-30 14:00:00'),by='1 hour'),30),
  category = sample(c('A','B','C'),replace = T,prob = c(0.6,0.1,0.3),size = 42600),
  content = sample(raw_content,replace = T,size = 42600)
)
library(dplyr)

## create 1 h windows per category
df_1h <- rawdf %>% mutate(date = as.POSIXct(date,format = '%d-%m-%Y %H')) %>%
  group_by(date,category) %>% summarize(value = n())

catB <- df_1h[df_1h$category=='B',]
plot(catB$date,catB$value,type = 'l')



write.csv(df_1h,"data/sample-1H.csv")
write.csv(rawdf,"data/sample-raw.csv")

