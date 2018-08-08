### This function return anomalizes based on Twitter's module found in https://github.com/twitter/AnomalyDetection
find_anomalies_twitter <- function(categoryDataset,is_ts = TRUE){
  
  library(AnomalyDetection)
  library(dplyr)
  
  res <- tryCatch(
    {
      categoryDataset <- categoryDataset %>% select(date, value)
      print(paste0("Performing anomaly detection... is_ts = ",is_ts))
      if(is_ts) AnomalyDetectionTs(categoryDataset, threshold='p95', direction='pos', plot=TRUE, title='Anomalies found using Twitter\'s anomaly detection.')
      if(!is_ts) AnomalyDetectionVec(categoryDataset$value, threshold='p95', direction='pos', period = 1440,plot=TRUE, title='Anomalies found using Twitter\'s anomaly detection with a period value of 1440.')
    },
    error=function(cond) {
      message("Failed to run the Twitter Anomaly Detection model. Message:")
      message(cond)
      return(NULL)
    },
    warning=function(cond) {
      message("Warning while running the Twitter Anomaly Detection model. message:")
      message(cond)
      return(NULL)
    }
  )
  
  return(res)
  
  

}


find_anomalies_prophet <- function(categoryDataset){
  ## Requires the prophet package.
  df <- categoryDataset %>% transmute(ds = date,y = value)
  m <- prophet(df)
  future <- make_future_dataframe(m, periods = 1)
  forecast <- predict(m, future)
  results <- forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')]
  
  joined <- inner_join(df,results,by='ds') %>% mutate(is_anomaly = (y > yhat_upper) | (y < yhat_lower))
  anomalies <- joined %>% filter(is_anomaly==T)
  plot(m,forecast) + geom_point(data = anomalies,colour = 'red') + ggtitle("Anomalies based on Facebook Prophet's confidence intervals")
  ggplot(joined,aes(x = ds,y =y)) + geom_point(data = joined %>% filter(is_anomaly == T),aes(x = ds,y=y),colour = 'red') + geom_line()
}
