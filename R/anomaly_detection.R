### This function return anomalizes based on Twitter's module found in https://github.com/twitter/AnomalyDetection
find_anomalies_twitter <- function(categoryDataset,is_ts = TRUE,threshold="p95",alpha=0.05,direction="pos"){
  
  library(AnomalyDetection)
  library(dplyr)
  
  res <- tryCatch(
    {
      categoryDataset <- categoryDataset %>% select(date, value)
      print(paste0("Performing anomaly detection... is_ts = ",is_ts))
      if(is_ts) {
        res <- AnomalyDetectionTs(categoryDataset, 
                                  threshold = threshold, 
                                  direction=direction,
                                  alpha = alpha, 
                                  plot=TRUE, 
                                  title='Anomalies found using Twitter\'s anomaly detection.')
      } else{ 
        res <- AnomalyDetectionVec(categoryDataset$value, 
                                   threshold=  threshold, 
                                   direction = direction,
                                   alpha = alpha, 
                                   period = 1440,
                                   plot=TRUE, 
                                   title='Anomalies found using Twitter\'s anomaly detection with a period value of 1440.')
      }
      
      if(is.null(res$plot)){
        res$plot <- ggplot(categoryDataset, aes(date, value)) + geom_line(colour='red',size=0.5)  + 
          scale_y_continuous(labels = scales::comma) + theme_bw() + 
          theme(axis.text.x = element_text(angle = 90, hjust = 1),panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
          ggtitle("No anomalies found. Original dataset plot")
      }
      
      res
    },
    error=function(cond) {
      message("Failed to run the Twitter Anomaly Detection model. Message:")
      message(cond)
      return(NULL)
    },
    warning=function(cond) {
      message("Warning while running the Twitter Anomaly Detection model. message:")
      message(cond)
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
