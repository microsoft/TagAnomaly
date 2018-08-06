library(shiny)
library(dplyr)
library(gridExtra)
library(parsedate) # for cases when the provided date wasn't in a specific pattern
library(DT) 
library(ggplot2) 


## Extend the max file upload size
options(shiny.maxRequestSize=150*1024^2)


## Shiny server function
server <- function(input,output, session) {
  
  ## Reactive values
  # A boolean checking if the provided dataset contains multiple categories or not. This affects the UI
  hasCategories <- reactiveVal(value = T,label='hasCategories')
  
  # A numeric holding the time series gap (difference between two consecutive samples) as inferred from the provided dataset
  timeSeriesGap <- reactiveVal(value = 12*60*60,label='timeSeriesGap')
  
  
  ####---- Time-Series data handling ----####
  
  ## Get time-series dataset from file upload
  getTimeSeriesDataset <- reactive({
    if(is.null(input$timeseriesfile)) return(NULL)
    
    dataset <- read.csv(input$timeseriesfile$datapath,stringsAsFactors = F)
    
    ## Parse date to POSIXct
    dataset$new_date <- as.POSIXct(dataset$date,tz = 'UTC',format = '%Y-%m-%d %H:%M:%S')
    
    ## If parsing failed, use parsedate to automatically parse the input date
    if(all(is.na(dataset$new_date))){
      warning('Error parsing date column, using parsedate to try parsing the date')
      library(parsedate)
      dataset$date <- parse_date(dataset$date)
    } else{
      dataset$date <- dataset$new_date
      dataset$new_date <- NULL
    }
    
    ## Check whether the time series has multiple categories
    if(is.null(dataset$category)){
      warning('Category not found, assuming one category')
      hasCategories(FALSE)
      ## Setup the time series gap as the median of all gaps in the time series
      dataset <- dataset %>% arrange(date)
      timeSeriesGap(median(diff(as.numeric(dataset$date))))
      
    } else{
      ## Setup the time series gap
      oneCategory <- dataset %>% filter(category == dataset[1,'category']) %>% arrange(date)
      gap <- diff(as.numeric(oneCategory$date))[1]
      timeSeriesGap(gap)
    }
    
    ## Fill missing values in time series if requested (Fill with 0 dates in which no value exists)
    if(input$interpolate){
      pad <- data.frame(date = seq(from = min(dataset$date),to = max(dataset$date),by = timeSeriesGap()))
      full_df <- full_join(dataset,pad) %>% mutate_each(funs(ifelse(is.na(.),0,.)))%>% arrange(date)
      full_df$date <- as.POSIXct(full_df$date,tz='UTC',origin = "1970-01-01")
      dataset <- full_df
    }
    dataset$sampleId <- 1:nrow(dataset)
    dataset
  })
  
  ## Get a dataset for a specific category
  getCategoryDataset <- reactive({
    if(hasCategories()==FALSE){
      return(getTimeSeriesDataset())
    }
    cate <- input$category
    if(is.null(cate)) return(NULL)
    
    dataset <- getTimeSeriesDataset() %>% filter(category == cate)
    dataset
  })
  
  ## Get the entire dataset, filtered by the slider range
  getTimeFilteredDataset <- reactive({
    dataset <- getTimeSeriesDataset()
    if(is.null(dataset)) return(NULL)
    if(is.null(input$slider)) return(NULL)
    
    dataset %>% filter(date >= input$slider[1], date <= input$slider[2])
  })
  
  ## Get category dataset, filtered by the slider range
  getTimeFilteredCategoryDataset <- reactive({
    dataset <- getCategoryDataset()
    if(is.null(dataset)) return(NULL)
    if(is.null(input$slider)) return(NULL)
    
    dataset %>% filter(date >= input$slider[1], date <= input$slider[2])
  })
  
  ####---- Raw data handling ----####
  
  ## Get raw data (an additional dataset for which the time-series dataset is an aggregation)
  ## See R/create_sample_data.R for a script that creates demo time-series and raw datasets
  getRawData <- reactive({
    cate <- input$category
    
    if(is.null(input$rawfile)) return(NULL)
    
    raw <- read.csv(input$rawfile$datapath,stringsAsFactors = F)
    raw$new_date <- as.POSIXct(strptime(raw$date,format = "%Y-%m-%d %H:%M:%S",tz = 'UTC'))
    
    ## If parsing failed, use parsedate to automatically parse the input date
    if(all(is.na(raw$new_date))){
      warning('Error parsing date column, using parsedate to try parsing the date')
      library(parsedate)
      raw$date <- parse_date(raw$date)
    } else{
      raw$date <- raw$new_date
      raw$new_date <- NULL
    }
    
    if(hasCategories()){
      raw <- raw %>% filter(category == cate)
    }
    
    raw
  })
  
  ## get raw data for a sample selected by the user
  getRawDataForSample <- reactive({
    lastclicked <- input$summaryTable_rows_selected
    if(is.null(lastclicked)) return(NULL)
    
    raw <- getRawData()
    if(is.null(raw)) stop('No raw data found for further inspection')
    selected <- selectedPoints()
    
    categoryDataset <- getTimeFilteredCategoryDataset()
    
    selectedRow <- which(categoryDataset$date == selected[lastclicked,'date'])
    if(selectedRow > nrow(categoryDataset)){
      nextSampleDate <- selected$date + timeSeriesGap()
    } else{
      nextSampleDate <- categoryDataset[selectedRow+1,'date']
    }
    if(is.null(selected)) return(NULL)
    
    sampleDate <- selected[lastclicked,'date']
    
    #get raw data only for this window
    raw <- raw %>% filter(date >= sampleDate & date < nextSampleDate, category == input$category)
    
    ## Select columns to show
    raw <- raw %>% select(date,category,content)
    raw
    
  })
  
  
  ####---- Infer time-series frequency ----####
  
  ## Determine the time series frequency for graphs
  observe({
    dataset <- getTimeFilteredCategoryDataset()
    if(is.null(dataset)) return(NULL)
    mini <- min(dataset$date)
    maxi <- max(dataset$date)
    diff <- as.numeric(maxi) - as.numeric(mini)
    
    sel <- '1 day'
    if(diff < 60){
      # difference is in seconds
      sel <- '1 second'
    } else if(diff < 60*60){
      sel <- '1 minute'
    } else if(diff < 60*60*24){
      sel <- '1 hour'
    } else if(diff < 60*60*24*7){
      sel <- '1 day'
    } else if(diff < 60*60*24*31){
      sel <- '1 week'
    } else if(diff < 60*60*24*365){
      sel <- '1 month'
    } else{
      sel <- '1 year'
    }
    
    ## Update the UI
    updateSelectInput(session, "breaks",selected = sel)
  })
  
  ####---- UI Components ----####
  
  ## Date slider
  output$slider <- renderUI({
    dataset <- getTimeSeriesDataset()
    if(is.null(dataset)) return(NULL)
    dataset <- dataset %>% arrange(date)
    
    mini = as.POSIXct(min(dataset$date),origin = '1970-01-01',tz = 'UTC')
    maxi = as.POSIXct(max(dataset$date),origin = '1970-01-01',tz = 'UTC')
    
    sliderInput("slider","Time range",min = mini,max = maxi,value = c(mini,maxi),step = 1,width = 400)
  })
  
  
  ## Select input for categories, based on the categories found in the time series dataset
  output$category <- renderUI({
    req(input$timeseriesfile)
    dataset <- getTimeSeriesDataset()
    dataset <- dataset %>% filter(category != '0' & category != 0)
    if(is.null(dataset)) return("")
    selectInput("category", "Choose category:", as.list(unique(dataset$category)),selected = unique(dataset$category)[1],multiple = F) 
  })
  
  ####---- Plots ----####
  
  ## Main plot
  output$plot <- renderPlot({
    withProgress({
      categoryDataset <- getTimeFilteredCategoryDataset()
      if(is.null(categoryDataset)) return(NULL)
      
      ggplot(categoryDataset, aes(date, value)) + geom_point(size = 3) + geom_line() + 
        scale_y_continuous(labels = scales::comma) + scale_x_datetime(date_breaks = input$breaks) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    },message = "Rendering plot...")
  })
  
  
  
  
  ## Capture the selected points on the graph
  selectedPoints <- reactive({
    user_brush <- input$user_brush
    brushedPoints(getTimeFilteredCategoryDataset(), user_brush, xvar = "date", yvar = "value")
  })
  
  ## Plot showing all categories
  output$allplot <- renderPlot({
    
    if(hasCategories()==FALSE){
      stop('This plot only shows multiple categories. No categories found in the data. Did you add a "category" column to the provided file?.')
      
    }
    
    
    dataset <- getTimeFilteredDataset()
    if(is.null(dataset)) stop('No dataset found.')
    
    categoryDataset <- getTimeFilteredCategoryDataset()
    if(is.null(categoryDataset)) stop('No data found for category.')
    
    minDate = min(categoryDataset$date)
    maxDate = max(categoryDataset$date)
    
    categories <- dataset %>% group_by(category) %>% summarise(perCat = sum(value))
    categories <- categories[categories$perCat > input$minPerCategory,'category'] %>% unlist()
    dataset <- dataset %>% filter(category %in% categories & category != input$category)
    
    if(nrow(dataset) == 0) stop('no dataset found.')
    
    thisplot <- categoryDataset %>% filter(category == input$category) %>%
      ggplot(aes(date, value)) + geom_line(stat="identity") + facet_grid(category ~.) + 
      ggtitle(paste0('Current category: ',input$category)) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_y_continuous(labels = scales::comma) +
      theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
    
    
    allplot <- dataset %>%
      ggplot(aes(date, value)) + 
      geom_line(stat="identity") + 
      facet_grid(category ~. , scales = 'free') + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
      ggtitle(paste0("All other categories with more than ",input$minPerCategory, " values")) +
      scale_y_continuous(labels = scales::comma) + scale_x_datetime(date_breaks = input$breaks)
    
    grid.arrange(thisplot, allplot, ncol = 1,nrow = 2,heights = c(200,1000))
    
  }, height = 1200)
  
  
  ## Plot distributions across categories
  output$alldistributions <- renderPlot({
    if(hasCategories()==FALSE){
      stop('This plot only shows multiple categories. No categories found in the data. Did you add a "category" column to the provided file?.')
    }
    
    dataset <- getTimeFilteredDataset()
    if(is.null(dataset)) stop('no dataset found.')
    categoryDataset <- getTimeFilteredCategoryDataset()
    if(is.null(categoryDataset)) stop('no dataset found.')
    
    minDate = min(categoryDataset$date)
    maxDate = max(categoryDataset$date)
    
    categories <- dataset %>% 
      group_by(category) %>% 
      summarise(perCat = sum(value))
    categories <- categories[categories$perCat > input$minPerCategoryDist,'category'] %>% unlist()
    dataset <- dataset %>% 
      filter(category %in% unique(c(categories,input$category)))
    
    if(nrow(dataset) == 0) stop('no data found')
    
    ggplot(dataset,aes(x = date, y = value,fill = category)) + 
      ggtitle("Distribution of counts per category") + 
      geom_bar(position = "fill",stat = "identity") + 
      scale_x_datetime(date_breaks = input$breaks) + 
      scale_colour_gradientn(colours=rainbow(4)) + 
      coord_flip()
    
  }, height = 1200)
  
  output$summaryTable <- DT::renderDataTable(expr = {DT::datatable(selectedPoints())}, selection = 'single',server = F)
  
  
  data_to_display<-eventReactive(input$summaryTable_rows_selected,ignoreNULL=TRUE,
                                 getRawDataForSample()
  )
  
  
  ## Render DataTable (DT) for raw data
  output$rawtable<-DT::renderDataTable(
    data_to_display(),options = list(
      pageLength = 25,order = list(list(1, 'asc'))))
  
  
  ####---- Anomaly detection model results ----####
  
  
  ## Plot anomalies based on Twitter's model
  output$twitteranomalies <- renderPlot({
    withProgress({
      source("R/anomaly_detection.R")
      dataset <- getTimeFilteredCategoryDataset()
      if(is.null(dataset)) stop('no dataset found.')
      res <- find_anomalies_twitter(dataset)
      res$plot
    },message = 'Finding anomalies...')
  })
  
  
  ####---- Data output ----####
  
  ## download selected points
  output$mydownload <- downloadHandler(
    filename = function(){
      random_string <- paste0(paste0(sample(LETTERS,2 , TRUE),collapse=''),sample(999, 1, TRUE), paste0(sample(LETTERS,2 , TRUE),collapse=''),collapse = '')
      paste0(gsub(".csv",replacement = "",input$timeseriesfile$name),'-',input$category,'-',random_string,'-labels.csv')
    },
    content = function(file) {
      write.csv(selectedPoints(),file)
    }
  )
}