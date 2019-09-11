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
  
  ####---- Reactive values ---####
  
  
  # A boolean checking if the provided dataset contains multiple categories or not. This affects the UI
  hasCategories <- reactiveVal(value = T,label='hasCategories')
  
  # A numeric holding the time series gap (difference between two consecutive samples) as inferred from the provided dataset
  timeSeriesGap <- reactiveVal(value = 12*60*60,label='timeSeriesGap')
  
  # Whether the first column holds a numeric value (TRUE), or a date value (FALSE)
  numericTimestamp <- reactiveVal(value = F,label = 'numericTimestamp')
  
  selectedPoints <- reactiveVal(value = data.frame(),label='selectedPoints')
  brushed <- reactive({
      brushedPoints(getTimeFilteredCategoryDataset(), input$user_brush)
    })
  
  #### Event observers ####
  
  # Update selected points when the user clicks 'Add'
  observeEvent(input$add, {
    selectedPoints(selectedPoints() %>% bind_rows(brushed()))
  })
  
  # Update selected points when the user clicks 'Remove'
  observeEvent(input$delete, {
    if (dim(selectedPoints())[1] > 0) {
      selectedPoints(selectedPoints()%>% anti_join(brushed()))
    }
  })
  
  ####---- Time-Series data injestion and handling ----####
  
  
  getDataset <- reactive({
    ## Get time-series dataset from file upload
    
    if(is.null(input$timeseriesfile)) return(NULL)
    dataset <- tryReadFile()
    
    
    validate(
      need(nrow(dataset) > 0, "Input file is empty"),
      need(('date' %in% names(dataset)),"date column not found. Consider renaming your timestamp column to date"),
      need(('value' %in% names(dataset)),"value column not found. Consider renaming your value column to value")
      
    )
    dataset
  })  

  tryReadFile <- function() {
    ## Read CSV input file from the user provided path
    out <- tryCatch(
      {
        read.csv(input$timeseriesfile$datapath,stringsAsFactors = F)
      },
      error=function(cond) {
        message("Failed to load file. Message:")
        message(cond)
        return(NULL)
      },
      warning=function(cond) {
        message("Warning:")
        message(cond)
        return(NULL)
      }
    )    
    return(out)
  }
  
  padMissingDates <- function(dataset,padValue = 0, timeSeriesGapValue){
    ## Interpolate missing time/date values
    category <- dataset[1,'category'] %>% unlist()
    
    pad <- data.frame(date = seq(from = min(dataset$date),to = max(dataset$date),by = timeSeriesGapValue))
    
    full_df <- full_join(dataset,pad) %>% mutate_all(funs(ifelse(is.na(.),padValue,.)))%>% arrange(date)
    
    full_df$date <- as.POSIXct(full_df$date,tz='UTC',origin = "1970-01-01")
    full_df$category <- category
    full_df$sampleId <- ifelse(full_df$sampleId == 0,NA,full_df$sampleId)
    dataset <- full_df
    
    dataset
  }

  
  
  getTimeSeriesDataset <- reactive({
    ### Turn dataset into a time series by transforming the date column into POSIXct.
    ### If dataset is numeric, turn numericTimestamp flag to TRUE.
    dataset <- getDataset()
    if(is.null(dataset)) return(NULL)
    
    if(is.numeric(dataset$date)){
      numericTimestamp(TRUE)
      
    }
    else{
      
      ## Parse date to POSIXct
      dataset$new_date <- as.POSIXct(dataset$date,tz = 'UTC',format = '%Y-%m-%d %H:%M:%S')
      
      ## If parsing failed, use parsedate to automatically parse the input date
      if(all(is.na(dataset$new_date))){
        warning('Error parsing date column, using parsedate to try parsing the date')
        library(parsedate)
        dataset$date <- parse_date(dataset$date)
      } else{
        dataset$date <- dataset$new_date
        
      }
      
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

    dataset$sampleId <- 1:nrow(dataset)
    dataset
  })
  

  getCategoryDataset <- reactive({
    ## Get a dataset for a specific category
    ts <- getTimeSeriesDataset()
    if(is.null(ts)) return(NULL)
    
    if(hasCategories()==FALSE){
      return(ts)
    }
    cate <- input$category
    if(is.null(cate)) return(NULL)
    
    dataset <- ts %>% filter(category == cate)
    
    ## Fill missing values in time series if requested (Fill with 0 dates in which no value exists)
    if(input$interpolate){
      dataset <- padMissingDates(dataset,timeSeriesGapValue = timeSeriesGap())
    }
    
    dataset
  })
  

  getTimeFilteredDataset <- reactive({
    ## Get the entire dataset, filtered by the slider range
    dataset <- getTimeSeriesDataset()
    if(is.null(dataset)) return(NULL)
    if(is.null(input$slider)) return(NULL)
    
    dataset %>% filter(date >= input$slider[1], date <= input$slider[2])
  })
  

  getTimeFilteredCategoryDataset <- reactive({
    ## Get category dataset, filtered by the slider range
    dataset <- getCategoryDataset()
    if(is.null(dataset)) return(NULL)
    if(is.null(input$slider)) return(NULL)
    
    
    session$resetBrush("input$user_brush")
    selectedPoints(data.frame())
    dataset %>% filter(date >= input$slider[1], date <= input$slider[2])
  })
  
  ####---- Raw data handling ----####
  

  getRawData <- reactive({
    ## Get raw data (an additional dataset for which the time-series dataset is an aggregation)
    ## See R/create_sample_data.R for a script that creates demo time-series and raw datasets
    
    cate <- input$category
    
    if(is.null(input$rawfile)) return(NULL)
    raw <- withProgress({
      read.csv(input$rawfile$datapath,stringsAsFactors = F)
    },message = "loading raw data file")
    if(!numericTimestamp()){
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
    }
    if(hasCategories()){
      raw <- raw %>% filter(category == cate)
    }
    
    raw
  })
  

  getRawDataForSample <- reactive({
    ## get raw data for a sample selected by the user
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
    
    if(numericTimestamp()){
      mini = min(dataset$date)
      maxi = max(dataset$date)
    } else{
      mini = as.POSIXct(min(dataset$date),origin = '1970-01-01',tz = 'UTC')
      maxi = as.POSIXct(max(dataset$date),origin = '1970-01-01',tz = 'UTC')
    }
    sliderInput("slider","Time range",min = mini-1,max = maxi+1,value = c(mini-1,maxi+1),step = 1,width = 400)
  })
  
  
  ## Select input for categories, based on the categories found in the time series dataset
  output$category <- renderUI({
    
    if(hasCategories()==TRUE){
      req(input$timeseriesfile)
      dataset <- getTimeSeriesDataset()
      if(is.null(dataset)) return("")
      selectInput("category", "Choose category:", as.list(unique(dataset$category)),selected = unique(dataset$category)[1],multiple = F) 
    } else{
      return(NULL)
    }
  })
  
  ####---- Plots ----####
  
  ## Main plot
  output$plot <- renderPlot({
    withProgress({
      categoryDataset <- getTimeFilteredCategoryDataset()
      if(is.null(categoryDataset)) return(NULL)
      
      g <- ggplot(categoryDataset, aes(date, value))+ geom_line(size=0.25,color="#D55E00") + geom_point(size = 0.5,shape=21,alpha=0.7)  + 
        scale_y_continuous(labels = scales::comma) + theme_bw()
      if(!numericTimestamp()){
        g <- g +  scale_x_datetime(date_breaks = input$breaks)
      }
      g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1),
                       panel.grid.minor = element_blank(), 
                       text = element_text(size = 14))
      

      
      if (dim(selectedPoints())[1] > 0) {
        g <- g + geom_point(aes(date, value), data = selectedPoints(), color = "red")
      } 
      
      g
    },message = "Rendering plot...")
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
    
    if(numericTimestamp()){
      allplot <- dataset %>%
        ggplot(aes(date, value)) + 
        geom_line(stat="identity") + 
        facet_grid(category ~. , scales = 'free') + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
        ggtitle(paste0("All other categories with more than ",input$minPerCategory, " values")) +
        scale_y_continuous(labels = scales::comma)
    } else{
      
      allplot <- dataset %>%
        ggplot(aes(date, value)) + 
        geom_line(stat="identity") + 
        facet_grid(category ~. , scales = 'free') + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
        ggtitle(paste0("All other categories with more than ",input$minPerCategory, " values")) +
        scale_y_continuous(labels = scales::comma) + 
        scale_x_datetime(date_breaks = input$breaks)
    }
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
    if(numericTimestamp()){
      ggplot(dataset,aes(x = date, y = value,fill = category)) + 
        ggtitle("Distribution of counts per category") + 
        geom_bar(position = "fill",stat = "identity") + 
        scale_colour_gradientn(colours=rainbow(4)) + 
        coord_flip()      
    } else{
      ggplot(dataset,aes(x = date, y = value,fill = category)) + 
        ggtitle("Distribution of counts per category") + 
        geom_bar(position = "fill",stat = "identity") + 
        scale_x_datetime(date_breaks = input$breaks) + 
        scale_colour_gradientn(colours=rainbow(4)) + 
        coord_flip()
    }
  }, height = 1200)
  
  output$summaryTable <- DT::renderDataTable(expr = selectedPoints(), selection = 'single',server = F)
  
  
  data_to_display<-eventReactive(input$summaryTable_rows_selected,
                                 ignoreNULL=TRUE,
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
      if(numericTimestamp()){
        dataset$date <- as.POSIXct(dataset$date,tz="UTC",origin="1970-01-01")
      }
      if(is.null(dataset)) stop('no dataset found.')
      res <- find_anomalies_twitter(dataset,
                                    is_ts = !numericTimestamp(),
                                    threshold = input$twitterThreshold,
                                    alpha = input$twitterAlpha, 
                                    direction = input$twitterDirection)
      if(is.null(res$plot)){
        stop("No anomalies found")
      }
      res$plot
    },message = 'Finding anomalies...')
  })
  
  
  ####---- Data output ----####
  
  ## download selected points
  
  output$mydownload <- downloadHandler(
    filename = function(){
      random_string <- paste0(paste0(sample(LETTERS,2 , TRUE),collapse=''),sample(999, 1, TRUE), paste0(sample(LETTERS,2 , TRUE),collapse=''),collapse = '')
      
      if(hasCategories()){
        paste0(gsub(".csv",replacement = "",input$timeseriesfile$name),'-',input$category,'-',random_string,'-labels.csv')
      } else{
        paste0(gsub(".csv",replacement = "",input$timeseriesfile$name),'-',random_string,'-labels.csv')
      }
    }, content = function(file) {
      write.csv(selectedPoints(),file)
    }
  )
  
}