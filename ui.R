library(shiny)
library(DT)
library(ggplot2)
library(shinythemes)
library(shinydashboard)

header <- dashboardHeader(title = 'Taganomaly - Anomaly detection labeling tool',
                          dropdownMenu(
                            type = "notifications", 
                            icon = icon("question-circle"),
                            badgeStatus = NULL,
                            headerText = 
                              helpText(HTML('<U>Instructions:</U> <P>
                <B>1. Import time series CSV file</B>. Assumed structure: <BR>
                          - date ("%Y-%m-%d %H:%M:%S")<BR>
                          - category<BR>
                          - value</P><P>
                <B>2. (Optional) Import raw data time series CSV file</B>.</P><P>
                          If the original time series is an aggreation over time windows, this time series is the raw values themselves. This way we could dive deeper into an anomalous value and see what it is comprised of.<BR>
                          Assumed structure: <BR>
                          - date ("%Y-%m-%d %H:%M:%S")<BR>
                          - category<BR>
                          - value</P><P></P><P>
                <B>2. Select category</B> (if exists)</P><P>
                <B>3. Select time range on slider</B></P><P>
                <B>4.Select points on plot</B> that look anomalous.</P><P>
                <B>5. Click "Add selected points"</B> to add the marked points to the candidate list.</P><P>
b                <BR>Optional (1): click on one time range on the table below the plot to see raw data on this time range
                <BR>Optional (2): Open the "All Categories" tab to see how other time series behave on the same time range.<P>
                <B>7.</B> Once you decide that these are actual anomalies, save the resulting table to csv by clicking on "Download labels set" and continue to the next category.</P>'
                              )))
)

sidebar <- dashboardSidebar(

  sidebarMenu(
    tags$style(".skin-red .sidebar a { color: #444; }"),
    fileInput("timeseriesfile", "Choose CSV File with counts per time frame",
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")
    ),
    #checkboxInput("header","My dataset has headers",value=TRUE),
    fileInput("rawfile", "Choose CSV File with raw data",
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")
    ),
    uiOutput("category"),
    checkboxInput('interpolate',label = "Interpolate missing points",value = FALSE),
    selectInput('breaks',"Select graph breaks",choices = c('1 sec','1 min','1 hour','1 day','1 week','1 month','1 year'),selected = '1 year'),
    uiOutput('slider')
  )
)

body <- dashboardBody(
  tags$head(tags$style(".shiny-notification {position: fixed; top: 20% ;left: 50%")),
  tabsetPanel(
    tabPanel("Current category",
             h2('Time Series for labeling:'),
             h5("Graph might take a few moments to load"),
             plotOutput("plot", brush = "user_brush"),
             actionButton("add", "Add selected points"),
             actionButton("delete", "Remove selected points"),
             h2('Currently marked points:'),
             dataTableOutput("summaryTable"),
             downloadButton(outputId = "mydownload", label = "Download labels set"),
             h2('Inspect raw data:'),
             h5('Select a point or more on the graph, then select a record on the \"Selected Points\" table to see raw data'),
             dataTableOutput("rawtable")
    ),
    tabPanel('All categories',
             h2('Inspect all other categories:'),
             numericInput('minPerCategory','Minimum samples for being a major category',min = 0,value = 100),
             plotOutput("allplot")
    ),
    tabPanel('Category distribution over time',
             h2('Inspect change in distribution over time:'),
             numericInput('minPerCategoryDist','Minimum samples for being a major category',min = 0,value = 100),
             plotOutput('alldistributions')
    ),
    tabPanel('Suggested anomalies (Twitter model)',
             h3("Based on Twitter's AnomalyDetection package:"),
             a('https://github.com/twitter/AnomalyDetection/', href = 'https://github.com/twitter/AnomalyDetection/'),
             h5("Might take a while to load..."),
             plotOutput('twitteranomalies'),
             fluidRow(
               column(2, selectInput("twitterThreshold",label="Threshold",choices=c("None", "med_max", "p95", "p99"),selected = "p95")),
               column(2, selectInput("twitterDirection",label="Direction",choices=c("pos", "neg", "both"),selected='pos')),
               column(2, numericInput("twitterAlpha","Alpha value",min=0,max=1,step=0.05,value=0.05))
               )
             
             
    )
  )
  
  
  
)


dashboardPage(header, sidebar, body,skin = "red")

