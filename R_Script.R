
###Install and Load the libraries needed

if (!('rvest' %in% installed.packages())) {
  install.packages('rvest')
}
if (!('dplyr' %in% installed.packages())) {
  install.packages('dplyr')
}
if (!('shiny' %in% installed.packages())) {
  install.packages('shiny')
}
if (!('tidyverse' %in% installed.packages())) {
  install.packages('tidyverse')
}
if (!('shinydashboard' %in% installed.packages())) {
  install.packages('shinydashboard')
}
if (!('fredr' %in% installed.packages())) {
  install.packages("fredr") 
}

library(rvest)
library(dplyr)
library(shiny)
library(tidyverse)
library(shinydashboard)
library(fredr)




### Functions that obtain data needed

#Get chiacgo midwewst economic activity data
get_chicago_eco_activity <- function(x) {
  
  fredr_set_key("81e294f58c77d6c0426412acbacc6a4e")
  
  df_Chicago_activity<-fredr(
    series_id = "MEIM683SFRBCHI",
    observation_start = as.Date("2000-01-01"),
    observation_end = as.Date("2021-02-01"))
  
  df_Chicago_activity
  
}


#Funtion to obtain the data for plot
get.data2 <- function(x){
  myurl <- read_html("https://www.bea.gov/news/2021/personal-income-and-outlays-february-2021") # read our webpage as html
  myurl <- html_table(myurl)  # convert to an html table for ease of use
  myurl
  to.parse <- myurl[[1]]  # pull the first item in the list
  
  #Cleanup the data frame that we scratched
  colnames(to.parse)<-NULL
  colnames(to.parse) <- c("Measure","M-4", "M-3", "M-2", "M-1","M1")
  to.parse <- to.parse[c(-1,-2,-7,-8,-9,-10,-11,-12,-13),]
  to.parse[2,1]<-"Personal Income (Current $)"
  to.parse[4,1]<-"Disposable personal income (Current $)"
  to.parse <- to.parse[c(-1,-3,-5),]
  to.parse[,2:6] <- sapply(to.parse[,2:6],as.double)
  to.parse
}

#Funtion to obtain the data for table
get.data <- function(x){
  myurl <- read_html("https://www.bea.gov/news/2021/personal-income-and-outlays-february-2021") # read our webpage as html
  myurl <- html_table(myurl)  # convert to an html table for ease of use
  myurl
  to.parse <- myurl[[1]]  # pull the first item in the list
  
  #Cleanup the data frame that we scratched
  colnames(to.parse)<-NULL
  to.parse[1,1]<-"Measure"
  names(to.parse) <- to.parse[1,]
  to.parse <- to.parse[c(-1,-2,-7,-8,-9,-10,-11,-12,-13),]
  to.parse[2,1]<-"Personal Income (Current $)"
  to.parse[4,1]<-"Disposable personal income (Current $)"
  to.parse <- to.parse[c(-1,-3,-5),]
  to.parse[,2:6] <- sapply(to.parse[,2:6],as.double)
  to.parse
}

#Obtain Consumer Sentiment reading (latest)
get_MI_sentiment <- function(x) {
  
  fredr_set_key("81e294f58c77d6c0426412acbacc6a4e")
  
  df_MI_sentiment<-fredr(
    series_id = "UMCSENT",
    observation_start = as.Date("2000-01-01"),
    observation_end = as.Date("2021-02-01"))
  
  df_MI_sentiment
  
}



get_MI_value <- function(x) {
  
  fredr_set_key("81e294f58c77d6c0426412acbacc6a4e")
  
  df_MI_sentiment<-fredr(
    series_id = "UMCSENT",
    observation_start = as.Date("2000-01-01"),
    observation_end = as.Date("2021-02-01"))

  df_MI_sentiment[rev(order(df_MI_sentiment$date)),][1,3]

}



get_MI_date <- function(x){

  fredr_set_key("81e294f58c77d6c0426412acbacc6a4e")
  
  df_MI_sentiment<-fredr(
    series_id = "UMCSENT",
    observation_start = as.Date("2000-01-01"),
    observation_end = as.Date("2021-02-01"))
  
  df_MI_sentiment[rev(order(df_MI_sentiment$date)),][1,1]
  
}



######################################################################################################
######################################################################################################

##################################### SET UP INTERFACE (UI)##########################################

######################################################################################################
######################################################################################################


#Setup dashboard

ui <- dashboardPage(
  # HEADER
  dashboardHeader(title = "Consumer Data information"),
  
  #SIDE BAR INFO
  dashboardSidebar(
    h5("We present rellevant consumer realted economic indicators in this dashboard"),
    
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    
    h6("Sid Singh, R (Rstudio), 
      [  R Core Team (2018). R: A language and environment for statistical computing. R Foundation for Statistical Computing,
  Vienna, Austria. URL https://www.R-project.org/]"),
    br(),
    h6(""),
    br(),
    a("sidaks716@gmail.com", href="https://www.linkedin.com/in/singh-sidak/")
    
  ),
  
  #MAIN BODY
  dashboardBody(
    
    fluidRow(
      
      # InfoBox
      infoBoxOutput("Value.MI",
                    width = 4),
      
      # InfoBox
      infoBoxOutput("MI.date",
                    width = 3)
      
    ),
    
    fluidRow(
      column(
        # Datatable
        box(
          status = "primary",
          headerPanel("Consumer Measures (Monthly Change)"),
          solidHeader = T,
          br(),
          DT::dataTableOutput("table", height = "350px"),
          width = 6,
          height = "500px"
        ),
        
        # Chart
        box(
          status = "primary",
          headerPanel("%Change Monthly"),
          solidHeader = T,
          br(),
          plotOutput("plot", height = "400px"),
          width = 6,
          height = "500px"
        ),
        width = 12
      )
      
    ),
    
    fluidRow(
      box(
        status = "primary",
        headerPanel("Activity_plot"),
        solidHeader = T,
        br(),
        plotOutput("Activity_plot", height = "500px"),
        width = 12,
        height = "600px"
      )
    ),
      
      fluidRow(
        box(
          status = "primary",
          headerPanel("Consumer Sentiment"),
          solidHeader = T,
          br(),
          plotOutput("Sentiment_plot", height = "500px"),
          width = 12,
          height = "600px"
        )
      
    )
  )
  
)

#########################################################################################################
######################################### S E R V E R ###################################################
#########################################################################################################

server <- function(input, output) {
  # R E A C T I V E 
  liveish_data <- reactive({
    invalidateLater(60000)    # refresh the report every 60k milliseconds (60 seconds)
    get.data()                # call our function from above
  })
  
  
  live.infobox.Value.MI <- reactive({
    invalidateLater(60000)    # refresh the report every 60k milliseconds (60 seconds)
    get_MI_value()         # call our function from above
  })
  
  
  live.infobox.Mi.date <- reactive({
    invalidateLater(60000)    # refresh the report every 60k milliseconds (60 seconds)
    get_MI_date()        # call our function from above
  })
  
  live.infobox.midwest <- reactive({
    invalidateLater(60000)    # refresh the report every 60k milliseconds (60 seconds)
    get_chicago_eco_activity()        # call our function from above
  })
  
  
  
  # D A T A   T A B L E   O U T P U T
  output$table <- DT::renderDataTable(DT::datatable({
    data <- liveish_data()}))
  
  # P L O T   O U T P U T
  output$plot <- renderPlot({ (ggplot(data=get.data2(), aes(x=Measure, y=M1)) +
                                 geom_bar(stat="identity", color = "blue", fill = "white") +
                                 ylab("% over last month")) + ggtitle("% change over previous month") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
    
  })
  
  # P L O T   O U T P U T
  output$Activity_plot <- renderPlot({ (ggplot(data=get_chicago_eco_activity(), aes(x=date, y=value)) +
                                          geom_line(color="red") +
                                          geom_point()+
                                          ylab("Index Std. Deviation")) + ggtitle("Midwest Economic Activity Index (Monthly)")
    
  })
  
  # P L O T   O U T P U T
  output$Sentiment_plot <- renderPlot({ (ggplot(data=get_MI_sentiment(), aes(x=date, y=value)) +
                                          geom_line(color="red") +
                                          geom_point()+
                                          ylab("INDEX 1966 Q1=100")) + ggtitle("Michigan Consumer Sentiment Index")
    
  })
  
  
  # I N F O B O X   O U T P U T - N A M E
  output$MI.date <- renderInfoBox({
    infoBox(
      "Date",
      live.infobox.Mi.date(),
      icon = icon("line-chart"),
      color = "black",
      fill = FALSE)
  })
  
  # I N F O B O X   O U T P U T - V A L
  output$Value.MI <- renderInfoBox({
    infoBox(
      "Most Recent Consumer INDEX reading",
      paste0(live.infobox.Value.MI(), " (1966Q1 = 100)"),
      icon = icon("signal"),
      color = "black",
      fill = FALSE)
  })

  
}


#####################
#### D E P L O Y ####
#####################
# Return a Shiny app objectshinyApp(ui = ui, server = server)
shinyApp(ui = ui, server = server)
  
  
