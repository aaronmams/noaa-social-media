library(ggplot2)
library(dplyr)
library(shiny)
library(gridExtra)
library(ggthemes)
library(data.table)

hashtags <- read.csv('hashtags.csv')
tweets <- read.csv('tweets.csv')

ui <- fluidPage(
  headerPanel('NOAA Social Media Engagement'),
  sidebarPanel(
    dateRangeInput('daterange','Date Range',start='2017-10-01',end='2018-02-16'),
    selectInput('metric', 'Top retweeted hashtags', c('retweets')),
    checkboxGroupInput('account','NOAA Account',
                       c('NOAAHabitat','NOAAFish_WCRO',
                         'NOAAFisheries','NOAAResearch'),
                       selected=c('NOAAFish_WCRO'))
    
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Engagement",
               
               plotOutput('plot1',width="100%"),
               plotOutput('plot2',width="100%")
      ),
      tabPanel("Table",
               tableOutput('table1')
      )
    )    
  )
)

server <- function(input, output) {

  rt <- reactive({
        tweets %>% filter(screen_name %in% input$account) %>% 
          filter(as.Date(created_at) <= input$daterange[2] & as.Date(created_at)>=input$daterange[1])  
          
        
      })  

#this data frame is going to hold each use of each hashtag    
  hash <- reactive({
    
    hash.list <- list()
        for(i in 1:nrow(rt())){
          h <- strsplit(as.character(rt()$hashtags[i]),";")
          r <- rt()$retweet_count[i]
          user <- rt()$screen_name[i]
          created <- rt()$created_at[i]
          hash.list[[i]] <- data.frame(rtw=r,hashtag=unlist(h),user=user,created=created)  
        }  
    
    tbl_df(data.frame(rbindlist(hash.list))) %>%
    mutate(hashtag=tolower(hashtag)) %>%
      group_by(hashtag) %>%
      mutate(created=as.Date(created,format='%Y-%m-%d %H:%M:%S')) %>%
      mutate(total=sum(rtw))
      

  })


  ts <- reactive({
    hash() %>% group_by(hashtag) %>% filter(row_number()==1) %>%
    group_by(hashtag) %>% mutate(start=input$daterange[1],end=input$daterange[2]) %>%
    rowwise() %>%
    do(data.frame(hashtag=.$hashtag, created=seq(.$start,.$end,by="1 day"))) %>%
    mutate(rtweets=0) 
    
  })

  #now we need to merge the expanded data with the sampled data and create the running sum
  ht <- reactive({
    hash() %>% group_by(hashtag,created) %>%
    summarise(rtw=sum(rtw))
  })
  
    rolling <- reactive({
    
    ts() %>% left_join(ht(),by=c('hashtag','created')) %>%
      mutate(rtw=ifelse(is.na(rtw),rtweets,rtw)) %>%
      mutate(rts=max(rtw,rtweets)) %>%
      group_by(hashtag) %>% arrange(hashtag,created) %>%
      mutate(retweets=cumsum(rts)) %>%
        filter(is.na(hashtag)==F) %>%
        mutate(total=max(retweets,na.rm=T)) %>%
        ungroup() %>%
        mutate(rank=dense_rank(-total)) %>%
        filter(rank<=5) %>%
        arrange(rank)
    
  })
          
  output$plot1 <- renderPlot({
    
    ggplot(hash() %>% 
             group_by(hashtag) %>% 
             summarise(retweets=sum(rtw)) %>%
             ungroup() %>%
             arrange(-retweets) %>%
             filter(row_number() <= 20),aes(x=reorder(hashtag,retweets),y=retweets)) + 
      geom_bar(stat='identity',color='blue',alpha=0.6) + coord_flip() + 
      theme_wsj() + ggtitle('Total Hashtag Retweets')
    
    
  })
  
  output$plot2 <- renderPlot({
    
    ggplot(rolling(),
             aes(x=created,y=retweets,color=hashtag,shape=hashtag)) + geom_line() + geom_point() + 
      theme_wsj() + ggtitle('Retweets')
    
    
  })
  
  
  
  output$table1 <- renderTable(rolling())
}

shinyApp(ui = ui, server = server)