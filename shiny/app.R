library(ggplot2)
library(dplyr)
library(shiny)
library(gridExtra)
library(ggthemes)
library(tidyverse)

hashtags <- read.csv('hashtags.csv')
tweets <- read.csv('tweets.csv')

ui <- fluidPage(
  headerPanel('NOAA Social Media Engagement'),
  sidebarPanel(
    dateRangeInput('daterange','Date Range',start='2017-01-01',end='2018-02-16'),
    selectInput('metric', 'Top retweeted hashtags', c('retweets')),
    checkboxGroupInput('account','NOAA Account',
                       c('NOAAHabitat','NOAAFish_WCRO',
                         'NOAAFisheries','NOAAResearch'),
                       selected=c('NOAAFish_WCRO'))
    
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Plot",
               
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
  
#  rt <- reactive({
#    hashtags %>% group_by(user) %>% arrange(-retweets) %>%
#      filter(row_number()<=10) %>% 
#      filter(user %in% input$account) %>%
#      arrange(retweets) %>%
#      mutate(hashtag=factor(hashtag,hashtag)) 
#    
#  })
  
  
#  output$plot1 <- renderPlot({
    
    
#    ggplot(rt(),
#           aes(x=retweets,y=hashtag,xmin = 0, xmax = retweets)) + geom_point()  + 
#      geom_errorbarh(height = .1) + 
#      facet_grid(user~.,scales='free',space='free') + theme_bw() + 
#      theme(axis.text=element_text(size=14))
    
    
    #ggplot(rt(),
    #  aes(x=hashtag,y=retweets)) + geom_bar(stat='identity') + coord_flip() + 
    #  facet_wrap(~user,scales='free_y') + theme_bw() + theme(axis.text=element_text(size=14))
    
    
 # },height=600)
  
  
#  output$table1 <- renderTable(print.data.frame(rt()))

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
          hash.list[[i]] <- data.frame(rtw=r,hashtag=unlist(h),user=user)  
        }  
    
    tbl_df(data.frame(rbindlist(hash.list))) %>%
    mutate(hashtag=tolower(hashtag)) %>%
      group_by(hashtag) %>%
      mutate(created=as.Date(created,format='%Y-%m-%d %H:%M:%S')) %>%
      mutate(total=sum(rtw))
      

  })


  ts <- reactive({
    hash() %>% group_by(hashtag) %>% filter(row_number()==1) %>%
    group_by(hashtag) %>%
      nest(input$daterange[1],input$daterange[2]) %>%
      mutate()
      do(data.frame(hashtag=.$hashtag, created=seq(.$start,.$end,by="1 day"))) 
      mutate(rtweets=0) 
    
  })

  #now we need to merge the expanded data with the sampled data and create the running sum
  
          
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
  
#  output$plot2 <- renderPlot({
    
    
#    ggplot(hash(),aes(x=reorder(hashtag,retweets),y=retweets)) + 
#      geom_bar(stat='identity',color='blue',alpha=0.6) + coord_flip() + 
#      theme_wsj() + ggtitle('Total Hashtag Retweets')
    
    
#  })
  
  
  
  output$table1 <- renderTable(hash())
}

shinyApp(ui = ui, server = server)