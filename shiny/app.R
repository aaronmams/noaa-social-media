library(ggplot2)
library(dplyr)
library(shiny)
library(gridExtra)

hashtags <- read.csv('hashtags.csv')


ui <- fluidPage(
  headerPanel('NOAA Social Media Engagement'),
  sidebarPanel(
    selectInput('metric', 'Top retweeted hashtags', c('retweets')),
    checkboxGroupInput('account','NOAA Account',
                       c('NOAAHabitat','NOAAFish_WCRO',
                         'NOAAFisheries','NOAAResearch'),
                       selected=c('NOAAFish_WCRO'))
    
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Plot",
               
               plotOutput('plot1',width="100%")
      ),
      tabPanel("Table",
               tableOutput('table1')
      )
    )    
  )
)

server <- function(input, output) {
  
  rt <- reactive({
    hashtags %>% group_by(user) %>% arrange(-retweets) %>%
      filter(row_number()<=10) %>% 
      filter(user %in% input$account) %>%
      arrange(retweets) %>%
      mutate(hashtag=factor(hashtag,hashtag)) 
    
  })
  
  
  output$plot1 <- renderPlot({
    
    
    ggplot(rt(),
           aes(x=retweets,y=hashtag,xmin = 0, xmax = retweets)) + geom_point()  + 
      geom_errorbarh(height = .1) + 
      facet_grid(user~.,scales='free',space='free') + theme_bw() + 
      theme(axis.text=element_text(size=14))
    
    
    #ggplot(rt(),
    #  aes(x=hashtag,y=retweets)) + geom_bar(stat='identity') + coord_flip() + 
    #  facet_wrap(~user,scales='free_y') + theme_bw() + theme(axis.text=element_text(size=14))
    
    
  },height=600)
  
  
  output$table1 <- renderTable(print.data.frame(rt()))
  
}

shinyApp(ui = ui, server = server)