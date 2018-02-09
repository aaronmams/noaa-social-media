rm(list=ls())
library(gridExtra)

tweets <- read.csv('data/tweets.csv')
user.info <- read.csv('data/user_info.csv')

users <- c('@NOAAHabitat','@NOAAFisheries','@NOAAFish_WCRO','@NOAA','@NOAAResearch')
NOAA_screen_names <- c('NOAAHabitat','NOAAFisheries','NOAAFish_WCRO','NOAA','NOAAResearch')

#----------------------------------------------------------
# most retweeted hashtags from NOAA accounts
noaa.tweets <- tbl_df(tweets) %>% filter(screen_name %in% NOAA_screen_names) %>%
                 group_by(screen_name,created_at,text) %>%
                 filter(row_number()==1)

hash.list <- list()
for(i in 1:nrow(noaa.tweets)){
  h <- strsplit(as.character(noaa.tweets$hashtags[i]),";")
  rt <- noaa.tweets$retweet_count[i]
  user <- noaa.tweets$screen_name[i]
hash.list[[i]] <- data.frame(rt=rt,hashtag=unlist(h),user=user)  
}  
  
hashtags <- data.frame(rbindlist(hash.list))
hashtags$hashtag <- tolower(hashtags$hashtag)

hashtags <- tbl_df(hashtags) %>% group_by(user,hashtag) %>% 
              summarise(retweets=sum(rt)) %>% arrange(-retweets)

#plot top 10 most retweeted hashtags for each account
plot.data <- hashtags %>% group_by(user) %>% arrange(-retweets) %>%
               filter(row_number()<=10) %>% 
              filter(user=='NOAAResearch') %>%
              arrange(retweets) %>%
              mutate(hashtag=factor(hashtag,hashtag)) 

ggplot(plot.data,
       aes(x=hashtag,y=retweets)) + geom_bar(stat='identity') + coord_flip() + 
       theme_bw() + ggtitle('@NOAAResearch')


#----------------------------------------------------------