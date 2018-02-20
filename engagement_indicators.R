rm(list=ls())
library(gridExtra)
library(dplyr)
library(ggplot2)
library(data.table)
library(lubridate)
library(ggthemes)

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
  arrange(user,retweets) %>%
  mutate(hashtag=factor(hashtag,hashtag)) 

ggplot(subset(plot.data,user %in% c('NOAAFish_WCRO','NOAAFisheries','NOAAHabitat')),
       aes(x=retweets,y=hashtag,xmin = 0, xmax = retweets)) + geom_point()  + 
  geom_errorbarh(height = .1) + 
  facet_grid(user~.,scales='free',space='free') + theme_bw()


#----------------------------------------------------------

#----------------------------------------------------------
# most retweeted hashtags from @ mentions

tweets.non.noaa <- tweets %>% filter(!screen_name %in% NOAA_screen_names)

hash.list <- list()
for(i in 1:nrow(tweets.non.noaa)){
  h <- strsplit(as.character(tweets.non.noaa$hashtags[i]),";")
  rt <- tweets.non.noaa$retweet_count[i]
  user <- tweets.non.noaa$screen_name[i]
  hash.list[[i]] <- data.frame(rt=rt,hashtag=unlist(h),user=user)  
}  

hashtags <- data.frame(rbindlist(hash.list))
hashtags$hashtag <- tolower(hashtags$hashtag)

hashtags <- tbl_df(hashtags) %>% group_by(user,hashtag) %>% 
  summarise(retweets=sum(rt)) %>% arrange(-retweets)

ggplot(hashtags,
       aes(x=hashtag,y=retweets)) + geom_bar(stat='identity') + coord_flip() + 
  theme_bw() + ggtitle('@NOAAResearch')

#----------------------------------------------------------

######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################

#----------------------------------------------------------
# Engagement overtime

#choose a time window
start <- as.Date('2018-01-01')
end <- as.Date('2018-02-11')

noaa.tweets <- tbl_df(tweets) %>% filter(screen_name %in% NOAA_screen_names) %>%
  filter(as.Date(created_at,format="%Y-%m-%d %H:%M:%S") <= end 
         & as.Date(created_at,format="%Y-%m-%d %H:%M:%S")>= start) %>%
  group_by(screen_name,created_at,text) %>%
  filter(row_number()==1)


hash.list <- list()
for(i in 1:nrow(noaa.tweets)){
  h <- strsplit(as.character(noaa.tweets$hashtags[i]),";")
  rt <- noaa.tweets$retweet_count[i]
  user <- noaa.tweets$screen_name[i]
  created <- noaa.tweets$created_at[i]
  hash.list[[i]] <- data.frame(rt=rt,hashtag=unlist(h),user=user,created=created)  
}  

hashtags <- data.frame(rbindlist(hash.list))
hashtags$hashtag <- tolower(hashtags$hashtag)
hashtags <- tbl_df(hashtags) %>% 
  group_by(hashtag) %>%
  mutate(created=as.Date(created,format='%Y-%m-%d %H:%M:%S')) %>%
  mutate(total=sum(rt)) %>%
  ungroup() %>%
  arrange(-total) %>%
  mutate(rank=dense_rank(-total))  %>%
  mutate(hashtag=ifelse(is.na(hashtag),"",hashtag))

#1 row per hashtag used in the time window
tags <- hashtags %>% group_by(hashtag) %>%
  filter(row_number()==1) %>% 
  mutate(start=start,end=end)


tags <- tags %>%
  rowwise() %>%
  do(data.frame(hashtag=.$hashtag, created=seq(.$start,.$end,by="1 day"))) %>%
  mutate(rtweets=0) 

hashtags <- hashtags %>% group_by(hashtag,created) %>% 
  summarise(rt=sum(rt))

df <- tags %>% 
  left_join(hashtags,by=c('hashtag','created')) %>%
  mutate(rt=ifelse(is.na(rt),rtweets,rt)) %>%
  mutate(rts=max(rt,rtweets)) 

df <- tbl_df(df) %>%  
  group_by(hashtag) %>% arrange(hashtag,created) %>%
  mutate(retweets=cumsum(rts)) 


#plot the top 20 hashtags during the time window
ht.total <- df %>% group_by(hashtag) %>% summarise(total=max(retweets,na.rm=T)) %>%  
  arrange(-total) %>% ungroup() %>% filter(row_number() <=20) %>%
  mutate(hashtag=factor(hashtag))

ggplot(ht.total,aes(x=reorder(hashtag,total),y=total)) + geom_bar(stat='identity',color='blue',alpha=0.6) + coord_flip() + 
  theme_wsj() + ggtitle('Total Hashtag Retweets')

#plot the top 5 non-empty hashtags by day over the time window
df <- df %>% filter(hashtag!="") %>%
  mutate(total=max(retweets,na.rm=T)) %>% 
  ungroup() %>% 
  mutate(rank=dense_rank(-total))  %>%
  filter(rank<=5) %>%
  arrange(rank) 

ggplot(df,aes(x=created,y=retweets,color=hashtag,shape=hashtag)) + geom_line() + geom_point() + 
  theme_wsj() + ggtitle('Retweets')


#----------------------------------------------------------------------------------------



#1 row per hashtag used in the time window
tags <- hashtags %>% group_by(hashtag) %>%
  filter(row_number()==1) %>% 
  mutate(start=start,end=end)


tags <- tags %>%
  rowwise() %>%
  do(data.frame(hashtag=.$hashtag, created=seq(.$start,.$end,by="1 day"))) %>%
  mutate(rtweets=0) 

hashtags <- hashtags %>% group_by(hashtag,created) %>% 
  summarise(rt=sum(rt))

df <- tags %>% 
  left_join(hashtags,by=c('hashtag','created')) %>%
  mutate(rt=ifelse(is.na(rt),rtweets,rt)) %>%
  mutate(rts=max(rt,rtweets)) 

df <- tbl_df(df) %>%  
  group_by(hashtag) %>% arrange(hashtag,created) %>%
  mutate(retweets=cumsum(rts)) 


ggplot(df,
       aes(x=created,y=retweets,color=hashtag)) + geom_line() +
  theme_bw()



#----------------------------------------------------------
