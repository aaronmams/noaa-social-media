require(twitteR)
library(data.table)
library(dplyr)
library(ggplot2)

#--------------------------------------------------------------------------
#first read in the current .csv file

tweets.ref <- read.csv('data/tweets.csv')
user.ref <- read.csv('data/user_info.csv')
#---------------------------------------------------------------------------

#----------------------------------------------------------------------------
#Now we do a data pull 

users <- c('@NOAAHabitat','@NOAAFisheries','@NOAAFish_WCRO','@NOAA','@NOAAResearch')

# get tweets from each of our accounts
tl <- get_timeline('@NOAAHabitat',n=1000)
tl <- rbind(tl,get_timeline('@NOAAFisheries',n=1000))
tl <- rbind(tl,get_timeline('@NOAAFish_WCRO',n=1000))
tl <- rbind(tl,get_timeline('@NOAA',n=500))
tl <- rbind(tl,get_timeline('@NOAAResearch',n=1000))

tl <- tl %>% select(created_at,user_id,screen_name,text,
                    is_quote,is_retweet,favorite_count,
                    retweet_count,hashtags,mentions_screen_name) %>%
       mutate(update_time=Sys.time())

#fix hashtags
hash <- unlist(lapply(tl$hashtags,function(x){collapse(c(unlist(x)),sep=";")}))
tl <- tl %>% mutate(hash1=hash) %>%
  select(-hashtags) %>%
  mutate(hashtags=hash1) %>%
  select(-hash1)

#fix @ mentions
mentions <- unlist(lapply(tl$mentions_screen_name,function(x){collapse(c(unlist(x)),sep=";")}))
tl <- tl %>% mutate(mentions1=mentions) %>%
  select(-mentions_screen_name) %>%
  mutate(mentions=mentions1) %>%
  select(-mentions1)

#-----------------------------------------------------------------------------------
#get tweets mentioning user, save them as a data frame, then bind the data frames
df <- lapply(users,function(x){
  rt <- search_tweets(
    x, n = 1000, include_rts = FALSE
  )
  df <- rt %>% select(created_at,user_id,screen_name,text,
                      is_quote,is_retweet,favorite_count,
                      retweet_count,hashtags,mentions_screen_name) %>% 
    mutate(update_time=Sys.time())
  return(df)  
})

df <- data.frame(rbindlist(df))

#fix hashtag list
hash <- unlist(lapply(df$hashtags,function(x){collapse(c(unlist(x)),sep=";")}))
df <- df %>% mutate(hash1=hash) %>%
  select(-hashtags) %>%
  mutate(hashtags=hash1) %>%
  select(-hash1)

#fix the @ mention list
mentions <- unlist(lapply(df$mentions_screen_name,function(x){collapse(c(unlist(x)),sep=";")}))
df <- df %>% mutate(mentions1=mentions) %>%
  select(-mentions_screen_name) %>%
  mutate(mentions=mentions1) %>%
  select(-mentions1)
#--------------------------------------------------------------------------


user.info <- lapply(users,function(x){
  user.info <- search_users(x) %>% 
    select(user_id,name,screen_name,location,description,followers_count,
           friends_count)
  return(user.info)
})

user.info <- data.frame(rbindlist(user.info))
#---------------------------------------------------------------------------------

#--------------------------------------------------------------------------------
#add any new tweets to the data set
tweets <- rbind(df,tl)
tweets <- rbind(tweets.ref,df) %>% group_by(user_id,created_at,text) %>% 
             filter(row_number() ==1)

write.csv(tweets,file='data/tweets.csv')
#--------------------------------------------------------------------------------


#---------------------------------------------------------------------------------
# for the user info we just bind it to what was already there

user.info <- rbind(user.ref,user.info)
write.csv(user.info,'data/user_info.csv')

#---------------------------------------------------------------------------------