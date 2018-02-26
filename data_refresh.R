require(twitteR)
library(data.table)
library(dplyr)
library(ggplot2)
library(rtweet)
library(RODBC)

#------------------------------------------------------------------
#Twitter credentials

#pw: Nmfsswfsc2016#
#username: aaronmams
#email: aaron.mams@gmail.com
#app name: mamultron
#owner: mamulsaurus
#owner id: 750554190489985025


# consumer key:
# c71wImbS4aZnwA0yKx14Z5cxz

# consumer secret
#HfRuYxKvAoXuTwTwUa9vslKlsB3AL5kQLv40U12Xsxz1ybkhuX

# access token
#750554190489985025-aOkEUbRXEXeyaKpHmM2VX5ZJ7WhoM7j

#access token secret
# 66o8C4tzc8LaqZNCE1XoJDU4QLFyPXu2qlUk3JlHBxd71

# this should be uncessary now
#consumer_key <- 'c71wImbS4aZnwA0yKx14Z5cxz'
#consumer_secret <- 'HfRuYxKvAoXuTwTwUa9vslKlsB3AL5kQLv40U12Xsxz1ybkhuX'
#access_token <- '750554190489985025-aOkEUbRXEXeyaKpHmM2VX5ZJ7WhoM7j'
#access_secret <- '66o8C4tzc8LaqZNCE1XoJDU4QLFyPXu2qlUk3JlHBxd71'

#twitter_token <- create_token(app='mamultron',
#                              consumer_key=consumer_key,
#                              consumer_secret=consumer_secret)

#setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
#------------------------------------------------------------------------------------


#------------------------------------------------------------------------------



#-------------------------------------------------------------------------
# get the old stuff
channel <- odbcConnect(dsn='pinniger', uid='',pwd='')
content <- sqlQuery(channel,'select * from [PacFIN_Econ].[dbo].[tweets_content]')
users.info <- sqlQuery(channel,'select * from [PacFIN_Econ].[dbo].[tweets_users]')
close(channel)
#-------------------------------------------------------------------------


#-----------------------------------------------------------------------
#pull some new stuff

users <- c('@NOAAHabitat','@NOAAFisheries','@NOAAFish_WCRO','@NOAA','@NOAAResearch','@NOAANCEIclimate',
           '@NOAAClimate','@MBNMS','@NOAA_CINMS','@Eileen_NOAAFish','@NOAAFisheriesAK','@NOAAFish_PIRO',
           '@NOAAFish_PIFSC','@NOAAFish_SERO','@CenterForBioDiv')

screen_names <- c('NOAAHabitat','NOAAFisheries','NOAAFish_WCRO','NOAA','NOAAResearch','NOAANCEIclimate',
                  'NOAAClimate','MBNMS','NOAA_CINMS','Eileen_NOAAFish','NOAAFisheriesAK','NOAAFish_PIRO',
                  'NOAAFish_PIFSC','NOAAFish_SERO','CenterForBioDiv')


# get tweets from each of our accounts
tl <- get_timeline('@NOAAHabitat',n=200,retryonratelimit=TRUE)
tl <- rbind(tl,get_timeline('@NOAAFisheries',n=200))
tl <- rbind(tl,get_timeline('@NOAAFish_WCRO',n=200))
tl <- rbind(tl,get_timeline('@NOAA',n=200))
tl <- rbind(tl,get_timeline('@NOAAResearch',n=200))

tl <- rbind(tl,get_timeline('@NOAANCEIclimate',n=200))
tl <- rbind(tl,get_timeline('@NOAAClimate',n=200))
tl <- rbind(tl,get_timeline('@MBNMS',n=200))
tl <- rbind(tl,get_timeline('@Eileen_NOAAFish',n=200))
tl <- rbind(tl,get_timeline('@NOAAFisheriesAK',n=200))
tl <- rbind(tl,get_timeline('@NOAAFish_PIRO',n=200))
tl <- rbind(tl,get_timeline('@NOAAFish_PIFSC',n=200))
tl <- rbind(tl,get_timeline('@NOAAFish_SERO',n=200))
tl <- rbind(tl,get_timeline('@CenterforBioDiv',n=200))


tl <- tl %>% select(created_at,user_id,screen_name,text,
                    is_quote,is_retweet,favorite_count,
                    retweet_count,hashtags,mentions_screen_name) %>%
  mutate(update_time=Sys.time())

#fix hashtags
hash <- unlist(lapply(tl$hashtags,function(x){paste(c(unlist(x)),collapse=";")}))

tl <- tl %>% mutate(hash1=hash) %>%
  select(-hashtags) %>%
  mutate(hashtags=hash1) %>%
  select(-hash1)

#fix @ mentions
mentions <- unlist(lapply(tl$mentions_screen_name,function(x){paste(c(unlist(x)),collapse=";")}))
tl <- tl %>% mutate(mentions1=mentions) %>%
  select(-mentions_screen_name) %>%
  mutate(mentions=mentions1) %>%
  select(-mentions1)


#-----------------------------------------------------------------------


#-----------------------------------------------------------------------------------
#get tweets mentioning user, save them as a data frame, then bind the data frames

dummy.df <- search_tweets('#love',n=10)


df <- lapply(users,function(x){
  rt <- search_tweets(
    x, n = 500, include_rts = TRUE
  )
  
  if(nrow(rt)==0){
    df <- dummy.df %>%  select(created_at,user_id,screen_name,text,
                               is_quote,is_retweet,favorite_count,
                               retweet_count,hashtags,mentions_screen_name) %>% 
      mutate(update_time=Sys.time()) 
  }else{
    df <- rt %>% select(created_at,user_id,screen_name,text,
                        is_quote,is_retweet,favorite_count,
                        retweet_count,hashtags,mentions_screen_name) %>% 
      mutate(update_time=Sys.time())
  }
  return(df)  
})

df <- data.frame(rbindlist(df))

#fix hashtag list
hash <- unlist(lapply(df$hashtags,function(x){paste(c(unlist(x)),collapse=";")}))
df <- df %>% mutate(hash1=hash) %>%
  select(-hashtags) %>%
  mutate(hashtags=hash1) %>%
  select(-hash1)

#fix the @ mention list
mentions <- unlist(lapply(df$mentions_screen_name,function(x){paste(c(unlist(x)),collapse=";")}))
df <- df %>% mutate(mentions1=mentions) %>%
  select(-mentions_screen_name) %>%
  mutate(mentions=mentions1) %>%
  select(-mentions1)
#--------------------------------------------------------------------------

#--------------------------------------------------------------------------
#get the user info

new.user.info <- lapply(users,function(x){
  user.info <- search_users(x) %>% 
    select(user_id,name,screen_name,location,description,followers_count,
           friends_count)
  return(user.info)
})

new.user.info <- data.frame(rbindlist(new.user.info))

new.user.info <- new.user.info %>% mutate(updated=as.POSIXct(Sys.time()))
#--------------------------------------------------------------------------
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#Now we have updated information and we want to:

# 1. append the new information to the old info in a data frame
# 2. push that data frame up to the PacFIN_Econ database

content <- rbind(content,tl,df)
users.info <- rbind(users.info,new.user.info) 

#get ride of any duplicates
content <- content %>% group_by(user_id,screen_name,created_at,text) %>%
           filter(row_number()==1)

users.info <- users.info %>%
              group_by(user_id,screen_name,updated) %>%
              filter(row_number()==1)

#save these as back up .csv file
write.csv(content,'data/tweet_content.csv')
write.csv(users.info,'data/tweet_users.csv')

#Final Step: push these data frames to the databases

# First, the content data frame

# the database tables

channel <- odbcConnect(dsn='pinniger', uid='',pwd='aaron24')
sqlQuery(channel,'USE PacFIN_Econ')
cols <- sqlColumns(channel,"tweets_content")
colTypes <- as.character(cols$TYPE_NAME)
colTypes[which(colTypes=='bit')] <- 'varchar(5)'
colTypes[which(colTypes=='varchar')] <- 'varchar(255)'
names(colTypes) <- as.character(cols$COLUMN_NAME)
sqlQuery(channel,"drop table tweets_content")
sqlSave(channel,content,tablename='tweets_content',append=T,rownames=F,varTypes=colTypes)
close(channel)


channel <- odbcConnect(dsn='pinniger', uid='',pwd='')
sqlQuery(channel,'USE PacFIN_Econ')
cols.users <- sqlColumns(channel,'tweets_users')
colTypes <- as.character(cols.users$TYPE_NAME)
colTypes[which(colTypes=='bit')] <- 'varchar(5)'
colTypes[which(colTypes=='varchar')] <- 'varchar(500)'
names(colTypes) <- as.character(cols.users$COLUMN_NAME)
sqlQuery(channel,"drop table tweets_users")
sqlSave(channel,users.info,tablename='tweets_users',append=T,rownames=F,varTypes=colTypes)
close(channel)

#--------------------------------------------------------------------------

