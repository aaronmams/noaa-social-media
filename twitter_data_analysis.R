
require(twitteR)
library(data.table)
library(dplyr)
library(ggplot2)

#first read in the current .csv file




#we are going to redo this whole thing using the rtweet package because it's
# waaaay better than twittR


users <- c('@NOAAHabitat','@NOAAFisheries','@NOAAFish_WCRO','@NOAA','@NOAAResearch')

#get tweets from each user, save them as a data frame, then bind the data frames
df <- list()
for(i in 1:length(users)){
  rt <- search_tweets(
    users[i], n = 500, include_rts = FALSE
  )
  
  df[[i]] <- rt %>% select(created_at,user_id,screen_name,text,
                      is_quote,is_retweet,favorite_count,
                      retweet_count,hashtags) %>% 
    mutate(update_time=Sys.time(),
           user=users[i])
Sys.sleep(60)
}




df <- lapply(users,function(x){
  rt <- search_tweets(
    x, n = 500, include_rts = FALSE
  )
  df <- rt %>% select(created_at,user_id,screen_name,text,
                      is_quote,is_retweet,favorite_count,
                      retweet_count,hashtags) %>% 
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

user.info <- lapply(users,function(x){
  user.info <- search_users(x) %>% 
    select(user_id,name,screen_name,location,description,followers_count,
           friends_count)
  return(user.info)
})

user.info <- data.frame(rbindlist(user.info))













############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################

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

consumer_key <- 'c71wImbS4aZnwA0yKx14Z5cxz'
consumer_secret <- 'HfRuYxKvAoXuTwTwUa9vslKlsB3AL5kQLv40U12Xsxz1ybkhuX'
access_token <- '750554190489985025-aOkEUbRXEXeyaKpHmM2VX5ZJ7WhoM7j'
access_secret <- '66o8C4tzc8LaqZNCE1XoJDU4QLFyPXu2qlUk3JlHBxd71'
  
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#------------------------------------------------------------------------------
#simple starter exercise:

# 1. pull twitter data on NOAA Fisheries tweets
# 2. who retweets NOAA science tweets
# 2A. where are they
# 2B. 
require(data.table)
require(ggplot2)
require(ggmap)
require(stringr)
require(tm)

noaa <- getUser("NOAAFisheries")
location(noaa)

#noaa_follower_IDs<-noaa$getFollowers(retryOnRateLimit=180)
noaa_follower_IDs<-noaa$getFollowers()
length(noaa_follower_IDs)
noaa_followers_df = rbindlist(lapply(noaa_follower_IDs,as.data.frame))

#remove followers that don't report a location
noaa_followers_df<-subset(noaa_followers_df, location!="")

# remove any instances of %
noaa_followers_df$location<-gsub("%", " ",noaa_followers_df$location)

#I also want to remove locations that I know geocode() won't be able to parse
# starting with anything with piping ("|")
#noaa_followers_df$location<-gsub("|", " ",noaa_followers_df$location)

#remove users whose location is some variation of USA
# returns string w/o leading or trailing whitespace
#trim <- function (x) gsub("^\\s+|\\s+$", "", x)
#trim(noaa_followers_df$location[1:200])

noaa_followers_df$location <- str_trim(noaa_followers_df$location,side='both')

noaa_followers_df <- noaa_followers_df[!noaa_followers_df$location %in% c("USA","U.S.A","United States",
                                                                          "UNITED STATES")]

#confine this example to a randomly sampled 2000 followers...this is because the 
# geocode() function from the ggmap package only allows you to ping the API 2,500 times
# unless you sign on as a developer...I don't want to mess with that at the moment so I'm
# just going to get a sample
geo <- function(i){
l <- noaa_followers_df$location[i]
loc <- geocode(l,output='all')

#error handling
if(is.na(loc)==T){
  d.tmp <- data.frame(lat=NA,long=NA) 
}else if(loc$status=='OK'){

    if(length(loc[[1]])==1){
      lat.tmp <- loc[[1]]
      lat.tmp <- lat.tmp[[1]]$geometry
      lat <- lat.tmp$location$lat
      long <- lat.tmp$location$lng
      d.tmp <- data.frame(lat=lat,long=long)
  
    }else{
      d.tmp <- data.frame(lat=NA,long=NA)  
    
    }
}else{
  d.tmp <- data.frame(lat=NA,long=NA)  
}
return(d.tmp)
}

t <- Sys.time()
follwer.geo <- lapply(c(101:500),geo)
Sys.time() - t

noaa.follower.geo <- data.frame(rbindlist(follwer.geo))

#going to split this up in case the batch geocoding routine fails
tmp1 <- cbind(noaa_followers_df$location[101:500],noaa.follower.geo)
tmp <- cbind(noaa_followers_df$location[1:100],noaa.follower.geo)
names(tmp) <- c('location','lat','long')
names(tmp1) <- c('location','lat','long')
tmp <- rbind(tmp,tmp1)

#now dump the lat/long from the tmp dataframe into ggmap and display a point for
#each user
map <- get_map(location = 'USA', zoom = 4)
ggmap(map) +
  geom_point(aes(x = long, y = lat), data = tmp, alpha = .5, size=3)




#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


#we are going to redo this whole thing using the rtweet package because it's
# waaaay better than twittR

library(rtweet)

users <- c('@NOAAHabitat','@NOAAFisheries','@NOAAFish_WCRO','@NOAA','@NOAAResearch')

#get tweets from each user, save them as a data frame, then bind the data frames
df <- lapply(users,function(x){
  rt <- search_tweets(
    x, n = 500, include_rts = FALSE
  )
  df <- rt %>% select(created_at,user_id,screen_name,text,
                      is_quote,is_retweet,favorite_count,
                      retweet_count,hashtags,
                      screen_name) %>% 
    mutate(update_time=Sys.time(),
           user=user)
return(df)  
})

df <- data.frame(rbindlist(df))


user.info <- lapply(users,function(x){
  user.info <- search_users(x) %>% 
    select(user_id,name,screen_name,location,description,followers_count,
           friends_count)
  return(user.info)
})

user.info <- data.frame(rbindlist(user.info))








#----------------------------------------------------------------------
#account info

# This only needs to happen once
user.info <- data.frame(rbindlist(lapply(users,function(x){
  df <- getUser(x)
  return(data.frame(user=x,description=df$description,location=df$location,id=df$id,created=df$created))
})))
#-----------------------------------------------------------------------

#------------------------------------------------------------------------
#pull follower count
users <- c('@NOAAHabitat','@NOAAFish_WCRO','@NOAAResearch','@NOAAFisheries')

#the follower count I want to pull once a week or so...
# that way we have a time series and we can see if certain tweets or events
#  increase viewership
lapply(users,function(x){
  df <- getUser(x)
  return(data.frame(user=x,followerCount=df$followersCount,favoritesCount=df$favoritesCount,
                    friendsCount=df$friendsCount,pull_date=Sys.time()))
})
#-------------------------------------------------------------------------

#------------------------------------------------------------------------
# Now pull individual tweets

tweet.pull <- function(x){
  query <- paste('from:',user,sep="")
  df <- searchTwitter(query,n=3000)
  
  content <- lapply(df,function(x){x$text})
  ids <- lapply(df,function(x){return(x$id)})
  rts <- lapply(df,function(x){return(x$retweetCount)})
  dates <- lapply(df,function(x){return(as.Date(x$created))})
  isrt <- lapply(df,function(x){return(x$isRetweet)})
  fav <- lapply(df,function(x){return(x$favoriteCount)})
  
  tweet.df <- data.frame(user=x,
                        text=unlist(content),
                         tweet_id=unlist(ids),
                         retweets=unlist(rts),
                         date=as.Date(unlist(dates),origin='1970-01-01'),
                         isRT=unlist(isrt),
                         favorites=unlist(fav))

  return(tweet.df)  
}

tweets <- data.frame(rbindlist(lapply(users,tweet.pull)))


extract.hashes = function(vec){
  
  hash.pattern = "#[[:alpha:]]+"
  have.hash = grep(x = vec, pattern = hash.pattern)
  
  hash.matches = gregexpr(pattern = hash.pattern,
                          text = vec[have.hash])
  extracted.hash = regmatches(x = vec[have.hash], m = hash.matches)
  
  df = data.frame(table(tolower(unlist(extracted.hash))))
  colnames(df) = c("tag","freq")
  df = df[order(df$freq,decreasing = TRUE),]
  return(df)
}






user <- users[i]
query <- paste('from:',user,sep="")
df <- searchTwitter(query,n=3000)

content <- lapply(noaa.wcro,function(x){x$text})
ids <- lapply(noaa.wcro,function(x){return(x$id)})
rts <- lapply(noaa.wcro,function(x){return(x$retweetCount)})
dates <- lapply(noaa.wcro,function(x){return(x$created)})
isrt <- lapply(noaa.wcro,function(x){return(x$isRetweet)})
fav <- lapply(noaa.wcro,function(x){return(x$favoriteCount)})

tweet.df <- data.frame(text=unlist(content),
           tweet_id=unlist(ids),
           retweets=unlist(rts),
           date=unlist(dates),
           isRT=unlist(isrt),
           favorites=unlist(fav))


#Get tweets from a NOAA account and 
noaa.hab = searchTwitter('from:NOAAHabitat',n=3000)

#Pull tweets with retweet and favorite info from WCRO
noaa.wcro = searchTwitter('from:NOAAFish_WCRO',n=3000)

content <- lapply(noaa.wcro,function(x){x$text})
ids <- lapply(noaa.wcro,function(x){return(x$id)})
rts <- lapply(noaa.wcro,function(x){return(x$retweetCount)})
dates <- lapply(noaa.wcro,function(x){return(x$created)})
isrt <- lapply(noaa.wcro,function(x){return(x$isRetweet)})
fav <- lapply(noaa.wcro,function(x){return(x$favoriteCount)})

wcro.tweet.df <- data.frame(
  account='@NOAAFish_WCRO',
  id=unlist(ids),
  retweetCount=unlist(rts),
  favCount=unlist(fav),
  date=unlist(dates),
  isRT=unlist(isrt),
  text=unlist(content)
)

write.csv(wcro.tweet.df,file="/Users/aaronmamula/Documents/social-media-analytics/R/wcro_tweets.csv')




#next step we want to see what the users who follow a certain account 
# (like @NOAAFisheries) tend to tweet about.  For this, we'll get the followers 
# for a user and then pull tweets from those users to see what they tend to tweet
# about
noaa.hab = searchTwitter('from:NOAAHabitat',n=3000)

tweets <- searchTwitter("@NOAAHabitat", n=100, lang="en", 
                        since="2018-01-20")

tweets <- searchTwitter("@NOAA", n=100, lang="en", 
                        since="2018-01-20")

tweets <- searchTwitter("@NOAAResearch", n=100, lang="en", 
                        since="2018-01-20")

tweets <- searchTwitter("@NOAAFisheries", n=100, lang="en", 
                        since="2018-01-20")

tweets <- searchTwitter("@NOAAFish_WCRO", n=100, lang="en", 
                        since="2018-01-20")




#get Ryan Mac's followers
nmac <- getUser('Rmac18')
nmac_followers <- nmac$getFollowers()
nmac_followers = rbindlist(lapply(nmac_followers,as.data.frame))

follower_tweets <- searchTwitter('from:@PaulTassi',n=100)
ft = twListToDF(follower_tweets)

#use some functions in the 'tm' package to count the occurrances of each word in
# PaulTassi's tweets
mac.text <- paste(ft$text, collapse=" ")
review_source <- VectorSource(mac.text)

#use some of tm's built in functions to clean the text data
corpus <- Corpus(review_source)
corpus <- tm_map(corpus, content_transformer(tolower))

corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords(“english”)
  
nmac = searchTwitter('@realDonaldTrump',n=3000)
dt <- twListToDF(trump_tweets)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

nbahash_tweets = searchTwitter("#nba",n=3000)
d = twListToDF(nbahash_tweets)

userInfo <- lookupUsers(d$screenName[1:10])  # Batch lookup of user info
userFrame <- twListToDF(userInfo)  # Convert to a nice dF
