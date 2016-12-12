library(streamR)
library(ggmap)
library(plyr)
library(dplyr)
library(stringr)
library(tidyr)

load("my_oauth.Rdata")



collect.data <- function(n, time){
  for(i in c(1:n)){
    file = str_c("tweetsUS", i, ".json")
    filterStream(file,
                 locations=c(-125, 25, -66, 50),timeout=time,
                 oauth=my_oauth)
  }
}


# collecting 24 hours worth of data - there were some connections lost, so I needed 51
collect.data(51, 1800)
## I did not include the data (they were 6.5gb), only the saved files at the end of this process

# removes tweets without hashtags
tweets.hash <- function(df){filter(df, grepl(str_c("#\\w+"), df$text))}


clean.import <- function(n){
  df = parseTweets("tweetsUS1.json")
  df = tweets.hash(df)
  for(i in c(2:n)){
    file.name = str_c("tweetsUS", i, ".json")
    temp.df = parseTweets(file.name)
    temp.df = tweets.hash(temp.df)
    df = rbind(df, temp.df)
  }
  df
}

# creates 1 dataframe from the 51 json files, with only tweets with hashtags
tweets.df <- clean.import(51)

#removes any unobtainable locations
tweets.df.1 <- tweets.df[!is.na(tweets.df$location) | !is.na(tweets.df$lan) | !is.na(tweets.df$lon),]


# when using the 1 hour data to check things out, I noticed a lot of job offers that didn't seem useful
no.job <- function(df){
  new.df = df
  # job hashtags
  jobs <- c("#Job", "#job", "#Jobs", "#Job", "#jobs", "#Hiring", "#hiring", "#Careerarc", "#CareerArc", "#careerarc")
  for(i in c(1:length(jobs))){
    new.df = filter(new.df, !grepl(jobs[i], new.df$text))
  }
  new.df
}


tweets.df.2 <- no.job(tweets.df.1)

# twitter data has geocode of location, from "full_name" of place
# there were 152,000 NA in lon, lat - this creates 0
tweets.df.2$lat[is.na(tweets.df.2$lat)] <- tweets.df.2$place_lat[is.na(tweets.df.2$lat)]
tweets.df.2$lon[is.na(tweets.df.2$lon)] <- tweets.df.2$place_lon[is.na(tweets.df.2$lon)]

tweets.df.2 <- tweets.df.2[tweets.df.2$lat >= 25 & tweets.df.2$lat <= 50,]
tweets.df.2 <- tweets.df.2[tweets.df.2$lon >= -125 & tweets.df.2$lon <= -66,]


# creates a data frame of id's and seperated hashtags
# takes a very long time to run
hashtags.df <- function(df){
  id_str = c()
  tag = c()
  for(i in 1:length(df$text)){
    hashtag = str_extract_all(df$text[i], "#\\w+", simplify = TRUE)
    for(j in 1:str_count(df$text[i], "#\\w+")){
      id_str = c(id_str, df$id_str[i])
      tag = c(tag, hashtag[,j])
    }
  }
  hashdat = data.frame('id_str' = id_str, 'tag' = tag)
}


# all hashtags with associated id
all.hashtags <- hashtags.df(tweets.df.2)
all.hashtags$tag <- as.character(all.hashtags$tag)

# each tweet has an observation for each hashtag
tweets.df.3 <- full_join(tweets.df.2, all.hashtags, by = "id_str")
tweets.df.3$tag <- as.character(tweets.df.3$tag)
# turning created at into regular POSIX for easier use
tweets.df.3$created_at <- strptime(tweets.df.3$created_at, "%a %b %d %H:%M:%S %z %Y", tz = "UTC")
tweets.df.3$created_at <- as.POSIXct(tweets.df.3$created_at)


saveRDS(all.hashtags, file = "hashtagList.rds")
saveRDS(all.hashtags, file = "shinyapp/hashtagList.rds")


saveRDS(tweets.df.3, file = "twitterData.rds")
saveRDS(tweets.df.3, file = "shinyapp/twitterData.rds")





# ranks and orders hashtags based on freq
# gives a data frame with unique hashtags and frequencies
# used in the shiny, to be used with all.hashtags 
top.tags <- function(df, n){
  freq = plyr::count(df, "tag")
  df = full_join(all.hashtags, freq, by = "tag")
  df <- distinct(df, tag, .keep_all = TRUE)
  df <- arrange(df, desc(freq))
  df <- slice(df, 1:n)
  df <- select(df, -id_str)
  df
}



