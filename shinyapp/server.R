# server
library(shiny)
library(ggplot2)
library(ggmap)
library(grid)
library(plyr)
library(dplyr)
library(stringr)
library(xtable)
library(randomcoloR)
library(geosphere)

top.tags <- function(df, n){
  freq = plyr::count(df, "tag")
  df = full_join(df, freq, by = "tag")
  df <- distinct(df, tag, .keep_all = TRUE)
  df <- arrange(df, desc(freq))
  df <- slice(df, 1:n)
  df <- select(df, -id_str)
  df
}

all.hashtags <- readRDS("hashtagList.rds")
tweet.df <- readRDS("twitterData.rds")
shinyServer(function(input, output){
  hashtag.rank <- reactive({
    # creates a vector of hexcodes for colors to keep color consistent when changing hashtags
    # colors are linked with hashtag rank
    color.vec <- distinctColorPalette(input$ntag)
    # either all hours or by hour
    if (input$slider == TRUE){
      rank.df <- top.tags(all.hashtags, input$ntag)
    } else {
    # only selects rows from specified hours
      if(input$time >= 10){
        time.hashtags = filter(tweet.df, grepl(str_c(" ", as.character(input$time), ":"), 
                                         tweet.df$created_at))}
      else{
        time.hashtags = filter(tweet.df, grepl(str_c(" 0", as.character(input$time), ":"), 
                                            tweet.df$created_at))}
      time.hashtags = select(time.hashtags, id_str, tag)
      rank.df <- top.tags(time.hashtags, input$ntag)
    }
    # add color vector to dataframe
    rank.df$color = color.vec
    rank.df
  })
  react.df <- reactive({
    # joining hashtag ranking data frame with the full data frame, so their freq is uncluded
    # only uses the hashtags included in top ranks
    if (input$slider == TRUE){
      right_join(tweet.df, hashtag.rank(), by = "tag")
    } else {
      if(input$time >= 10){
      time.df = filter(tweet.df, grepl(str_c(" ", as.character(input$time), ":"), 
                                            tweet.df$created_at))}
      else{time.df = filter(tweet.df, grepl(str_c(" 0", as.character(input$time), ":"), 
                                            tweet.df$created_at))}
      right_join(time.df, hashtag.rank(), by = "tag")
    }
  })
  dist.df <- reactive({
    mean_lon <- c()
    mean_lat <- c()
    # finding mean location and distance from mean location, adding to dataframe
    for(i in 1:length(hashtag.rank()$tag)){
      av_lon = sum(react.df()[react.df()$tag == hashtag.rank()$tag[i],]$lon) /
        hashtag.rank()$freq[i]
      av_lat = sum(react.df()[react.df()$tag == hashtag.rank()$tag[i],]$lat) /
        hashtag.rank()$freq[i]
      mean_lon = c(mean_lon, av_lon)
      mean_lat = c(mean_lat, av_lat)
    }
    loc.df = data.frame(hashtag.rank(), mlon = mean_lon, mlat = mean_lat)
    loc.df = right_join(react.df(), loc.df, by = "tag")
    # proper distance based on lon and lat, in miles
    coord.vec = distGeo(data.frame(loc.df$mlon, loc.df$mlat), 
                       data.frame(loc.df$lon, loc.df$lat))/1609.34
    loc.df$coord.dist = coord.vec
    if (input$tagrank != "All"){
      loc.df = loc.df[loc.df$tag == hashtag.rank()$tag[as.numeric(input$tagrank)],]}
    # adds time since first appearance of tweet in minutes
    loc.df = loc.df %>% group_by(tag) %>% 
        mutate(time.taken = as.numeric(difftime(created_at, min(created_at), units = "mins")))
    loc.df
  })
  output$tag <- renderUI({
    # reactive UI for selecting specific hashtags by rank
    # reactive b/c number of hashtags must be known first
    rankings <- c("All", 1:input$ntag)
      selectInput("tagrank",
                  "Select Hashtag Ranking:",
                  rankings,
                  selected = "All")
  })
  output$map <- renderPlot({
    map.data = map_data("state")
    # Choosing points to display based on which hashtag rank is selected
    # point size changes based on selection, colors stay the same
    if(input$tagrank == "All"){
      points = data.frame(Hashtag=factor(react.df()$tag),
                          x=as.numeric(react.df()$lon),
                          y=as.numeric(react.df()$lat),
                          col = react.df()$color)
      if(input$slider == TRUE){
        point.size = 1.3} else{
          point.size = 2
        }
    } else {
      dat <- right_join(tweet.df, hashtag.rank()[as.integer(input$tagrank),], by = "tag")
      points = data.frame(x=as.numeric(dat$lon),
                          y=as.numeric(dat$lat),
                          Hashtag = factor(dat$tag),
                          col = dat$color)
      point.size = 2
    }
    # creating map
    ggplot(map.data)+
      geom_map(aes(map_id=region),
               map=map.data,
               fill="white", 
               color="grey20",size=0.25)+
      expand_limits(x=map.data$long,y=map.data$lat)+ 
      theme(axis.line=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank(), 
            axis.title=element_blank(),
            panel.background=element_blank(),
            panel.border=element_blank(), 
            panel.grid.major=element_blank(),
            plot.background=element_blank(), 
            plot.margin=unit(0*c(-1.5,-1.5,-1.5,-1.5),"lines")) + 
      geom_point(data=points,
            aes(x=x,y=y),size=point.size,
            alpha=0.5, color = points$col, show.legend = FALSE) +
      coord_fixed(1.3)
  })
  output$tagname <- renderText({
    # write hashtag name of the selected rank
    if(input$tagrank == "All"){
      "All Hashtags"
    } else {
      id = as.numeric(input$tagrank)
      hashtag.rank()$tag[id]}
  })
  output$legend <- renderTable({
    # create legend of sidepanel, so the legend stays consistent when selecting individual hashtags
    legend.df <- data.frame(Rank = seq.int(nrow(hashtag.rank())), Freq = hashtag.rank()$freq, Hashtag = hashtag.rank()$tag)
    xtable(legend.df)
  })
  output$dist <- renderPlot({
    # creates a pdf of distance from mean location, only when specific hashtags are selected
    if(input$tagrank == "All"){
    } else {
      line.col = hashtag.rank()$color[as.numeric(input$tagrank)]
      ggplot(dist.df()) + geom_area(aes(x=coord.dist), fill = line.col, stat = "density") + 
        xlab("Miles From Mean Location") + 
        ylab("Probability Density") + 
        ggtitle("Probability Distribution of Distance from Mean Location")
    }
  })
  output$test <- renderPrint({
    #linear regression of distance vs time since first appearance of hashtag
      summary(lm(coord.dist ~ time.taken, data = dist.df()))
  })
  output$reg <- renderPlot({
    # plots points of distance vs time since first appearance
    # also adds lm line
    ggplot(dist.df(), aes(time.taken, coord.dist)) + geom_point() +
      geom_smooth(method = lm, se = FALSE) + 
      xlab("Minutes Since First Observation of the Hashtag") +
      ylab("Miles From Mean Location") +
      ggtitle("Distance and Time from Initial Posting")
  })
})