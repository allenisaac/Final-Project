# ui
library(shiny)

shinyUI(fluidPage(
  headerPanel("Mapping Top Hashtags"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("ntag",
                   "Number of Hashtags:",
                   value = 30,
                   min = 5,
                   max = 50,
                   step = 1),

      checkboxInput("slider",
                    "Show The Entire Day?",
                   TRUE),
    conditionalPanel(  
      condition = "input.slider == false",
      sliderInput("time",
                  "Hour Posted:",
                  min = 0,
                  max = 23,
                  value = 15)),
    tableOutput("legend")
    ),
    mainPanel(
      fluidRow(column(
        width = 6,
        uiOutput("tag"),  
      column(width = 12,
            h4(textOutput("tagname"))
      ))),
      tabsetPanel(
        tabPanel("Summary", br(),   p("This application uses Twitter data from Friday, December 9th at 5pm EST to Saturday, December 10th at 5pm EST.  The time
                      data itself is output in UTC timezone, and therefore spans from 10pm - 10pm, or 20 - 20, when viewed numerically.
                     The purpose is to view the top used hashtags that had geolocation enabled."), br(),
                     h5("Hypotheses:"), br(),
                     p("What I wanted to look at, beyond just the map, was information about distances.  My first idea was that the distance of the tweets
                     from their mean location would be normally distributed in a multivariate normal distribution.  To simplify this I decided to plot distributions
                     in a 2d [0:inf) manner, by looking at distance rather than vectors from mean location to tweet.  I thought this would result in a squared normal
                     distribution.  Just from the visuals alone I could see there was no consistency between hashtags. These distribution graphs are generated on the 
                     bottom of the 'Distributions' tab.  There is no graph for all hashtags, as that wouldn't give any useful information."), br(),
                     p("Next I decided to look at distance versus the time since the first appearance of the hashtag.  I considered there may be a linear relationship 
                     between the two, however were not very clear.  Though a simple linear regression usually showed a significant slope, that slope was always close
                     to zero, even when the scales for distance and time were similar.  The significance could have also been helped by the large amounts of data.  In
                     addition, the R-square term is horendously low.  The graphs and a summary table of the regression are shown at the top of the 'Distributions' tab."),
                     br(),
                     h5("The UI:"), br(),
                     p("The Number of hashtags allows the user to change how many hashtags are considered and plotted, spanning from 5 to 50.
                     The user can also choose to look at single hashtags, which are chosen according to ranking."), br(),
                     p("It may be of interest to look at the top hashtags by hour, it is important to note that the values are in UTC timezone hours.  Also
                     as a result of data collection, the first hour chronologically is 20, and the last is 19."), br(),
                     p("On the left side is a legend of hashtags and their ranks for the selected parameters.")),
        tabPanel("Map", plotOutput("map")),
        tabPanel("Distribution", plotOutput("reg"),
                 verbatimTextOutput("test"),
                 plotOutput("dist")))
  )))
)