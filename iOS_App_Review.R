# Analyzing iOS App Store iTunes Reviews
# https://www.r-bloggers.com/analysing-ios-app-store-itunes-reviews-in-r/
# https://datascienceplus.com/analysing-ios-app-store-itunes-reviews-in-r/

# Set CRAN mirror
options(repos = c(CRAN = "http://cran.rstudio.com"))

# Set time zone
options(tz="America/New_York")

# install itunesr directly from CRAN:
install.packages("itunesr")
# the development version from GitHub:
# install.packages("devtools")
# devtools::install_github("amrrs/itunesr")
library(itunesr)

# Starbucks page:  https://apps.apple.com/us/app/starbucks/id331177714
# Latest (Page 1) Starbucks Reviews for the Country: US
sbux_reviews <- getReviews(331177714,'us',1)
# Displaying the column names 
names(sbux_reviews)
head(sbux_reviews,2)

# Ratings Trend
install.packages("highcharter")
# install.packages("lubridate")
# install.packages("dplyr")
library(highcharter)
library(dplyr)
library(lubridate)
dt <- sbux_reviews
dt$Date <- as.Date(dt$Date)
dt$Rating <- as.numeric(dt$Rating)    
dt <- dt %>% select(Date,Rating) %>% group_by(Date) %>% summarise(Rating = round(mean(Rating),2))
highchart() %>%   hc_add_series_times_values(dt$Date,dt$Rating, name = 'Average Rating')

# Sentiment Analysis 
install.packages("sentimentr")
library(sentimentr)
library(tidytext)
library(stringr)
# identify and define sentiments/words
get_sentiments("bing")
# pull in sbux info
reviews_only <- as.character(sbux_reviews$Review)
head(reviews_only)
sentiment_scores <- reviews_only %>% sentiment_by(by=NULL)
highchart() %>% hc_xAxis(sentiment_scores$element_id) %>% hc_add_series(sentiment_scores$ave_sentiment, name = 'Reviews Sentiment Scores')
