# Use newsflash package to illustrate prevalence of Irma on the news
# Code prepared on 9/12/17

# Newsflash documentation
# https://github.com/hrbrmstr/newsflash

# Install packages
devtools::install_github("hrbrmstr/newsflash")
install.packages("tidyverse")
install.packages("ggalt")
install.packages("brbrthemes")
install.packages("anytime")

# Load libraries
library(newsflash)
library(tidyverse)
library(ggalt)
library(hrbrthemes)
library(anytime) # Get errors without this

# Look at chryons (captioning)
ch <- read_chyrons("2019-09-02")
head(ch)

# Examine where Dorian mentioned ("mention" column)
# Convert time to hourly chunks ("hour" column)
ch2 <- mutate(ch, 
  hour = lubridate::hour(ts),
  text = tolower(text),
  mention = grepl("dorian", text))
head(ch2)

ch2 %>% filter(mention) %>% 
  count(hour, channel) %>% 
  ggplot(aes(hour, n)) +
  geom_segment(aes(xend=hour, yend=0), 
               color = "lightslategray", size=1) + 
  scale_x_continuous(name="Hour (GMT)", breaks=seq(0, 23, 6),
                   labels=sprintf("%02d:00", seq(0, 23, 6))) +
  scale_y_continuous(name="# Chyrons", limits=c(0,20)) +
  facet_wrap(~channel, scales="free") +
  labs(title="'Dorian' Mentions per Hour on 2019 Labor Day") 


# How about networks?
ne <- list_networks(widget=FALSE)
print(ne)

# Query Dorian data
# Orlando networks
# Primary term = Dorian
# Secondary term = Hurricane
# Starting just before Labor Day weekend
# https://rdrr.io/github/hrbrmstr/newsflash/man/query_tv.html
dorian <- query_tv("Dorian", mode = "TimelineVol",
                   start_date = "2019-08-27", end_date = "2019-09-12")
head(dorian)

# Visualize network coverage over 3 weeks
# Themes causing errors, so commenting out parts
# https://github.com/hrbrmstr/hrbrthemes
ne2 <- query_tv('Dorian', mode = "TimelineVol",
                start_date = "2019-08-27", end_date = "2019-09-12") %>% 
        arrange(date) %>% 
        ggplot(aes(date, value, group=network)) +
        ggalt::geom_xspline(aes(color=network)) +
        ggthemes::scale_color_tableau(name=NULL) +
        labs(x=NULL, y="Volume Metric", 
             title="'Dorian' Trends Across National Networks") +
        facet_wrap(~network) +
        # theme_ipsum_rc(grid="XY") +
        theme(legend.position="none") 
ne2


# What are the top words associated with Dorian?
# Let's do a word cloud
wc <- query_tv('Dorian', mode = "WordCloud", 
               start_date = "2019-09-01", end_date = "2019-09-03")
head(wc)

wc2 <- ggplot(wc, aes(x=1, y=1)) +
        ggrepel::geom_label_repel(aes(label=label, size=count), segment.colour="#00000000", segment.size=0) +
        scale_size_continuous(trans="sqrt") +
        labs(x=NULL, y=NULL) +
        theme_ipsum_rc(grid="") +
        theme(axis.text=element_blank()) +
        theme(legend.position="none") 

wc2

# The End