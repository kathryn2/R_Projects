# install.packages("remotes")
remotes::install_github("GuangchuangYu/nCov2019")
# select "none"

library(nCov2019)
library(tidyverse)

# Get the latest data
x <- get_nCov2019(lang = 'en')
head(x)


# just the global level data
covid_data_global <- x$global %>% 
  filter(deadRate > 0) %>% 
  mutate(deadRate = as.numeric(deadRate)) %>% 
  arrange(desc(deadRate)) 
covid_data_global

lolli <- ggplot(covid_data_global, aes(x=deadRate, y=reorder(name,deadRate))) +
  theme_classic() +
  geom_point(size=2, aes(colour='red')) +
  geom_segment(aes(x=0, xend=deadRate, y=name, yend=name)) +
  ylab("") +
  xlab("Death Rate (Percent)") +
  ggtitle("Death Rate by Country",subtitle = "Percent who die with positive test") +
  theme(legend.position = "none")
lolli


# show cumulative deaths
glbl_deaths <- x$global %>% 
  filter(!is.na(dead)) %>%
  filter(dead > 4) %>% 
  mutate(dead = as.numeric(dead)) %>% 
  arrange(desc(dead)) 
glbl_deaths

lolli2 <- ggplot(glbl_deaths, aes(x=dead, y=reorder(name,dead))) +
  theme_classic() +
  geom_point(size=2, aes(colour='red')) +
  geom_segment(aes(x=0, xend=dead, y=name, yend=name)) +
  ylab("") +
  xlab("# Deaths") +
  ggtitle("Deaths by Country",subtitle = "Countries with 5 deaths or more") +
  theme(legend.position = "none")
lolli2


# load historical data which is more useful for global time series
covid_data_ts <- load_nCov2019(lang = 'en', source='github')
head(covid_data_ts)

covid_data_ts_glbl <- covid_data_ts$global
head(covid_data_ts_glbl)

############ same as lollipop analysis above ############
# find the current death count by country
# covid_global_ttl <- covid_data_ts_glbl %>% 
#   group_by(country) %>% 
#   summarise(current_dead = max(cum_dead)) %>% 
#   arrange(desc(current_dead)) %>% 
#   filter(current_dead > 0)

# current death count by country
# covid_global_ttl
############ same as lollipop analysis above ############


# sort by date and show cumulative deaths
covid_global_daily <- covid_data_ts_glbl %>% 
  group_by(time) %>% 
  summarise(current_dead = max(cum_dead)) %>% 
  arrange(desc(time))
head(covid_global_daily)

# show total death rate globally
ttl_plt <- ggplot(covid_global_daily, aes(x=time, y=current_dead)) +
  ylab("Cumulative Deaths") +
  xlab("Dec '19 to Present") +
  ggtitle("Worldwide Deaths from COVID-19") +
  geom_line(stat="identity") + 
  # change theme to include grid for visible reference lines
  theme_minimal()
ttl_plt



# narrow down country list until 8 or less categorical variables
# this ensures each is distinguishable
covid_data_filtered <- covid_data_ts_glbl %>% filter(cum_dead > 29)

# manually select colors that are distinct from one another
# https://medialab.github.io/iwanthue/
fill_manual <- c("#77b592",
                 "#7e46b8",
                 "#91cf53",
                 "#c2568d",
                 "#be9c56",
                 "#8e98c4",
                 "#c4513a",
                 "#4c3b3c")

top_count <- ggplot(covid_data_filtered, aes(x=time, y=cum_dead, fill=country)) +
  geom_area() + 
  ggtitle('Confirmed Global Deaths From COVID 19', subtitle = 'running total by countries with 30+ deaths as of March 17, 2020') +
  ylab("") +
  xlab("") +
  # change theme to include grid for visible reference lines
  theme_minimal() +
  scale_fill_manual(values=fill_manual) +
  theme(legend.title=element_blank())
top_count



# create spiral with all countries with deaths
# leveraging code, with edits, from https://github.com/GuangchuangYu/nCov2019/blob/master/example.R

install.packages("shadowtext")
install.packages("ggplotify")
require(shadowtext)
require(ggplotify)

glbl_dtaset <- covid_data_ts['global']

dd <- filter(glbl_dtaset, time == time(covid_data_ts)) %>% 
  arrange(desc(cum_confirm)) 

# prep data
dd = dd[1:40, ]
dd$country = factor(dd$country, levels=dd$country)
cols <- rev(RColorBrewer::brewer.pal(10, "RdYlGn"))
dd$angle = 1:40 * 360/40
label_cut = 150
i = dd$angle >= 180 & dd$cum_confirm > label_cut
dd$angle[i] = dd$angle[i] + 180
j = dd$angle < 180 & dd$cum_confirm < 1000
dd$angle[j] = dd$angle[j] - 90
dd$vjust = 1
dd$vjust[i] = 0
dd$vjust[j] = 0.5
dd$covid_data_ts = dd$cum_confirm *.8
dd$covid_data_ts[j] = dd$covid_data_ts[j] * .7

# plot data
spiral <- ggplot(dd, aes(country, cum_confirm, fill=cum_confirm)) + 
  geom_col(width=1, color='grey90') + 
  geom_col(aes(y=I(5)), width=1, fill='grey90', alpha = .2) +       
  geom_col(aes(y=I(3)), width=1, fill='grey90', alpha = .2) +    
  geom_col(aes(y=I(2)), width=1, fill = "white") +
  scale_y_log10() + 
  scale_fill_gradientn(colors=cols, trans="log") + 
  geom_shadowtext(aes(label=paste(country, cum_confirm, sep="\n"), 
                      y = covid_data_ts, angle=angle, 
                      vjust=vjust), 
                  data=function(glbl_dtaset) glbl_dtaset[glbl_dtaset$cum_confirm > label_cut,], 
                  size=3, colour = "white", bg.colour="black", 
                  fontface="bold")  + 
  geom_text(aes(label=paste0(cum_confirm, ", ", country), 
                y = max(cum_confirm) * 2,  
                angle=angle+90), 
            data=function(glbl_dtaset) glbl_dtaset[glbl_dtaset$cum_confirm < label_cut,], 
            size=3, vjust=1) + 
  coord_polar(direction=-1) + 
  theme_void() + 
  theme(legend.position="none") 

ggplotify::as.ggplot(spiral, scale=1.2, vjust=-.1)



