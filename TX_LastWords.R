# Data Source:
# https://www.kaggle.com/ianmobbs/texas-death-row-executions-info-and-last-words
# The name, age, race, county, date, and last words of Texas death row inmates from 1982 to 2017.

# Set up tidytext
require(tidytext)

# Set working directory
setwd("~/Desktop/R/TX_Executions/text/")

# Load dataset
offenders <- read.table("Last_Statement.txt")
#offenders <- readLines(file.choose())
head(offenders)

# Load other packages
require(dplyr)
require(ggplot2)
require(wordcloud)
require(stringr)
require(tm)
require(SnowballC)
require(RColorBrewer)

# Convert text to corpus
lastWords <- Corpus(VectorSource(offenders))
inspect(lastWords)

# Clean up text
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
lastWords <- tm_map(lastWords, toSpace, "/")
lastWords <- tm_map(lastWords, toSpace, "@")
lastWords <- tm_map(lastWords, toSpace, "\\|")
lastWords <- tm_map(lastWords, toSpace, "\x89")
lastWords <- tm_map(lastWords, toSpace, "xdb")
lastWords <- tm_map(lastWords, toSpace, "xcf")
lastWords <- tm_map(lastWords, toSpace, "xe5")
lastWords <- tm_map(lastWords, toSpace, "xca")

# Convert the text to lower case
lastWords <- tm_map(lastWords, content_transformer(tolower))

# Remove numbers
lastWords <- tm_map(lastWords, removeNumbers)

# Remove english common stopwords
lastWords <- tm_map(lastWords, removeWords, stopwords("english"))

# Remove punctuations
lastWords <- tm_map(lastWords, removePunctuation)

# Eliminate extra white spaces
lastWords <- tm_map(lastWords, stripWhitespace)



# Construct term document matrix
dtm <- TermDocumentMatrix(lastWords)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# Create word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 20,
          max.words=75, random.order=FALSE, rot.per=0.00, 
          colors=brewer.pal(4, "RdGy"))

# Plot word frequencies
# Bar chart
barplot(d[1:15,]$freq, las = 2, names.arg = d[1:15,]$word,
        col ="grey", main ="Most Spoken Last Words",
        ylab = "Frequency")

