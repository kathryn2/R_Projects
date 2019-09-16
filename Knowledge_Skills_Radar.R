# Set working directory
# Note, I use a Mac
setwd("~/Desktop")

# Install and load needed libraries
install.packages("radarchart")
library(radarchart)

# Obtain and load data
# Source:  https://www.onetcenter.org/database.html?p=3
# Knowledge download = Knowledge.xlsx
df3 <- read.csv(file="Knowledge3.csv", header=TRUE, sep=",")
# Preview data
head(df3)

# Create radar chart
chartJSRadar(df3, maxScale=5)


