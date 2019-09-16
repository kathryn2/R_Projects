# Install needed packages
library(ggplot2)
library(vcd)
library(stats)
library(car)

# Load data that will be used for this analysis
# Rename Football Stats dataset (with drank rankings) 
footballstats.data<-read.csv("Dataset1FootballStats.csv")
# Rename Team Football Stats dataset 
teamfootballstats.data<-read.csv("Dataset2TeamFootballStats.csv")
# Rename Deflategate dataset 
football.data<-read.csv("Dataset3FootballDeflatagate.csv")


#Data Preparation
footballstats.data$Rank <- as.numeric(as.character(footballstats.data$Rank)) 
head(footballstats.data$Rank, n=10)

#histogram
hist(footballstats.data$Rank,breaks=10, col="green", border = "blue", main = "Histogram of Rank") 
hist(footballstats.data$Wins,breaks=10, col="green", border = "blue", main = "Histogram of Wins")

#scatter plot
plot(footballstats.data$Rank, footballstats.data$Losses) + title(main="Scatter Plot of Draft Rank to Team Losses")

#Correlations
cor(footballstats.data$Rank, footballstats.data$Wins, use="complete", method="pearson") 
# 0.4007555
cor(footballstats.data$Grade, footballstats.data$Rank, use="complete", method="pearson")
# -0.5723498

#Data Preparation
teamfootballstats.data$Wins <- as.numeric(as.character(footballstats.data$Rank)) 
NFC <- subset(teamfootballstats.data,NFC==1)
AFC <- subset(teamfootballstats.data,NFC==0)
Redskins <- subset(teamfootballstats.data,Team.Name=="Washington.Redskins") 
Tampa <- subset(teamfootballstats.data,Team.Name=="Tampa.Bay.Bucaneers") 
Jaguars <- subset(teamfootballstats.data,Team.Name=="Jacksonville.Jaguars")

#histogram
hist(teamfootballstats.data$Wins,breaks=10, col="blue", border = "green", main = "Histogram of Wins - ALL TEAMS")
hist(NFC$Wins,breaks=10, col="blue", border = "green", main = "Histogram of Wins - NFC") 
hist(AFC$Wins,breaks=10, col="blue", border = "green", main = "Histogram of Wins - AFC")

#Correlations
cor(teamfootballstats.data$Wins, teamfootballstats.data$Playoff.Berth, use="complete", method="pearson") 
# 0.7324997
cor(teamfootballstats.data$Wins, teamfootballstats.data$Clinched.Division, use="complete", method="pearson")
# 0.608269
cor(teamfootballstats.data$Wins, teamfootballstats.data$PCT, use="complete", method="pearson") 
# 0.9992527
cor(NFC$Wins, NFC$Playoff.Berth, use="complete", method="pearson")
# 0.7592389
cor(NFC$Wins, NFC$Clinched.Division, use="complete", method="pearson")
# 0.5499623
cor(NFC$Wins, NFC$PCT, use="complete", method="pearson")
# 0.9991874
cor(AFC$Wins, AFC$Playoff.Berth, use="complete", method="pearson")
# 0.7086859
cor(AFC$Wins, AFC$Clinched.Division, use="complete", method="pearson")
# 0.658829
cor(AFC$Wins, AFC$PCT, use="complete", method="pearson")
# 0.993231

#T-Test
t.test(NFC$Wins, AFC$Wins)

#ANOVA
anova.Wins <- aov(Playoff.Berth ~ Wins + Loss, data=teamfootballstats.data) 
summary(anova.Wins)

#Regression - Number of wins
fit.Wins <- lm(Wins ~ Playoff.Berth + Clinched.Division + Clinched.1st.Round.Bye, data=teamfootballstats.data) 
coefficients(fit.Wins)
fitted(fit.Wins) # predicted values 
residuals(fit.Wins) # residuals 
anova(fit.Wins) # anova table plot(fit.Wins)
plot(fit.Wins)

#Regression - Playoffs
fit.Playoffs <- lm(Playoff.Berth ~ Wins, data=teamfootballstats.data) 
coefficients(fit.Playoffs)
fitted(fit.Playoffs) # predicted values
residuals(fit.Playoffs) # residuals
anova(fit.Playoffs) # anova table
plot(fit.Playoffs)


#create Colts & Patriots data subsets
Colts <- subset(football.data,Team=="Colts") 
Patriots <- subset(football.data, Team=="Patriots")

#correlation between Blakeman and Prioleau
cor(football.data$Blakeman, football.data$Prioleau, use="complete", method="pearson")
# 0.9218864
t.test(Colts$Blakeman, Patriots$Blakeman) 
t.test(Colts$Prioleau, Patriots$Prioleau)

# Deflategate - Dataset V2 (Combined Officials Measurements) 
# rename data set to data 
football.data2<-read.csv("Dataset3FootballDeflatagatev2.csv")

# Create Colts & Patriots data subsets
Colts2 <- subset(football.data2,Team=="Colts") 
Patriots2 <- subset(football.data2, Team=="Patriots")
# Were the balls inflated similarly?
t.test(Colts2$Pressure, Patriots2$Pressure)
