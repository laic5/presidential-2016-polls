setwd("C:/Users/Cindy/Documents/Davis/JUNIOR/STA 141A/project")

library(tidyverse)
library(plyr)

dat = read.csv("president_general_polls_2016.csv")
summary(dat)

unique(dat$state)

# look at overall grade distribution
national_polls = dat[which(dat$state == "U.S."),]
summary(national_polls$grade)
ggplot(national_polls, aes(grade)) + geom_bar()

# subsetting Florida
florida = dat[which(dat$state == "Florida"),]
summary(florida$startdate)

# convert startdate to Date class
dat$startdate = as.Date(dat$startdate, "%m/%d/%Y")

# looking at clinton raw vs. adjusted polls over time
florida_time = ggplot(florida, aes(startdate, rawpoll_clinton, adjpoll_clinton)) + geom_line(aes(startdate, rawpoll_clinton), color = "grey") + geom_line(aes(startdate, adjpoll_clinton), color = "blue") + scale_x_date(); florida_time + ggtitle("Florida Polls for Clinton over time: Raw vs. Adjusted %")


# adjust polls for clinton and trump over time, divided by grade
florida_trump.vs.clinton = ggplot(florida, aes(startdate, rawpoll_clinton, adjpoll_clinton)) + geom_line(aes(startdate, adjpoll_clinton), color = "blue") + geom_line(aes(startdate, adjpoll_trump), color = "red") + scale_x_date(); florida_trump.vs.clinton + ggtitle("Florida Clinton vs. Trump, Divided By Grade") + facet_grid(grade ~ .)

summary(florida$grade)

# close up to after August
recent = subset(florida, startdate > "2016-08-08")

ggplot(recent, aes(startdate, adjpoll_clinton)) + geom_line(color = "blue") + geom_line(aes(startdate, adjpoll_trump), color = "red") + facet_grid(grade ~ .)


sum(florida$poll_wt * florida$rawpoll_clinton/100)
sum(florida$poll_wt * florida$rawpoll_trump/100) # notice how Trump's is smaller


good = dat[which(dat$grade == c("A", "A+", "A-")),]

iowa = good[which(good$state == "Iowa"),]
plus = iowa[which(iowa$type == "polls-plus"),]
mean(plus$adjpoll_clinton)

states.list = as.vector(unique(dat$state))

state.mean = ldply(states.list, function(stat) {
  state.data = good[which(good$state == stat),]
  
  plus = state.data[which(state.data$type == "polls-plus"),]
  now = state.data[which(state.data$type == "now-cast"),]
  only = plus = state.data[which(state.data$type == "polls-only"),]
  
  c(stat, mean(plus$adjpoll_clinton), mean(now$adjpoll_clinton), mean(only$adjpoll_clinton))
  
  })

state.mean[which(state.mean$V2 >= 50 | state.mean$V3 >= 50 | state.mean $V4 >= 50),1]
