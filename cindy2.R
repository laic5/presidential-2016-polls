setwd("C:/Users/Cindy/Documents/Davis/JUNIOR/STA 141A/project")

library(tidyverse)
library(plyr)


dat = read.csv("./datasets/president_general_polls_2016.csv")
elec.col = read.csv("./datasets/electoral_college.csv", header = F)
# add the dates of the major presidential events (speeches, world news, fiascos, debates)
dates = as.Date(c("2016-09-26", "2016-10-09", "2016-10-19"))


# convert startdate to Date class
dat$startdate = as.Date(dat$startdate, "%m/%d/%Y")
dat$enddate = as.Date(dat$enddate, "%m/%d/%Y")

polls = dat[which(dat$type == "polls-plus"),]
florida = polls[which(polls$state == "Florida"),]
iowa = polls[which(polls$state == "Iowa"),]

#unique.dates = unique(polls$enddate)

weighted_average = function(dt, ds) {
  date.polls = ds[which(ds$enddate == dt),]
  if(nrow(date.polls) == 1) return (c(date.polls$adjpoll_clinton, date.polls$adjpoll_trump))
  weight.sum = sum(date.polls$poll_wt)
  date.polls$poll_wt = date.polls$poll_wt/weight.sum
  avg.clinton = sum(date.polls$adjpoll_clinton * date.polls$poll_wt)
  avg.trump = sum(date.polls$adjpoll_trump * date.polls$poll_wt)
  return(c(avg.clinton, avg.trump))
}

weighted_average("2016-01-07", iowa)
weighted_state = function(sta) {
  state.polls = polls[which(polls$state == sta),]
  unique.dates = unique(state.polls$enddate)
  new.df = ldply(unique.dates, function(date){ 
    new.percents = weighted_average(date, state.polls)
    c(as.character(date), new.percents)
    })
  return (new.df)
} 

new.florida = weighted_state("Florida")

# weighted.polls = ldply(unique.dates, function(date) {
#   new.percents = as.character(weighted_average(date))
#   return (c(as.character(date), new.percents))
# })


# florida = weighted.polls[which(polls$state == "Florida"),]
# summary(florida$startdate)


# looking at clinton raw vs. adjusted polls over time
florida_time = ggplot(florida, aes(startdate, rawpoll_clinton, adjpoll_clinton)) + geom_line(aes(startdate, rawpoll_clinton), color = "grey") + geom_line(aes(startdate, adjpoll_clinton), color = "blue") + scale_x_date() + ggtitle("Florida Polls for Clinton over time: Raw vs. Adjusted %"); florida_time
# raw is a lot more extreme




# adjust polls for clinton and trump over time
florida_trump.vs.clinton = ggplot(florida, aes(startdate, rawpoll_clinton, adjpoll_clinton)) + geom_line(aes(startdate, adjpoll_clinton), color = "blue") + geom_line(aes(startdate, adjpoll_trump), color = "red") + scale_x_date(); florida_trump.vs.clinton + ggtitle("Florida Clinton vs. Trump over Time")
# a lot more noise starting from September

# close up to after August
recent = subset(florida, startdate > "2016-09-01")


# Compare startdate vs. enddate. Is there a difference?
ggplot(recent, aes(startdate, adjpoll_clinton)) + geom_line(color = "blue") + geom_line(aes(startdate, adjpoll_trump), color = "red") + ggtitle("Florida Clinton vs. Trump Polls Sept 1 - Nov 6")

#ggplot(recent, aes(enddate, adjpoll_clinton)) + geom_line(color = "blue") + geom_line(aes(enddate, adjpoll_trump), color = "red")

which.min(recent$adjpoll_clinton) # 97
# look into this!! what happened around this time?



ggplot(recent, aes(enddate, adjpoll_clinton)) + geom_line(color = "blue", size = 0.75) + geom_line(aes(enddate, adjpoll_trump), color = "red", size = .75) + ggtitle("Florida Clinton vs. Trump Polls Sept 1 - Nov 6") + 
  geom_vline(aes(xintercept=as.numeric(dates[1])), linetype=4, colour="black") + 
  geom_vline(aes(xintercept=as.numeric(dates[2])), linetype=4, colour="black") + 
  geom_vline(aes(xintercept=as.numeric(dates[3])), linetype=4, colour="black")


ggplot(subset(recent, startdate > "2016-10-10"), aes(startdate, adjpoll_clinton)) + geom_line(color = "blue") + geom_line(aes(startdate, adjpoll_trump), color = "red")
