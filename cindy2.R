setwd("C:/Users/Cindy/Documents/Davis/JUNIOR/STA 141A/project")

library(tidyverse)
library(plyr)
library(reshape2)


dat = read.csv("./datasets/president_general_polls_2016.csv")

# add the dates of the major presidential events (speeches, world news, fiascos, debates)
dates = as.Date(c("2016-09-26", "2016-10-09", "2016-10-19", "2016-07-18", "2016-07-25"))


# convert startdate to Date class
dat$startdate = as.Date(dat$startdate, "%m/%d/%Y")
dat$enddate = as.Date(dat$enddate, "%m/%d/%Y")

polls = dat[which(dat$type == "polls-plus"),]

# calculates a weighted average for polls that ended on the same date. 
# based off poll_wt
weighted_average = function(dt, ds) {
  date.polls = ds[which(ds$enddate == dt),]
  if(nrow(date.polls) == 1) return (c(date.polls$adjpoll_clinton, date.polls$adjpoll_trump))
  weight.sum = sum(date.polls$poll_wt)
  date.polls$poll_wt = date.polls$poll_wt/weight.sum
  avg.clinton = sum(date.polls$adjpoll_clinton * date.polls$poll_wt)
  avg.trump = sum(date.polls$adjpoll_trump * date.polls$poll_wt)
  return(c(avg.clinton, avg.trump))
}

# calculates new weighted polls for each unique date in a state
weighted_state = function(sta) {
  state.polls = polls[which(polls$state == sta),]
  unique.dates = unique(state.polls$enddate)
  new.df = ldply(unique.dates, function(date){ 
    new.percents = weighted_average(date, state.polls)
    c(sta, as.character(date), new.percents)
    })
  colnames(new.df) = c("state", "Date", "adjpoll_clinton", "adjpoll_trump")
  new.df$Date = as.Date(new.df$Date)
  new.df$adjpoll_clinton = as.double(new.df$adjpoll_clinton)
  new.df$adjpoll_trump = as.double(new.df$adjpoll_trump)
  return (new.df)
} 


# EXAMINING SWING STATES
new.florida = weighted_state("Florida")
new.michigan = weighted_state("Michigan")
new.ohio = weighted_state("Ohio")
new.penn = weighted_state("Pennsylvania")

# plotting function for Clinton vs. Trump with the presidential debates
plotpolls = function(dat) {
  ggplot(dat, aes(Date, adjpoll_clinton)) + geom_line(aes(Date, adjpoll_trump), color = "black", size = .75) + geom_line(aes(Date, adjpoll_clinton), color = "cornflowerblue", size = 0.75, linetype = "F1") + 
    geom_vline(aes(xintercept=as.numeric(dates[1])), linetype=4, colour="black") + 
    geom_vline(aes(xintercept=as.numeric(dates[2])), linetype=4, colour="black") + 
    geom_vline(aes(xintercept=as.numeric(dates[3])), linetype=4, colour="black") +
    geom_vline(aes(xintercept=as.numeric(dates[4])), linetype="dotted", colour="red") +
    geom_vline(aes(xintercept=as.numeric(dates[5])), linetype="dotted", colour="blue") +
    ggtitle(paste(c(dat$state[1], "Trump (Black) vs. Clinton (Dashed)"), collapse = " "))
}

plotpolls(new.florida)
plotpolls(new.ohio)
plotpolls(new.michigan)
plotpolls(new.penn)


## taking a closer look at recent polls (after Sept 1)

plotpolls(new.florida[which(new.florida$Date > "2016-09-01"),])
plotpolls(new.ohio[which(new.ohio$Date > "2016-09-01"),])
plotpolls(new.michigan[which(new.michigan$Date > "2016-09-01"),])
plotpolls(new.penn[which(new.penn$Date > "2016-09-01"),])



# look at California for contrast
new.ca = weighted_state("California")
plotpolls(new.ca)
plotpolls(new.ca[which(new.ca$Date > "2016-09-01"),])
