# extraneous code. mainly for exploration. most time series is found in cindy2.R

elec.col = read.csv("./datasets/electoral_college.csv", header = F)

florida = polls[which(polls$state == "Florida"),]

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