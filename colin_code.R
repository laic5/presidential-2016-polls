# STA141A Project Code
# Colin P. Santos 
# 11.20.16

##################################################
data <- read.csv("project_data.csv")
attach(data)

# Here are the dataframes sorted by adjustment types and raw
polls_plus <- subset(data, type == "polls-plus", select = -c(type, rawpoll_clinton, rawpoll_johnson, rawpoll_mcmullin, rawpoll_trump))
polls_only <- subset(data, type == "polls-only", select = -c(type, rawpoll_clinton, rawpoll_johnson, rawpoll_mcmullin, rawpoll_trump))
now_cast   <- subset(data, type == "now-cast"  , select = -c(type, rawpoll_clinton, rawpoll_johnson, rawpoll_mcmullin, rawpoll_trump))

raw_polls  <- subset(data, select = -c(type, adjpoll_clinton, adjpoll_johnson, adjpoll_mcmullin, adjpoll_trump))[1:(nrow(data)/length(levels(type))),]
raw_second <- subset(data, select = -c(type, adjpoll_clinton, adjpoll_johnson, adjpoll_mcmullin, adjpoll_trump))[4209:(4208*2),]
raw_third  <- subset(data, select = -c(type, adjpoll_clinton, adjpoll_johnson, adjpoll_mcmullin, adjpoll_trump))[(4208*2+1):(4208*3),]
##################################################
all(raw_second == raw_third, na.rm = TRUE)
nrow(raw_polls)
nrow(raw_second)
nrow(raw_third)
table(raw_polls["rawpoll_clinton"] == raw_second["rawpoll_clinton"])

# Exploratory Scratch

state[14]
state[19]
astate[8]
state[9] #New Mexico
state[10]
state[151] #New Mexico
which(state == "California")
which(state == "New Mexico")
which(state == levels(state)[5])
which(state == levels(state)[37])
levels(state)[37]


nrow(data)
head

attach(data)
levels(branch)
levels(as.factor(cycle))
levels(type)
levels(matchup)
levels(forecastdate)
levels(grade)
ncol(data)
levels(multiversions)
levels(timestamp)
length(grade)
levels(pollster)
levels(state)
length(levels(state)) #57 "state"s
us_congressio
library(USAboundaries)
plot(us_congressional())
va_congressional <- us_congressional(states="california")
if (require(sp)) {
  plot(va_congressional)
}


head(data)
data[9,]
data$state[9]
data$state
as.character(data$state)
table(as.character(data$state))
sort(table(as.character(data$state)))
library(lattice)
?lattice
xyplot(sort(table(as.character(data$state))))
xyplot(y=sort(table(as.character(data$state))))
plot(sort(table(as.character(data$state))))

##################################################
