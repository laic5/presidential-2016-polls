# STA141A Project Code
# Colin P. Santos 
# Start 11.20.16
# Current 12.4.16
##################################################
## Subsetting the Data
##################################################
data <- read.csv("Desktop/project_data.csv")
attach(data)

# Here are the dataframes sorted by adjustment types and raw
polls_plus <- subset(data, type == "polls-plus", select = -c(type, rawpoll_clinton, rawpoll_johnson, rawpoll_mcmullin, rawpoll_trump))
polls_only <- subset(data, type == "polls-only", select = -c(type, rawpoll_clinton, rawpoll_johnson, rawpoll_mcmullin, rawpoll_trump))
now_cast   <- subset(data, type == "now-cast"  , select = -c(type, rawpoll_clinton, rawpoll_johnson, rawpoll_mcmullin, rawpoll_trump))

# Here are the dataframes of rawpoll used for the three poll types
# Note that only about 1/3 of the raw polls were the same for each poll type
raw_plus  <- subset(data, select = -c(type, adjpoll_clinton, adjpoll_johnson, adjpoll_mcmullin, adjpoll_trump))[1:(nrow(data)/length(levels(type))),]
raw_only  <- subset(data, select = -c(type, adjpoll_clinton, adjpoll_johnson, adjpoll_mcmullin, adjpoll_trump))[4209:(4208*2),]
raw_cast  <- subset(data, select = -c(type, adjpoll_clinton, adjpoll_johnson, adjpoll_mcmullin, adjpoll_trump))[(4208*2+1):(4208*3),]

# poll_plus with just states or just national
polls_plus_state  <- polls_plus[which(polls_plus$state != "U.S."),]
polls_plus_nation <- polls_plus[which(polls_plus$state == "U.S."),]

# raw_plus with just states or just national
raw_plus_state  <- raw_plus[which(raw_plus$state != "U.S."),]
raw_plus_nation <- raw_plus[which(raw_plus$state == "U.S."),]

detach(data)
##################################################
## Preparing Actual Vote Count Data
##################################################
results <- read.csv("Documents/presidential-2016-polls/datasets/actual_votes_by_party.csv")
# LMAO David why y u make dataset with non-numerics
# Clean data for usablility
results <- sapply(results, gsub, pattern = ",", replacement = "" )
state <- as.data.frame(results[,1]); names(state) <- "state"
trump_count <- as.numeric(results[,2])
clinton_count <- as.numeric(results[,3])
total_count <- as.numeric(results[,4])

# Get the percentages for the raw results
results <- cbind(state, trump_count, clinton_count, total_count)
trump_per <- results[2]/results[4]
clinton_per <- results[3]/results[4]
actual_per <- cbind(results$state, trump_per, clinton_per); names(actual_per)[1] = "state"
names(actual_per)[2:3] <- c("per_trump", "per_clinton")

##################################################
## Accuracy of State Polls
##################################################
library(plyr)

# This function takes one of the subsets of the data (or the whole), and
# returns a data frame contaning the difference of poll percentages by state
get.state.difference <- function(polls, raw = FALSE) {
  # Get rid of congressional districts to match states in the actual_votes_by_party data frame
  polls      <- polls[which(polls$state != "Maine CD-1")   ,]
  polls      <- polls[which(polls$state != "Maine CD-2")   ,]
  polls      <- polls[which(polls$state != "Nebraska CD-1"),]
  polls      <- polls[which(polls$state != "Nebraska CD-2"),]
  polls_no_CD  <- polls[which(polls$state != "Nebraska CD-3"),]
  # ポルソノシヂ！！！
  
  # Make naming constistent
  actual_per$state <- sort(unique(polls_no_CD$state))
  
  # Poll_data the function uses depends on if user specified if the poll was raw or not; default: FALSE
  if(raw == TRUE) {
    avg_trump   <- ddply(polls_no_CD, .(polls_no_CD$state), summarize, mean = mean(rawpoll_trump)  /100)
    avg_clinton <- ddply(polls_no_CD, .(polls_no_CD$state), summarize, mean = mean(rawpoll_clinton)/100)
  } else {
    avg_trump   <- ddply(polls_no_CD, .(polls_no_CD$state), summarize, mean = mean(adjpoll_trump)  /100)
    avg_clinton <- ddply(polls_no_CD, .(polls_no_CD$state), summarize, mean = mean(adjpoll_clinton)/100)
  }
  
  names(avg_trump)   <- c("state",   "avg_per_trump")
  names(avg_clinton) <- c("state", "avg_per_clinton")
  
  # Merge the data frames made
  avg_polls <- merge(avg_trump, avg_clinton, by = "state")
  
  # Find out how different the percentages are
  diff_trump   <- abs(actual_per$per_trump   - avg_polls$avg_per_trump  ) * 100
  diff_clinton <- abs(actual_per$per_clinton - avg_polls$avg_per_clinton) * 100
  
  # Return results as a data frame
  difference_state <- cbind(state, diff_trump, diff_clinton)
  difference_state$state <- sort(unique(polls_no_CD$state))
  difference_state
}

pps_difference <- get.state.difference(polls_plus_state, raw = FALSE)
rps_difference <- get.state.difference(raw_plus_state  , raw = TRUE )

##################################################
## Accuracy of National Polls
##################################################
# Get national percentages from the actual results
trump_sum   <- sum(results$trump_count  )
clinton_sum <- sum(results$clinton_count)
total_sum   <- sum(results$total_count  )
national_results <- c(trump_sum/total_sum, clinton_sum/total_sum) * 100
names(national_results) <- c("trump_percent", "clinton_percent")

# Get the national percentages from the adjusted polls
trump_ppn_mean      <- mean(polls_plus_nation$adjpoll_trump)
clinton_ppn_mean    <- mean(polls_plus_nation$adjpoll_clinton)
national_ppn        <- c(trump_ppn_mean, clinton_ppn_mean) 
names(national_ppn) <- c("trump_percent", "clinton_percent")

# Get the national percentages from the raw polls
trump_rpn_mean      <- mean(raw_plus_nation$rawpoll_trump)
clinton_rpn_mean    <- mean(raw_plus_nation$rawpoll_clinton)
national_rpn        <- c(trump_rpn_mean, clinton_rpn_mean) 
names(national_rpn) <- c("trump_percent", "clinton_percent")

# Get the differences in percentages of the raw and polls plus adjusted from the actual results
ppn_difference <- abs(national_results - national_ppn)
rpn_difference <- abs(national_results - national_rpn)

# polls_plus_state  <- polls_plus_state[which(polls_plus_state$state != "Maine CD-1")   ,]
# polls_plus_state  <- polls_plus_state[which(polls_plus_state$state != "Maine CD-2")   ,]
# polls_plus_state  <- polls_plus_state[which(polls_plus_state$state != "Nebraska CD-1"),]
# polls_plus_state  <- polls_plus_state[which(polls_plus_state$state != "Nebraska CD-2"),]
# polls_no_CD         <- polls_plus_state[which(polls_plus_state$state != "Nebraska CD-3"),]
# # ピピエスノシヂ！！！
# 
# # Make naming constistent
# actual_per$state <- sort(unique(polls_no_CD$state))
# 
# avg_trump   <- ddply(polls_no_CD, .(polls_no_CD$state), summarize, mean = mean(adjpoll_trump)  /100)
# avg_clinton <- ddply(polls_no_CD, .(polls_no_CD$state), summarize, mean = mean(adjpoll_clinton)/100)
# 
# names(avg_trump)   <- c("state",   "avg_per_trump")
# names(avg_clinton) <- c("state", "avg_per_clinton")
# 
# avg_polls <-  merge(avg_trump, avg_clinton, by = "state")
# ##################################################
# 
# diff_trump   <- abs(actual_per$per_trump   - avg_polls$avg_per_trump  )
# diff_clinton <- abs(actual_per$per_clinton - avg_polls$avg_per_clinton)
# 
# accuracy_state <- cbind(state, diff_trump, diff_clinton)
# accuracy_state$state <- sort(unique(polls_no_CD$state))




# all(raw_second == raw_third, na.rm = TRUE)
# nrow(raw_first)
# nrow(raw_second)
# nrow(raw_third)
# table(raw_first["rawpoll_clinton"] == raw_second["rawpoll_clinton"])
# 
# # Exploratory Scratch
# 
# state[14]
# state[19]
# astate[8]
# state[9] #New Mexico
# data$state[9]
# state[10]
# state[151] #New Mexico
# which(state == "California")
# which(state == "New Mexico")
# which(state == levels(state)[5])
# which(state == levels(state)[37])
# levels(state)[37]
# 
# nrow(data)
# head
# 
# attach(data)
# levels(branch)
# levels(as.factor(cycle))
# levels(type)
# levels(matchup)
# levels(forecastdate)
# levels(grade)
# ncol(data)
# levels(multiversions)
# levels(timestamp)
# length(grade)
# levels(pollster)
# levels(state)
# length(levels(state)) #57 "state"s
# us_congression
# library(USAboundaries)
# plot(us_congressional())
# va_congressional <- us_congressional(states="california")
# if (require(sp)) {
#   plot(va_congressional)
# }
# 
# head(data)
# data[9,]
# data$state[9]
# data$state
# as.character(data$state)
# table(as.character(data$state))
# 
# sort(table(data$state))
# 
# as.list(sort(table(as.character(data$state))))
# 
# library(lattice)
# ?lattice
# xyplot(sort(table(as.character(data$state))))
# xyplot(y=sort(table(as.character(data$state))))
# plot(sort(table(as.character(data$state))))



##################################################



