# 2016-11-20

## LOADING IN DATA
setwd("/Users/edie/Box Sync/STA 141A/presidential-2016-polls")
project_data = read.csv("president_general_polls_2016.csv")
attach(project_data)
project_data[is.na(project_data)] = 0
electoral_college = read.csv("electoral_college.csv", header=FALSE)


# effect of sample size on accuracy
summary(project_data$samplesize)
#   min   |   q1    |   m   |   q3    |   max
#   0/35  |   447   | 1148  |   1236  |   84290   





## PROJECTION MODEL

# for (state in 50states) {
#   for (pollster in all_pollsters) {
#     poll_clinton = mean(all_clinton_%)
#     poll_trump = mean(all_trump_%)
#     save in some sort of structure
# }
# 
# sum(grade*poll_i_clinton)
# sum(grade*poll_i_trump)
# }


# # levels(grades)
# #
# levels(grade)
# 
# levels(california$pollster)
# poll_wt[which(pollster == "YouGov")]
# 
# ?matrix
# grade_numeric <- matrix(ncol = 1, nrow = length(grade))
# 
# grade_numeric[which(grade== "A+")] = 10
# grade_numeric[which(grade== "A")] = 9
# grade_numeric[which(grade== "A-")] = 8
# grade_numeric[which(grade== "B+")] = 7
# grade_numeric[which(grade== "B")] = 6
# grade_numeric[which(grade== "B-")] = 5
# grade_numeric[which(grade== "C+")] = 4
# grade_numeric[which(grade== "C")] = 3
# grade_numeric[which(grade== "C-")] = 2
# grade_numeric[which(grade== "D")] = 1
# grade_numeric[which(grade== "")] = NA
# grade_numeric
# project_data = cbind(project_data, grade_numeric)
# 
# sum(poll_wt)






## SUBSETTING DATA FOR RED AND BLUE STATES
# we are only considering here the mean of adjusted polls per state
all_states = as.vector(levels(state))
blue_states = c()
red_states = c()

for (i in 1:length(all_states)) {
  c = mean(adjpoll_clinton[which(state==all_states[i])], na.rm=TRUE)
  t = mean(adjpoll_trump[which(state==all_states[i])], na.rm=TRUE)
  if (c>t) {
    blue_states = cbind(blue_states, all_states[i])
  }
  if (t>c) {
    red_states = cbind(red_states, all_states[i])
  }
}


## RED STATE THINGS
red_states = as.vector(unique(red_states))
red_states = red_states[! red_states %in% c("Maine CD-2", "Nebraska CD-1", "Nebraska CD-2", "Nebraska CD-3")]

red_electoral_votes = c()
for(i in 1:length(red_states)) {
  row = which(electoral_college$V1==red_states[i])
  electoral_votes = electoral_college$V2[row]
  red_electoral_votes = rbind(red_electoral_votes, electoral_votes)
}

red_states_info = cbind(red_states, red_electoral_votes)

red_coords = c()
for(i in 1:length(red_states_info[,1])) {
  geo = geocode(red_states_info[i,1])
  red_coords = rbind(red_coords, geo)
}

red_states_info = cbind(red_states_info, red_coords)

## BLUE STATE THINGS
blue_states = as.vector(unique(blue_states))
blue_states = blue_states[! blue_states %in% c("District of Columbia", "Maine CD-1", "U.S.")]


blue_electoral_votes = c()
for(i in 1:length(red_states)) {
  row = which(electoral_college$V1==blue_states[i])
  electoral_votes = electoral_college$V2[row]
  blue_electoral_votes = rbind(blue_electoral_votes, electoral_votes)
}

blue_states_info = cbind(blue_states, blue_electoral_votes)

blue_coords = c()
for(i in 1:length(blue_states_info[,1])) {
  geo = geocode(blue_states_info[i,1])
  blue_coords = rbind(blue_coords, geo)
}

blue_states[5]
blue_states_info = cbind(blue_states_info, blue_coords)


## USING MAPS
library(maps)
map(database = "state")
map(database = "state",regions = blue_states ,col = "blue",fill=T,add=TRUE)
map(database = "state",regions = red_states ,col = "red",fill=T,add=TRUE)


## USING GGMAP
library(ggplot2)

#load us map data
usa_blue = map_data("state", region=blue_states)
usa_red = map_data("state", region=red_states)

#plot all states with ggplot
usa_map = ggplot()
usa_map = usa_map + geom_polygon(data=usa_blue, aes(x=long, y=lat, group = group),colour="white", fill="darkblue") + geom_text(data=blue_states_info, aes(x=lon, y=lat, label=V2), colour="white")
usa_map = usa_map + geom_polygon(data=usa_red, aes(x=long, y=lat, group = group),colour="white", fill="darkred") + geom_text(data=red_states_info, aes(x=lon, y=lat, label=V2), colour="white")
usa_map = usa_map + ggtitle("Fuck Donald!") + theme(plot.title = element_text(family = "Tahoma", color="black", face="bold", size=24, hjust=0)) + theme(axis.title = element_text(family = "Tahoma", color="black", face="bold", size=22))
usa_map


# wow washington and new york way to kill my buzz
# dk bout alaska and hawaii lol