### visualizing colin code's code
library(ggplot2)

### STATE LEVEL VISUALIZATIONS

### PPS MEANS
mean_bools <- pps_means$avg_per_trump > pps_means$avg_per_clinton
state_colors <- sapply(mean_bools, function(bools) {
  if(bools == TRUE) {"red"}
  else {"blue"}
})
state_colors <- cbind(as.character(pps_means$state), state_colors )
blue_states <- state_colors[which(state_colors[,2] == "blue")]
red_states = state_colors[which(state_colors[,2] == "red")]

#load us map data
usa_blue = map_data("state", region=blue_states)
usa_red = map_data("state", region=red_states)

#plot all states with ggplot
pps_map = ggplot()
pps_map = pps_map + geom_polygon(data=usa_blue, aes(x=long, y=lat, group = group), colour="white", size=0.5, fill="darkblue")
pps_map = pps_map + geom_polygon(data=usa_red, aes(x=long, y=lat, group = group), colour="white", size=0.5, fill="darkred")
pps_map = pps_map + ggtitle("2016 Presidential Election - Polls Plus") + xlab("longitude") + ylab("latitude") + theme(plot.title = element_text(family = "Helvetica Neue", color="black", size=25, hjust=0, face="bold")) + theme(axis.title = element_text(family = "Helvetica Neue", color="black", size=14))
pps_map = pps_map + theme(panel.background = element_rect(fill = "white"))
pps_map




### RPS MEANS
mean_bools_2 <- pps_means$avg_per_trump > pps_means$avg_per_clinton
state_colors_2 <- sapply(mean_bools, function(bools) {
  if(bools == TRUE) {"red"}
  else {"blue"}
})
state_colors_2 <- cbind(as.character(pps_means$state), state_colors )
blue_states_2 <- state_colors[which(state_colors[,2] == "blue")]
red_states_2 = state_colors[which(state_colors[,2] == "red")]

#load us map data
usa_blue_2 = map_data("state", region=blue_states_2)
usa_red_2 = map_data("state", region=red_states_2)

#plot all states with ggplot
rps_map = ggplot()
rps_map = rps_map + geom_polygon(data=usa_blue_2, aes(x=long, y=lat, group = group), colour="white", size=0.5, fill="darkblue")
rps_map = rps_map + geom_polygon(data=usa_red_2, aes(x=long, y=lat, group = group), colour="white", size=0.5, fill="darkred")
rps_map = rps_map + ggtitle("2016 Presidential Election - Raw Polls") + xlab("longitude") + ylab("latitude") + theme(plot.title = element_text(family = "Helvetica Neue", color="black", size=25, hjust=0, face="bold")) + theme(axis.title = element_text(family = "Helvetica Neue", color="black", size=14))
rps_map = rps_map + theme(panel.background = element_rect(fill = "white"))
rps_map



### PPS DIFFERENCE
all_states = levels(pps_difference[,1])[pps_difference[,1]]
all_states[!all_states %in% c("District of Columbia")]

#load us map data
all_states_md = map_data("state", region=all_states)

new_df = cbind(all_states_md, c(1:length(all_states_md$region)), c(1:length(all_states_md$region)))
names(new_df)[7] = "trump"
names(new_df)[8] = "clinton"

for(i in 1:length(all_states)) {
  these_indices = which(new_df$region==tolower(all_states[i]))
  new_df$trump[these_indices] = pps_difference$diff_trump[i]
  new_df$clinton[these_indices] = pps_difference$diff_clinton[i]
}

## poll inaccuracy
#plot all states with ggplot
pps_diff_map = ggplot()
# pps_difference$diff_trump
pps_diff_map = pps_diff_map + geom_polygon(data=new_df, aes(x=long, y=lat, group=group, fill=trump), colour="white", size=0.5) + scale_fill_gradient(low="black", high="orange", name="absolute %\ninaccuracy")
pps_diff_map = pps_diff_map + ggtitle("Prediction Inaccuracy per State (Polls Plus)") + xlab("longitude") + ylab("latitude") + theme(plot.title = element_text(family = "Helvetica Neue", color="black", size=20, hjust=0, face="bold")) + theme(axis.title = element_text(family = "Helvetica Neue", color="black", size=14))
pps_diff_map = pps_diff_map + theme(panel.background = element_rect(fill = "white"))
pps_diff_map



### RPS DIFFERENCE
all_states = levels(rps_difference[,1])[rps_difference[,1]]
all_states[!all_states %in% c("District of Columbia")]

#load us map data
all_states_md = map_data("state", region=all_states)

new_df = cbind(all_states_md, c(1:length(all_states_md$region)), c(1:length(all_states_md$region)))
names(new_df)[7] = "trump"
names(new_df)[8] = "clinton"

for(i in 1:length(all_states)) {
  these_indices = which(new_df$region==tolower(all_states[i]))
  new_df$trump[these_indices] = pps_difference$diff_trump[i]
  new_df$clinton[these_indices] = pps_difference$diff_clinton[i]
}

## poll inaccuracy
#plot all states with ggplot
rps_diff_map = ggplot()
# pps_difference$diff_trump
rps_diff_map = rps_diff_map + geom_polygon(data=new_df, aes(x=long, y=lat, group=group, fill=trump), colour="white", size=0.5) + scale_fill_gradient(low="black", high="orange", name="absolute %\ninaccuracy")
rps_diff_map = rps_diff_map + ggtitle("Prediction Inaccuracy per State (Raw Polls)") + xlab("longitude") + ylab("latitude") + theme(plot.title = element_text(family = "Helvetica Neue", color="black", size=20, hjust=0, face="bold")) + theme(axis.title = element_text(family = "Helvetica Neue", color="black", size=14))
rps_diff_map = rps_diff_map + theme(panel.background = element_rect(fill = "white"))
rps_diff_map
