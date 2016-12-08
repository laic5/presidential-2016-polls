### visualizing colin code's code
library(ggplot2)

### STATE LEVEL VISUALIZATIONS

### PPS MEANS
# pps_means = get.state.means(polls_plus_state, raw = FALSE)

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
# rps_means = get.state.means(raw_plus_state, raw = TRUE)

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
# pps_difference <- get.state.difference(polls_plus_state, raw = FALSE)
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
# rps_difference <- get.state.difference(raw_plus_state  , raw = TRUE )
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


# # not so informative:
# ### NATIONAL LEVEL VISUALIZATIONS
# 
# # Get the national percentages from the adjusted polls
# trump_ppn_mean      <- mean(polls_plus_nation$adjpoll_trump)
# clinton_ppn_mean    <- mean(polls_plus_nation$adjpoll_clinton)
# 
# natl_ppn_mean_map = ggplot()
# natl_ppn_mean_map = pps_map + geom_polygon(data=usa_blue, aes(x=long, y=lat, group = group), colour="white", size=0.5, fill=alpha("darkblue", 0.5))
# natl_ppn_mean_map = pps_map + geom_polygon(data=usa_red, aes(x=long, y=lat, group = group), colour="white", size=0.5, fill=alpha("darkred", 0.5))
# natl_ppn_mean_map = natl_ppn_mean_map + geom_polygon(data=all_states_md, aes(x=long, y=lat, group=group), colour="white", fill=alpha("darkblue", 0.3), size=0.05)
# natl_ppn_mean_map = natl_ppn_mean_map + theme(panel.background = element_rect(fill = "white"))
# natl_ppn_mean_map
# 
# 
# 
# # raw stuffs
# trump_rpn_mean      <- mean(raw_plus_nation$rawpoll_trump)
# clinton_rpn_mean    <- mean(raw_plus_nation$rawpoll_clinton)
# 
# natl_rpn_mean_map = ggplot()
# natl_rpn_mean_map = rps_map + geom_polygon(data=usa_blue, aes(x=long, y=lat, group = group), colour="white", size=0.5, fill="darkblue")
# natl_rpn_mean_map = rps_map + geom_polygon(data=usa_red, aes(x=long, y=lat, group = group), colour="white", size=0.5, fill="darkred")
# natl_rpn_mean_map = natl_rpn_mean_map + geom_polygon(data=all_states_md, aes(x=long, y=lat, group=group), colour="white", fill=alpha("darkblue", 0.3), size=0.05)
# natl_rpn_mean_map = natl_rpn_mean_map + theme(panel.background = element_rect(fill = "white"))
# natl_rpn_mean_map


# ## purple maps for raw
# natl_map = ggplot()
# natl_map = natl_map + geom_polygon(data=all_states_md, aes(x=long, y=lat, group=group), colour="white", fill=alpha("darkblue", 0.4398), size=0.05)
# natl_map = natl_map + geom_polygon(data=all_states_md, aes(x=long, y=lat, group=group), colour="white", fill=alpha("darkred", 0.3), size=0.3963)
# natl_map = natl_map + theme(panel.background = element_rect(fill = "white"))
# natl_map
# 
# # purple maps for polls plus
# natl_map_2 = ggplot()
# natl_map_2 = natl_map_2 + geom_polygon(data=all_states_md, aes(x=long, y=lat, group=group), colour="white", fill=alpha("darkblue", 0.4488), size=0.05)
# natl_map_2 = natl_map_2 + geom_polygon(data=all_states_md, aes(x=long, y=lat, group=group), colour="white", fill=alpha("darkred", 0.3), size=0.4167)
# natl_map_2 = natl_map_2 + theme(panel.background = element_rect(fill = "white"))
# natl_map_2


# # Get the differences in percentages of the raw and polls plus adjusted from the actual results
# ppn_difference <- abs(national_results - national_ppn)
# rpn_difference <- abs(national_results - national_rpn)
