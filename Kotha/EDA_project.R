#test codes
library(tidyverse)
nwsl_team_stats <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/nwsl_team_stats.csv")

filter(nwsl_team_stats, season == 2022)

levels(factor(nwsl_team_stats$team_name))

theme_set(theme_light())

nwsl_team_stats |> 
  filter(team_name %in% c("Chicago Red Stars", "Houston Dash", "NJ/NY Gotham FC", "OL Reign", "Portland Thorns FC", "Washington Spirit")) |> 
  ggplot(aes(x = season, y = goal_differential, group = team_name, color = team_name)) +
  geom_line() +
  labs(
    x = "Season",
    y = "Goal Differential")


median_goal_differential <-nwsl_team_stats |> 
  group_by(season) |> 
  summarize(median_goal_differential = median(goal_differential, na.rm = TRUE))
  print(median_goal_differential)
  
  
  ggplot(median_goal_differential, aes(x = season, y = median_goal_differential)) +
  geom_line() +
  geom_point()
  
    
  
  



# yearly_batting |>
#  ggplot (aes(x = yearID, y = total_hr)) +
#  geom_point(aes(color = total_so, size = total_bb)) +
#  geom_line(color = "purple", linetype = "dashed", size = 0.5) +
#  scale_color_gradient(low = "darkblue", high = "gold") +
  # scale_size_continuous(breaks = seq(0, 20000, 2500)) +
