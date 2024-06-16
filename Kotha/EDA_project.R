#test codes
library(tidyverse)
library(ggthemes)
library(dplyr)
library(viridis) 
library(ggmosaic)
attach(nwsl_team_stats)
nwsl_team_stats <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/nwsl_team_stats.csv")

levels(factor(nwsl_team_stats$team_name))

theme_set(theme_light())


#Mean Shot Accuracy by Year
mean_shot_acc <-nwsl_team_stats |> 
  group_by(season) |> 
  summarize(mean_shot_acc = mean(shot_accuracy))
print(mean_shot_acc) 


#First Place through Fifth Place Data Frame
topfive = data.frame(Place = rep(c("1", '2', '3', '4', '5', 'Mean'), each = 6),
                     Season = rep(c('2016', '2017', '2018', '2019', '2021',
                                    '2022'), times = 6),
                     shot_acc = c(54, 43.53, 46.1, 46.54, 42.94, 49.1,#first
                                  48.68, 48.96, 48.65, 47.06, 44.76, 51.67,#second
                                  48.58, 50, 49.27, 45.28, 47.16, 45.06,#third
                                  50, 49.6, 45.68, 40.79, 45.98, 45.33,#fourth
                                  53.97, 50.21, 44.24, 44.8, 38.43, 50.23,#fifth
                                  49, 47.9, 45.4, 44.3, 44.4, 47.7))#mean
topfive |> 
  ggplot(aes(x = Season, y = shot_acc, color = Place)) +
  geom_point() +
  geom_line(aes(group = Place))+
  scale_color_manual(values = c("black", 
                                viridis_pal()(length(unique(topfive$Place)))))  # Customize line colors here


# Plotting the data: Shot Accuracy of Top 5 Ranked Teams Compared to League Mean
ggplot(topfive, aes(x = Season, y = shot_acc, color = Place)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_line(aes(group = Place, linetype = ifelse(Place == "Mean", "dashed", "solid")), size = 1) +
  scale_color_manual(values = c("#CC79A7", "#710193", "#009E73", "#D55E00", "#56B4E9", "black")) +
   scale_linetype_identity() +
  labs(
    title = "Shot Accuracy of Top 5 Ranked Teams Compared to League Mean",
    x = "Season",
    y = "Shot Accuracy (%)",
    color = "Team Rank"
  ) +
  theme_solarized() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12, color = "black"),
    legend.text = element_text(size = 10, color = "black"),
  )

# creating data for team rankings
team_rankings <- data.frame(
  team_name = c(
    'Portland Thorns FC', 'Washington Spirit', 'Chicago Red Stars', 'Western New York Flash', 'OL Reign', 'FC Kansas City', 'Houston Dash', 'NJ/NY Gotham FC', 'Orlando Pride', 'Boston Breakers', # 2016
    'North Carolina Courage', 'Portland Thorns FC', 'Orlando Pride', 'Chicago Red Stars', 'OL Reign', 'NJ/NY Gotham FC', 'FC Kansas City', 'Houston Dash', 'Boston Breakers', 'Washington Spirit',   # 2017
    'North Carolina Courage', 'Portland Thorns FC', 'OL Reign', 'Chicago Red Stars', 'Utah Royals FC', 'Houston Dash', 'Orlando Pride', 'Washington Spirit', 'NJ/NY Gotham FC',             # 2018
    'North Carolina Courage', 'Chicago Red Stars', 'Portland Thorns FC', 'OL Reign', 'Washington Spirit', 'Utah Royals FC', 'Houston Dash', 'NJ/NY Gotham FC', 'Orlando Pride',         # 2019
    'Portland Thorns FC', 'OL Reign', 'Washington Spirit', 'Chicago Red Stars', 'NJ/NY Gotham FC', 'North Carolina Courage', 'Houston Dash', 'Orlando Pride', 'Racing Louisville FC', 'Kansas City Current',   # 2021
    'OL Reign', 'Portland Thorns FC', 'San Diego Wave FC', 'Houston Dash', 'Chicago Red Stars', 'North Carolina Courage', 'Angel City FC', 'Racing Louisville FC', 'Orlando Pride', 'Washington Spirit', 'NJ/NY Gotham FC'   # 2022
  ),
  season = c(
    rep(2016, 10), rep(2017, 10), rep(2018, 9), rep(2019, 9), rep(2021, 10), rep(2022, 11)
  ),
  team_rankings = c(
    1, 2, 3, 4, 5, 6, 7, 8, 9, 10,   # Ranks for 2016
    1, 2, 3, 4, 5, 6, 7, 8, 9, 10,   # Ranks for 2017
    1, 2, 3, 4, 5, 6, 7, 8, 9,       # Ranks for 2018
    1, 2, 3, 4, 5, 6, 7, 8, 9,       # Ranks for 2019
    1, 2, 3, 4, 5, 6, 7, 8, 9, 10,   # Ranks for 2021
    1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 # Ranks for 2022
  )
)

#merge with orginal data set
nwsl_team_stats <- nwsl_team_stats %>%
  left_join(team_rankings, by = c("team_name", "season"))

# Print the updated dataset to check the result
print(head(nwsl_team_stats))



#median goal conversion percentage
  median_goal_conversion_pct <-nwsl_team_stats |> 
    group_by(season) |> 
    summarize(median_goal_conversion_pct = median(goal_conversion_pct, na.rm = TRUE))
  print(median_goal_conversion_pct ) 
  

  
  
  ggplot(nwsl_team_stats, aes(x = shot_accuracy, y = goal_conversion_pct)) +
    geom_point()
  
  #Creating categorical variables
shot_acc.cat = cut(shot_accuracy,
                     breaks = c(quantile(shot_accuracy, 0), 
                                quantile(shot_accuracy, 1/3), 
                                quantile(shot_accuracy, 2/3),
                                quantile(shot_accuracy, 1)), 
                     labels = c("Low", "Medium", "High"), include.lowest = TRUE, 
                     ordered_result = TRUE)

table(shot_acc.cat)

#line graph break down (low, medium, high) mean shot accuracy over seasons
nwsl_team_stats <- nwsl_team_stats |> 
  mutate(shot_acc_cat = cut(shot_accuracy,
                            breaks = c(quantile(shot_accuracy, 0), 
                                       quantile(shot_accuracy, 1/3), 
                                       quantile(shot_accuracy, 2/3),
                                       quantile(shot_accuracy, 1)), 
                            labels = c("Low", "Medium", "High"), 
                            include.lowest = TRUE, 
                            ordered_result = TRUE))

# Checking the distribution of the categories
table(nwsl_team_stats$shot_acc_cat)



# Aggregating data to calculate the mean shot accuracy per category per season
agg_data <- nwsl_team_stats |> 
  group_by(season, shot_acc_cat) |> 
  summarize(mean_shot_accuracy = mean(shot_accuracy))

# Plotting the data
ggplot(agg_data, aes(x = season, y = mean_shot_accuracy, color = shot_acc_cat, group = shot_acc_cat)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(title = "Shot Accuracy Categories Over Seasons",
       x = "Season",
       y = "Mean Shot Accuracy",
       color = "Shot Accuracy Category") +
  scale_x_continuous(breaks = seq(2016, 2022, 1)) +
  theme_bw()


  table(shot_acc.cat)
  
  #Splitting up shot accuracy into low, medium, and high usage.
  goal_conv.cat = cut(goal_conversion_pct, 
                      breaks = c(quantile(goal_conversion_pct, 0), 
                                 quantile(goal_conversion_pct, 1/3), 
                                 quantile(goal_conversion_pct, 2/3),
                                 quantile(goal_conversion_pct, 1)), 
                      labels = c("Low", "Medium", "High"), include.lowest = TRUE, 
                      ordered_result = TRUE)
  table(goal_conv.cat)
  #Splitting up goal conversion% into low, medium, and high usage.
  nwsl_team_stats |> 
    ggplot()+
    geom_mosaic(aes(x=product(shot_acc.cat, goal_conv.cat), fill = shot_acc.cat))
  table(shot_acc.cat, goal_conv.cat)  
  
  
   
  median_goals <-nwsl_team_stats |> 
    group_by(season) |> 
    summarize(median_goals = median(goals, na.rm = TRUE))
  print(median_goals) 
  
  

ggplot(nwsl_team_stats |> 
         filter(team_name %in% c("Chicago Red Stars", "Houston Dash", "NJ/NY Gotham FC", "OL Reign", "Orlando Pride", "Portland Thorns FC", "Washington Spirit")), 
       aes(x = season, y = pass_pct_half_diff, fill = team_name)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Goals Scored by Each Team per Season",
       x = "Season",
       y = "Goals Scored",
       fill = "Team Name") +
  theme_bw()

  # Season Goal Differential of Folded Teams with median legend
  ggplot() +
    # First dataset: median_goal_differential
    geom_line(data = median_goal_differential, aes(x = season, y = median_goal_differential, linetype = "Median Goal Differential"), color = 'black', linewidth = 1.3) +
    geom_point(data = median_goal_differential, aes(x = season, y = median_goal_differential), color = 'black', show.legend = FALSE) +
    
    # Second dataset: nwsl_team_stats filtered data
    geom_line(data = nwsl_team_stats |> 
                filter(!team_name %in% c("Chicago Red Stars", "Houston Dash", "NJ/NY Gotham FC", "OL Reign", "Orlando Pride", "Portland Thorns FC", "Washington Spirit")), 
              aes(x = season, y = goal_differential, group = team_name, color = team_name), linewidth = 1) +
    geom_point(data = nwsl_team_stats |> 
                 filter(!team_name %in% c("Chicago Red Stars", "Houston Dash", "NJ/NY Gotham FC", "OL Reign", "Orlando Pride", "Portland Thorns FC", "Washington Spirit")), 
               aes(x = season, y = goal_differential, group = team_name, color = team_name), size = 2) +
  
      #scale line colors and legend
    scale_linetype_manual(values = c("Median Goal Differential" = "dashed")) +
    scale_color_manual(values = c('firebrick2', 'darkorange1', 'gold2', 'springgreen', 'mediumturquoise', 'mediumorchid', 'darkblue', 'deeppink', 'coral1')) +   

    
    labs(
      x = "Season",
      y = "Goal Differential",
      color = "Team Name",
      linetype = "Median Goal Differential"
    ) +
    theme_bw() +
    ggtitle('Folded Teams Goal Differential Compared to League Median') +
    theme(legend.position = "right", 
          plot.title = element_text(hjust = 0, face = 'bold')) 
    
  
  # Season Goal Differential of Lasting Teams with median legend
  ggplot() +
    # First dataset: median_goal_differential
    geom_line(data = median_goal_differential, aes(x = season, y = median_goal_differential, linetype = "Median Goal Differential"), color = 'black', linewidth = 1.3) +
    geom_point(data = median_goal_differential, aes(x = season, y = median_goal_differential), color = 'black', show.legend = FALSE) +
    
    # Second dataset: nwsl_team_stats filtered data
    geom_line(data = nwsl_team_stats |> 
                filter(team_name %in% c("Chicago Red Stars", "Houston Dash", "NJ/NY Gotham FC", "OL Reign", "Orlando Pride", "Portland Thorns FC", "Washington Spirit")), 
              aes(x = season, y = goal_differential, group = team_name, color = team_name), linewidth = 1) +
    geom_point(data = nwsl_team_stats |> 
                 filter(team_name %in% c("Chicago Red Stars", "Houston Dash", "NJ/NY Gotham FC", "OL Reign", "Orlando Pride", "Portland Thorns FC", "Washington Spirit")), 
               aes(x = season, y = goal_differential, group = team_name, color = team_name), size = 1) +
   
    # Customizing the linetype scale to include the dashed line in the legend
    scale_linetype_manual(values = c("Median Goal Differential" = "dashed")) +
    scale_color_manual(values = c('firebrick2', 'darkorange1', 'gold2', 'springgreen','mediumturquoise','darkblue', 'mediumorchid')) +
  
    labs(
      x = "Season",
      y = "Goal Differential",
      color = "Team Name",
      linetype = "Median Goal Differential"
    ) +
    theme_bw() +
    ggtitle('Regular Teams Goal Differential Compared to League Median') +
    theme(legend.position = "right", 
          plot.title = element_text(hjust = 0, face = 'bold')) 