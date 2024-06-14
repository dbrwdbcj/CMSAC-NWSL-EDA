#test codes
library(tidyverse)
library(ggthemes)
nwsl_team_stats <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/nwsl_team_stats.csv")

levels(factor(nwsl_team_stats$team_name))

theme_set(theme_light())

median_goal_differential <-nwsl_team_stats |> 
  group_by(season) |> 
  summarize(median_goal_differential = median(goal_differential, na.rm = TRUE))
  print(median_goal_differential) 
  
  median_goal_conversion_pct <-nwsl_team_stats |> 
    group_by(season) |> 
    summarize(median_goal_conversion_pct = median(goal_conversion_pct, na.rm = TRUE))
  print(median_goal_conversion_pct ) 
  
  # ggplot(median_goal_differential, aes(x = season, y = median_goal_differential)) +
  #   geom_line() +
  #   geom_point() +
  #   labs(
  #     x = "Season",
  #     y = "Median Goal Differential")
  
  
  # plotly_test <- nwsl_team_stats |> 
  #   filter(!team_name %in% c("Chicago Red Stars", "Houston Dash", "NJ/NY Gotham FC", "OL Reign", "Portland Thorns FC", "Washington Spirit")) |> 
  #   ggplot(aes(x = season, y = goal_differential, group = team_name, color = team_name)) +
  #   geom_line() +
  #   labs(
  #     x = "Season",
  #     y = "Goal Differential")

  # # Season Goal Differential of Folded Teams without median goal legend
  # ggplot() +
  #   # First dataset: median_goal_differential
  #   geom_line(data = median_goal_differential, aes(x = season, y = median_goal_differential), color = 'black', linetype = 'dashed', linewidth = 1) +
  #   geom_point(data = median_goal_differential, aes(x = season, y = median_goal_differential), color = 'black') +
  # 
  #   # Second dataset: nwsl_team_stats filtered data
  #   geom_line(data = nwsl_team_stats |> 
  #               filter(!team_name %in% c("Chicago Red Stars", "Houston Dash", "NJ/NY Gotham FC", "OL Reign", "Portland Thorns FC", "Washington Spirit")), 
  #             aes(x = season, y = goal_differential, group = team_name, color = team_name), linewidth = 1) +
  #   
  #   # Labels and theme
  #   labs(
  #     x = "Season",
  #     y = "Goal Differential",
  #     color = "Team Name"
  #   ) +
  #   theme_economist() +
  #   ggtitle('Layered Line Graphs: Goal Differentials') +
  #   theme(legend.position = "right") 
  
  median_goals <-nwsl_team_stats |> 
    group_by(season) |> 
    summarize(median_goals = median(goals, na.rm = TRUE))
  print(median_goals) 
  
  pass_pct_half_diff <- nwsl_team_stats |> 
    group_by(team_name, season) |> 
    summarize(pass_pct_half_diff = mean(pass_pct - pass_pct_opposition_half, na.rm = TRUE)) |>
    arrange(team_name, season)
  print(pass_pct_half_diff)
  
 nwsl_team_stats <- nwsl_team_stats |> 
      mutate(pass_pct_half_diff = pass_pct - pass_pct_opposition_half)
  ggplot()+
  #(nwsl_team_stats |> 
       # filter(team_name %in% c("Chicago Red Stars", "Houston Dash", "NJ/NY Gotham FC", "OL Reign", "Orlando Pride", "Portland Thorns FC", "Washington Spirit")),
     #    aes(x = season, y = pass_pct_half_diff, fill = team_name)) +
  #  geom_bar(stat = "identity", position = "dodge") +
  geom_line(data = nwsl_team_stats |> 
                filter(team_name %in% c("Chicago Red Stars", "Houston Dash", "NJ/NY Gotham FC", "OL Reign", "Orlando Pride", "Portland Thorns FC", "Washington Spirit")), 
              aes(x = season, y = pass_pct_half_diff, group = team_name, color = team_name), linewidth = 1) +
    labs(
      title = "Difference in Pass Accuracy Between First and Second Half by Season",
      x = "Season",
      y = "Pass Accuracy Difference (%)",
      fill = "Team Name"
    ) +
    theme_bw() 
  
ggplot(nwsl_team_stats |> 
       filter(!team_name %in% c("Chicago Red Stars", "Houston Dash", "NJ/NY Gotham FC", "OL Reign", "Orlando Pride", "Portland Thorns FC", "Washington Spirit")), 
       aes(x = season, y = pass_pct_half_diff, fill = team_name)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Goals Scored by Each Team per Season",
       x = "Season",
       y = "Goals Scored",
       fill = "Team Name") +
  theme_bw()


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