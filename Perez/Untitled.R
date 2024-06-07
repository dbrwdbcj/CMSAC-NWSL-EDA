library(tidyverse)
library(dplyr)
library(ggmosaic)
library(devtools)
library(ranger)
library(glmnet)
library(ggthemes)
library(ggplot2)
library(ggbeeswarm)
library(GGally)
library(tidyr)


nwsl_team_stats <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/nwsl_team_stats.csv")
head(nwsl_team_stats)

levels(factor(nwsl_team_stats$team_name))
table(nwsl_team_stats$team_name)

league_parity <- nwsl_team_stats |> 
  select (season, team_name, goal_differential)

league_parity |> 
  filter(season==2016) |> 
  ggplot(aes(x = goal_differential, y='')) +
  geom_violin() +
  geom_boxplot(width=0.5)

#league_parity |> 
#  ggplot(aes(x = goal_differential, y = season, color=season)) +
#  geom_violin(width = 0.2) +
#  geom_boxplot(width = 0.1)

league_parity_2 <- league_parity

league_parity_2$season = as.character(league_parity$season)

summary(league_parity_2)
summary(league_parity)

league_parity_2 |> 
  ggplot(aes(x = goal_differential, y = season, color=season)) +
  geom_violin() +
  geom_boxplot(width = 0.2)+
  labs(
    title = 'Measuring Parity in the NWSL',
    subtitle = 'Distribution of Goal Differential per Season',
    xlab = 'Goal Differential',
    ylab = 'Season'
  ) +
  theme_bw()+
  theme(legend.position="none")

#################

season_goals <- nwsl_team_stats |> 
  select (season, team_name, games_played, goals)

table(season_goals$season)

goals_by_year <- aggregate(goals ~ season, data = season_goals, FUN = sum)

goals_by_year |> 
  ggplot(aes(x=season, y=goals))+
  geom_point()+
  geom_line()


###################

improvement <- nwsl_team_stats |> 
  select(season, goals, goal_conversion_pct, shot_accuracy)


average_goals <- aggregate(goals ~ season, data = improvement, FUN = mean)

# Create the ggplot
average_goals |> 
  ggplot(aes(x = factor(season), y = goals)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Average Goals per Year",
       x = "Year",
       y = "Average Goals") +
  theme_bw()



shot_acc_year <- aggregate(shot_accuracy ~ season, data = improvement, FUN = mean)

shot_acc_year |> 
  ggplot(aes(x = factor(season), y = shot_accuracy)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Average Shot Accuracy per Year",
       x = "Year",
       y = "Average Shot Accuracy") +
  theme_bw()


goal_convert_year <- aggregate(goal_conversion_pct ~ season, data = improvement, FUN = mean)

goal_convert_year |> 
  ggplot(aes(x = factor(season), y = goal_conversion_pct)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Average Goal Conversion % per Year",
       x = "Year",
       y = "Average Goal Conversion % ") +
  theme_bw()