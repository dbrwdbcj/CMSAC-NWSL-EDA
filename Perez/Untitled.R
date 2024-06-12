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

league_parity_2 <- league_parity

league_parity_2$season = as.character(league_parity$season)

summary(league_parity_2)
summary(league_parity)

###############################


league_parity_2 |> 
  ggplot(aes(x = goal_differential, y = season, color=season)) +
  geom_violin() +
  geom_boxplot(width = 0.2)+
  labs(
    title = 'Measuring Parity in the NWSL',
    subtitle = 'Distribution of Goal Differential per Season',
    x = 'Goal Differential',
    y = 'Season'
  ) +
  theme_bw()+
  theme(legend.position="none")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.subtitle = element_text(hjust = 0.5))


league_parity_2 |> 
  ggplot(aes(x = goal_differential, y = season, fill=season)) +
  geom_violin() +
  geom_boxplot(width = 0.2)+
  labs(
    title = 'Measuring Parity in the NWSL',
    subtitle = 'Distribution of Goal Differential per Season',
    x = 'Goal Differential',
    y = 'Season'
  ) +
  theme_economist()+
  theme(legend.position="none")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.subtitle = element_text(hjust = 0.5))

league_parity_2 |> 
  ggplot(aes(x = goal_differential, y = season, color = season)) +
  geom_violin(fill=NA) +
  geom_boxplot(width = 0.2, fill=NA)+
  labs(
    title = 'Measuring Parity in the NWSL',
    subtitle = 'Distribution of Goal Differential per Season',
    x = 'Goal Differential',
    y = 'Season'
  ) +
  theme_solarized()+
  theme(legend.position="none")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.subtitle = element_text(hjust = 0.5))

league_parity_2 |> 
  ggplot(aes(x = goal_differential, y = season, fill=season)) +
  geom_violin() +
  geom_boxplot(width = 0.2)+
  labs(
    title = 'Measuring Parity in the NWSL',
    subtitle = 'Distribution of Goal Differential per Season',
    x = 'Goal Differential',
    y = 'Season'
  ) +
  theme_solarized()+
  theme(legend.position="none")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.subtitle = element_text(hjust = 0.5))

#####################
library(flexclust)
library(dslabs)


nwsl_team_stats <- nwsl_team_stats |> 
  mutate(shots = round(goals*(100/goal_conversion_pct)))

nwsl_team_stats |> 
  ggplot(aes(x=shots))+
  geom_histogram(bins=10)

nwsl_team_stats |> 
  ggplot(aes(x=goals))+
  geom_histogram(bins=10)

nwsl_team_stats <- nwsl_team_stats |> 
  mutate(
    std_shots = as.numeric(scale(shots, center = TRUE, scale = TRUE)),
    std_goals = as.numeric(scale(goals, center = TRUE, scale = TRUE)),
    std_goal_conversion = as.numeric(scale(goal_conversion_pct, center = TRUE, scale = TRUE))
  )


####################
#clustering attempt 3
nwsl_kmpp <- function(k) {
  
  kmeans_results <- nwsl_team_stats |>
    select(std_shots, std_goal_conversion) |>
    kcca(k = k, control = list(initcent = "kmeanspp"))
  
  kmeans_out <- tibble(
    clusters = k,
    total_wss = sum(kmeans_results@clusinfo$size * 
                      kmeans_results@clusinfo$av_dist)
  )
  return(kmeans_out)
}

n_clusters_search <- 2:12
kmpp_search <- n_clusters_search |> 
  map(nwsl_kmpp) |> 
  bind_rows()
kmpp_search |> 
  ggplot(aes(x = clusters, y = total_wss)) +
  geom_line() + 
  geom_point(size = 4) +
  scale_x_continuous(breaks = n_clusters_search)

###########

set.seed(10)
init_kmeanspp <- nwsl_team_stats |> 
  select(std_shots, std_goal_conversion) |> 
  kcca(k = 5, control = list(initcent = "kmeanspp"))

nwsl_team_stats$shot_cluster <- as.factor(init_kmeanspp@cluster)

nwsl_team_stats |>
  mutate(
    shot_cluster = as.factor(init_kmeanspp@cluster)
  ) |>
  ggplot(aes(x = shots, y = goal_conversion_pct,
             color = shot_cluster)) +
  geom_point(size = 4) + 
  ggthemes::scale_color_colorblind() +
  theme(legend.position = "bottom")+
  theme_bw()


nwsl_team_stats |>
  mutate(
    shot_cluster = as.factor(init_kmeanspp@cluster)
  ) |>
  ggplot(aes(x = shots, y = goal_conversion_pct,
             color = shot_cluster, size = goals)) +
  geom_point(alpha=0.7) + 
  labs(
    title = 'NWSL Offensive Seasons',
    x = 'Shots Attempted',
    y = 'Goal Conversion %'
  )+
  theme_solarized()+
  theme(legend.position = "bottom", 
        legend.background = element_rect(fill = NA),
        legend.key = element_rect(fill=NA))+
  theme(plot.title = element_text(hjust = 0.5))

############







