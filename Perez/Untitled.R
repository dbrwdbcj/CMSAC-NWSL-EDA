library(tidyverse)
library(dplyr)
library(ggmosaic)
library(devtools)
library(ranger)
library(glmnet)
library(ggthemes)

nwsl_team_stats <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/nwsl_team_stats.csv")
head(nwsl_team_stats)

levels(factor(nwsl_team_stats$team_name))
table(nwsl_team_stats$team_name)

#nwsl_team_stats |> 
# select (team_name, season, goal_differential) +