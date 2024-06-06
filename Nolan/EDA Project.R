library(tidyverse)
nwsl_team_stats <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/nwsl_team_stats.csv")

head(nwsl_team_stats) 

levels(factor(nwsl_team_stats$team_name))

attach(nwsl_team_stats)
levels(factor(team_name))
