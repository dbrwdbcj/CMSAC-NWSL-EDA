#test codes
library(tidyverse)
nwsl_team_stats <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/nwsl_team_stats.csv")

filter(nwsl_team_stats, season == 2022)

levels(factor(nwsl_team_stats$team_name))
