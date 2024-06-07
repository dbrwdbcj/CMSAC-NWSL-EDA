library(tidyverse)
nwsl_team_stats <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/nwsl_team_stats.csv")

head(nwsl_team_stats) 

levels(factor(nwsl_team_stats$team_name))

attach(nwsl_team_stats)
levels(factor(team_name))
library(plotly)

gd_vs_pp <- nwsl_team_stats |> 
  ggplot(aes(x=possession_pct, y=goal_differential))+
  geom_point(color="red")+
  labs(
    title='Does Controlling the Ball Lead to Success?'
  )+
  xlab("Possession Percentage")+
  ylab("Goal Differential")+
  geom_smooth(method="lm")+
  theme_gray()+
    theme(
    plot.title = element_text(hjust=0.5, face="bold")
  )

ggplotly(gd_vs_pp)
