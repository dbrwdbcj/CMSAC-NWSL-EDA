library(tidyverse)
nwsl_team_stats <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/nwsl_team_stats.csv")

head(nwsl_team_stats) 

levels(factor(nwsl_team_stats$team_name))

attach(nwsl_team_stats)
levels(factor(team_name))
library(plotly)

plotly_test <- nwsl_team_stats |> 
  select(TeamName = team_name,
         PossPct = possession_pct,
         GoalDiff = goal_differential,
         Season = season) |> 
  ggplot(aes(x=PossPct, y=GoalDiff, color=Season, 
             text = paste( "",TeamName, "<br>", "Season:", Season, 
                           "<br>", "Possession%:", PossPct, "<br>", 
                           "Goal Differential:", GoalDiff)))+
  geom_point()+
  labs(
    title='Does Controlling the Ball Lead to Success?'
  )+
  xlab("Possession Percentage")+
  ylab("Goal Differential")+
  geom_smooth(mapping = aes(x=PossPct, y=GoalDiff, 
                            text = paste("", "Possession%:", 
                                         round(after_stat(x), 2), "<br>", 
                                         "Goal Differential:", round(after_stat(y), 2))), method="lm", 
              color="black", inherit.aes = FALSE)+
  theme_gray()+
  theme(
    plot.title = element_text(hjust=0.5, face="bold")
  )+
  theme(plot.background = element_rect(fill = "#ffeabd"))+
  theme(legend.background = element_rect(fill = "#ffeabd", color = "#ffeabd"))

ggplotly(plotly_test, tooltip = "text")

# #Team GD in 2016 vs 2022
# b=nwsl_team_stats
# b = remove(team_name=="Utah Royals FC")
# ggplot(nwsl_team_stats, aes(x=season, y=goal_differential, color=team_name))+
#   geom_point()+
#   geom_line()
# 
# 
# #Pass Accuracy vs. Pass Accuracy in the Second Half
# c <- nwsl_team_stats |> 
#   select(pass_pct, pass_pct_opposition_half) |> 
#   ggplot(aes(x=pass_pct, y=pass_pct_opposition_half))+
#   geom_point()+
#   geom_smooth(method="lm")
# 
# c
# cor(pass_pct, pass_pct_opposition_half)
# library(ggcorrplot)


#Clustering
#Creating the Shots variable
nwsl_cluster = nwsl_team_stats |> 
  mutate(Shots = round((goals/goal_conversion_pct)*100))

#Standardizing and selecting
nwsl_cluster <- nwsl_cluster |> 
  mutate(
    std_shots = as.numeric(scale(Shots)),
    std_gcp = as.numeric(scale(goal_conversion_pct))
  ) 
#Computing Distance Matrix
nwsl_dist <- nwsl_cluster |> 
  select(std_shots, std_gcp) |> 
  dist()
#Creating clusters using Complete Linkage Hierarchical Clustering
nwsl_complete <- nwsl_dist |> 
  hclust()
#Building scatterplot
nwsl_cluster |> 
  mutate(cluster = factor(cutree(nwsl_complete, k=5))) |> 
  ggplot(aes(x=Shots, y=goal_conversion_pct, color=cluster,
             size = nwsl_team_stats$goals))+
  geom_point(alpha=.5)


