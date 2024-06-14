library(tidyverse)
nwsl_team_stats <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/nwsl_team_stats.csv")

head(nwsl_team_stats) 

levels(factor(nwsl_team_stats$team_name))

attach(nwsl_team_stats)
levels(factor(team_name))
library(plotly)
library(ggplot2)
library(tidyverse)
library(ggthemes)
library(viridis)

#Turning season into a discrete variable
season_disc <- as.factor(season)


#PLOT ONLY APPEARS IN MARKDOWN!
plotly_test <- nwsl_team_stats |> 
  select(TeamName = team_name,
         PossPct = possession_pct,
         GoalDiff = goal_differential,
         Season = season) |> 
  ggplot(aes(x=PossPct, y=GoalDiff, color=Season, 
             text = paste( "",TeamName, "<br>", "Season:", Season, 
                           "<br>", "Possession%:", PossPct, "<br>", 
                           "Goal Differential:", GoalDiff)))+
  geom_point(alpha=0.6, size = 2)+
  scale_color_viridis(discrete=FALSE)+
  labs(
    title='Does Controlling the Ball Lead to Success?',
    subtitle = "Goal Differential vs. Possession Percentage"
  )+
  xlab("Possession Percentage")+
  ylab("Goal Differential")+
  geom_smooth(mapping = aes(x=PossPct, y=GoalDiff, 
                            text = paste("", "Possession%:", 
                                         round(after_stat(x), 2), "<br>", 
                                         "Goal Differential:", round(after_stat(y), 2))), method="lm", 
              color="gray20", inherit.aes = FALSE)+
  theme_solarized()+
  theme(plot.title = element_text(hjust=0.5, family="Arial", size= 14),
        axis.title = element_text(family="Arial", size=12),
        axis.text = element_text(family="Arial", size=12),
        legend.title = element_text(family = "Arial", size = 10),  
        legend.text = element_text(family = "Arial", size = 8),
        panel.border = element_rect(color = "gray", fill = NA, linewidth = .5)
  )

ggplotly(plotly_test, tooltip = "text")


#Clustering: NOT USED
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

#Creating categorical variables
tackle.cat = cut(tackle_success_pct, 
                 breaks = c(quantile(tackle_success_pct, 0), 
                            quantile(tackle_success_pct, 1/3), 
                            quantile(tackle_success_pct, 2/3),
                            quantile(tackle_success_pct, 1)), 
                 labels = c("Low", "Medium", "High"), include.lowest = TRUE, 
                 ordered_result = TRUE)
table(tackle.cat)
#Splitting up tackle success% into low, medium, and high usage.
gc.cat = cut(goals_conceded, 
                 breaks = c(quantile(goals_conceded, 0), 
                            quantile(goals_conceded, 1/3), 
                            quantile(goals_conceded, 2/3),
                            quantile(goals_conceded, 1)), 
                 labels = c("Low", "Medium", "High"), include.lowest = TRUE, 
                 ordered_result = TRUE)
table(gc.cat)
#Splitting up goals conceded into low, medium, and high usage.

library(ggmosaic)
nwsl_team_stats |> 
  ggplot()+
  geom_mosaic(aes(x=product(gc.cat, tackle.cat), fill = gc.cat))
table(gc.cat, tackle.cat)  

ggplot(nwsl_team_stats, aes(x=shot_accuracy, y=goal_conversion_pct))+
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