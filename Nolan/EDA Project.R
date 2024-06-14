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

#Bee Swarm Plot
library(ggbeeswarm)
nwsl_team_stats |> 
  ggplot(aes(x = goal_conv.cat, y = shot_accuracy)) +
  geom_beeswarm(cex = 2)

#Dotplot (Half of the beeswarm plot)
taylor_all_songs |> 
  ggplot(aes(x=duration))+
  geom_dotplot(binwidth = 0.1)

#Histogram
nwsl_team_stats |> 
  ggplot(aes(x= goal_conv.cat, y=shot_accuracy))+
  geom_histogram()

#Hexbin Plot
library(hexbin)
ggplot(nwsl_team_stats, aes(x = shot_accuracy, y = goal_conversion_pct)) +
  geom_hex(bins = 10) +
  #labs(x= "Average Slider Speed (mph)", y = "Average Slider Break (in.)", 
       #title = "Graphing Pitchers' Average Speed and Break of a Slider")+
  #scale_fill_continuous("Number of Pitchers") +
  theme(plot.background = element_rect(fill = "#ffeabd"))+
  theme(legend.background = element_rect(fill = "#ffeabd"))

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
  ggplot(aes(x = Season, y=shot_acc, color = Place))+
  geom_point()+
  geom_line(aes(group = Place))+
  scale_color_manual(values = c("black", 
                                viridis_pal()(length(unique(topfive$Place)))))  # Customize line colors here
