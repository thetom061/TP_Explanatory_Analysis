library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

#data scrapped from basketball-reference.com 
# https://www.kaggle.com/drgilermo/nba-players-stats?select=Seasons_Stats.csv data set
#set working directory to source file location in IDE (session -> set working directory -> to source file location)
#reading seasons_stats
df <- read.csv("Seasons_Stats.csv")

#We want to see total wolume of three point attempt by season, we need to summarise every season
#and determine total 3pa
otherdf <- drop_na(df[df$Year>=1982,c("Year","X3PA")])
final <- otherdf %>% group_by(Year) %>% summarise(X3PA=sum(X3PA))

# area graph + point to show scale of evolution and to be able to see the number of 
#attempts in a given season 
final %>% ggplot(aes(x=Year,y=X3PA)) +
  geom_area(fill="lightblue") +
  geom_point(color="steelblue",size=3) +
  labs(
    title="Three point attempts by season in the NBA from 1982 to 2017",
    subtitle="Data from 1982 to 2017 scrapped from basketball-reference",
    x="Season",
    y="Number of 3 points attempts"
  ) +
  theme(
    plot.title=element_text(size= 16, face="bold", hjust=0.5),
    plot.subtitle=element_text(size=13, face="bold", hjust=0.5),
    plot.caption=element_text(face = "italice", hjust=0),
    axis.title.x = element_text(size=16,face="bold"),
    axis.title.y = element_text(size=16,face="italic")
  ) +
  expand_limits(y=c(0,80000)) +
  scale_y_continuous(
    labels = unit_format(unit="K",scale=1e-3)
  ) 

ggsave("3PA.png")
