library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

#data scrapped from basketball-reference.com 
# https://www.kaggle.com/drgilermo/nba-players-stats?select=Seasons_Stats.csv data set used with 
#set working directory to source file location in IDE (session -> set working directory -> to source file location)
#reading seasons_stats
df <- read.csv("Seasons_Stats.csv")


#Taking into account data from 1979 upwards (1979 last season without 3 point line)
df <- df[df$Year>1978,]
# The total sum of points in the season is the sum of every point every player scored in the season
pts <- df %>% group_by(Year) %>% summarise(Total=sum(na.omit(PTS)))
pts <- na.omit(pts)
tail(pts,20)

#Showing the data using line_plot to better show evolution through time + points 
#to make it easier to see the give total in a year

pts %>% ggplot(aes(x=Year,y=Total)) +
  geom_line(color="steelblue",size=1) +
  geom_point(color="steelblue",size=3) +
  labs(
    x="Year",
    y="Points scored",
    title="Points Per Season in the NBA",
    subtitle = "Data from 1979 to 2017 scrapped from basketball-reference"
  ) +
  theme(
    plot.title=element_text(size= 20, face="bold", hjust=0.5),
    plot.subtitle=element_text(size=13, face="bold", hjust=0.5),
    plot.caption=element_text(face = "italice", hjust=0),
    axis.title.x = element_text(size=16,face="bold"),
    axis.title.y = element_text(size=16,face="italic")
  ) +
  expand_limits(y=c(120000,300000)) +
  scale_y_continuous(
    labels = unit_format(unit="K",scale=1e-3)
  )

ggsave("Points.png")
