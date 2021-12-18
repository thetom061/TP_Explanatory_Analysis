library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

#data scrapped from basketball-reference.com 
# https://www.kaggle.com/drgilermo/nba-players-stats?select=Seasons_Stats.csv data set
#set working directory to source file location in IDE (session -> set working directory -> to source file location)
#reading seasons_stats
df <- read.csv("Seasons_Stats.csv")

#We want to show evolution of shooting percentage in grouped seasons (from 1982 to 2017)
#boxplot seems appropriate
#grouping seasons 
intervaux <- seq(1982,2018,4)
otherdf <- drop_na(df[df$Year>=1982,c("Year","X3P.")])
otherdf$interval <- as.factor(intervaux[findInterval(otherdf$Year,intervaux)])

#doing the boxplot
otherdf %>% ggplot(aes(x=interval,y=X3P.)) + 
  geom_boxplot(notch=TRUE,outlier.shape = NA) +
  stat_summary(fun=mean, geom="point", size=2,color="blue") +
  labs(
    title="3 Point Shooting percentages by grouped NBA seasons",
    subtitle="Data from 1982 to 2017 scrapped from basketball-reference",
    x="Grouped Seasons",
    y="Shooting percentage"
  ) +
  theme(
    plot.title=element_text(size= 16, face="bold", hjust=0.5),
    plot.subtitle=element_text(size=13, face="bold", hjust=0.5),
    plot.caption=element_text(face = "italice", hjust=0),
    axis.title.x = element_text(size=16,face="bold"),
    axis.title.y = element_text(size=16,face="italic")
  )+ 
  coord_cartesian(ylim = c(0,0.8)) +
  scale_y_continuous(
    labels = unit_format(unit="%",scale=1e2)
  ) 

ggsave("3PointPer.png")
