library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

#data scrapped from basketball-reference.com 
# https://www.kaggle.com/drgilermo/nba-players-stats?select=Seasons_Stats.csv data set used with 
#set working directory to source file location in IDE (session -> set working directory -> to source file location)
#reading seasons_stats
df <- read.csv("Seasons_Stats.csv")

#We want to plant to 2 pointers and 3 pointers scored from 1982 to 2017
#We group years by groups of 4 (eg 1982-1983-1984-1985) to make it easier to visualize 
#the bar graph
#grouping in intervals with categories we want (year,x2p,x3p)
intervaux <- seq(1982,2018,4)
newdf <- drop_na(df[df$Year>=1982,c("Year","X2P","X3P")])
newdf$interval <- intervaux[findInterval(newdf$Year,intervaux)]


#we determine the mean for each interval of time and switch up the dataframe so it 
#can be visualized (having shot_type as a value)

inter <- newdf %>%  group_by(interval) %>% summarise(Two_Pointer=sum(X2P)*2,Three_Pointer=sum(X3P)*3)
final <- pivot_longer(inter,cols=c("Two_Pointer","Three_Pointer"),names_to = "Shot_type",values_to ="value")


# 
ggplot(final,aes(x=interval, y=value,fill=Shot_type,label=value)) + 
  geom_col( position='dodge') +
  scale_y_continuous(
    labels = unit_format(unit="K",scale=1e-3)
  ) +
  scale_fill_brewer(palette= "Set1") +
  labs(
    title="Points per season grouping according to type of shot in the NBA",
    subtitle="Data from 1982 to 2017 scrapped from basketball-reference",
    x="Grouped Seasons",
    y="Points"
  ) +
  theme(
    plot.title=element_text(size= 15, face="bold", hjust=0.5),
    plot.subtitle=element_text(size=13, face="bold", hjust=0.5),
    plot.caption=element_text(face = "italice", hjust=0),
    axis.title.x = element_text(size=16,face="bold"),
    axis.title.y = element_text(size=16,face="italic")
  )

ggsave("Type.png")

