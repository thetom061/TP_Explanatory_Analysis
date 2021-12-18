library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

#data scrapped from basketball-reference.com 
# https://www.kaggle.com/drgilermo/nba-players-stats?select=Seasons_Stats.csv data set used with 
#set working directory to source file location in IDE (session -> set working directory -> to source file location)
#reading seasons_stats
df <- read.csv("Seasons_Stats.csv")



#We want to compare defensive rebounds in 1982 2017
#Histogram with both year seems good 
otherdf <- drop_na(df[df$Year==1982 | df$Year==2017,c("Year","DRB")])
otherdf$Year <- as.factor(otherdf$Year)

#Mean to be able to compare more than the profile
means <- otherdf %>% group_by(Year) %>% mutate(mean=mean(DRB))

means %>% ggplot(aes(x=DRB,fill=Year)) +
  #histogram
  geom_histogram(alpha=0.7,position="identity") +
  #showing mean 
  geom_vline(aes(xintercept=mean,colour=Year),linetype="dashed", size=0.8) +
  labs(
    title="Histogram of defensive rebounds in 1982 and 2017 NBA seasons",
    subtitle="Data of the 1982 and 2017 NBA Seasons scrapped from basketball-reference",
    x="Number of defensive rebounds",
    y="Count"
  ) +
  theme(
    plot.title=element_text(size= 16, face="bold", hjust=0.5),
    plot.subtitle=element_text(size=12, face="bold", hjust=0.5),
    plot.caption=element_text(face = "italice", hjust=0),
    axis.title.x = element_text(size=16,face="bold"),
    axis.title.y = element_text(size=16,face="italic")
  ) 
ggsave("DRB.png")
