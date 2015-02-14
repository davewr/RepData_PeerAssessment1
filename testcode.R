# PeerAssess1
# Smartphones
library("lubridate")
library(dplyr)

activity <- read.csv("J:/coursera/DataScience/RepResearch/RepData_PeerAssessment1/activity.csv",
         stringsAsFactors=FALSE)

missedSteps <- is.na(activity.steps) 

myIncomplete <- activity[!complete.cases(activity),]
completeActivity <- activity[complete.cases(activity),]

tail(completeActivity)
head(completeActivity)

ndate1 <- as.Date(completeActivity$date, "%Y-%m-%d")
class(ndate1)

zz <- data.frame(as.Date(completeActivity$date, "%Y-%m-%d"))
xx <- data.frame(ymd(completeActivity$date))

yy <- cbind(completeActivity, zz)
head(yy)
tail(yy)

colnames(yy)[4] <- 'date1'

yy$month <- month(yy$date1)
yy$day <- day(yy$date1)

yy$DOY <- strftime(yy$date1, format = "%j")

group = factor(unique(g1$dt.DOY))
g1 <- data.frame(group=group,dt=yy)

hmgroup = factor(unique(g1$dt.interval))
hg2 <- data.frame(group=hmgroup, dt=yy)

#agg1 <- aggregate(g1$dt.steps, by=g1$group, FUN=sum)
#agg2 <- aggregate(g1$dt.steps, by=hg2$group, FUN=mean)

spdt <- summarise(group_by(g1, group), spd = sum(dt.steps))
hist(spdt$spd)

spit <- summarise(group_by(hg2, group), spm = mean(dt.steps))
barplot(spit$spm, names=spit$group, main ="Steps per 5 min. interval")

# -- or -- 
library(ggplot2)
qplot(group,spm, data=spit, geom="bar", stat="identity", main ="Steps per 5 min. interval")

ggp1 <- ggplot(spit, aes(x=group, y=spm)) + 
    geom_bar(stat="identity") + 
    theme(axis.text.x = element_text(angle=60, hjust=1))+
    ylab("Mean Steps per 5min Period") +
    xlab("5 min Group interval") +
    scale_x_discrete(breaks=seq(0, 2355, 60))

ggp1

ggp1 
    

plot(ggp1)


max(spit$spm)
spit[spit$spm>206,]

       #main ="Steps per 5 min. interval")


max(activity$interval)
