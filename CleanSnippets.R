# Clean code for report:

library("lubridate")
library(dplyr)

activity <- read.csv("J:/coursera/DataScience/RepResearch/RepData_PeerAssessment1/activity.csv",
                     stringsAsFactors=FALSE)

activity$date <- as.Date(activity$date, "%Y-%m-%d")
activity$date <- as.Date(activity$date, "%Y-%m-%d")
activity$dow <- wday(activity$date)
activity$DOY <- strftime(activity$date, format = "%j")

activity$daytype[with(activity, dow > 1 & dow <7)] <- "weekday" 
activity$daytype[with(activity, dow == 1 | dow ==7)] <- "weekend" 


myIncomplete <- activity[!complete.cases(activity),]
completeActivity <- activity[complete.cases(activity),]

ca <- completeActivity
rm(g1)
g1 <- ca
group <- factor(unique(ca$DOY))
g1 <- data.frame(group=group,ca)

hmgroup = factor(unique(ca$interval))
hg2 <- data.frame(group=hmgroup, ca)

#agg1 <- aggregate(g1$dt.steps, by=g1$group, FUN=sum)
#agg2 <- aggregate(g1$dt.steps, by=hg2$group, FUN=mean)

spdt <- summarise(group_by(g1, group), spd = sum(steps))
averageStepsPerDay <- round(mean(spdt$spd),0)

hist(spdt$spd, main = "Histogram of Steps per day", xlab="Steps per Day")


spit <- summarise(group_by(hg2, group), spm = mean(steps))
barplot(spit$spm, names=spit$group, main ="Mean of Steps per 5 min. interval",
        xlab = "5 min Time Interval", ylab= "Mean STep Frequency")
# Prefer this barplot



