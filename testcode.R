# PeerAssess1
# Smartphones
library("lubridate")
library(dplyr)

activity <- read.csv("J:/coursera/DataScience/RepResearch/RepData_PeerAssessment1/activity.csv",
         stringsAsFactors=FALSE)

# missedSteps <- is.na(activity$steps) 

myIncomplete <- activity[!complete.cases(activity),]
completeActivity <- activity[complete.cases(activity),]

tail(completeActivity)
head(completeActivity)

activity <- read.csv("J:/coursera/DataScience/RepResearch/RepData_PeerAssessment1/activity.csv",
                     stringsAsFactors=FALSE)

activity$date <- as.Date(activity$date, "%Y-%m-%d")
activity$dow <- wday(activity$date)
activity$DOY <- strftime(activity$date, format = "%j")

activity$daytype[with(activity, dow > 1 & dow <7)] <- "weekday" 
activity$daytype[with(activity, dow == 1 | dow ==7)] <- "weekend" 


myIncomplete <- activity[!complete.cases(activity),]
completeActivity <- activity[complete.cases(activity),]

#yy <- completeActivity


#yy$month <- month(yy$date1)
#yy$day <- day(yy$date1)

#yy$DOY <- strftime(yy$date1, format = "%j")
#yy$weekdays <- weekdays(yy$date1)
#yy$dow <- wday(yy$date1)
# junk$nm[junk$nm == "B"] <- "b"
# df$Items[with(df, Store.Type == "A" | Store.Type == "C")] <- 0L

#yy$daytype[with(yy, dow > 1 & dow <7)] <- "weekday" 
#yy$daytype[with(yy, dow == 1 | dow ==7)] <- "weekend" 

#yy[yy$dow==2,]
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

# -- or -- 

# Don't like this plot as much as simple barplot...
library(ggplot2)
qplot(group,spm, data=spit, geom="bar", stat="identity", main ="Steps per 5 min. interval")

ggp1 <- ggplot(spit, aes(x=group, y=spm)) + 
    geom_bar(stat="identity") + 
    theme(axis.text.x = element_text(angle=60, hjust=1))+
    ylab("Mean Steps per 5min Period") +
    xlab("5 min Group interval") +
    scale_x_discrete(breaks=seq(0, 2355, 30)) 

ggp1
# -- or --
plot(ggp1)


max(spit$spm)
mean(spit$spm)
spit[spit$spm>206,]


max(activity$interval)

# merge myIncomplete and spit (which contains means based on interval)
# The merge and cobersion to "integer" works well enoough for this purpose
merged <- merge(myIncomplete, spit, by.x="interval", by.y="group", 
                all=F)
merged$spm <- round(merged$spm, 0)
merged$steps  <- merged$spm
merged <- merged[,-4]

completeMerged <- rbind(completeActivity, merged)

# dd[with(dd, order(-z, b)), ]

newMerge <- completeMerged[with(completeMerged, order(date,interval)),]
