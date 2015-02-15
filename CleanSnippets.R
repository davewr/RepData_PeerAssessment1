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
        xlab = "5 min Time Interval", ylab= "Mean Step Frequency")
# Prefer this barplot




tpMax <- max(spit$spm)
tpMaxRow <- spit[spit$spm >= tpMax,]
tpmr <- tpMaxRow[1,1]

# Imputing Values calculation
# ***************************
# merge myIncomplete and spit (which contains means based on interval)
# The merge and cobersion to "integer" works well enoough for this purpose
merged <- merge(myIncomplete, spit, by.x="interval", by.y="group", 
                all=F)
merged$spm <- round(merged$spm, 0)
merged$steps  <- merged$spm
merged <- merged[,-7]

# Bind the Complete and Imcomplet tables
completeMerged <- rbind(completeActivity, merged)
# Sort the data
newMerge <- completeMerged[with(completeMerged, order(date,interval)),]

incomplete <- nrow(myIncomplete)

# shorten "completeActivity" name for convenience
cm <- completeMerged

g1m <- cm

groupcm <- factor(unique(cm$DOY))
g1m <- data.frame(group=groupcm,cm)

hmgroupm = factor(unique(cm$interval))
hg2cm <- data.frame(group=hmgroupm, cm)

spdtcm <- summarise(group_by(g1m, group), spd = sum(steps))

spitcm <- summarise(group_by(hg2cm, group), spm = mean(steps))

cmaverageStepsPerDay <- round(mean(spdtcm$spd),0)
cmmedianStepsPerDay <- round(median(spdtcm$spd),0)


histinfo <- hist(spdtcm$spd, main = "Histogram of Steps per day", 
                 xlab="Steps per Day", breaks=10)

barplot(spit$spm, names=spitcm$group, main ="Mean of Steps per 5 min. interval",
        xlab = "5 min Time Interval", ylab= "Mean Step Frequency")
# Prefer this barplot

cmMax <- max(spitcm$spm)
cmMaxRow <- spitcm[spitcm$spm >= cmMax,]
cmmr <- tpMaxRow[1,1]

