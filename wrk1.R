fileZip <- "activity.zip"
fileCSV <- "activity.csv"

act <- read.csv(unz(fileZip, fileCSV))
act$date <- as.Date(act$date)

#investigation
names(act)
head(act,500)
nrow(act)
table(act$date)
table(act$interval)
class(act$date)
act$date <- as.Date(act$date)


actNAOut <- act[!is.na(act$steps), ]
library(plyr)


#1
# group values by date
actNAOutByDate <- ddply(actNAOut, ~date, summarise, sum = sum(steps))
head(actNAOutByDate)
hist(actNAOutByDate$sum, col="darkBlue", xlab="Steps p/day", ylab="days with this sum of steps"
        ,main="Number of steps taken each day")

meanactNAOutByDate <- mean(actNAOutByDate$sum)
medianactNAOutByDate <- median(actNAOutByDate$sum)


#2
actNAOutAvgPerInterval <- ddply(actNAOut, .(interval), summarise, avgSteps = mean(steps))
head(actNAOutAvgPerInterval)
plot(actNAOutAvgPerInterval$interval, actNAOutAvgPerInterval$avgSteps, type='l'
        , xlab = '5-minute interval', ylab = 'Avg steps', main='Time series plot
        , sum of 5-minute interval')

maxValStepsactNAOut <- max(actNAOutAvgPerInterval$avgSteps)
maxLine <- actNAOutAvgPerInterval[actNAOutAvgPerInterval$avgSteps == maxValStepsactNAOut, ]$interval

#3
#31
sum(is.na(act$steps))
#32
actNAOutAvgPerInterval
#33
actV2 <- act
actV2NARows <- is.na(actV2$steps)
actV2[actV2NARows,]$steps <- actNAOutAvgPerInterval[match(actV2[actV2NARows,]$interval
                , actNAOutAvgPerInterval$interval), "avgSteps"]
head(act)
head(actV2)
#34
actV2ByDate <- ddply(actV2, ~date, summarise, sum = sum(steps))
hist(actV2ByDate$sum, col="darkBlue", xlab="Steps p/day", ylab="days with this sum of steps"
     ,main="Number of steps taken each day, NA elimited with global average of the interval")

meanactByDate <- mean(actV2ByDate$sum)
medianactByDate <- median(actV2ByDate$sum)

#4
Sys.setlocale("LC_TIME", "C")
actV2$WeekDayType  <- ifelse(weekdays(actV2$date) == "Saturday" | weekdays(actV2$date) == 
                       "Sunday", "Weekend", "Weekday")
        
actV2$WeekDayType <- as.factor(actV2$WeekDayType)
head(actV2)

actV2DayTypeComparsion <- ddply(actV2, .(interval, WeekDayType), summarise, avgSteps = mean(steps))

library('lattice')
xyplot(avgSteps ~ interval | WeekDayType, data = actV2DayTypeComparsion
        , type = "l", layout=c(1,2), xlab = 'interval', ylab = 'steps')



install.packages('knitr', dependencies = TRUE)
library(knitr)
knit2html("PA1_template.Rmd")

opts_knit$set(base.dir = 'docs')


Use fig.path to set the plot directory relative to your working directory (OR)
Use base.dir to set an absolute directory in which to save plots.
