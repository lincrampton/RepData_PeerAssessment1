Reproducible Research, Peer Assessment #1
Coursera, Johns Hopkins, Prof. Peng

========================================================
## Loading and preprocessing the data
### Load libraries

```r
library("ggplot2")
library("RColorBrewer")
library("lubridate")
```

### Acquire Data

```r
sourceUrl = "https://d396qusza40orc.cloudfront.net"
sourceFileName = "repdata_data_activity.zip"
zipFile = "activity.zip"
csvFile = "activity.csv"
### readData reads in datafile if it exists, or recreates it as much as
### necessary if downloading, notes the download date in file
### 'dateUrlDownloaded.txt'
readData = function() {
    if (!file.exists(csvFile)) {
        # if the csvFile exists, move on only download when no zipFile in dir
        if (!file.exists(zipFile)) 
            {
                download.file(sourceUrl, destfile = zipFileName, method = "curl")
                sink("./dateUrlDownloaded.txt")
                downloadDate = date()
                sink()
            }  #end download sourceUrl
        unzip(zipFile)
    }
    dataIn = read.csv(csvFile, header = T, sep = ",")
}
```

### Load and Examine Data

```r
activityData = readData()
names(activityData)
```

```
## [1] "steps"    "date"     "interval"
```

```r
str(activityData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
summary(activityData)
```

```
##      steps               date          interval   
##  Min.   :  0.0   2012-10-01:  288   Min.   :   0  
##  1st Qu.:  0.0   2012-10-02:  288   1st Qu.: 589  
##  Median :  0.0   2012-10-03:  288   Median :1178  
##  Mean   : 37.4   2012-10-04:  288   Mean   :1178  
##  3rd Qu.: 12.0   2012-10-05:  288   3rd Qu.:1766  
##  Max.   :806.0   2012-10-06:  288   Max.   :2355  
##  NA's   :2304    (Other)   :15840
```

```r
dim(activityData)
```

```
## [1] 17568     3
```

```r
summary(activityData$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##     0.0     0.0     0.0    37.4    12.0   806.0    2304
```

# What is the mean total number of steps taken per day?

```r
dailySteps <- aggregate(steps ~ date, data = activityData, FUN = sum)
meanDailySteps = mean(dailySteps$steps)
medianDailySteps = median(dailySteps$steps)
maxStepsPerDay = dailySteps$day[which.max(dailySteps$steps)]
intervalSteps <- aggregate(steps ~ interval, data = activityData, FUN = sum)
meanIntervalSteps = mean(intervalSteps$steps)
medianIntervalSteps = median(intervalSteps$steps)
maxStepsPerInterval = intervalSteps$interval[which.max(intervalSteps$steps)]
```


## What is the average daily activity pattern?

```r
### plot number of steps per day
barplot(dailySteps$steps, names.arg = dailySteps$date, xlab = "Date", ylab = "Steps", 
    cex.names = 0.5, col = "blue")
```

![plot of chunk ave_activity](figure/ave_activity1.png) 

```r

### plot number of steps per time interval
barplot(intervalSteps$steps, names.arg = intervalSteps$interval, col = "blue", 
    xlab = "Time Interval", ylab = "Steps", cex.names = 0.5, main = "Steps per Interval over the Course of a Day")
```

![plot of chunk ave_activity](figure/ave_activity2.png) 

## Imputing missing values

```r
sum(is.na(activityData))
```

```
## [1] 2304
```

```r
sum(is.na(activityData$steps))
```

```
## [1] 2304
```

```r
sum(is.na(activityData$date))
```

```
## [1] 0
```

```r
sum(is.na(activityData$interval))
```

```
## [1] 0
```

```r
missingVals = is.na(activityData$steps)
str(missingVals)
```

```
##  logi [1:17568] TRUE TRUE TRUE TRUE TRUE TRUE ...
```

```r

activityData <- merge(activityData, intervalSteps, by = "interval", suffixes = c("", 
    ".splined"))
names(activityData)
```

```
## [1] "interval"      "steps"         "date"          "steps.splined"
```

```r
activityData$steps[missingVals] <- activityData$steps.splined[missingVals]
splinedDailySteps <- aggregate(steps.splined ~ date, data = activityData, FUN = sum)
sum(is.na(splinedDailySteps))
```

```
## [1] 0
```

```r
meanSplinedDailySteps = mean(splinedDailySteps$steps)
medianSplinedDalySteps = median(splinedDailySteps$steps)
```

## Are there differences in activity patterns between weekdays and weekends?

```r
is.weekend = function(date) {
    if (wday(date, label = T) %in% c("Sat", "Sun")) {
        "Weekend"
    } else {
        "Weekday"
    }
}
activityData$isWeekend = as.factor(sapply(activityData$date, is.weekend))
names(activityData)
```

```
## [1] "interval"      "steps"         "date"          "steps.splined"
## [5] "isWeekend"
```

```r

weekendData = subset(activityData, isWeekend == "Weekend")
weekdayData = subset(activityData, isWeekend == "Weekday")
par(mfrow = c(2, 1))
plot(weekendData$interval, weekendData$steps, type = "h", xlab = "Time Interval", 
    ylab = "Steps per Interval", col = "blue", main = "Weekend Data")
plot(weekdayData$interval, weekdayData$steps, type = "h", xlab = "Time Interval", 
    ylab = "Steps per Interval", col = "red", main = "Weekday Data")
```

![plot of chunk weekends_different](figure/weekends_different.png) 


## Report Date

```r
### Document report date
reportDate = date()
```


