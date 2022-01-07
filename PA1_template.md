---
title: "Reproducible Research: Peer Assessment"
author: "MV"
date: "1/1/2022"
output: 
    html_document:
        keep_md: true
---

Loading and preprocessing the data

```r
ActivityData <- read.csv("activity.csv")
summary(ActivityData)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```


Mean and median total number of steps taken per day

```r
StepsPerDay <- aggregate(steps~date, ActivityData, sum, na.rm=TRUE)
hist(StepsPerDay$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
meanStepsPerDay <- mean(StepsPerDay$steps)
meanStepsPerDay
```

```
## [1] 10766.19
```

```r
medianStepsPerDay <- median(StepsPerDay$steps)
medianStepsPerDay
```

```
## [1] 10765
```

Average daily activity pattern and maximum number of the steps per interval

```r
StepsPerInterval <- aggregate(steps~interval, ActivityData, mean, na.rm=TRUE)
plot(steps~interval, data = StepsPerInterval, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
MaxIntervalSteps <- StepsPerInterval[which.max(StepsPerInterval$steps),]$intervaL
MaxIntervalSteps
```

```
## NULL
```

Inputting missing values and creating new dataset without missing values

```r
TotalMissingValues <- sum(is.na(ActivityData$steps))
TotalMissingValues
```

```
## [1] 2304
```

```r
MeanStepPerInterval <- function(interval){
    StepsPerInterval[StepsPerInterval$interval==interval,]$steps
}

ActivityDataNoNAs <- ActivityData
    for(i in 1:nrow(ActivityDataNoNAs)){
        if(is.na(ActivityDataNoNAs[i,]$steps)){
            ActivityDataNoNAs[i,]$steps <- MeanStepPerInterval(ActivityDataNoNAs[i,]$interval)
        }
    }

StepsPerDayNoNAs <- aggregate(steps~date, data=ActivityDataNoNAs, sum)
hist(StepsPerDayNoNAs$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
meanStepsPerDayNoNAs <- mean(StepsPerDayNoNAs$steps)
medianStepsPerDayNoNAs <- median(StepsPerDayNoNAs$steps)
meanStepsPerDayNoNAs
```

```
## [1] 10766.19
```

```r
medianStepsPerDayNoNAs
```

```
## [1] 10766.19
```

Are there any differences in activity patterns weekdays and weekends?

```r
ActivityDataNoNAs$date <- as.Date(strptime(ActivityDataNoNAs$date, format = "%Y-%m-%d"))
ActivityDataNoNAs$day <- weekdays(ActivityDataNoNAs$date)
for (i in 1:nrow(ActivityDataNoNAs)) {
    if (ActivityDataNoNAs[i,]$day %in% c("Saturday", "Sunday")) {
        ActivityDataNoNAs[i,]$day <- "weekend"
    }
    else {
        ActivityDataNoNAs[i,]$day <-"weekday"
    }
}
StepsByDay <- aggregate(ActivityDataNoNAs$steps~ActivityDataNoNAs$interval + ActivityDataNoNAs$day, ActivityDataNoNAs, mean)

#make panel plot
names(StepsByDay) <- c("interval", "day", "steps")
library(lattice)
xyplot(steps~interval | day, StepsByDay, type="l", layout= c(1, 2),
       xlab= "Interval", ylab= "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
