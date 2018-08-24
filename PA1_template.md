---
title: "Assignment 2 -- PA1_template"
author: "Mike Segura"
date: "August 23, 2018"
output: 
  html_document: 
    keep_md: yes
---


### Load several functions in the event they're not loaded

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

### Question #1: Import Activity file and remove NA from steps column

```r
ActivityDataImport <- read.csv("activity.csv")
## Remove NA in data
Final_Data <- ActivityDataImport[ with (ActivityDataImport, { !(is.na(steps)) } ), ]
```


### Question #2: What is mean total number of steps taken per day?

```r
## Part A: Calculate the total number of steps taken per day

summary(Final_Data$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00    0.00    0.00   37.38   12.00  806.00
```

```r
## Group for by date steps aggregation
ByDate <- group_by (Final_Data, date)
StepsByDate <- summarise(ByDate, total = sum(steps))
StepsByDate
```

```
## # A tibble: 53 x 2
##    date       total
##    <fct>      <int>
##  1 2012-10-02   126
##  2 2012-10-03 11352
##  3 2012-10-04 12116
##  4 2012-10-05 13294
##  5 2012-10-06 15420
##  6 2012-10-07 11015
##  7 2012-10-09 12811
##  8 2012-10-10  9900
##  9 2012-10-11 10304
## 10 2012-10-12 17382
## # ... with 43 more rows
```

```r
## Part B: Make a histogram of the total number of steps taken each day
hist(StepsByDate$total, col = "green", main="Histogram of Total Steps by Day",
                      xlab = "Total Number of Steps in a Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
## Part C: Calculate and report the mean and median of the total number of steps taken per day
summary(StepsByDate)
```

```
##          date        total      
##  2012-10-02: 1   Min.   :   41  
##  2012-10-03: 1   1st Qu.: 8841  
##  2012-10-04: 1   Median :10765  
##  2012-10-05: 1   Mean   :10766  
##  2012-10-06: 1   3rd Qu.:13294  
##  2012-10-07: 1   Max.   :21194  
##  (Other)   :47
```
### Question #3: What is the average daily activity pattern?

```r
## Part A: Make a time series plot (i.e.type="l") of the 5-minute interval (x-axis) and the 
## average number of steps taken, averaged across all days (y-axis).
DailyAVG <- aggregate(x=list(steps=Final_Data$steps), by=list(interval=Final_Data$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=DailyAVG, aes(x=interval, y=steps), type = "1") +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
## Part B: Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

MaxInterval <- which.max(DailyAVG$steps)
DailyAVG[MaxInterval, ]
```

```
##     interval    steps
## 104      835 206.1698
```

### Question #3: Imputing missing values

```r
## Part A: Calculate and report the total number of missing values in the dataset
## i.e. the total number of rows with NAs.
NA_ROWS <- sum(is.na(ActivityDataImport))
NA_ROWS
```

```
## [1] 2304
```

```r
## Part B: Devise a strategy for filling in all of the missing values in the dataset. 
## Part C:Create a new dataset that is equal to the original dataset but with the missing data filled in.
ActivityIMPUTE <- ActivityDataImport
ActivityIMPUTE$steps[is.na(ActivityIMPUTE$steps)] <- mean(ActivityDataImport$steps, na.rm = TRUE)

## checking on NA counts
colSums(is.na(ActivityIMPUTE))
```

```
##    steps     date interval 
##        0        0        0
```

```r
## Part D: Make a histogram of the total number of steps taken each day and Calculate and 
## report the mean and median total number of steps taken per day.
ImputeSteps <- tapply(ActivityIMPUTE$steps, ActivityIMPUTE$date, sum)

ImputSteps2 <- melt(ImputeSteps)
names(ImputSteps2) <- c("Date", "SumofSteps")

hist(ImputSteps2$SumofSteps, main = "Histogram of Total Number of Steps per Day on  Impute Value", 
    xlab = "Total Number of Steps per Day", ylab = "Frequency", col = "blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
##  Do these values differ from the estimates from the first part of the assignment? 

## Original Data Set
summary(Final_Data$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00    0.00    0.00   37.38   12.00  806.00
```

```r
## Imputed Data Set
summary(ActivityIMPUTE$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00    0.00    0.00   37.38   37.38  806.00
```

```r
## Imputed Data Set -- comparison if this new data set to the original. 3rd quartile looks larger but 
## others look inline with original data set values.

## What is the impact of imputing missing data on the estimates of the total daily number of steps?
### ANSWER: it increased the number of instances where total number of steps existed between 10,000 and 15,000 total
### number of steps in a day.
```

### Question #4: Are there differences in activity patterns between weekdays and weekends?

```r
WDay_Wend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) 
        return("weekday") else if (day %in% c("Saturday", "Sunday")) 
        return("weekend") else stop("invalid date")
}

ActivityIMPUTE$date <- as.Date(ActivityIMPUTE$date)
ActivityIMPUTE$day <- sapply(ActivityIMPUTE$date, FUN = WDay_Wend)

averages <- aggregate(steps ~ interval + day, data = ActivityIMPUTE, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) + 
    xlab("5-minute interval") + ylab("Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
## ANSWER: It looks like overall the weekend has more intervals of over 100 steps, however, 
## this was using Imputed data and that may or may not reflect actual historical patterns.
## Looks like the average steps by interval is higest within the weekday 800 interval
## which is within the 8:00 AM hour of the day. Could be the steps of people commuting to work.
```

