# Reproducible Research: Peer Assessment 1


## Loading data and transform the date in the data to Date type

```r
data <- read.csv("activity.csv")
data$date <- as.factor(data$date)
data$interval <- as.factor(data$interval)
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
```

As we can see, the data contain 3 varibles which are numeric variables "steps", date variable "date" (converted to factors) and numerical variable "interval" (converted to factors).  


## What is mean total number of steps taken per day?

```r
library("ggplot2")
stepsDaySum <- data.frame(stepSums = rep(0, length(levels(data$date))), day = "2000-1-1", 
    stringsAsFactors = FALSE)
for (i in 1:length(levels(data$date))) {
    stepsDaySum$stepSums[i] <- sum(subset(data$steps, data$date == levels(data$date)[i]), 
        na.rm = TRUE)
    stepsDaySum$day[i] <- levels(data$date)[i]
}
qplot(stepSums, data = stepsDaySum)
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
meanStepsSumDay <- mean(stepsDaySum$stepSums, na.rm = TRUE)
medianStepsSumDay <- median(stepsDaySum$stepSums, na.rm = TRUE)
```

The mean of the steps in every day is about 

```r
meanStepsSumDay
```

```
## [1] 9354
```

The median of the steps in every day is about 

```r
medianStepsSumDay
```

```
## [1] 10395
```



## What is the average daily activity pattern?

```r
stepsIntervalAverage <- data.frame(stepAverage = rep(0, length(levels(data$interval))), 
    interval = "0", stringsAsFactors = FALSE)
for (i in 1:length(levels(data$interval))) {
    stepsIntervalAverage$stepAverage[i] <- mean(subset(data$steps, data$interval == 
        levels(data$interval)[i]), na.rm = TRUE)
    stepsIntervalAverage$interval[i] <- levels(data$interval)[i]
}
qplot(interval, stepAverage, data = stepsIntervalAverage)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

```r

maximInterval <- stepsIntervalAverage$interval[which.max(stepsIntervalAverage$stepAverage)]
```


The interval containing the maxim average of steps in a day is (0-5 minutes is interval 0) : 

```r
maximInterval
```

```
## [1] "835"
```



## Imputing missing values

```r
naNumbers <- nrow(subset(data, is.na(data$steps) == TRUE))
```



The total number of missing values in the dataset is:

```r
naNumbers
```

```
## [1] 2304
```






## Are there differences in activity patterns between weekdays and weekends?
