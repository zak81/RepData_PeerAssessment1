# Reproducible Research Assignment 1

## Loading and preprocessing the data

```r
# load dplyr package
suppressPackageStartupMessages(library(dplyr))
library(ggplot2)

# load the data
if(!exists('activity.csv'))
        activity <- read.csv('activity.csv')
```

## What is mean total number of steps taken per day?

```r
# calculate the total number of steps taken per day
activity_total <- activity %>%
        group_by(date) %>%
        summarise(ttl_steps = sum(steps, na.rm=TRUE))

# make a histogram of the total number of steps taken each day
g <- ggplot(activity_total, aes(x=ttl_steps))
g + geom_histogram(binwidth=2000) + 
        xlab("total steps") +
        ggtitle("Total steps taken per day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
# mean of the total number of steps taken each day
mean(activity_total$ttl_steps)
```

```
## [1] 9354.23
```

```r
# median of the total number of steps taken each day
median(activity_total$ttl_steps)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

```r
# make a time series plot of the 5-minute interval and the average number of steps taken
activity_avg <- activity %>%
        group_by(interval) %>%
        summarise(avg_steps = mean(steps, na.rm=TRUE))
h <- ggplot(activity_avg, aes(interval, avg_steps))
h + geom_line() +
        xlab("interval(minutes)") +
        ylab("average number of steps") +
        ggtitle("Number of steps taken in 5-minute intervals, averaged across all days")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
# Which 5-minute interval contains the maximum number of steps?
filter(activity_avg, avg_steps==max(avg_steps))
```

```
## Source: local data frame [1 x 2]
## 
##   interval avg_steps
## 1      835  206.1698
```

## Inmputing missing values

```r
# calculate the total number of missing values in the dataset
sum(is.na(activity$steps))
```

```
## [1] 2304
```

```r
# strategy for filling in missing values: Replace NA's with the mean for that 5-minute interval
new_dataset <- merge(activity, activity_avg, by="interval")
new_dataset$steps[is.na(new_dataset$steps)] = new_dataset$avg_steps
```

```
## Warning in new_dataset$steps[is.na(new_dataset$steps)] =
## new_dataset$avg_steps: number of items to replace is not a multiple of
## replacement length
```

```r
# create a new dataset with the missing data filled in
new_dataset <- new_dataset %>%
        arrange(date, interval) %>%
        subset(select=interval:date)

# make a histogram of the total number of steps taken each day
data_total <- new_dataset %>%
        group_by(date) %>%
        summarise(ttl_steps = sum(steps, na.rm=TRUE))

k <- ggplot(data_total, aes(x=ttl_steps))
k + geom_histogram(binwidth=2000) +
        xlab("total steps") +
        ggtitle("Total steps taken per day")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
# mean of the total number of steps taken each day
mean(data_total$ttl_steps)
```

```
## [1] 9371.437
```

```r
# median of the total number of steps taken each day
median(data_total$ttl_steps)
```

```
## [1] 10395
```

```r
# Comparing two mean values, new dataset is 17.207 greater in total steps per day. Median values do not differ. Histograms show no change because the difference is insignificant. In my experiment, impact of imputing missing values is insignificant.
```

## Are there differences in activity patterns between weekdays and weekends?

```r
# create a new factor variable in the dataset with two levels-"weekday" and "weekend"
new_dataset$date <- as.Date(new_dataset$date)
wend <- c('Saturday', 'Sunday')
new_dataset$day <- factor((weekdays(new_dataset$date) %in% wend), levels=c('FALSE', 'TRUE'), labels=c('weekday', 'weekend'))

# Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
data_grouped <- new_dataset %>%
        group_by(interval, day) %>%
        summarise(avg_steps=mean(steps))

m <- ggplot(data_grouped, aes(interval, avg_steps))
m + facet_grid(day~.) +
        geom_line() +
        xlab("interval(minutes)") +
        ylab("average number of steps") +
        ggtitle("Number of steps taken in 5-minute invervals, averaged across all weekday days or weekend days") +
        theme(title=element_text(size=8))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 
