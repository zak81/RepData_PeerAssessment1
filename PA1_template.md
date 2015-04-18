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

```
## Warning in file(file, "rt"): cannot open file 'activity.csv': No such file
## or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

## What is mean total number of steps taken per day?

```r
# calculate the total number of steps taken per day
activity_total <- activity %>%
        group_by(date) %>%
        summarise(ttl_steps = sum(steps, na.rm=TRUE))
```

```
## Error in eval(expr, envir, enclos): object 'activity' not found
```

```r
# make a histogram of the total number of steps taken each day
g <- ggplot(activity_total, aes(x=ttl_steps))
```

```
## Error in ggplot(activity_total, aes(x = ttl_steps)): object 'activity_total' not found
```

```r
g + geom_histogram(binwidth=2000) + 
        xlab("total steps") +
        ggtitle("Total steps taken per day")
```

```
## Error in eval(expr, envir, enclos): object 'g' not found
```

```r
# mean of the total number of steps taken each day
mean(activity_total$ttl_steps)
```

```
## Error in mean(activity_total$ttl_steps): object 'activity_total' not found
```

```r
# median of the total number of steps taken each day
median(activity_total$ttl_steps)
```

```
## Error in median(activity_total$ttl_steps): object 'activity_total' not found
```

## What is the average daily activity pattern?

```r
# make a time series plot of the 5-minute interval and the average number of steps taken
activity_avg <- activity %>%
        group_by(interval) %>%
        summarise(avg_steps = mean(steps, na.rm=TRUE))
```

```
## Error in eval(expr, envir, enclos): object 'activity' not found
```

```r
h <- ggplot(activity_avg, aes(interval, avg_steps))
```

```
## Error in ggplot(activity_avg, aes(interval, avg_steps)): object 'activity_avg' not found
```

```r
h + geom_line() +
        xlab("interval(minutes)") +
        ylab("average number of steps") +
        ggtitle("Number of steps taken in 5-minute intervals, averaged across all days")
```

```
## Error in eval(expr, envir, enclos): object 'h' not found
```

```r
# Which 5-minute interval contains the maximum number of steps?
filter(activity_avg, avg_steps==max(avg_steps))
```

```
## Error in filter_(.data, .dots = lazyeval::lazy_dots(...)): object 'activity_avg' not found
```

## Inmputing missing values

```r
# calculate the total number of missing values in the dataset
sum(is.na(activity$steps))
```

```
## Error in eval(expr, envir, enclos): object 'activity' not found
```

```r
# strategy for filling in missing values: Replace NA's with the mean for that 5-minute interval
new_dataset <- merge(activity, activity_avg, by="interval")
```

```
## Error in merge(activity, activity_avg, by = "interval"): object 'activity' not found
```

```r
new_dataset$steps[is.na(new_dataset$steps)] = new_dataset$avg_steps
```

```
## Error in eval(expr, envir, enclos): object 'new_dataset' not found
```

```r
# create a new dataset with the missing data filled in
new_dataset <- new_dataset %>%
        arrange(date, interval) %>%
        subset(select=interval:date)
```

```
## Error in eval(expr, envir, enclos): object 'new_dataset' not found
```

```r
# make a histogram of the total number of steps taken each day
data_total <- new_dataset %>%
        group_by(date) %>%
        summarise(ttl_steps = sum(steps, na.rm=TRUE))
```

```
## Error in eval(expr, envir, enclos): object 'new_dataset' not found
```

```r
k <- ggplot(data_total, aes(x=ttl_steps))
```

```
## Error in ggplot(data_total, aes(x = ttl_steps)): object 'data_total' not found
```

```r
k + geom_histogram(binwidth=2000) +
        xlab("total steps") +
        ggtitle("Total steps taken per day")
```

```
## Error in eval(expr, envir, enclos): object 'k' not found
```

```r
# mean of the total number of steps taken each day
mean(data_total$ttl_steps)
```

```
## Error in mean(data_total$ttl_steps): object 'data_total' not found
```

```r
# median of the total number of steps taken each day
median(data_total$ttl_steps)
```

```
## Error in median(data_total$ttl_steps): object 'data_total' not found
```

```r
# Comparing two mean values, new dataset is 17.207 greater in total steps per day. Median values do not differ. Histograms show no change because the difference is insignificant. In my experiment, impact of imputing missing values is insignificant.
```

## Are there differences in activity patterns between weekdays and weekends?

```r
# create a new factor variable in the dataset with two levels-"weekday" and "weekend"
new_dataset$date <- as.Date(new_dataset$date)
```

```
## Error in as.Date(new_dataset$date): object 'new_dataset' not found
```

```r
wend <- c('Saturday', 'Sunday')
new_dataset$day <- factor((weekdays(new_dataset$date) %in% wend), levels=c('FALSE', 'TRUE'), labels=c('weekday', 'weekend'))
```

```
## Error in weekdays(new_dataset$date): object 'new_dataset' not found
```

```r
# Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
data_grouped <- new_dataset %>%
        group_by(interval, day) %>%
        summarise(avg_steps=mean(steps))
```

```
## Error in eval(expr, envir, enclos): object 'new_dataset' not found
```

```r
m <- ggplot(data_grouped, aes(interval, avg_steps))
```

```
## Error in ggplot(data_grouped, aes(interval, avg_steps)): object 'data_grouped' not found
```

```r
m + facet_grid(day~.) +
        geom_line() +
        xlab("interval(minutes)") +
        ylab("average number of steps") +
        ggtitle("Number of steps taken in 5-minute invervals, averaged across all weekday days or weekend days") +
        theme(title=element_text(size=8))
```

```
## Error in eval(expr, envir, enclos): object 'm' not found
```
