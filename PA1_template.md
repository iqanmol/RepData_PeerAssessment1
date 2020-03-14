---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data

#### 1.Load the data

```r
unzip("activity.zip")
data<-read.csv("activity.csv")
```

#### 2.Process/transform the data (if necessary) into a format suitable for your analysis

```r
data$date<-as.Date(data$date)
```

## What is mean total number of steps taken per day?

#### 1.Calculate the total number of steps taken per day

```r
total_steps_day<-aggregate(data$steps~data$date,data,sum,na.rm=TRUE)
colnames(total_steps_day)<-c("date","TotalSteps")
head(total_steps_day)
```

```
##         date TotalSteps
## 1 2012-10-02        126
## 2 2012-10-03      11352
## 3 2012-10-04      12116
## 4 2012-10-05      13294
## 5 2012-10-06      15420
## 6 2012-10-07      11015
```

#### 2.Make a histogram of the total number of steps taken each day

```r
hist(total_steps_day$TotalSteps,xlab = "Total Steps per day",main = "Histogram of Total Steps per day",col = "Gold")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

#### 3.Calculate and report the mean and median of the total number of steps taken per day

```r
mean_data<-mean(total_steps_day$TotalSteps)
median_data<-median(total_steps_day$TotalSteps)
print(mean_data)
```

```
## [1] 10766.19
```

```r
print(median_data)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

#### 1.Make a time series plot (i.e.type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
average_steps<-tapply(data$steps,data$interval,mean,na.rm=TRUE)
average_steps<-as.data.frame(average_steps)
colnames(average_steps)<-"AverageSteps"
average_steps$Interval<-as.integer(rownames(average_steps))
par(col="thistle")
plot(x=average_steps$Interval,y=average_steps$AverageSteps,xlab ="Interval",ylab ="Average number of steps taken",main = "Time Series Plot",type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

#### 2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
average_steps[which.max(average_steps$AverageSteps),]$Interval
```

```
## [1] 835
```

## Imputing missing values

#### 1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(data))
```

```
## [1] 2304
```

#### 2.Devise a strategy for filling in all of the missing values in the dataset. 
We will be filling in the missing values for a particular interval by replacing it with the average of that particular interval averaged across all days.

```r
fillmissingvalues<-function(interval)
{
  average_steps[average_steps$Interval==interval,1]
}
```

#### 3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
newdata<-data
nrows<-nrow(data)
for(i in 1:nrows)
{
  if(is.na(newdata[i,"steps"]))
    newdata[i,"steps"]<-fillmissingvalues(newdata[i,"interval"])
}
head(newdata)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

#### 4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
new_total_steps_day<-tapply(newdata$steps,newdata$date,sum,na.rm=TRUE)
new_total_steps_day<-as.data.frame(new_total_steps_day)
colnames(new_total_steps_day)<-"TotalSteps"
new_total_steps_day$date<-rownames(new_total_steps_day)
par(cex=1,cex.lab=0.8,cex.main=0.7)
hist(new_total_steps_day$TotalSteps,xlab = "Total Steps per day",main = "Histogram of Total Steps per day for New dataset",col = "deeppink1")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
newmean<-mean(new_total_steps_day$TotalSteps)
newmedian<-median(new_total_steps_day$TotalSteps)
print(newmean)
```

```
## [1] 10766.19
```

```r
print(newmedian)
```

```
## [1] 10766.19
```
After imputing missing data,the mean has not changed.  
The median has changed by 0.1%.

## Are there differences in activity patterns between weekdays and weekends?

#### 1.Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
f<-character()
days<-c("Monday","Tuesday","Wednesday","Thursday","Friday")
for(i in 1:nrows)
{
  if(weekdays(newdata[i,"date"]) %in% days)
     f[i]<-"weekday"
  else
    f[i]<-"weekend"
}
f<-as.factor(f)
newdata$daytype<-f
head(newdata)
```

```
##       steps       date interval daytype
## 1 1.7169811 2012-10-01        0 weekday
## 2 0.3396226 2012-10-01        5 weekday
## 3 0.1320755 2012-10-01       10 weekday
## 4 0.1509434 2012-10-01       15 weekday
## 5 0.0754717 2012-10-01       20 weekday
## 6 2.0943396 2012-10-01       25 weekday
```

#### 2.Make a panel plot containing a time series plot (i.e.type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
avg_steps_day<-aggregate(newdata$steps~newdata$interval + newdata$daytype,newdata,mean)
colnames(avg_steps_day)<-c("interval","daytype","averagesteps")
library(ggplot2)
result<-ggplot(avg_steps_day,aes(x=interval,y=averagesteps))+ geom_line(color="dodgerblue") + facet_wrap(~daytype,nrow=2)
result
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
