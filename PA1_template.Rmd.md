# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data


```r
data<-read.csv("C:/Users/zhaoyuxuan/Documents/repdata_data_activity/activity.csv")
```


## What is mean total number of steps taken per day?


```r
total_steps<-rep(0,times=61)
for (i in 1:61){
    total_steps[i]<-sum(data[1:288+(i-1)*288,1],na.rm=TRUE)
}
hist(total_steps,main="total steps taken per day",xlab="")
```

![](PA1_template.Rmd_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
mean<-mean(total_steps)
median<-median(total_steps)
```
The mean of the total steps taken per day is 9354.2295082, and the median is 1.0395\times 10^{4}.

## What is the average daily activity pattern?


```r
average_steps<-rep(0,times=288)
for (i in 1:288){
    average_steps[i]<-mean(data[i+288*0:60,1],na.rm=TRUE)
}
plot(data[1:288,3],average_steps,type="l",main="average daily activity pattern",xlab="5-minute interval", ylab="average number of steps")
```

![](PA1_template.Rmd_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
index<-which.max(average_steps)
```

The index of 5-minute interval which on average accross all the days contains the maximum number of steps is 104.

##Imputing missing values


```r
findna<-which(is.na(data[,1]))
number<-length(findna)
newdata<-data
newdata[findna,1]<-rep(mean,number)
total_steps2<-rep(0,times=61)
for (i in 1:61){
    total_steps2[i]<-sum(newdata[1:288+(i-1)*288,1],na.rm=TRUE)
}
hist(total_steps2,main="total steps taken per day after replacing missing values",xlab="")
```

![](PA1_template.Rmd_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
mean2<-mean(total_steps2)
median2<-median(total_steps2)
```
After filling in all of the missing values in the dataset, the mean of the total steps taken per day is 3.6266808\times 10^{5}, and the median is 1.1458\times 10^{4}. 

As we can see from the results above, if we use the mean of total steps taken per day to replace missing values, the histogram is quite different from the original one. There are some days when the total steps exceed 250000 steps. And the mean and the median both increase.

## Are there differences in activity patterns between weekdays and weekends?


```r
##Copy the data into tdata for processing calculation.
tdata<-data
tdata[,2]<-as.Date(as.character(data[,2]))
week<-weekdays(tdata[,2])
weekend<-c("星期六","星期日")
week<-factor((week %in% weekend),levels=c(TRUE,FALSE),labels=c("weekend","weekday"))
tdata[,2]<-week

##Calculate the average steps for weekdays or weekends.
data_weekday<-data[(tdata[,2] %in% c("weekday")),]
data_weekend<-data[(tdata[,2] %in% c("weekend")),]
aver_day<-sapply(split(data_weekday[,1],data_weekday[,3]),function(x){mean(x,na.rm=TRUE)})
aver_end<-sapply(split(data_weekend[,1],data_weekend[,3]),function(x){mean(x,na.rm=TRUE)})
finaldata<-data.frame(average_steps=c(aver_day,aver_end),date=c(rep("weekday",times=288),rep("weekend",times=288)),interval=c(data[1:576,3]))

library(lattice)
xyplot(average_steps ~ interval|date,data=finaldata,layout=c(1,2),type="l")
```

![](PA1_template.Rmd_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
