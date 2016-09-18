# 𝙿𝙰𝟷_𝚝𝚎𝚖𝚙𝚕𝚊𝚝𝚎
Ahmed Mohy  
September 18, 2016  


Load the Data




```r
activity =  read.csv("/Users/AMOHY/activity.csv")
```

Calculate the total number of steps per day


```r
library(dplyr)
```

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

```r
library(ggplot2)
Summary <- activity %>% group_by(date)  %>% summarise(steps = sum(steps,na.rm=TRUE))
qplot(date,steps,data=activity,geom ="line")
```

![](PA1_template_files/figure-html/total number-1.png)<!-- -->

Calculate and report the mean and median of the total number of steps taken per day


```r
Mean = mean(activity$steps,na.rm=TRUE)
Median = median(activity$steps,na.rm=TRUE)
```

Which 5-minute interval contains the maximum average number of steps


```r
Summary1 <- activity %>% group_by(interval)  %>% summarise(steps = mean(steps,na.rm=TRUE))
plot(Summary1$interval,Summary1$steps,type="l")
```

![](PA1_template_files/figure-html/Time series-1.png)<!-- -->

```r
max_interval = Summary1[which(Summary1$steps==max(Summary1$steps)),1]
```

report the total number of missing values in the dataset


```r
missing_values = length(which(is.na(activity$steps)==1))
```

strategy for filling in all of the missing values in the dataset


```r
new_activity <- inner_join(activity,Summary1, by = "interval")
new_activity$steps.x[which(is.na(new_activity$steps.x)==1)]=new_activity$steps.y[which(is.na(new_activity$steps.x)==1)]
new_activity=new_activity[,1:3]
names(new_activity)=c("steps","date","interval")
```

Calculate the total number of steps taken per day after imputing missing values


```r
Summary3<- new_activity %>% group_by(date)  %>% summarise(steps = sum(steps))
qplot(date,steps,data=new_activity,geom ="line")
```

![](PA1_template_files/figure-html/total number 2-1.png)<!-- -->

Calculate and report the mean and median of the total number of steps taken per day after imputing missing values


```r
Mean_1 = mean(new_activity$steps)
Median_1 = median(new_activity$steps)
```

differences in activity patterns between weekdays and weekends


```r
new_activity <- cbind(new_activity,weekdays(as.Date(new_activity$date)))
Weekday <- replicate(length(new_activity$steps),"weekday")
new_activity <- cbind(new_activity,Weekday)
DayType <- factor(c("weekday","weekend"))
names(new_activity)<-c("steps","date","interval","Day","DayType")
new_activity$Day <- as.character(new_activity$Day)
new_activity$DayType <- as.character(new_activity$DayType)
new_activity$DayType[which(new_activity$Day=="Saturday" | new_activity$Day=="Sunday")]  ="weekend"
Summary4<- new_activity %>% group_by(interval,DayType)  %>% summarise(steps = mean(steps))
qplot(interval,steps,data=Summary4,facets=DayType~.,geom=c("line"))
```

![](PA1_template_files/figure-html/weekday Vs. weekend-1.png)<!-- -->
