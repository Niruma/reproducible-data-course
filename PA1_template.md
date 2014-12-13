---
title: "peer_assessment"
author: "Student"
date: "Friday, November 14, 2014"
output: html_document
---

This is an R Markdown document. The first step is to read the file and create a histogram of the total number of steps taken each day
#These steps show loading and initial processing of data
library(knitr)
setwd("~/R/R_lectures/Data_cousera/reproducible/)
knit2html("PA1_template.Rmd")
browseURL("PA1_template.html")

```{r, echo=TRUE}
activity <- read.csv("~/R/R_lectures/Data_cousera/reproducible/activity.csv")
activity$date <- as.Date(activity[,2], format="%m/%d/%Y")
```
#Ignoring the missing values a histogram of the total number of steps taken each day has been created
```{r, echo=TRUE}
hist(tapply(activity$steps, activity$date, sum), xlab = "Total daily steps", breaks = 20, 
    main = "Total of steps taken per day")
```
#mean and median total number of steps taken per day is calculated
```{r, echo=TRUE}
total_steps <- as.numeric(tapply(activity$steps, activity$date, sum))
step_mean <- mean(total_steps, na.rm = TRUE)
step_median <- median(total_steps, na.rm = TRUE)
step_mean
step_median
```
#Now, a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis) is created here

```{r, echo=TRUE}
library(ggplot2)
average <- aggregate(x=list(steps=activity$steps), by=list(interval=activity$interval), FUN=mean, na.rm=TRUE)
ggplot(data=average, aes(x=interval, y=steps))+geom_line()+xlab("5-min interval")+ ylab("average across all days")
max(average$steps, na.rm=TRUE)
```


#Number of days/intervals where there are missing values (coded as NA). The mean for the 5 min interval is used for filling in the missing values. The sum of the missing rows is calculated here.The dataset activity is split. Omitting the rows containing the dataset, a new dataset(nona_activity) is created.Another dataset with na containing rows is created(na_activity). the na containing dataset is recoded with the interval averages.Both the dataset are merged together as "imputed" dataset

```{r, echo=TRUE}
sum(is.na(activity$steps))
nona_activity<-na.omit(activity)
na_activity<-activity[is.na(activity[]),]
na_activity[,1]<-sapply(na_activity[,3],function(x) round(average[average$interval==x,2]))
imputed<-rbind(nona_activity,na_activity)
imputed_activity<- tapply(imputed$steps,imputed$date,sum)
```

#Mean, median is calculated for the imputed datasets
```{r, echo=TRUE}
step_mean
step_median
mean(imputed_activity)
median(imputed_activity)
qplot(imputed_activity, xlab='Total steps')
```
#After imputation the mean and median of total steps did not change much!

#Created a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

#Made a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
```{r, echo=TRUE}
library(lubridate)
imputed$day<- wday(imputed$date, label=TRUE)
imputed$daytype<- imputed$day
levels(imputed$daytype) <- list(
    weekday = c("Mon", "Tues", "Wed", "Thurs", "Fri"),
    weekend = c("Sun", "Sat"))
library(dplyr)
activity.typeday<- imputed %>%
  group_by(daytype, interval)  %>% 
  summarize(total.steps=sum(steps, na.rm=TRUE), average.steps=mean(steps, na.rm=TRUE))
library(lattice)
xyplot(average.steps~interval|daytype, data=activity.typeday, type='l', layout=(c(1,2)),
   main="Average Daily Activity Pattern by Type of Day",
   ylab="Average Number of Steps Taken per Interval", xlab="5-minute Time Interval")  
```
