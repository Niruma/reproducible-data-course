Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

Data

The data for this assignment can be downloaded from the course web site:

Dataset: Activity monitoring data [52K]
The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

date: The date on which the measurement was taken in YYYY-MM-DD format

interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

Assignment

This assignment will be described in multiple parts. You will need to write a report that answers the questions detailed below. Ultimately, you will need to complete the entire assignment in a single R markdown document that can be processed by knitr and be transformed into an HTML file.

Throughout your report make sure you always include the code that you used to generate the output you present. When writing code chunks in the R markdown document, always use echo = TRUE so that someone else will be able to read the code. This assignment will be evaluated via peer assessment so it is essential that your peer evaluators be able to review the code for your analysis.

For the plotting aspects of this assignment, feel free to use any plotting system in R (i.e., base, lattice, ggplot2)

Fork/clone the GitHub repository created for this assignment. You will submit this assignment by pushing your completed files into your forked repository on GitHub. The assignment submission will consist of the URL to your GitHub repository and the SHA-1 commit ID for your repository state.

NOTE: The GitHub repository also contains the dataset for the assignment so you do not have to download the data separately.
Readme file for the data and the answers:

Loading and preprocessing the data

Show any code that is needed to

Load the data (i.e. read.csv())

Process/transform the data (if necessary) into a format suitable for your analysis

What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

Make a histogram of the total number of steps taken each day

Calculate and report the mean and median total number of steps taken per day

What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Create a new dataset that is equal to the original dataset but with the missing data filled in.

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.This is an R Markdown document. The first step is to read the file and create a histogram of the total number of steps taken each day
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