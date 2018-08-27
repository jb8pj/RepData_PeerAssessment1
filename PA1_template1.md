---
title: "RR Peer Assignment 1"
author: "jb8pj"
date: "18 August 2018"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)
library(knitr)
```

## Reproducible Research - Course Project #1

##Loading and preprocessing the data
Show any code that is needed to

1.Load the data (i.e. 
2.Process/transform the data (if necessary) into a format suitable for your analysis

##Load Data

```{r }
unzip("F_activity.zip")
Adata<- read.csv("activity.csv")
```

##Processing/transforming
```{r}
Adata$date <- as.Date(as.character(Adata$date))
 Adata_NA<- is.na(Adata$steps)
 Not_NA<- Adata[!Adata_NA,]
``` 
 

##What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

##Calculate the total number of steps taken per day

```{r}
Sumbyday<- aggregate(Adata$steps, by=list(Adata$date), sum)
```

## Make a histogram of the total number of steps taken each day

```{r}
ggplot(Sumbyday, aes(x=total_steps))+ geom_histogram(fill="turquoise", binwidth = 1000)+labs(title="Total Steps by Day", x= "Steps", y="Frequency")
```

##Calculate and report the mean and median of the total number of steps taken per day
```{r}
mean(Sumbyday$total_steps, na.rm=TRUE)
median(Sumbyday$total_steps, na.rm=TRUE)
```

## What is the average daily activ ity pattern?
## Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r}
MeanbyInterval<- aggregate(Not_NA$steps, by=list(Not_NA$interval), mean)
names(MeanbyInterval)[1]="Interval" 
names(MeanbyInterval)[2]="Steps"
ggplot(MeanbyInterval, aes(x = Interval, y = Steps))+labs(title="Steps by Interval (Sum)" , x = "Interval", y= "Steps")+geom_line(color="firebrick")
```

## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
MaxInterval<- MeanInterval[which.max(MeanbyInterval$steps),]
MaxInterval
```

## Imputing missing values 

## Calculate and report the total number of missing values in the dataset

```{r}
Missing_values<- sum(Adata_NA)
Missing_values
```

##Devise a strategy for filling in all of the missing values in the dataset.
We can use the mean interval steps 

## Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
Adata2<- Adata
Adata_NA2<-Adata2[is.na(Adata2$steps),]
Not_NA2<- Adata2[!is.na(Adata2$steps),]
MeanbyInterval2<- aggregate(Not_NA2$steps, by=list(Not_NA2$interval), sum)
names(MeanbyInterval2)[1]= "Interval"
names(MeanbyInterval2)[2]= "Steps"
missingData<- is.na(Adata2$steps)
mean_values<- tapply(Not_NA$steps, Not_NA$interval, mean, na.rm = TRUE, simplify=TRUE)
Adata2$steps[missingData]<- mean_values[as.character(Adata2$interval[missingData])]
sum(missingData)
sum(is.na(Adata2$steps))

```

## Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? 

```{r}
ByDay<- aggregate(Adata2$steps, by=list(Adata2$date), sum)
names(ByDay)[1]="Date"
names(ByDay)[2]="total_steps"
ggplot(ByDay, aes(x=total_steps))+geom_histogram(fill="turquoise", binwidth = 1000)+labs(title="Daily Steps", x= "Steps", y="Frequency")
mean(ByDay$total_steps)
median(ByDay$total_steps)
```

The mean is the same. The median is off by 1.19. 

##Are there differences in activity patterns between weekdays and weekends?

## Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day

```{r}
Adata2$weekday<-weekdays(Adata2$date)
Adata2$weekend<- ifelse(Adata2$weekday=="Saturday" | Adata2$weekday == "Sunday", "Weekend", "Weekday")
Adata2$weekend<- as.factor(Adata2$weekend)
head(Adata2, 8)
```

## Make a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days. 

```{r}
MeanDataWeek <- aggregate(Adata2$steps, by=list(Adata2$weekend, Adata2$interval), mean)
names(MeanDataWeek)[1]="Weekend"
names(MeanDataWeek)[2]="Interval"
names(MeanDataWeek)[3]="Steps"
ggplot(MeanDataWeek, aes(x = Interval, y = Steps, color=Weekend))+geom_line()+facet_grid(Weekend~.)+labs(title="Mean/ Steps by Interval", x = "Interval", y = "Steps")
```

