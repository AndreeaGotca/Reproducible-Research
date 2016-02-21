# Reproducible Research: Peer Assessment 1

## if you don't have them already
## you need to install the below packages for the code to run without error
## install.packages("downloader")
## install.packages("dplyr")

## require the packages for this R session
library(downloader)
library(dplyr)

## download the data for the url and unzip it into working directory folder
## set downloaded file as new working directory
path <- getwd()
url <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
download(url,dest=file.path(path,'repdata_data_activity.zip'),mode='wb')
unzip(file.path(path,'repdata_data_activity.zip'))
if(file.exists('repdata_data_activity')){
  setwd(file.path(path,'repdata_data_activity'))}

## read the data from csv file
dat <- read.csv('activity.csv')

# What is mean total number of steps taken per day?

## 1. using dyplr, group by date and sum the total steps per day
steps_per_day <- dat %>% 
                  group_by(date) %>% 
                  summarize(total_steps=sum(steps))

## 2. make a histogram plot of the total number of steps taken each day
hist(steps_per_day$total_steps, breaks=30,
     main='Histogram of Total No. of Steps Taken Each Day',
     xlab='No. of Steps Taken Each Day',
     col='blue')

## 3. calculate mean and median of the total number of steps taken per day
mean(steps_per_day$total_steps,na.rm=T)
median(steps_per_day$total_steps,na.rm=T)

# What is the average daily activity pattern?

## 1. time series plot 
## of the 5-minute interval& the average number of steps taken, 
## averaged across all days
avg_steps_interval <- dat %>%
                      group_by(interval) %>%
                      summarize(steps_interval=mean(steps,na.rm=T))
plot(avg_steps_interval$steps_interval ~ unique(dat$interval),
    type='l',col='red',lwd=3,
    main='Average Steps Taken per Interval \n Across All Days',
    xlab='5-minute Interval',
    ylab='Average No. Steps Taken Across All Days')

## 2. Which 5-minute interval, on average across all the days in the dataset, 
## contains the maximum number of steps?
avg_steps_interval[avg_steps_interval$steps_interval == max(avg_steps_interval$steps_interval),]

# Imputing missing values

## 1. total number of missing values in the dataset 
nrow(dat[is.na(dat$steps),])

## 2 & 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
## Missing values replaced with the mean for that 5-minute interval calculated in a previous point
## and saved in the avg_steps_interval table
new_dat1 <- dat %>%
           merge(avg_steps_interval,by.x='interval',by.y='interval') %>%
           filter(is.na(steps)) %>%
           mutate(steps=steps_interval) %>%
           select(steps,date,interval)
new_dat2 <- dat[!is.na(dat$steps),]
new_dat <- rbind(new_dat1,new_dat2) %>%
          arrange(date)

## 
new_steps_per_day <- new_dat %>% 
                      group_by(date) %>% 
                      summarize(total_steps=sum(steps))
hist(new_steps_per_day$total_steps, breaks=30,
     main='Histogram of Total No. of Steps Taken Each Day',
     xlab='No. of Steps Taken Each Day',
     col='blue')
mean(new_steps_per_day$total_steps)
median(new_steps_per_day$total_steps)

# Are there differences in activity patterns between weekdays and weekends?

## 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend"
new_dat$weekdayT <- ifelse(weekdays(as.POSIXct(new_dat$date)) %in% c("Satuday", "Sunday"), 
                              "weekend", "weekday")
head(new_dat)

## 2. 
avg_steps_interval_weekday <- new_dat %>%
                              filter(weekdayT=='weekday') %>%
                              group_by(interval) %>%
                              summarize(steps_interval=mean(steps))
avg_steps_interval_weekend <- new_dat %>%
                              filter(weekdayT=='weekend') %>%
                              group_by(interval) %>%
                              summarize(steps_interval=mean(steps))
par(mfrow=c(2,1))
plot(avg_steps_interval_weekday$steps_interval ~ unique(new_dat$interval),
     type='l',col='red',lwd=3, 
     main='Weekdays Steps', xlab='Interval', ylab='Avg No. of Steps')
plot(avg_steps_interval_weekend$steps_interval ~ unique(new_dat$interval),
     type='l',col='blue',lwd=3, 
     main='Weekends Steps', xlab='Interval', ylab='Avg No. of Steps')
