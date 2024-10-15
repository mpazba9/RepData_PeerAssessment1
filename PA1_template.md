---
title: "Reproducible Research: Peer Assessment 1"
author: Miriam Paz
date: 15-10-2024
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

I loaded the activity dataframe using the "read.csv()" function. Because this function belongs to the "readr" package, installation of the package is required before loading the data table. Importantly, this script requires that the work directory is set to the directory where the activity.zip file is located.

After loading the "readr" package and I used the "read.csv()" function to load the activity dataset ("activity.zip"). Then, I viewed the information and variables with "summary()" and "spec()" functions. 


```r
setwd("C:/Users/glori/Downloads") #If you need to change the directory, type the new one in this line
library(readr)
activity <- read_csv("activity.zip") 
```

```
## Rows: 17568 Columns: 3
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## dbl  (2): steps, interval
## date (1): date
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
spec(activity)
```

```
## cols(
##   steps = col_double(),
##   date = col_date(format = ""),
##   interval = col_double()
## )
```

```r
summary(activity)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

The dataset contains information about 3 variables: steps, date and interval.

### Histogram showing the total number of steps taken each day
First I calculated the total steps without missing values (NA), and I used the package "tidyverse", which has functions that allows to group activity per day and calculates the total amount of steps. 


```r
sum(activity$steps,na.rm=TRUE)
```

```
## [1] 570608
```

```r
library(tidyverse)
```

```
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.1.1     ✔ purrr     1.0.1
## ✔ forcats   1.0.0     ✔ stringr   1.5.0
## ✔ ggplot2   3.4.1     ✔ tibble    3.2.1
## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

```r
steps_perday <- activity %>%
  group_by(date) %>% 
  summarize(total_steps = sum(steps, na.rm = TRUE)) 

#Then I made a histogram using "hist()" showing the mean steps per day:

hist(steps_perday$total_steps,
     main="Steps each day", 
     col= "darkmagenta", 
     breaks = 10
)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

## What is mean total number of steps taken per day?

To calculate the mean and median number of steps taken each day, I used the functions "mean()" and "median()" from the "steps_perday" data. 


```r
mean(steps_perday$total_steps)
```

```
## [1] 9354.23
```

```r
median(steps_perday$total_steps)
```

```
## [1] 10395
```
The calculated mean is 9354.23 and the median is 10395. 


## What is the average daily activity pattern?
First I created another dataframe on the mean steps per 5-minute interval called "steps_perinterval" and made a time series plot with this data by using the "plot()" function. 


```r
steps_perinterval <- activity %>% 
  group_by(interval) %>% 
  summarize(mean_steps = mean(steps, na.rm = TRUE))

plot(steps_perinterval, 
     xlab="5-min interval",
     ylab= "Number of steps", 
     main= "Time series of the average number of steps taken each day",
     type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

With this, a time series of the average number of steps taken each day is shown.  


To calculate the 5-minute interval that contains the maximum number of steps, first I used the "max()". 


```r
max(steps_perinterval$mean_steps)
```

```
## [1] 206.1698
```

```r
steps_perinterval$interval[which.max(steps_perinterval$mean_steps)]
```

```
## [1] 835
```
The interval with the maximum number of steps is 835, with around 206 steps. 

## Imputing missing values
First, I used the function "sum()" and "is.na()" to identify how many NA are there in our dataframe. 

```r
sum(is.na(activity))
```

```
## [1] 2304
```
The total number of missing values (NA) in the dataset is 2304. 

I used dplyr functions "left_join()", "mutate()" and "select()" to fill-in the missing values (NA) in the dataset with the average of steps per interval. 


```r
activity_imputedValues <- activity %>%
  left_join(steps_perinterval, by = "interval") %>%
  mutate(steps = case_when(is.na(steps) ~ mean_steps, 
                           TRUE ~ steps)) %>%
  select(-mean_steps)

activity_imputedValues_stepsperday <- activity_imputedValues %>% 
  group_by(date) %>% 
  summarize(total_steps = sum(steps, na.rm = TRUE)) 
```
The new dataset created is called "activity_imputedValues_stepsperday", which I used to make the histogram of the total number of steps taken each day after missing values are imputed. 


```r
hist(activity_imputedValues_stepsperday$total_steps,
     main="Steps each day", 
     col= "blue", 
     breaks = 10
)
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
mean(activity_imputedValues_stepsperday$total_steps)
```

```
## [1] 10766.19
```

```r
median(activity_imputedValues_stepsperday$total_steps)
```

```
## [1] 10766.19
```

### Calculate and report the mean and median total number of steps taken per day 
The mean and median values are 10766.19, and don't differ much from the previously calculated mean and median. 
The impact is mostly on the frequency values between both datasets (with or without missing data), which can be seen in the new histogram. 



## Are there differences in activity patterns between weekdays and weekends?

For that, I used the dataset with filled-in values "activity_imputedValues", and I used dplyr functions "mutate" to create a column that would write weekend if it's "Saturday" and "Sunday" ("sábado" and "domingo"), or else call it weekday. 


```r
activity_imputedValues_weekends_weekdays <- activity_imputedValues %>%
  mutate(is_weekend = case_when(weekdays(date) %in% c("sábado", "domingo") 
                                ~ "weekend",
                           TRUE ~ "weekday")) 
```

Then, I created a new factor variable with the 2 levels created above, "weekend" and "weekday". 


```r
activity_imputedValues_weekends_weekdays$is_weekend <- factor(activity_imputedValues_weekends_weekdays$is_weekend)
```

Later, I created 2 dataframes, one for the data on weekends "activity_imputedValues_weekendsonly" and the other for weekdays "activity_imputedValues_weekdays", by using the "filter()" function.


```r
activity_imputedValues_weekendsonly <- activity_imputedValues_weekends_weekdays %>% 
  filter(is_weekend == "weekend")

activity_imputedValues_weekdays <- activity_imputedValues_weekends_weekdays %>% 
  filter(is_weekend == "weekday")
```

And I created two dataframes with the average number of steps per weekday and weekend days separately, compared to the 5-minute interval. 


```r
steps_perinterval_weekends <- activity_imputedValues_weekendsonly %>%
  group_by(interval) %>%
  summarize(mean_steps = mean(steps, na.rm = TRUE))

steps_perinterval_weekdays <- activity_imputedValues_weekdays %>%
  group_by(interval) %>%
  summarize(mean_steps = mean(steps, na.rm = TRUE))
```


Here I used the function "par()" to create a multi-panel figure of 2x1 with the time-series of both dataframes, to compare the average steps per weekend versus weekdays. 


```r
par(mfrow = c(2,1), mar = c(4.1, 4.1, 1.5, 2.1)) 
plot(steps_perinterval_weekends, 
     xlab="5-min interval",
     ylab= "Number of steps", 
     main= "Average number of steps taken each day on weekend",
     type = "l")
plot(steps_perinterval_weekdays, 
     xlab="5-min interval",
     ylab= "Number of steps", 
     main= "Average number of steps taken each day on weekday",
     type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

  
