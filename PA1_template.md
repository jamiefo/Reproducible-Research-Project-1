# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

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
setwd("C:/Users/uszlj8j/Desktop/Learning R/Data Science CourseRA/R_Reproducable_Research")
Activity <- read.csv("activity.csv", header = TRUE)

View(head(Activity))
Activity$steps <- as.numeric(Activity$steps)
Activity$interval <- as.numeric(Activity$interval)
```




## What is mean total number of steps taken per day?

```r
#Calculate the number of Steps per day

Nbr_steps <- select(Activity, date, steps) %>% group_by(date) %>% summarise(Steps_per_day = sum(steps))

hist(Nbr_steps$Steps_per_day)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
#Mean and Standard Deviation of steps per day

Mean_Median_steps <- Nbr_steps %>% summarise(Mean_Steps_per_day = mean(Steps_per_day, na.rm = TRUE), 
                                           Median_Steps_per_day = median(Steps_per_day, na.rm = TRUE))

Mean_Median_steps
```

```
## # A tibble: 1 x 2
##   Mean_Steps_per_day Median_Steps_per_day
##                <dbl>                <dbl>
## 1           10766.19                10765
```



## What is the average daily activity pattern?

```r
data <- select(Activity,interval, steps) %>% group_by(interval) %>% 
                        summarise(Avg_Steps_per_interval = mean(steps, na.rm = TRUE))

plot( x=data$interval, y=data$Avg_Steps_per_interval, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
#Which interval contains max number of steps
# Answer: Interval 835 by looking at the output dataset
```



## Imputing missing values

```r
#Count the number of NAs
Steps_NA <- sum(is.na(Activity$steps))
Steps_NA
```

```
## [1] 2304
```

```r
#Impute Missing Values, use Mean for the Interval to impute missing values
#Create a new dataframe
Activity_Impute <- Activity %>% group_by(interval) %>% mutate(ifelse(is.na(steps),mean(steps, na.rm = TRUE),steps))
colnames(Activity_Impute)[4] = "Steps_Imputed"


#Calculate the number of Steps per day

Nbr_steps_Impute <- select(Activity_Impute, date, Steps_Imputed) %>% group_by(date) %>% summarise(Steps_per_day = sum(Steps_Imputed))
```

```
## Adding missing grouping variables: `interval`
```

```r
hist(Nbr_steps_Impute$Steps_per_day)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
#Calculate Mean and Median Steps per day

Mean_Median_steps_Impute <- ungroup(Activity_Impute) %>% select(Steps_Imputed)  %>% 
                            summarise(Mean_Steps_per_day = mean(Steps_Imputed), 
                                    Median_Steps_per_day = median(Steps_Imputed))

Mean_Median_steps_Impute
```

```
## # A tibble: 1 x 2
##   Mean_Steps_per_day Median_Steps_per_day
##                <dbl>                <dbl>
## 1            37.3826                    0
```



## Are there differences in activity patterns between weekdays and weekends?

```r
#Convert date variable into Date funciton, create a new variable that gives the weekday

Activity_Impute$date <- as.Date(Activity_Impute$date)

Activity_Impute <- Activity_Impute %>% mutate(weekdays = weekdays(date))
Activity_Impute <- Activity_Impute %>% mutate(Week_Type = ifelse((weekdays %in% c("Saturday", "Sunday")),"Week_End", "Week_Day"))

Week_day <- select(Activity_Impute,interval, Steps_Imputed, Week_Type) %>% filter(Week_Type == "Week_Day") %>%
             group_by(interval, Week_Type) %>% summarise(Avg_Steps_per_interval = mean(Steps_Imputed, na.rm = TRUE))

Week_End <- select(Activity_Impute,interval, Steps_Imputed, Week_Type) %>% filter(Week_Type == "Week_End") %>%
             group_by(interval, Week_Type) %>% summarise(Avg_Steps_per_interval = mean(Steps_Imputed, na.rm = TRUE))

 
par(mfrow=c(2,1)) 
plot(x= Week_day$interval, y=Week_day$Avg_Steps_per_interval, type = "l")
plot(x= Week_End$interval, y=Week_End$Avg_Steps_per_interval, type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

