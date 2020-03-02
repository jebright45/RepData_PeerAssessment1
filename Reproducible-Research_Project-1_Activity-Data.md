---
title: 'Reproducible Research: Project 1: Activity Data'
author: "Jake Ebright"
date: "2/25/2020"
output: 
  html_document: 
    keep_md: yes
keep_md: true
---



```r
activity <- read.csv("G:/My Drive/Coursera John's Hopkins Data Science Specialization/Course 5_ Reproducible Research/Week 2/activity.csv")

sum(activity$steps, na.rm = TRUE) # 570, 608 steps with NA's removed.
```

```
## [1] 570608
```

```r
mean(activity$steps, na.rm = TRUE) # 37.3826 
```

```
## [1] 37.3826
```

```r
median(activity$steps, na.rm = TRUE) # 0
```

```
## [1] 0
```

```r
# Copying the original data set into a new one so that the date column can be manipulated without impacitng the orginal data set.
activity2 <- activity

# Converting the original "date" column to be recognized by R as a Date columns.
activity2$Date <- as.Date(activity2$date, "%Y-%m-%d")
        
# Creating unique columns for year, month, day
activity2$year <- as.numeric(format(activity2$Date, "%Y")) # Extract year of purchase from Date column to make an entirely new year column. 
activity2$month <- as.numeric(format(activity2$Date, "%m"))
activity2$day <-  as.numeric(format(activity2$Date, "%d"))
```



# Calculations Regarding Total Number of Steps Taken Each Day 

```r
# Total number of steps taken each day (ignoring missing values)
totalstepsperday <- aggregate(activity2$steps, by = list(activity2$day), FUN = sum, na.rm = TRUE)

names(totalstepsperday) <- c("Day", "TotalSteps")

# Histogram like plot (no spaces) of total steps per day

barplot(height = totalstepsperday$TotalSteps, names.arg = totalstepsperday$Day, space = 0, col = "lightblue", main = "Total Number of Steps per Day", ylab = "Number of Steps", xlab = "Day",
        ylim = c(0, 35000), axes = TRUE, cex.names = 0.7)
```

![](figs/fig-unnamed-chunk-2-1.png)<!-- -->

```r
# Calculating mean and median of thte total number of steps taken per day.
mean(totalstepsperday$TotalSteps) # 18,406.71
```

```
## [1] 18406.71
```

```r
median(totalstepsperday$TotalSteps) # 20,525
```

```
## [1] 20525
```



# Calculations Regarding Daily Activity Patterns

```r
# Plotting Total Steps per Day Before Mean Replacement using ggplot2 plotting system.
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

totalsteps_perday_premeanreplace_plot <- totalstepsperday %>% ggplot(aes(x = Day, y = TotalSteps)) + geom_line()
totalsteps_perday_premeanreplace_plot <- totalsteps_perday_premeanreplace_plot + xlab(label = "Day") + ylab(label = "Total Steps") + ggtitle(label = "Total Steps by Day Before Mean Replacement") + theme(plot.title = element_text(hjust = 0.5))
totalsteps_perday_premeanreplace_plot
```

![](figs/fig-unnamed-chunk-3-1.png)<!-- -->

```r
# Arranging totalstepsbyinterval by largest to smallest to see which is the greatest to answer their question.
# The 104th interval (8 hrs and 35 minutes) has the highest average number of steps at about 206.
library(dplyr)
totalstepsbyinterval <- aggregate(activity2$steps, by = list(activity2$interval), FUN = mean, na.rm = TRUE)
head(totalstepsbyinterval %>% arrange(desc(x)))
```

```
##   Group.1        x
## 1     835 206.1698
## 2     840 195.9245
## 3     850 183.3962
## 4     845 179.5660
## 5     830 177.3019
## 6     820 171.1509
```

```r
aggregate(activity2$steps, by = list(activity2$day), FUN = mean, na.rm = FALSE)
```

```
##    Group.1        x
## 1        1       NA
## 2        2 18.62153
## 3        3 38.06076
## 4        4       NA
## 5        5 41.20312
## 6        6 41.23958
## 7        7 41.48958
## 8        8       NA
## 9        9       NA
## 10      10       NA
## 11      11 39.77778
## 12      12 48.86632
## 13      13 34.30903
## 14      14       NA
## 15      15 17.67361
## 16      16 35.63368
## 17      17 48.24826
## 18      18 43.69097
## 19      19 35.88542
## 20      20 25.81076
## 21      21 37.51389
## 22      22 58.83160
## 23      23 52.27778
## 24      24 39.64062
## 25      25 24.87153
## 26      26 31.14583
## 27      27 41.25868
## 28      28 37.57118
## 29      29 20.94618
## 30      30       NA
## 31      31 53.52083
```



# Imputing Missing Values

```r
# What the data set looks like before mean replacement.
# How many NA's vs Completes there are for the steps column.
head(activity2)
```

```
##   steps       date interval       Date year month day
## 1    NA 2012-10-01        0 2012-10-01 2012    10   1
## 2    NA 2012-10-01        5 2012-10-01 2012    10   1
## 3    NA 2012-10-01       10 2012-10-01 2012    10   1
## 4    NA 2012-10-01       15 2012-10-01 2012    10   1
## 5    NA 2012-10-01       20 2012-10-01 2012    10   1
## 6    NA 2012-10-01       25 2012-10-01 2012    10   1
```

```r
# Calculate number of rows with NAs. 2,304
sum(!complete.cases(activity2$steps))
```

```
## [1] 2304
```

```r
# Calculating the number of rows with complete values. 15,264
sum(complete.cases(activity2$steps))
```

```
## [1] 15264
```

```r
# Alternative way to count the number of NAs. 2,304
sum(is.na(activity2$steps))
```

```
## [1] 2304
```

```r
# Mean replacement of NA values for steps by average total steps per interval. I got this iteratively (ie. pulling stuff from stack overflow and fiddling around with it until I got it to work).
# Still not totally 100% sure how it works/how each part in it is working specifically.
for(i in 1:ncol(activity2)){
        activity2[is.na(activity2[ , 1]), i] <- totalstepsbyinterval[ , 2]
}

# What the data set looks like after mean replacement
# How many NA's vs Completes there are for the steps column.
head(activity2)
```

```
##       steps       date interval       Date year month day
## 1 1.7169811 2012-10-01        0 2012-10-01 2012    10   1
## 2 0.3396226 2012-10-01        5 2012-10-01 2012    10   1
## 3 0.1320755 2012-10-01       10 2012-10-01 2012    10   1
## 4 0.1509434 2012-10-01       15 2012-10-01 2012    10   1
## 5 0.0754717 2012-10-01       20 2012-10-01 2012    10   1
## 6 2.0943396 2012-10-01       25 2012-10-01 2012    10   1
```

```r
sum(!complete.cases(activity2$steps)) # None
```

```
## [1] 0
```

```r
sum(complete.cases(activity2$steps)) # Our full data set, 17,568.
```

```
## [1] 17568
```

```r
# Comparing the mean and median of total steps per day of the orginal vs. mean replacement data sets. The mean and median for the data set with NA's mean replaced by interval are both higher.
totalstepsperday_meanreplaced <- aggregate(activity2$steps, by = list(activity2$day), FUN = sum, na.rm = TRUE)
names(totalstepsperday_meanreplaced) <- c("Day", "TotalSteps")

mean(totalstepsperday_meanreplaced$TotalSteps) # 21,185.08
```

```
## [1] 21185.08
```

```r
median(totalstepsperday_meanreplaced$TotalSteps) # 21,641
```

```
## [1] 21641
```

```r
# Plotting Total Steps per Day After Mean Replacement using ggplot2 plotting system.

totalsteps_perday_plot <- totalstepsperday_meanreplaced %>% ggplot(aes(x = Day, y = TotalSteps)) + geom_line()
totalsteps_perday_plot <- totalsteps_perday_plot + xlab(label = "Day") + ylab(label = "Total Steps") + ggtitle(label = "Total Steps by Day After Mean Replacement") + theme(plot.title = element_text(hjust = 0.5))
totalsteps_perday_plot
```

![](figs/fig-unnamed-chunk-4-1.png)<!-- -->



# Computing Potential Differences in Weekday vs Weekend

```r
# Creating a new column that says what day of the week the date in the Date column was.
activity2$weekday <- weekdays(activity2$Date)

# Creating a new column to say whether the day is a weekday or weekend. This method using %in% is like what I tried to do with some of the Charter Spectrum Commnunity Solutions Cluster
# Analysis but there I used the | or "pipe" separator because doing " == c(..., ..., ...)" wasn't working right. Instead of all that, I can just use the %in% like I did here.
activity2$daytype <- ifelse(test = activity2$weekday %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), yes = "weekday", no = "weekend")

# Before this next line, str(activity2) would report that the daytype column was a character column. Now, it returns as a factor column with 2 levels. Cha-ching.
activity2$daytype <- as.factor(as.character(activity2$daytype))


library(ggplot2)
library(dplyr)

# Plotting average step by interval for each day type. The aggregate() funciton is a dream. Let's you put mnultiple variables to condition on in the list() section.
totalstepsbyinterval_bydaytype <- aggregate(activity2$steps, by = list(activity2$interval, activity2$daytype), FUN = mean, na.rm = TRUE)
names(totalstepsbyinterval_bydaytype) <- c("interval", "daytype", "steps")

steps_byinterval_byday_plot <- totalstepsbyinterval_bydaytype %>% ggplot(aes(x = interval, y = steps,  group = daytype, color = daytype)) + geom_line()

# That theme() script centers the title because ggplot2 automatically puts the title on the left because apparently that location is better if you have subtitles.
steps_byinterval_byday_plot <- steps_byinterval_byday_plot + ggtitle(label = "Weekday vs. Weekend: Average Steps by Interval") + theme(plot.title = element_text(hjust = 0.5))

# Average Steps by Interval on Different Plots
# Not 100% sure what the "period" does but I know it works. I know in regression having the period includes all remaning variables as indpendent variables, but not exactly sure what it does here.
# Anyway, I got help for how to do the panel plots in ggplot 2 from the following website: http://www.sthda.com/english/wiki/ggplot2-facet-split-a-plot-into-a-matrix-of-panels
steps_byinterval_byday_plot <- steps_byinterval_byday_plot + facet_grid(daytype ~ .)
steps_byinterval_byday_plot
```

![](figs/fig-unnamed-chunk-5-1.png)<!-- -->


