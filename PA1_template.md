This R markdown file describes the first *Course Project* of **Reproducible Research** course on **Coursera**. This document also contains the implementation. One can use this document to reproduce the results of the following study.

Introduction
------------

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a [Fitbit](http://www.fitbit.com), [Nike Fuelband](http://www.nike.com/us/en_us/c/nikeplus-fuelband), or [Jawbone Up](https://jawbone.com/up). These type of devices are part of the "quantified self" movement -- a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

Data
----

The data for this assignment can be downloaded from the course web site:

-   Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) \[52K\]

The variables included in this dataset are:

-   **steps**: Number of steps taking in a 5-minute interval (missing values are coded as `NA`)

-   **date**: The date on which the measurement was taken in YYYY-MM-DD format

-   **interval**: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

Assignment
----------

This assignment will be described in multiple parts. You will need to write a report that answers the questions detailed below. Ultimately, you will need to complete the entire assignment in a **single R markdown** document that can be processed by **knitr** and be transformed into an HTML file.

Throughout your report make sure you always include the code that you used to generate the output you present. When writing code chunks in the R markdown document, always use `echo = TRUE` so that someone else will be able to read the code. **This assignment will be evaluated via peer assessment so it is essential that your peer evaluators be able to review the code for your analysis**.

For the plotting aspects of this assignment, feel free to use any plotting system in R (i.e., base, lattice, ggplot2)

Fork/clone the [GitHub repository created for this assignment](http://github.com/rdpeng/RepData_PeerAssessment1). You will submit this assignment by pushing your completed files into your forked repository on GitHub. The assignment submission will consist of the URL to your GitHub repository and the SHA-1 commit ID for your repository state.

NOTE: The GitHub repository also contains the dataset for the assignment so you do not have to download the data separately.

### Loading and preprocessing the data

Show any code that is needed to

1.  Load the data (i.e. `read.csv()`)

``` r
activityData <- read.csv("activity/activity.csv")
head(activityData)
```

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

1.  Process/transform the data (if necessary) into a format suitable for your analysis

``` r
#converting date column strings to Date format as YYYY-MM-DD
activityData$date <- as.Date( activityData$date, "%Y-%M-%d")
#remove NA values
activityDataWithoutNA <- subset(activityData, !is.na(steps))
```

### What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1.  Make a histogram of the total number of steps taken each day

``` r
totalStepsPerDay <- tapply(activityDataWithoutNA$steps, activityDataWithoutNA$date, sum)

plot(as.Date(names(totalStepsPerDay), "%Y-%M-%d"), totalStepsPerDay/1000, type = "h", lwd = 5, xlab = "Day", ylab = "Total steps per day (In Thousands)", main = "Total number of steps taken each day")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-3-1.png)

1.  Calculate and report the **mean** and **median** total number of steps taken per day

``` r
meanTSPD <- round( mean(totalStepsPerDay), digits = 2 )
medianTSPD <- median(totalStepsPerDay)
```

The mean is **1.91446210^{4}** and median is **20670** for total number of steps taken per day.

### What is the average daily activity pattern?

1.  Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

``` r
stepsPerInterval <- tapply(activityDataWithoutNA$steps, activityDataWithoutNA$interval, mean)

plot(stepsPerInterval, type = "l", lwd = 3, col = "Red", main = "Average number of steps per Interval", xlab = "Five minutes interval", ylab = "Average number of steps")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-5-1.png)

1.  Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

``` r
maxStepsInterval <- stepsPerInterval[stepsPerInterval == max(stepsPerInterval)]
```

**835** is the 5-minute interval which contains the maximum number of steps **206.1698113**, on average across all the days.

### Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as `NA`). The presence of missing days may introduce bias into some calculations or summaries of the data.

1.  Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)

``` r
numberOfRowsWithNA <- nrow(activityData) - nrow(activityDataWithoutNA)
```

There are **2304** in the dataset with missing values.

1.  Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Using **Hmisc** package for imputing NA values with w.r.t. mean for that 5-minute interval

``` r
library(Hmisc)
```

    ## Warning: package 'Hmisc' was built under R version 3.4.2

    ## Loading required package: lattice

    ## Loading required package: survival

    ## Loading required package: Formula

    ## Loading required package: ggplot2

    ## Warning: package 'ggplot2' was built under R version 3.4.2

    ## 
    ## Attaching package: 'Hmisc'

    ## The following objects are masked from 'package:base':
    ## 
    ##     format.pval, round.POSIXt, trunc.POSIXt, units

``` r
#create a new vector equal with the size of activityData, which is 61 times greater
stepsData <- rep(stepsPerInterval, times = 61)
#retrieve NA locations
naIndices <- is.na(activityData$steps)
#subset stepsData with NA locations
stepsDataSub <- subset(stepsData, naIndices)
#impute steps w.r.t. mean for its respective 5-minute interval
imputedSteps <- with( activityData, impute(steps, stepsDataSub) )
```

1.  Create a new dataset that is equal to the original dataset but with the missing data filled in.

``` r
imputedActivityData <- activityData
#fill in steps with the imputed data, rounding of mean for its respective interval as steps are not in decimal values
imputedActivityData$steps <- round( imputedSteps, digits = 0 )
```

1.  Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

``` r
totalStepsPerDayImputed <- tapply(imputedActivityData$steps, imputedActivityData$date, sum)

plot(as.Date(names(totalStepsPerDayImputed), "%Y-%M-%d"), totalStepsPerDayImputed/1000, type = "h", lwd = 5, xlab = "Day", ylab = "Total steps per day (In Thousands)", main = "Total number of steps taken each day with imputed data")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-10-1.png)

``` r
meanTSPDImputed <- round( mean(totalStepsPerDay), digits = 2 )
medianTSPDImputed <- median(totalStepsPerDay)
```

After imputing NA values with mean of its respective interval, the mean is **1.91446210^{4}** and median is **20670** for total number of steps taken per day.

Impact of imputing missing data on the estimates of the total daily number of steps is shown by the following plot

``` r
plot( as.Date(names(totalStepsPerDay), "%Y-%M-%d"), totalStepsPerDay/1000, type = "h", lwd = 5, col = "Black", xlab = "Day", ylab = "Total steps per day (In Thousands)", main = "Impact of imputing missing data" )
points( as.Date(names(totalStepsPerDayImputed), "%Y-%M-%d"), totalStepsPerDayImputed/1000, type = "h", col = alpha("Green", 0.5), lwd = 5 )
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-11-1.png)

### Are there differences in activity patterns between weekdays and weekends?

For this part the `weekdays()` function may be of some help here. Use the dataset with the filled-in missing values for this part.

1.  Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

``` r
imputedActivityData$dayOfWeek <- as.factor( sapply( weekdays(imputedActivityData$date), function(x){ifelse(x %in% c("Saturday", "Sunday"), "Weekend", "Weekday")} ) )
```

1.  Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using **simulated data**:

``` r
averageStepsByIntervalWeekday <- aggregate(steps ~ interval + dayOfWeek, data = imputedActivityData, FUN = mean)
#Panel plot with ggplot2 package
#panelPlot <- ggplot( averageStepsByIntervalWeekday, aes( x = interval, y = steps ) )
#panelPlot + geom_line() + facet_grid(dayOfWeek~.) + ggtitle("Average Daily Activity Pattern") + xlab("5-minute Interval") + ylab("Average Number of Steps")
#Panel plot with lattice package
xyplot(steps ~ interval | dayOfWeek, averageStepsByIntervalWeekday, main = "Average Daily Activity Pattern",
       type = "l", lwd = 2, xlab = "5-minute Interval", ylab = "Average Number of Steps", layout = c( 1, 2 ) )
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-13-1.png)
