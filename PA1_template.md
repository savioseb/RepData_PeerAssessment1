---
title: "PA1_template.Rmd"
author: "Savio Sebastian"
date: "February 15, 2015"
output: html_document
---

## Basic Settings
Setting up the basic settings for this R Markdown

```{r cache=TRUE}
echo = TRUE  # Always make code visible
options(scipen = 1)  # Turn off scientific notations for numbers
library(ggplot2)
```

<br>
<br>
<br>

## Loading and preprocessing the data
Load the data into R using **<code>read.csv</code>** function and display the structure (*using <code>str</code> function*) to understand the data before further analysis.


```{r loading  ,cache=TRUE}

unzip("repdata-data-activity.zip")
activityData <- read.csv( "activity.csv")
str(activityData)

```

Structure reveals that:
<ul>
    <li> the data is ingested into R as a <code>data.frame</code>
    <li> **<code>steps</code>** is of type <code>integer</code> with NA values
    <li> **<code>date</code>** is ingested as a <code>Factor</code>. There are 61 days.
    <li> **<code>interval</code>** is ingested as an <code>integer</code> - needs more analysis to determine how to codify it into time
</ul>

<br>
### Transformations

```{r transformData,  cache=TRUE}
activityData$date <- as.Date( activityData$date )
activityData$interval <- as.factor( activityData$interval )
activityData$month <- as.numeric( format(activityData$date, "%m"))  ## creating a special month value to handle the data

```

Top 20 rows of **<code>activityData</code>**:

```{r top20 ,  cache=TRUE}
head( activityData, 20)
```

<br>
### Removing the NA Values
```{r,  cache=TRUE}
noNA <- na.omit(activityData)
rownames(noNA) <- 1:nrow(noNA)
```


<br>
<br>
<br>




## What is Mean Total Number of Steps Per Day?
To answer this question, make a histogram of the total number of steps taken each day.

```{r charting,  cache=TRUE}

ggplot(noNA, aes(date, steps)) + 
    geom_bar(stat = "identity", colour = "steelblue", fill = "steelblue", width = 0.7) + 
    facet_grid(. ~ month, scales = "free") + 
    labs(
        title = "Histogram of Total Number of Steps Taken Per Day", 
        x = "Date", 
        y = "Total Number of Steps"
        )

```
<br>

### The Mean & Median Total Number of Steps Per Day

```{r meanMedian , cache=TRUE}
totalSteps <- aggregate(noNA$steps, list(Date = noNA$date), FUN = "sum")$x
```

Mean: **<code>`r as.integer( mean(totalSteps) )`</code>**

Median: **<code>`r median(totalSteps)`</code>**

<br>
<br>
<br>

## What is the Average Daily Activity Pattern?
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r aggregate, cache=TRUE}
avgSteps <- aggregate( 
    noNA$steps, 
    list(interval = as.numeric(as.character( noNA$interval ) ) ), 
    FUN = "mean" )

names(avgSteps)[2] <- "meanOfSteps"

ggplot(avgSteps, aes(interval, meanOfSteps)) + 
    geom_line(color = "steelblue", size = 0.8) + 
    labs(
        title = "Time Series Plot of the 5-minute Interval", 
        x = "5-minute intervals", 
        y = "Average Number of Steps Taken"
        )
```

### 5-minute interval, on average across all the days in the dataset, containing maximum number of steps:

```{r avgSteps, cache=TRUE}
avgSteps[avgSteps$meanOfSteps == max(avgSteps$meanOfSteps), ]
```


<br>
<br>
<br>



## Imputing Missing Values

Total number of rows with NAs: **<code>`r sum(is.na( activityData ) )`</code>**

<br>

### Fill All Missing Values in the Data Set
Use the mean for that 5-minute interval to fill each NA value in the steps column.

* Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r newData, cache=TRUE}

newData <- activityData 

for (i in 1:nrow(newData)) {
    if (is.na(newData$steps[i])) {
        newData$steps[i] <- 
            avgSteps[which( newData$interval[i] == avgSteps$interval), ] $meanOfSteps
    }
}

head(newData)
sum(is.na(newData))
```

<br>

### Histogram of Total Number of Steps Per Day

```{r plot, cache=TRUE}

ggplot(newData, aes(date, steps)) + 
    geom_bar(stat = "identity", colour = "steelblue", fill = "steelblue", width = 0.7) +
    facet_grid(. ~ month, scales = "free") + 
    labs(
        title = "Total Number of Steps Taken Per Day (no missing data)", 
        x = "Date", 
        y = "Total Number of Steps"
        )

```

<br>

### Difference from Estimates, Impact of Imputing Missing Data

* Mean total number of steps taken per day:

```{r newTotalSteps, cache=TRUE}

newTotalSteps <- aggregate(newData$steps, 
                           list(Date = newData$date), 
                           FUN = "sum")$x
newMean <- mean(newTotalSteps)
newMean

```

* Median total number of steps taken per day:
```{r cache=TRUE}

newMedian <- median(newTotalSteps)
newMedian

```

* Compare them with the two before imputing missing data:

```{r cache=TRUE}

oldMean <- mean(totalSteps)
oldMedian <- median(totalSteps)
newMean - oldMean
newMedian - oldMedian

```

So, after imputing the missing data

- the new mean of total steps taken per day is the same as that of the old mean; 

- the new median of total steps taken per day is *slightly* greater than that of the old median.



<br>
<br>
<br>

## Are there differences in activity patterns between weekdays and weekends?


### Indicate Whether a Given Date is a Weekday or Weekend

```{r cache=TRUE}

newData$weekdays <- factor(format(newData$date, "%A"))

levels(newData$weekdays) <- 
    list(
        weekday = c( "Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),
        weekend = c("Saturday", "Sunday" ) 
        )

table(newData$weekdays)

```

A sample of the data

```{r cache=TRUE}

newData[ 1435:1445,, drop = F]

```

<br>

### Plotting the Average Number of Steps Taken - Averaged Across All Weekdays Or Weekend Days

```{r cache=TRUE}

avgSteps <- aggregate(
    newData$steps, 
    list(interval = as.numeric(as.character(newData$interval)), weekdays = newData$weekdays),
    FUN = "mean")

names(avgSteps)[3] <- "meanOfSteps"

library(lattice)

xyplot(avgSteps$meanOfSteps ~ avgSteps$interval | avgSteps$weekdays, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Number of Steps")

```



<br>
<br>
<br>