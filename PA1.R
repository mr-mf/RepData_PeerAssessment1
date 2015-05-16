
setwd("C:/Users/Michael/reprodResearch")


# load the data into R
# convert date varible to date class
# load dplyr and ggplot2 ands stats packages

input <- read.csv("activity.csv", header = T, sep = ",")
input$date <- as.Date(input$date, "%Y-%m-%d")
library(dplyr)
library(ggplot2)
library(stats)

# calculate the number of steps per day with dplyr package
# ignoring the missing values
stepPerDay <- input %>%
        group_by(date) %>%
        summarize(total_steps = sum(steps))%>%
        arrange(date)
######
# What is mean total number of steps taken per day?
######

# plot a histogram of total number of steps per day
histStepDay <- ggplot(stepPerDay, aes(total_steps))
histStepDay <- histStepDay + geom_histogram(fill = "steelblue")
histStepDay <- histStepDay + ggtitle( "Number of Steps by Day" )
histStepDay <- histStepDay + xlab("Steps per Day")
histStepDay <- histStepDay + ylab("Number of Days")
histStepDay

# the mean of the total number of steps taken per day
mean(stepPerDay$total_steps, na.rm = T)
# the median of the total number of steps taken per day
median(stepPerDay$total_steps, na.rm = T)

#####
# What is the average daily activity pattern?
#####

# the average number of steps taken over a 5 minute interval, averaged across all days.

fiveMinAve <- input %>%
        group_by(interval) %>%
        summarize(avg_step = mean(steps, na.rm = T)) %>%
        arrange(interval)


# create a ts plot
ts <- ggplot(fiveMinAve, aes(x = interval, y = avg_step))
ts <- ts + geom_line(colour = "steelblue", size = 0.5)
ts <- ts + ggtitle("Average Steps over Interval")
ts + xlab("five minute interval") + ylab("Average number of Steps")

# Which 5-minute interval, on average across all the days in the dataset,
# contains the maximum number of steps?

as.data.frame(fiveMinAve[fiveMinAve$avg_step %in% max(fiveMinAve$avg_step),])

###
# Imputing missing values
###

# number of missin values in the data set (obly steps varialbe considered)

sum(is.na(input$steps))

# filling in all of the missing values in the dataset
# mean value for the interval

# join the input data with the 5-minute interval average

joinedData <- inner_join(input, fiveMinAve)

# if the step value is NA replace it with the average
# value for the 5 minute interval

for(i in 1:dim(joinedData)[1]){
        if(is.na(joinedData[i,]$steps)){
                joinedData[i,]$steps <- joinedData[i,]$avg_step                
        }
}
# Create a new dataset that is equal to the original dataset 
# but with the missing data filled in
joinedData <-joinedData[,1:3]

# get the total number of steps per day
stepPerDay2 <- joinedData %>%
        group_by(date) %>%
        summarize(total_step = sum(steps)) %>%
        arrange(date)

# plot a histogram of total number of steps per day
# with missing values being replaced
histDay2 <- ggplot(stepPerDay2, aes(total_step))
histDay2 <- histDay2 + geom_histogram(fill = "steelblue")
histDay2 <- histDay2 + ggtitle( "Number of Steps by Day (NA's imputed)" )
histDay2 <- histDay2 + xlab("Steps per Day")
histDay2 <- histDay2 + ylab("Number of Days")
histDay2

# the new mean of the total number of steps taken per day
mean(stepPerDay2$total_step)
# the new median of the total number of steps taken per day
median(stepPerDay2$total_step)

# there isn't a significat different between the mean/meadian claculated with NA's
# and after the NA's were replaced with the average value of steps in the five
# minute interval

####
# Are there differences in activity patterns between weekdays and weekends?
####

# Create a new factor variable in the dataset 
# with two levels - "weekday" and "weekend"
joinedData$weekday <-weekdays(joinedData$date, abbreviate = FALSE)

# create a factor variable which takes value 1 if the day is weekday
# and the value 2 if the day is weekend day
joinedData$day_index <- 0
for(i in 1:dim(joinedData)[1]){
        if (joinedData[i,]$weekday %in% c(
                "Monday","Tuesday","Wednesday","Thursday","Friday" )){
                joinedData[i,]$day_index <- 1
        }else{
                joinedData[i,]$day_index <- 2    
        }
}

# coerce day_index into factor variable
joinedData$day_index <- as.factor(joinedData$day_index)
levels(joinedData$day_index) <- c("weekday", "weekend")
head(joinedData)

# calculate the average step per interval grouped by
# week/weekend day
weekdayPlotData <- joinedData %>%
        select(steps, interval, day_index) %>%
        group_by(day_index, interval) %>%
        summarize(mean_step = mean(steps)) %>%
        arrange(day_index)

options(scipen = 999)
print(weekdayPlotData, n = 300)
str(weekdayPlotData)

# create a panel ts plot which splits the mean
# number of steps over the five minute interval
# according to weekdays and weekends
panelPlot <- ggplot(weekdayPlotData, aes(x = interval, y = mean_step))
panelPlot <- panelPlot + geom_line(colour = "steelblue", size = 0.2)
panelPlot <- panelPlot + facet_wrap(~ day_index, nrow = 2, ncol = 1)
panelPlot <- panelPlot + ggtitle("Mean Number Steps per Interval (Weekdays/Weekends)")
panelPlot + xlab("Mean Number of Steps") + ylab("5 minute interval")
