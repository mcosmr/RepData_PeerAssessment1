#read data 

activity_data <- read.csv("activity.csv", sep = ",")

## Change class of Date Colum (Factor) to class "Date"
activity_data$date <- as.Date(activity_data$date, format="%Y-%m-%d")

#Calculate the total number of steps taken per day (ignoring the missing values in the dataset)

Total_steps <- aggregate(steps ~ date, data = activity_data, na.rm =TRUE, sum)


#histogram of the total number of steps taken each day
hist(Total_steps$steps, xlab = "Total Steps")

dev.copy(png,filename="Rplot1.png")
dev.off ()

#Calculate and report the mean and median of the total number of steps taken per 

median_steps_day <- median(Total_steps$steps)
mean_steps_day <- mean(Total_steps$steps)

#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
#and the average number of steps taken, averaged across all days (y-axis)
#time_series <- aggregate(activity_data$steps, by = list(activity_data$interval), na.rm =TRUE, median)

time_series <- aggregate(steps ~ interval, data = activity_data, na.rm =TRUE, mean)

plot(time_series$interval, time_series$steps, type = "l", xlab = "Interval", ylab = "Average Steps")

dev.copy(png,filename="Rplot2.png")
dev.off ()


#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps
max_steps <- time_series[time_series$steps == max(time_series$steps),]
max_steps
#Calculate and report the total number of missing values in the dataset

NA_count <- sum(is.na(activity_data))
NA_count
#filling in all of the missing values in the dataset. 
# For example, you could use the mean/median for that day,
#or the mean for that 5-minute interval, etc.
#index_Na <- which(is.na(activity_data))

#first, replacing NA values in data steps with mean for that interval
replace_NA<- replace(activity_data[, "steps"], is.na(activity_data[, "steps"])&activity_data$interval %in% time_series$interval , time_series[,"steps"])
#create a new data set with no NA values

data2 <- activity_data
data2$steps <- replace_NA


#Make a histogram of the total number of steps taken each day 
#and Calculate and report the mean and median total number of steps taken per day.

Total_steps2 <- aggregate(steps ~ date, data = data2, sum)

hist(Total_steps2$steps, xlab = "Total Steps", main = "Steps each day")

dev.copy(png,filename="Rplot3.png")
dev.off ()

median2_steps_day <- median(Total_steps2$steps)
median2_steps_day
mean2_steps_day <- mean(Total_steps2$steps)
mean2_steps_day
#comparing both data

par(mfrow = c(2,1))

hist(Total_steps$steps, xlab = "Total Steps", main ="NA removed")
hist(Total_steps2$steps, xlab = "Total Steps ", main = "NA Filled in")

dev.copy(png,filename="Rplot4.png")
dev.off ()


#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps

max_steps_no_NA <- time_series2[time_series2$steps == max(time_series2$steps),]
max_steps_no_NA
#Create a new factor variable in the dataset with two levels - "weekday" and "weekend" 
#indicating whether a given date is a weekday or weekend day.

Sys.setlocale("LC_TIME", "English")
data2$week_days<- weekdays(data2$date)
data2$week_days <- ifelse(data2$week_days == "Saturday" | data2$week_days == "Sunday", "Weekend", "Weekday")
data2$week_days <- as.factor(data2$week_days)


serie_weekday<- aggregate(data2$steps, list(data2$interval, data2$week_days), mean)
colnames(serie_weekday) <- c("interval", "week_day", "steps")

library(ggplot2)
qplot(interval, steps, data = serie_weekday, geom = "line", facets = week_day~.)
dev.copy(png,filename="Rplot5.png")
dev.off ()
