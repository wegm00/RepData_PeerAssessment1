#Load Principle libraries
library(ggplot2)
library(dplyr)

rfilename <- "activity.zip"


# Checking if folder exists
if (!file.exists("activity.cvs")) { 
  unzip(rfilename) 
}

activity <- read.csv("activity.csv")
stepsdaily <- aggregate(steps~date,activity,sum, na.rm=TRUE)
hist(stepsdaily$steps, main = "Total number of steps taken per day",
     xlab = "Total steps taken per day", col = "blue")

mean(stepsdaily$steps)
median(stepsdaily$steps)

#next question

stepsinterval <- aggregate(steps~interval,activity,mean, na.rm=TRUE)

plot(steps~interval, data=stepsinterval, type="l",col = "red", 
     main = "Average steps per time interval" )

stepsinterval[which.max(stepsinterval$steps),]$interval

# finding and replace missing data

sum(is.na(activity$steps))



activity$fullsteps <- ifelse(is.na(activity$steps), 
                             round(stepsinterval$steps[match(activity$interval, stepsinterval$interval)],0),
                             activity$steps)

activityfull <- data.frame(steps=activity$fullsteps, interval=activity$interval, date=activity$date)

stepsdailyfull <- aggregate(steps~date,activityfull,sum, na.rm=TRUE)
hist(stepsdailyfull$steps, main = "Total number of steps taken per day",
     xlab = "Total steps taken per day", col = "blue")

mean(stepsdailyfull$steps)
median(stepsdailyfull$steps)

activityfull$date <- as.Date(activityfull$date, format = "%Y-%m-%d")

activityfull$datetype <- sapply(activityfull$date, function(x) {
  if (weekdays(x) == "sábado" | weekdays(x) =="domingo") 
  {y <- "Weekend"} else 
  {y <- "Weekday"}
  y
})

stepsintervalfull <- aggregate(steps~interval+datetype,data=activityfull,FUN=mean)


g <-  ggplot(stepsintervalfull, aes(x = interval , y = steps, color = datetype)) +
  geom_line() +
  labs(title = "Average daily steps by type of date", x = "Interval", y = "Average number of steps") +
  facet_wrap(~datetype, ncol = 1, nrow=2)
print(g)