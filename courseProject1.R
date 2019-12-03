# Load Packages
library(tidyverse)

# Get data
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = "activity.zip")
unzip("activity.zip")

# Read in data
activity <- read_csv("activity.csv")
head(activity)

activityStepSum <- activity %>%
        dplyr::group_by(date) %>%
        dplyr::summarize(stepSum = sum(steps))


hist(activityStepSum$stepSum)

summary(activityStepSum$stepSum)


intSummary <- activity %>%
        dplyr::group_by(interval) %>%
        dplyr::summarize(stepIntervalAvg = mean(steps, na.rm = T))

ggplot(intSummary, aes(x = interval, y = stepIntervalAvg)) + geom_area(alpha = 0.5)
# Region between 750 and 1000 appears to have the highest amount of average steps
# Let's find it definitively
intSummary[which.max(intSummary$stepIntervalAvg),]
# It's interval 835, to be exact


summary(activity$steps)
# Looks like we have 2304 NA's

# Let's use the median for that interval for the NAs, which tends to be more
# robust than the mean

activityNaFill <- activity %>%
        dplyr::group_by(interval) %>%
        dplyr::mutate(steps = median(steps, na.rm = T)) %>%
        dplyr::summarize(stepSum = sum(steps))

hist(activityNaFill$stepSum)
# Looks like a lot of the NAs were during a time where people weren't typically moving at all.
# Maybe they were sleeping, and turned the tracker off?

summary(activityNaFill$stepSum)

isWeekend <- weekdays(activity$date) %in% c("Saturday", "Sunday")

activityNaFillWeekend <- activity %>%
        dplyr::if_else(as.list(isWeekend), "Weekend", "Weekday") %>%
        dplyr::group_by() %>%
        dplyr::mutate(steps = median(steps, na.rm = T))


ggplot(activityNaFillWeekend, )