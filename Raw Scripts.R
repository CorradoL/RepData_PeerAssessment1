## read zip file
activity.data <- read.csv(unz("activity.zip", "activity.csv"))


str(activity.data)
require(dplyr)
summarize(activity.data)
head(activity.data)

nonNA.activity <- filter(activity.data, steps != "NA")

sum.per.date <- nonNA.activity %>% group_by(date) %>% summarize(steps=sum(steps))

barplot(sum.per.date$steps,names.arg = sum.per.date$date)

max(sum.per.date$steps)


head(nonNA.activity)

avr.per.date <- nonNA.activity %>% group_by(interval) %>% summarize(steps=mean(steps))


head(avr.per.date)

iterval.max.steps <- avr.per.date %>% filter(steps==max(steps))


step_sum <- tapply(activity.data$steps, activity.data$date, sum, na.rm = TRUE, simplify = TRUE)
mean(step_sum)

head(step_sum)
