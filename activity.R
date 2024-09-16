library(tidyverse)

setwd('/Users/skim/Desktop/temp/CS/Week2_project1')

### 1
activity <- read_csv('./activity.csv')

### 2
hist(tapply(activity$steps, activity$date, sum), main = 'Steps by date',  xlab = 'Total number of steps')

### 3
activity |>
  group_by(date) |>
  summarize(Mean = mean(steps, na.rm = T), Median = median(steps, na.rm = T)) -> Mean_median_steps_by_date
Mean_median_steps_by_date

### 4
activity |>
  group_by(interval) |>
  summarize(Mean = mean(steps, na.rm = T)) -> Mean_steps_by_interval
plot(Mean_steps_by_interval$interval, Mean_steps_by_interval$Mean, type = 'l')

### 5
Mean_steps_by_interval$interval[which.max(Mean_steps_by_interval$Mean)]

### 6
# Value to be imputed for NA
moddf2 <- activity |> 
        group_by(date) |>
        mutate(mean=mean(steps))
imputed<-moddf2$mean[is.na(moddf2$mean)!= TRUE]

# Replace NA to mean of interval value
replace_activity <- activity |>
                    group_by(date) |>
                    mutate(replaced_steps=ifelse(is.na(steps),yes = imputed,no=steps)) 


### 7
hist(tapply(replace_activity$replaced_steps, replace_activity$date, sum), main = 'Steps by date',  xlab = 'Total number of steps')

### 8
activity$date <- as.Date(strptime(activity$date, format="%Y-%m-%d"))
activity$datetype <- sapply(activity$date, function(x) {
  if (weekdays(x) == "Saturday" | weekdays(x) =="Sunday") 
  {y <- "Weekend"} 
  else 
  {y <- "Weekday"}
  y
})

activity_by_date <- aggregate(steps~interval + datetype, activity, mean, na.rm = TRUE)
g<-qplot(x=interval,y=steps,data = activity_by_date,facets = .~datetype,geom = "line",group=1)
print(g)

