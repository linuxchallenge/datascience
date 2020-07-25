library(lubridate)

#read the data
input_data <- read.csv('Article popularity dataset.csv')

#extract the dates and convert to day of week and hence a flag
m <- regexpr("\\d{4}/\\d{2}/\\d{2}", input_data$URL, perl=TRUE)

input_data$date <- regmatches(input_data$URL, m)
input_data$dayofweek <- weekdays(as.Date(input_data$date,'%Y/%m/%d'))

weekend_flag <- function(day){
  if(day=="Saturday" | day == "Sunday"){
    out = 1
  }else{
    out = 0
  }
  return(out)
}
input_data$is_weekend <- lapply(input_data$dayofweek,weekend_flag)

pop2<- input_data

# subset: weekday, weekend, socmed and others
weekend <- subset(pop2, is_weekend == 1)
weekday <- subset(pop2, is_weekend == 0)
socmed <- subset(pop2, data_channel_is_socmed == 1)
others <- subset(pop2, data_channel_is_socmed == 0)

# weekend versus weekdays
t.test(weekend$shares, weekday$shares, conf.level = 0.99)


# weekend: socmed versus others
t.test(subset(weekend, data_channel_is_socmed == 1)$shares, subset(weekend, data_channel_is_socmed == 0)$shares, conf.level = 0.99)


# weekdays: socmed versus others
t.test(subset(weekday, data_channel_is_socmed == 1)$shares, subset(weekday, data_channel_is_socmed == 0)$shares, conf.level = 0.99)

# socmed: weekend versus weekday
t.test(subset(weekday, data_channel_is_socmed == 1)$shares, subset(weekend, data_channel_is_socmed == 1)$shares, conf.level = 0.95)