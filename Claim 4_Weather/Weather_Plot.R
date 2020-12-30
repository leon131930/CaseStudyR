
#import libraries
library(magrittr)
library(data.table)
library(ggplot2)
library(ggpubr)

# Import files: Weather, TimeProvince
weather <- fread("./extData/Weather.csv")
timeProvince <- fread("./extData/TimeProvince.csv")


#As date
weather[, date:= as.IDate(date)]
timeProvince[,date:=as.Date(date,"%d/%m/%y")]
timeProvince[, date:= as.IDate(date)]


# Combine tables
# Province Sejong is missing in table Weather -> not all confirmed cases can be found in melted_dt
melted_dt <- merge(weather,timeProvince,by=c('province','date'))

# Plot data
