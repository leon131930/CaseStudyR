
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

# Plot data: avg weather
ggplot(melted_dt,aes(avg_temp,daily_cases,color=province))+geom_point()

#Plot data: avg relative humidity
ggplot(melted_dt,aes(avg_relative_humidity,daily_cases,color=province))+geom_point()

# Correlation
cor_temp <- cor(melted_dt$avg_temp, melted_dt$daily_cases, method = 'pearson')
cor_humidity <- cor(melted_dt$avg_relative_humidity, melted_dt$daily_cases, method = 'pearson')


# Plot data without province Daegu (Shincheonji church is located there -> Superspreader Event)



# Province Busan
melted_dt_Busan <- melted_dt[province=="Busan"]
ggplot(melted_dt_Busan,aes(avg_temp,daily_cases))+geom_point()
cor(melted_dt_Busan$avg_temp, melted_dt_Busan$daily_cases, method = 'pearson')
cor(melted_dt_Busan$avg_relative_humidity, melted_dt_Busan$daily_cases, method = 'pearson')