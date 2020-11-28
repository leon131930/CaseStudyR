#Education policies 

#import libraries
library(magrittr)
library(data.table)
library(ggplot2)

# import files: Time, RKI data
time <- fread("./extData/Time.csv")
RKI_data <- fread("./extData/DE_InfectionCases.csv")
policy <- fread("./extData/Policy.csv")

summary(time)

#total cases in Korea per day
time[,confirmed_date:=as.Date(date,"%d/%m/%y")]
time[, confirmed_date:= as.IDate(confirmed_date)]
ggplot(time, aes(x=confirmed_date, y=daily_cases)) +
  geom_line()


#get nb of infections per day
infection_cases <- time[, daily_cases]

#extract education measurement from table policy
date_education <- policy[type == "education"]


#plot start of education policies on infection cases per day within
#the time frame 

ggplot()