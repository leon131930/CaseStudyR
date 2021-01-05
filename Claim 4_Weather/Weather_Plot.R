
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

cor_temp_spear <- cor(melted_dt$avg_temp, melted_dt$daily_cases, method = 'spearman')

# PearsonTest: Correlation temperature and daily cases
# Nullhypothese: Keine Korrelation
ho_0 <- 0
# Na's in Daten?
anyNA(melted_dt$avg_temp, melted_dt$daily_cases)
n <- nrow(melted_dt)
# Teststatistik
t <- cor_temp/ sqrt(1-cor_temp^2)*sqrt(n-2)
# kritischer Wert
alpha <- 0.05
z_krit <- qnorm(1-alpha/2)
# Lehne H0 ab, wenn der Absolutwert (Betrag) der Teststatistik größer ist als der kritische Wert:
abs(t) > z_krit


# Spearman Test: Correlation temperature and daily cases
# Nullhypothese: Keine Korrelation
ho_0 <- 0
# Na's in Daten?
anyNA(melted_dt$avg_temp, melted_dt$daily_cases)
n <- nrow(melted_dt)
# Teststatistik
t_spear <- cor_temp_spear/ sqrt(1-cor_temp_spear^2)*sqrt(n-2)
# kritischer Wert
alpha <- 0.05
z_krit <- qt(p=0.95,df=n-1)
# Lehne H0 ab, wenn der Absolutwert (Betrag) der Teststatistik größer ist als der kritische Wert:
abs(t_spear) > z_krit











# Data without province Daegu (Shincheonji church is located there -> Superspreader Event)
# Prepare data
melted_dt_without_Daegu = melted_dt[province!='Daegu']
# Plot data: avg weather
ggplot(melted_dt_without_Daegu,aes(avg_temp,daily_cases,color=province))+geom_point()
# Plot data: avg relative humidity
ggplot(melted_dt_without_Daegu,aes(avg_relative_humidity,daily_cases,color=province))+geom_point()
# Correlation
cor_temp_without_Daegu <- cor(melted_dt_without_Daegu$avg_temp, melted_dt_without_Daegu$daily_cases, method = 'spearman')
cor_humidity_without_Daegu <- cor(melted_dt_without_Daegu$avg_relative_humidity, melted_dt_without_Daegu$daily_cases, method = 'pearson')
