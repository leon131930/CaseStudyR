
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
ggplot(melted_dt,aes(avg_temp,daily_cases,color=ifelse(province=='Daegu', "Daegu", "remaining provinces")))+geom_point() +
  labs(x = "average temperature", y = "daily cases", 
       title = "No correlation between avg temp & daily cases") +
  theme(legend.title=element_blank(),plot.title=element_text(hjust=0.5)) 

ggplot(melted_dt,aes(avg_temp,daily_cases))+geom_point() +
  geom_smooth(method = "loess")


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
#Ergebnis: Nullhypothese wird verworfen





# Pearson Test
# Nullhypothese: negative Korrelation zwischen temperature und daily cases
rho_0 <- 0
# Na's in Daten?
anyNA(melted_dt$avg_temp, melted_dt$daily_cases)
n <- nrow(melted_dt)
# Teststatistik
t_2 <- cor_temp/ sqrt(1-cor_temp^2)*sqrt(n-2)
# kritischer Wert
alpha <- 0.05
z_krit_2 <- qnorm(1-alpha)
t_2 > z_krit_2
#Ergebnis: Nullhypothese wird nicht verworfen


# Pearson Test
# Nullhypothese: Korrelation zwischen temperature und daily cases kleiner als -0.3
rho_0 <- -0.3
# Na's in Daten?
anyNA(melted_dt$avg_temp, melted_dt$daily_cases)
n <- nrow(melted_dt)
# Teststatistik
t_3 <- 1/2 * (log((1 + cor_temp) / (1 - cor_temp)) -log((1 + -0.3) / (1 - -0.3))) * sqrt(n - 3)
# kritischer Wert
alpha <- 0.05
z_krit_3 <- qnorm(1-alpha)
t_3 > z_krit_3
#Ergebnis: Nullhypothese wird verworfen











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
# T-verteilung mit n-1 Freiheitsgraden
z_krit_spear <- qt(p=0.975,df=n-1)
# Lehne H0 ab, wenn der Absolutwert (Betrag) der Teststatistik größer ist als der kritische Wert:
abs(t_spear) > z_krit
#Ergebnis: Nullhypothese kann nicht verworfen werden




# Spearman Test: Correlation temperature and daily cases
# Nullhypothese: negative Korrelation zwischen temperature und daily cases
rho_0 <- 0
# Na's in Daten?
anyNA(melted_dt$avg_temp, melted_dt$daily_cases)
n <- nrow(melted_dt)
# Teststatistik
t_spear_2 <- cor_temp_spear/ sqrt(1-cor_temp_spear^2)*sqrt(n-2)
# kritischer Wert
alpha <- 0.05
# T-verteilung mit n-1 Freiheitsgraden
z_krit_spear_2 <- qt(p=0.95,df=n-1)
t_spear > z_krit
#Ergebnis: Nullhypothese kann nicht verworfen werden







# Data without province Daegu (Shincheonji church is located there -> Superspreader Event)
# Prepare data
melted_dt_without_Daegu = melted_dt[province!='Daegu']
# Plot data: avg weather
ggplot(melted_dt_without_Daegu,aes(avg_temp,daily_cases))+geom_point() +
  geom_smooth(method = "loess")
# Plot data: avg relative humidity
ggplot(melted_dt_without_Daegu,aes(avg_relative_humidity,daily_cases,color=province))+geom_point()
# Correlation
cor_temp_without_Daegu <- cor(melted_dt_without_Daegu$avg_temp, melted_dt_without_Daegu$daily_cases, method = 'spearman')
cor_humidity_without_Daegu <- cor(melted_dt_without_Daegu$avg_relative_humidity, melted_dt_without_Daegu$daily_cases, method = 'pearson')
