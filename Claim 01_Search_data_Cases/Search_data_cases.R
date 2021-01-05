install.packages("patchwork")
install.packages("hrbrthemes")
library(magrittr)
library(data.table)
library(ggplot2)
library(dplyr)
library(patchwork)
library(hrbrthemes)
library(ggpubr)

######## DATA Preparation ############
# importing Case, PatientInfo, Searchtrend
case <- fread("./extData/Case.csv")
patientinfo <- fread("./extData/PatientInfo.csv")
searchtrend <- fread("./extData/SearchTrend.csv")

#create new column for number of daily cases, based on count of confirmed_date
patientinfo <- patientinfo[, daily_cases := .N, by = confirmed_date]

#rename confirmed_date to date (otherwise not able to join)
patientinfo[, date := sub("confirmed_date", "date", confirmed_date)]
#change date class to "IDate"
patientinfo[, date:= as.IDate(date)]

#full outer join of patientinfo and searchtrend
search_patient <- merge(patientinfo, searchtrend, by = "date", all = TRUE)


#set colors for visualization
searchcolour <- "#00c7c6"
dailycasecolour <- "#ff0000"

#########Visualization with patientinfo table############

# cold search volume & daily cases
ggplot(search_patient[date >= "2019-12-20" & date < "2020-05-01"], aes(x=date)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y")+
  geom_line(aes(y=daily_cases, col = "Daily Cases")) +
  geom_line(aes(y=cold*30, col = "Searches for 'cold'")) +
  labs(title="Daily covid cases & search volume for 'cold'")+
  scale_y_continuous(
    # Features of the first axis
    name = "daily confirmed cases",
    # Add a second axis and specify its features
    sec.axis = sec_axis(trans=~./30*10000, name="Search Volume"))+
  theme(legend.position="right", legend.title = element_blank(), 
        axis.title.y.right = element_text(angle=90, color = searchcolour),
        axis.title.y.left = element_text(color = dailycasecolour))

#########Visualization with time table############

time <- fread("./extData/Time.csv")
time <- time[, date:= as.IDate(date, format= "%d/%m/%Y")]

ggplot(time[date < "2020-05-01"], aes(x=date)) +geom_line(aes(y=daily_cases))
merged_dt <- merge(time, searchtrend, by = "date", all = TRUE)


#plot with 2 axis
ggplot(merged_dt[date %between% c("2020-02-15", "2020-06-15")],aes(x=date)) +
  scale_x_date(date_breaks = "1 week", date_labels = "%W")+
  geom_line(aes(y=daily_cases, col = "Daily Cases")) +
  geom_line(aes(y=(coronavirus)*8, col = "Searches for 'coronavirus'")) +
  labs(x="Calendar week 2020", title="Daily covid cases & relative search volume for 'coronavirus'")+
  scale_y_continuous(
    name = "daily confirmed cases",
    sec.axis = sec_axis(trans=~./8, name="relative search volume",labels = scales::comma))+
  theme(legend.position="bottom", legend.title = element_blank(), 
        axis.title.y.right = element_text(angle=90, color = searchcolour),
        axis.title.y.left = element_text(color = dailycasecolour))

#same plot, but with search volume on left y axis
ggplot(merged_dt[date %between% c("2020-02-15", "2020-06-29")],aes(x=date)) +
 # scale_x_date(date_breaks = "1 week", date_labels = "%W")+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y")+
  geom_line(aes(y=(coronavirus), col = "relative search volume")) +
  geom_line(aes(y=daily_cases/8, col = "daily confirmed cases")) +
  labs(x="Calendar week 2020", title="Daily covid cases & relative search volume for 'coronavirus'")+
  scale_y_continuous(
    name = "relative search volume",
    sec.axis = sec_axis(trans=~.*8, name="daily confirmed cases",labels = scales::comma))+
  theme(legend.position="bottom", legend.title = element_blank(), 
        axis.title.y.right = element_text(angle=90, color = dailycasecolour),
        axis.title.y.left = element_text(color = searchcolour))

#explore maximum search volume data points
#merged_dt[date %between% c("2019-12-01", "2020-05-01")][order(-coronavirus)]


#calculate observed correlation for date range Mid February - End of June
merged_dt1 <- merged_dt[date %between% c("2020-02-15", "2020-06-29")]
corr <- cor(merged_dt1$daily_cases, merged_dt1$coronavirus, method = 'pearson')
corr


# Test: Correlation search volume and daily cases
# Nullhypothesis: No Correlation
ho_0 <- 0
# Na's in data?
anyNA(merged_dt1$daily_cases, merged_dt1$coronavirus)
n <- nrow(merged_dt1)


#test statistic
t <- corr/ sqrt(1-(corr)^2)*sqrt(n-2)
t
# critical value
alpha <- 0.05
z_krit <- qnorm(1-alpha/2)
# Lehne H0 ab, wenn der Absolutwert (Betrag) der Teststatistik größer ist als der kritische Wert:
abs(t) > z_krit


#####permutation testing
dt_permuted <- copy(merged_dt1)
set.seed(0)

#function to calculate correlation
correlation <- function(dt_permuted){
  corr_p <- cor(dt_permuted$daily_cases, dt_permuted$coronavirus_sampled, method = 'pearson')
}

# create list with 1000 NA-values
m <- 1000
T_permuted <- rep(NA, m)

#repeat 1000 times: sample the column "coronavirus", calculate correlation and store variable in T_permuted
for(i in 1:m){
  dt_permuted[, coronavirus_sampled := sample(coronavirus)]
  T_permuted[i] <- correlation(dt_permuted)
}

#plot T_permuted as histogram
ggplot( data.table(T_permuted), aes(x = T_permuted)) +
  geom_histogram() +
  geom_vline(aes(xintercept= corr, color = "oberserved correlation"))


# -> It seems the correlation is not likely to have arisen by chance









