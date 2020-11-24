
library(magrittr)
library(data.table)
library(ggplot2)


# importing Case, PatientInfo, Time, Searchtrend
case <- fread("./extData/Case.csv")
patientinfo <- fread("./extData/PatientInfo.csv")
time <- fread("./extData/Time.csv")
searchtrend <- fread("./extData/SearchTrend.csv")


#patientinfo: confirmed_date important
dim(patientinfo)
case[, .N, by=group]
summary(patientinfo)


#time: shows accumulated number of tests etc. -> not used for search

#plot daily cases per day:
patientinfo <- patientinfo[, daily_cases := .N, by = confirmed_date]
ggplot(patientinfo, aes(x=confirmed_date, y=daily_cases)) + geom_line()


# plot search data for "coronavirus" search
searchtrend[date >= "2020-01-01"] %>%
  ggplot(aes(x=date, y=coronavirus)) + geom_line()
colnames(patientinfo)

#merge patientinfo and searchtrend
  #rename confirmed_date to date
patientinfo[, date := sub("confirmed_date", "date", confirmed_date)]
  #change date class to "IDate"
patientinfo[, date:= as.IDate(date)]
class(patientinfo$date)

#full outer merge of patientinfo and searchtrend
search_patient <- merge(patientinfo, searchtrend, by = "date", all = TRUE)
colnames(search_patient)
summary(search_patient)


#subset search_patient to Jan-March 2020
search_patientsubset <- search_patient[date >= "2020-01-01" & date < "2020-05-01"]

# plot final with two graphs:
ggplot(search_patientsubset, aes(x=date)) +
  geom_line(aes(y=daily_cases, colour = "daily_cases")) +
  geom_line(aes(y=coronavirus, colour = "coronavirus"))



=======

library(magrittr)
library(data.table)
library(ggplot2)


# importing Case, PatientInfo, Time, Searchtrend
case <- fread("./extData/Case.csv")
patientinfo <- fread("./extData/PatientInfo.csv")
time <- fread("./extData/Time.csv")
searchtrend <- fread("./extData/SearchTrend.csv")


#patientinfo: confirmed_date important
dim(patientinfo)
case[, .N, by=group]
summary(patientinfo)


#time: shows accumulated number of tests etc. -> not used for search

#plot daily cases per day:
patientinfo <- patientinfo[, daily_cases := .N, by = confirmed_date]
ggplot(patientinfo, aes(x=confirmed_date, y=daily_cases)) + geom_line()


# plot search data for "coronavirus" search
searchtrend[date >= "2020-01-01"] %>%
  ggplot(aes(x=date, y=coronavirus)) + geom_line()
colnames(patientinfo)

#merge patientinfo and searchtrend
  #rename confirmed_date to date
patientinfo[, date := sub("confirmed_date", "date", confirmed_date)]
  #change date class to "IDate"
patientinfo[, date:= as.IDate(date)]
class(patientinfo$date)

#full outer merge of patientinfo and searchtrend
search_patient <- merge(patientinfo, searchtrend, by = "date", all = TRUE)
colnames(search_patient)
summary(search_patient)


#subset search_patient to Jan-March 2020
search_patientsubset <- search_patient[date >= "2020-01-01" & date < "2020-05-01"]

# plot final with two graphs:
ggplot(search_patientsubset, aes(x=date)) +
  geom_line(aes(y=daily_cases, colour = "daily_cases")) +
  geom_line(aes(y=coronavirus, colour = "coronavirus"))



>>>>>>> 623b6f4e3b8217b0466eea9c6a3dddd5e51453b9
