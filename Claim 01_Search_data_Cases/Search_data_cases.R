
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


#plot daily cases per day:
patientinfo <- patientinfo[, daily_cases := .N, by = confirmed_date]
ggplot(patientinfo, aes(x=confirmed_date, y=daily_cases)) + geom_line()


# plot search data for "coronavirus" search
searchtrend[date >= "2020-01-01"] %>%
  ggplot(aes(x=date)) +
  geom_line(aes(y=coronavirus, colour = "coronavirus")) +
  geom_line(aes(x=date, y=flu, colour = "flu")) + 
  geom_line(aes(x=date, y=pneumonia, colour = "pneumonia"))
colnames(patientinfo)


#rename confirmed_date to date (otherwise not able to join)
patientinfo[, date := sub("confirmed_date", "date", confirmed_date)]
#change date class to "IDate"
patientinfo[, date:= as.IDate(date)]
class(patientinfo$date)

#full outer join of patientinfo and searchtrend
search_patient <- merge(patientinfo, searchtrend, by = "date", all = TRUE)
colnames(search_patient)
summary(search_patient)
?shift

#subset search_patient to Jan-March 2020
search_patientsubset <- search_patient[date >= "2020-01-01" & date < "2020-05-01"]

# plot final with two graphs:
ggplot(search_patientsubset, aes(x=date)) +
  geom_line(aes(y=daily_cases, colour = "daily_cases")) +
  geom_line(aes(y=coronavirus, colour = "coronavirus")) +
  geom_line(aes(y=flu*100, colour = "flu")) 
#+geom_line(aes(y=pneumonia*10, colour = "pneumonia"))


