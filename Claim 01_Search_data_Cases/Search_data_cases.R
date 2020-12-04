install.packages("patchwork")
install.packages("hrbrthemes")
library(magrittr)
library(data.table)
library(ggplot2)
library(dplyr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)


# importing Case, PatientInfo, Time, Searchtrend
case <- fread("./extData/Case.csv")
patientinfo <- fread("./extData/PatientInfo.csv")
time <- fread("./extData/Time.csv")
searchtrend <- fread("./extData/SearchTrend.csv")

#create new column for number of daily cases, based on count of confirmed_date
patientinfo <- patientinfo[, daily_cases := .N, by = confirmed_date]


#rename confirmed_date to date (otherwise not able to join)
patientinfo[, date := sub("confirmed_date", "date", confirmed_date)]
#change date class to "IDate"
patientinfo[, date:= as.IDate(date)]

#full outer join of patientinfo and searchtrend
search_patient <- merge(patientinfo, searchtrend, by = "date", all = TRUE)


#search volume comparison
ggplot(search_patient[date >= "2020-01-01" & date < "2020-03-15"], aes(x=date)) +
  geom_line(aes(y=coronavirus, col = "coronavirus")) +
  geom_line(aes(y=flu*20, col = "flu")) +
  geom_line(aes(y=cold*20, col = "cold"))+
  labs(y="search volume", title="Search volume per keyword over time")






# plot final with two graphs:
ggplot(search_patient[date >= "2020-01-01" & date < "2020-05-01"], aes(x=date)) +
  geom_line(aes(y=daily_cases, col = "daily_cases")) +
  geom_line(aes(y=flu*30, col = "flu")) +
  geom_line(aes(y=cold*30, col = "cold"))+
  labs(title="Daily covid cases & search data")+
  scale_y_continuous(
    # Features of the first axis
    name = "daily confirmed cases",
    # Add a second axis and specify its features
    sec.axis = sec_axis(trans=~./30*10000, name="Search Volume")
  )





  
# flu search volume & daily cases
#setting colour
searchcolour <- "#00c7c6"
dailycasecolour <- "#ff0000"

ggplot(search_patient[date >= "2019-09-01" & date < "2020-05-01"], aes(x=date)) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %y")+
  geom_line(aes(y=daily_cases, col = "Daily Cases")) +
  geom_line(aes(y=flu*100, col = "Searches for 'flu'")) +
  labs(title="Daily covid cases & search volume for 'flu'")+
  scale_y_continuous(
    # Features of the first axis
    name = "daily confirmed cases",
    # Add a second axis and specify its features
    sec.axis = sec_axis(trans=~./100*10000, name="Search Volume"))+
  theme(legend.position="right", legend.title = element_blank(), 
        axis.title.y.right = element_text(angle=90, color = searchcolour),
        axis.title.y.left = element_text(color = dailycasecolour))


# cold search volume & daily cases
#setting colour
searchcolour <- "#00c7c6"
dailycasecolour <- "#ff0000"

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



