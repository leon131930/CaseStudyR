### Region visualization ###

#install.packages("maps")
library(magrittr)
library(data.table)
library(ggplot2)
library(maps)
library(ggrepel)
theme_set(theme_bw())
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyr)
library(dplyr)

# Importing the external data we will need
case <- fread("./extData/Case.csv")
patientinfo <- fread("./extData/PatientInfo.csv")
searchtrend <- fread("./extData/SearchTrend.csv")
head(case)

# Let's have a look what were the main infection reasons using the case datatable
infection_cases <- aggregate(x= case$confirmed,
                             by= list(case$infection_case),
                             FUN= sum)
infection_cases
# rename column names
infection_cases <- infection_cases %>% 
  rename(
    infection_reason = Infection_Reason,
    people_confirmed = People_Confirmed)
   
top_15_cases <- top_n(infection_cases, 15, people_confirmed)    

ggplot(data = top_15_cases, aes(x=people_confirmed, y=infection_reason)) + 
  geom_bar(stat="identity", width = 0.7, fill="blue") +
  labs(x="Number of Cases", y="Infection Reason", title = "Top 15 Infection Reasons")

# TO DO: rearrange the plot, make sure color changes going down, determine what to do with "Etc"

# As one can clearly see the first main spreader event was damn important and had serious influences
# on the whole Covid-19 outbreak in South Korea

# Unfortunately we are not able to calculate the total daily cases using the case datatable. However, we are
# able to do that using the patientinfo data table. Let's first determine whether the patientinfo datatable can
# be seen as a subset of the case datatable

# Graph showing total daily cases over time, patientinfo database
## doesn't matter Leon already defined this 
patientinfo[, daily_cases := .N, by = confirmed_date]
total_cases_patientinfo <- ggplot(data = patientinfo, aes(x=confirmed_date, y=daily_cases)) +
  geom_line(colour="black") + labs(x="Time", y="Cases per Day")
total_cases_patientinfo

# Graph showing total daily cases over time, time database
## doesn't matter for final markdown, only so this will work here
#total cases in Korea per day
time[,confirmed_date:=as.Date(date,"%d/%m/%y")]
time[, confirmed_date:= as.IDate(confirmed_date)]
total_cases_time <- ggplot(time, aes(x=confirmed_date, y=daily_cases)) + geom_line()

# Putting both graphs next to each other to show patientinfo is a sample of time

ggarrange(total_cases_patientinfo, total_cases_time, ncol=2, nrow=1)