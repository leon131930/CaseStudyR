library(magrittr)
library(data.table)
library(ggplot2)

# Importing Patientinfo, Case and Region
case <- fread("./extData/Case.csv")
patientinfo <- fread("./extData/PatientInfo.csv")
region <- fread("./extData/Region.csv")

# Playing with data - who was infected by which infection case?
patientinfo[, .N, by = 'infection_case']

# Let's see what were the main 15 infection cases
patientinfo %>% count(infection_case) %>% top_n(15, n) %>% arrange(desc(n))
ggplot(data = patientinfo, aes(x=infection_case)) + geom_bar() #to be improved!! 

# Calculating the daily cases and adding this as another column
patientinfo[, daily_cases := .N, by = confirmed_date]
patientinfo

# Creating a graph showing the Daily Cases over time
ggplot(data = patientinfo, aes(x=confirmed_date, y=daily_cases)) + geom_line(colour="orange") + labs(x="Time", y="Cases per Day")
ggplot(data = patientinfo, aes(x=confirmed_date, y=daily_cases)) + geom_line(colour="orange") + labs(x="Time", y="Cases per Day") + geom_hline(yintercept=100, color="green")

# Do we have some "Super-spreader" people within our data set?
patientinfo %>% count(infected_by) %>% top_n(15, n) %>% arrange(desc(n))

# Adding the 5 big Spreader Events to our Graph showing Daily Cases over Time
ggplot(data = patientinfo, aes(x=confirmed_date, y=daily_cases)) +
  geom_line(colour="grey") + geom_line(data = subset(patientinfo, infection_case=="Itaewon Clubs"), colour="green") + geom_line(data = subset(patientinfo, infection_case=="Richway"), colour="blue") + geom_line(data = subset(patientinfo, infection_case=="Guro-gu Call Center"), colour="purple") + geom_line(data = subset(patientinfo, infection_case=="Shincheonji Church"), colour="red") + geom_line(data = subset(patientinfo, infection_case=="Coupang Logistics Center"), colour="black") + labs(x="Time", y="Cases per Day") + geom_hline(yintercept=100, color="orange")
## suuuuuper long line of code - according to Google much easier when melting data beforehand - to be checked out 