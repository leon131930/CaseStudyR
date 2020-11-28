library(magrittr)
library(data.table)
library(ggplot2)
library(tidyr)
library(dplyr)

# Importing Patientinfo, Case and Region
case <- fread("./extData/Case.csv")
patientinfo <- fread("./extData/PatientInfo.csv")
region <- fread("./extData/Region.csv")

# Playing with data - who was infected by which infection case?
patientinfo[, .N, by = 'infection_case']

# Let's see what were the main 15 infection cases
patientinfo %>% count(infection_case) %>% top_n(15, n) %>% arrange(desc(n))
ggplot(data = patientinfo, aes(x=infection_case)) + geom_bar() 
#to be improved!! limit it to max. 15 main events

# Calculating the daily cases and adding this as another column
patientinfo[, daily_cases := .N, by = confirmed_date]
patientinfo

# Creating a graph showing the Daily Cases over time
ggplot(data = patientinfo, aes(x=confirmed_date, y=daily_cases)) +
  geom_line(colour="orange") + labs(x="Time", y="Cases per Day")

ggplot(data = patientinfo, aes(x=confirmed_date, y=daily_cases)) +
  geom_line(colour="orange") + labs(x="Time", y="Cases per Day") +
  geom_hline(yintercept=100, color="green")

# Do we have some "Super-spreader" people within our data set?
patientinfo %>% count(infected_by) %>% top_n(15, n) %>% arrange(desc(n))

# Adding the 5 big Spreader Events to our Graph showing Daily Cases over Time
ggplot(data = patientinfo, aes(x=confirmed_date, y=daily_cases)) +
  geom_line(colour="black") +
  geom_line(data = subset(patientinfo, infection_case=="Itaewon Clubs"), colour="green") +
  geom_line(data = subset(patientinfo, infection_case=="Richway"), colour="blue") +
  geom_line(data = subset(patientinfo, infection_case=="Guro-gu Call Center"), colour="purple") +
  geom_line(data = subset(patientinfo, infection_case=="Shincheonji Church"), colour="red") +
  geom_line(data = subset(patientinfo, infection_case=="Coupang Logistics Center"), colour="orange") +
  labs(x="Time", y="Cases per Day") +
  geom_hline(yintercept=100, color="orange")
# legend to be added - colour code to be changed

# Trying to add vertical lines - but first: create data table with the main spreader events
super_spreader <- data.table(Event_Name= c("Itaewon Club", "Richway", "Guro-gu Call Center", "Shincheonji Church", "Coupang Logistics Center"), 
                           Date=c("2020-05-01", "2020-06-01", "2020-03-05", "2020-02-15", "2020-05-20"))
super_spreader

# Convert Date column as date, so we can use it in one graph with our Patientinfo & daily cases
super_spreader[, Date:=as.Date(Date)]
class(super_spreader$Date)

# Try ggplot with one vertical line for first main spreader event - the Shincheonji Church in Feb 2020
ggplot(data = patientinfo, aes(x=confirmed_date, y=daily_cases)) + geom_line(colour="black") +
  geom_vline(data = subset(super_spreader, Event_Name=="Shincheonji Church"),
             aes(xintercept = Date, colour = "Shincheonji Church (15. February 2020)")) +
  labs(x="Time", y="Cases per Day") +
  theme(legend.position="bottom")

# Showing Date of Shincheonji Church Outbreak and the cases that were connected with this event in one graph
ggplot(data = patientinfo, aes(x=confirmed_date, y=daily_cases)) + geom_line(colour="black") +
  geom_line(data = subset(patientinfo, infection_case=="Shincheonji Church"), colour="red") +
  geom_vline(data = subset(super_spreader, Event_Name=="Shincheonji Church"),
             aes(xintercept = Date, colour = "Shincheonji Church (15. February 2020)")) +
  labs(x="Time", y="Cases per Day") +
  theme(legend.position="bottom")

# OPEN TO-DO's
  ## understand how to integrate the "real" daily cases from the "time" data table
  ## understand how to structure bar chart from left to right and how to subset the data so we only get
  ## the top 15 or 10 entries
  ## understand how to make legend nicer
  ## Storyline in the end: here are the top reasons for infections (starting with patient contact) - we wil show
  ## you what kind of intensive impact the spreader events have had - starting with the Shincheonji Church