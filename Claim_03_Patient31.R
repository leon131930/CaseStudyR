# Load the libraries that will be needed for our analysis
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
time <- fread("./extData/time.csv")
case_old <- fread("./extData/Case.csv")

#We already realized, that in the case datatable we have a lot more patient cased listed, as we do have in
#the patientinfo data table. We also know, that there was quite some rumour about a church-event causing the
#spread in South Korea. To double check that rumour we will use the case datatable to first determine, 
#what caused the Covid-spread in South Korea to go wild.

overview_infection_cases <- aggregate(x= case$confirmed,by= list(case$infection_case),FUN= sum)
overview_infection_cases

#Rename column names to make referencing easier
overview_infection_cases <- overview_infection_cases %>% rename(infection_reason = Group.1,people_confirmed = x)
overview_infection_cases

#Determine top 15 reasons and plot them as a bar chart
top_15_cases <- top_n(overview_infection_cases, 15, people_confirmed)
top_15_cases
ggplot(data = top_15_cases, aes(x=people_confirmed, y=infection_reason)) + 
  geom_bar(stat="identity", width = 0.7, fill="orange") +
  labs(x="Number of Cases", y="Infection Reason", title = "Top 15 Infection Reasons")

#Seems like in fact the event in the Shincheonji Church had a large number of following Covid-cases. Let's have
#a look at the timeline - when exactly did this event happen and how did it effect the following daily cases?

#Disclaimer: we will use the patientinfo data table because in this datatable we have information for both, the
#number of daily cases as well as the infection reason for those cases. Unfortunately we don't have an infection
#reason within the time datatable. Since the time datatable includes a lot more daily cases than the patientinfo
#datatable we will first have a look, whether the patientinfo dataset can be seen as a subset of the time dataset.

# Graph showing total daily cases over time, patientinfo database
patientinfo[, daily_cases := .N, by = confirmed_date]
total_cases_patientinfo <- ggplot(data = patientinfo, aes(x=confirmed_date, y=daily_cases)) +
  geom_line(colour="black") + labs(x="Time", y="Cases per Day")
total_cases_patientinfo

# Graph showing total daily cases over time, time database, total cases in Korea per day
time[,confirmed_date:=as.Date(date,"%d/%m/%y")]
time[, confirmed_date:= as.IDate(confirmed_date)]
total_cases_time <- ggplot(time, aes(x=confirmed_date, y=daily_cases)) + geom_line(colour="black")+
  labs(x="Time", y="Cases per Day")
total_cases_time

#Let's put both graphs next to each other
library(ggpubr)
ggarrange(total_cases_patientinfo, total_cases_time, ncol=2, nrow=1)

#Let's see whether it makes more sense to put both graph lines into one graph
ggplot(data = patientinfo, aes(x=confirmed_date, y=daily_cases)) +
  geom_line(colour="black") +
  geom_line(data = time, colour="green") +
  labs(x="Time", y="Cases per Day")

# Create data table with the main spreader events
super_spreader <- data.table(Event_Name= c("Itaewon Club", "Richway", "Guro-gu Call Center", "Shincheonji Church", "Coupang Logistics Center"), 
                             Date=c("2020-05-01", "2020-06-01", "2020-03-05", "2020-02-16", "2020-05-20"))
# Convert Date column as date, so we can use it in one graph with our Patientinfo & daily cases
super_spreader[, Date:=as.Date(Date)]

# Look for the first patient infected by Covid-19 that was associated witht he Corona virus
# Are we able to find the 60 year old something patient number 31?
df = subset(patientinfo, select = -c(country, province))
latest_dates <- df %>% group_by(infection_case) %>% top_n(1, confirmed_date)
earliest_dates <- setDT(df)[, .SD[which.min(confirmed_date)], by = infection_case]

#let's look for the row with the min value for the Shincheonji church
earliest_dates %>% filter(infection_case == "Shincheonji Church")
#turns out we are! Patient Number 31, in her 60s was diagnosed with Covid on Tuesday, 18th Feb 2020

#Let's dive a bit deeper and plot the Daily Covid cases associated with the Church against the
#overall daily cases

#Include sidenote, that Patientinfo is a representable subset of time dataset (is it though?)
ggplot(data = patientinfo, aes(x=confirmed_date, y=daily_cases)) + geom_line(colour="black") +
  geom_line(data = subset(patientinfo, infection_case=="Shincheonji Church"), colour="red") +
  geom_vline(data = subset(super_spreader, Event_Name=="Shincheonji Church"),
             aes(xintercept = Date, colour = "Patient 31 visiting Shincheonji Church (Sunday,16th Feb 2020)")) +
  geom_vline(data = subset(earliest_dates, infection_case == "Shincheonji Church"),
             aes(xintercept = confirmed_date, colour = "Patient 31 confirmed as Covid-19 positive (Tuesday, 18th Feb 2020")) +
  labs(x="Time", y="Cases per Day", title = "Patient 31 and the Shincheonji Covid-Outbreak") +
  theme(legend.position="bottom", legend.title = element_blank())

#Okay that looks like a lot of people that were infected within this church. But is this seriously
#tracable back to only one person? Isn't it possible that there were like 5-10 infected people?

#Well, let's see whether we are able to trace back all the connections that were infected
#by patient 31, shall we?








#Ok wow, this is huge. But still, if all of this happened within in Church in Daegu why
#did cases rose all over the country? I think it is a bit harsh to blame only the church
#for a pandemic that - after all - had global scale. What happened in South Korea wasn't
#a singular incident, you know?

#Yes sure and no one is saying that the Church was responsible for the virus in general
#but we had cases under control and without the church allowing patient 31 to come to their
#event on Sunday Feb 16th, the situation in South Korea could have been so much better!
#Because peole travel, you know? The people that got infected spread all over SK!
#Let me show you:

#subsetting to remove empty values (no coordinates)
case_old <- case_old[longitude != "-"]
# change data type from character to double
case_old[, latitude := as.double(latitude)][, longitude := as.double(longitude)]
head(case)

# plot coordinates with cases, different size
ggplot(case_old, aes(x=longitude, y=latitude)) +
  geom_point(aes(size=confirmed, col=province)) +
  scale_x_continuous(labels = scales::comma)+  #to show comma numbers
  scale_y_continuous(labels = scales::comma)+  # to show comma numbers
  labs(title = "Confirmed Covid-cases mapped")

#Plotting the world, especially South Korea
world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot(data = world) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map")
# How can we zoom this in on South Korea in a way that it matches the dots we created above?
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(125.14, 130.15), ylim = c(33.02, 38.98), expand = FALSE)

#Creating datatable with spreader locations
spreader_event_locations <- data.frame(Event_Name = c("Shincheonji Church", "Itaewon Clubs", "Richway", "Guro-gu Call Center", "Coupang Logistics Center"), 
                                       City = c("Daegu", "Seoul", "Seoul", "Guro-gu", "Gyeonggi"), 
                                       Longitude = c(128.600006, 127.024612, 127.024612, 126.8502, 127.2500), 
                                       Latitude = c(35.866669, 37.532600, 37.532600 ,37.49447,37.5000))

#Showing a first version of the graph
ggplot(data = world) +
  geom_sf(fill= "antiquewhite") +
  geom_point(data = case, aes(x=longitude, y=latitude, size = confirmed, color = province)) +
  geom_text_repel(data = spreader_event_locations, aes(x = Longitude, y = Latitude, label = Event_Name), 
                  fontface = "bold", nudge_x = c(1, -2, 2, 2, -1), 
                  nudge_y = c(0.5, -0.5, 0.5, 0.5, -0.5)) +
  coord_sf(xlim = c(125.14, 130.15), ylim = c(33.02, 38.98), expand = FALSE) +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.2), panel.background = element_rect(fill = "aliceblue")) +
  labs(title = "Confirmed Covid-cases mapped")


