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
library(ggpubr)

# Importing the external data we will need
case <- fread("./extData/Case.csv")
patientinfo <- fread("./extData/PatientInfo.csv")
time <- fread("./extData/time.csv")
case_old <- fread("./extData/Case.csv")
region <- fread("./extData/Region.csv")

#####################
#Let's first have a look at all the Covid-Cases spread throughout the country. Where there some
#areas the cases were really high or was it a more "even" spread throughout the whole country?

# Loading South Korea Data
south_korea <- getData("GADM", country = "South Korea", level = 1)
class(south_korea)

# Fortify shape file to dataframe format
south_korea.f <- fortify(south_korea, region = "NAME_1")
class(south_korea.f)
head(south_korea.f)

# Changing Case Data so we can map the longitudes and latitudes
#subsetting to remove empty values (no coordinates)
case <- case[longitude != "-"]

# change data type from character to double
case[, latitude := as.double(latitude)][, longitude := as.double(longitude)]
head(case)

# Plotting map of South Korea with different sized dots according to number of cases
ggplot() +
  geom_polygon(data = south_korea.f, aes(x = long, y = lat, group = group), fill = "lightgrey",
               colour = "grey", size = 0.25) +
  geom_point(data = case, aes(x=longitude, y=latitude, size = confirmed, color = "Covid Cases")) + 
  coord_map() + labs(title = "Covid-Cases spread over South Korea")

# Seems like there were two hotspots indeed! Probably some major cities? Seoul? Also we heard, that 
# there was quite some rumor about a church-event causing the spread in South Korea. Could that
# be the reason for the larger outbreak in the south-east?

# minor thing: we do have these things in our data tables, we don't have to do that manually!
# DOUBLE CHECK THIS AND FIX IT BEFORE THE FINAL SUBMISSION!
locations_seoul_church <- data.frame(Location = c("Seoul", "Shincheonji Church"),
                                     City = c("Seoul", "Daegu"),
                                     Longitude = c(127.024612,128.600006),
                                     Latitude = c(37.532600,35.866669))

ggplot() +
  geom_polygon(data = south_korea.f, aes(x = long, y = lat, group = group), fill = "lightgrey",
               colour = "grey", size = 0.25) +
  geom_point(data = case, aes(x=longitude, y=latitude, size = confirmed, color = "Covid Cases")) + 
  geom_text_repel(data = locations_seoul_church, aes(x = Longitude, y = Latitude, label = Location), 
                  fontface = "bold", nudge_x = c(1, -2), 
                  nudge_y = c(0.5, -0.5)) +
  coord_map() + labs(title = "Covid-Cases spread over South Korea")

# Alright, around the location of the church there seemed to be a lot of cases. But this doesn't
# nexessarily mean that those cases are actually all related to the church. Right?

#####################

# To double check the church theory we will use the case data table 
# to first determine, what caused the Covid-spread in South Korea to go wild. Meaning, which
# infection reason caused by far the most infections?

# aggregate confirmed cases so we can see which infection reason caused the most cases
overview_infection_cases <- aggregate(x= case$confirmed,by= list(case$infection_case),FUN= sum)
overview_infection_cases

# Rename column names to make referencing easier
overview_infection_cases <- overview_infection_cases %>% rename(infection_reason = Group.1,people_confirmed = x)
overview_infection_cases

# Determine top 15 reasons and plot them as a bar chart
top_15_cases <- top_n(overview_infection_cases, 15, people_confirmed)
top_15_cases

ggplot(data = top_15_cases, 
       aes(x=people_confirmed,
           y=reorder(infection_reason,people_confirmed))) +
  geom_bar(stat="identity", width = 0.7, fill = "lightblue") +
  geom_text(aes(label=people_confirmed), hjust=-.1, color="black", size=3) +
  labs(x="Number of Cases", y="Infection Reason", title = "Top 15 Infection Reasons")

# Top 10 cases without "etc"
overview_infection_cases
overview_without_etc <- overview_infection_cases[!(overview_infection_cases$infection_reason=="etc"),]

top_10_cases_without_etc <- top_n(overview_without_etc, 10, people_confirmed)

# Plot these cases
ggplot(data = top_10_cases_without_etc, 
       aes(x=people_confirmed,
           y=reorder(infection_reason,people_confirmed))) +
  geom_bar(stat="identity", width = 0.7, fill = "lightblue") +
  geom_text(aes(label=people_confirmed), hjust=-.1, color="black", size=3) +
  labs(x="Number of Cases", y="Infection Reason", 
       title = "Top 10 Infection Reasons")

########################

# Alright, seems like we actually found a bad guy Infection cases related to the Shincheonji
# Church are in fact pretty high. Even higher as "Contact with patient" and "Oversees inflow"
# Combined. Let's have a closer look at the Shincheonji Church Events. How did it start? And
# how did it effect the following daily cases?

###DISCLAIMER
#Disclaimer: we will use the patientinfo data table because in this datatable we have information for both, the
#number of daily cases as well as the infection reason for those cases. Unfortunately we don't have an infection
#reason within the time datatable. Since the time datatable includes a lot more daily cases than the patientinfo
#datatable we will first have a look, whether the patientinfo dataset can be seen as a subset of the time dataset.

##TALK ABOUT THIS WITH THE OTHERS!
#Let's see whether it makes more sense to put both graph lines into one graph - we should talk about this!
#ggplot(data = patientinfo, aes(x=confirmed_date, y=daily_cases)) +
#geom_line(colour="black") +
#geom_line(data = time, colour="green") +
#labs(x="Time", y="Cases per Day")

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
#library(ggpubr)
ggarrange(total_cases_patientinfo, total_cases_time, ncol=2, nrow=1)

###DISCLAIMER END

# Alright, seems like we actually found a bad guy Infection cases related to the Shincheonji
# Church are in fact pretty high. Even higher as "Contact with patient" and "Oversees inflow"
# Combined. Let's have a closer look at the Shincheonji Church Events. How did it start? And
# how did it effect the following daily cases?

#Include sidenote, that Patientinfo is a representable subset of time dataset (is it though?)
ggplot(data = patientinfo, aes(x=confirmed_date, y=daily_cases)) + geom_line(colour="black") +
  geom_line(data = subset(patientinfo, infection_case=="Shincheonji Church"), colour="red") +
  geom_vline(data = subset(locations_seoul_church, Location=="Shincheonji Church"),
             aes(xintercept = Date, colour = "Patient 31 visiting Shincheonji Church (Sunday,16th Feb 2020)")) +
  geom_vline(data = subset(earliest_dates, infection_case == "Shincheonji Church"),
             aes(xintercept = confirmed_date, colour = "Patient 31 confirmed as Covid-19 positive (Tuesday, 18th Feb 2020")) +
  labs(x="Time", y="Cases per Day", title = "Patient 31 and the Shincheonji Covid-Outbreak") +
  theme(legend.position="bottom", legend.title = element_blank())

#Okay that looks like a lot of people that were infected within this church. But is this seriously
#traceable back to only one person? Isn't it possible that there were like 5-10 infected people?

#Well, let's see whether we are able to trace back all the connections that were infected
#by patient 31, shall we?







##########################
# But why did it start in the church in the first place? Was there one certain trigger?
# In the news everyone is talking about this mysterious patient 31, are we able to find her?

# Look for the first patient infected by Covid-19 that was associated with the Shincheonji Church
# Are we able to find the 60 year old something patient number 31 the news talk about?
df = subset(patientinfo, select = -c(country, province))
latest_dates <- df %>% group_by(infection_case) %>% top_n(1, confirmed_date)
earliest_dates <- setDT(df)[, .SD[which.min(confirmed_date)], by = infection_case]

#let's look for the row with the min value for the Shincheonji church
earliest_dates %>% filter(infection_case == "Shincheonji Church")
#turns out we are! Patient Number 31, in her 60s was diagnosed with Covid on Tuesday, 18th Feb 2020
#and she has an insane amount of contact numbers! No wonder Shincheonji Church cases were so
#high... imagine those 1160 contacts infecting even more people!
















# Create a data table with the main spreader events
super_spreader <- data.table(Event_Name= c("Itaewon Club", "Richway", "Guro-gu Call Center", "Shincheonji Church", "Coupang Logistics Center"), 
                             Date=c("2020-05-01", "2020-06-01", "2020-03-05", "2020-02-16", "2020-05-20"))
# Convert Date column as date, so we can use it in one graph with our Patientinfo & daily cases
super_spreader[, Date:=as.Date(Date)]

# Look for the first patient infected by Covid-19 that was associated with the Corona virus
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
#traceable back to only one person? Isn't it possible that there were like 5-10 infected people?

#Well, let's see whether we are able to trace back all the connections that were infected
#by patient 31, shall we?

#how many people were infected by patient 31
flights[, .N, by = 'AIRLINE']


patientinfo[, .N, by = 'infected_by']
sum(patientinfo$infected_by == "1200000031")
sum(patientinfo$infected_by == "1200012411")


patientinfo2 = copy(patientinfo)
patientinfo2

patientinfo2 <- subset(patientinfo2, select = -c(sex, age, country, province, city, contact_number, symptom_onset_date, confirmed_date, released_date, deceased_date, state, daily_cases))


patientinfo2 <- subset(patientinfo2, select = -c(infection_case))
patientinfo2

from <- patientinfo2 %>%
  distinct(infected_by) %>%
  rename(label = infected_by)

to <- patientinfo2 %>%
  distinct(patient_id) %>%
  rename(label = patient_id)



#Ok wow, this is huge. But still, if all of this happened within in Church in Daegu why
#did cases rose all over the country? I think it is a bit harsh to blame only the church
#for a pandemic that - after all - had national scale. What happened in South Korea wasn't
#a singular incident, you know?

#Yes sure and no one is saying that the Church was responsible for the virus in general
#but we had cases under control and without the church allowing patient 31 to come to their
#event on Sunday Feb 16th, the situation in South Korea could have been so much better!
#Because people travel, you know? The people that got infected spread all over SK!
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


