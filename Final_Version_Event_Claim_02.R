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

# Creating copies from those dataframes for super spreader claim - now our claim 02
case_02 <- copy(case)
case_old_02 <- copy(case_old)
patientinfo_02 <- copy(patientinfo)
time_02 <- copy(time)
region_02 <- copy(region)


#####################
#Let's first have a look at all the Covid Cases spread throughout the country. Where there some
#areas the cases were really high or was it a more "even" spread throughout the whole country?

# Loading South Korea Data
south_korea <- getData("GADM", country = "South Korea", level = 1)
class(south_korea)

# Fortify shape file to dataframe format
south_korea.f <- fortify(south_korea, region = "NAME_1")
class(south_korea.f)
head(south_korea.f)

# Changing Case Data so we can map the longitudes and latitudes
# subsetting to remove empty values (no coordinates)
case_02_withcoordinates <- case_02[longitude != "-"]

# change data type from character to double
case_02_withcoordinates[, latitude := as.double(latitude)][, longitude := as.double(longitude)]

# Plotting map of South Korea with different sized dots according to number of cases
ggplot() +
  geom_polygon(data = south_korea.f, aes(x = long, y = lat, group = group), fill = "lightgrey",
               colour = "grey", size = 0.25) +
  geom_point(data = case_02_withcoordinates, aes(x=longitude, y=latitude, size = confirmed),
             color = "violetred") +
  scale_size_continuous(range = c(2, 5)) +
  coord_map() + labs(title = "Covid-Cases spread over South Korea")

# Seems like there were two hot-spots indeed! Probably some major cities? Seoul? Also we heard, that 
# there was quite some rumor about a church-event causing the spread in South Korea. Could that
# be the reason for the larger outbreak in the south-east?

# Researching locations for Seaoul as well as the Church in Daegu on Google Maps
locations_seoul_church <- data.frame(Location = c("Seoul", "Shincheonji Church"),
                                     City = c("Seoul", "Daegu"),
                                     Longitude = c(127.024612,128.600006),
                                     Latitude = c(37.532600,35.866669))

ggplot() +
  geom_polygon(data = south_korea.f, aes(x = long, y = lat, group = group), fill = "lightgrey",
               colour = "grey", size = 0.25) +
  geom_point(data = case_02_withcoordinates, aes(x=longitude, y=latitude, size = confirmed),
             color = "violetred") +
  scale_size_continuous(range = c(2, 5)) +
  geom_text_repel(data = locations_seoul_church, aes(x = Longitude, y = Latitude, label = Location), 
                  fontface = "bold", nudge_x = c(1, -2), 
                  nudge_y = c(0.5, -0.5)) +
  coord_map() + labs(title = "Covid-Cases spread over South Korea")

# Alright, around the location of the church there seemed indeed to be a lot of cases. But this 
# doesn't necessarily mean that those cases are actually all related to the church. Could also be 
# a highly popular area where a lot of foreign people from other countries travel to. Right?

#####################

# To double check the church theory we will use the case data table 
# to first determine, what caused the Covid spread in South Korea to go wild. Meaning, which
# infection reason caused by far the most infections?

# aggregate confirmed cases so we can see which infection reason caused the most cases
overview_infection_cases <- aggregate(x= case_02$confirmed,by= list(case_02$infection_case),FUN= sum)
overview_infection_cases

# Rename column names to make referencing easier
overview_infection_cases <- overview_infection_cases %>% rename(infection_reason = Group.1,people_confirmed = x)

# Drop etc cases because we cannot connect them to a specific reason & plot the top 10 cases 
#without "etc"
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

###DISCLAIMER###
# Disclaimer: we will now use the patientinfo data table because in this datatable we have information for both, the
# number of daily cases as well as the infection reason for those cases. Unfortunately we don't have an infection
# reason within the time datatable. We are not able to merge both tables because the time table provice cumulated 
# information. Since the time datatable includes a lot more daily cases than the patientinfo
# datatable we will first have a look, whether the patientinfo dataset can be seen as a subset of the time dataset.

##TALK ABOUT THIS WITH THE OTHERS!
#Let's see whether it makes more sense to put both graph lines into one graph - we should talk about this!
#ggplot(data = patientinfo_02, aes(x=confirmed_date, y=daily_cases)) +
#geom_line(colour="black") +
#geom_line(data = time_02, colour="green") +
#labs(x="Time", y="Cases per Day")

# Graph showing total daily cases over time, patientinfo database
patientinfo_02[, daily_cases := .N, by = confirmed_date]
total_cases_patientinfo <- ggplot(data = patientinfo_02, aes(x=confirmed_date, y=daily_cases)) +
  geom_line(colour="black") + labs(x="Time", y="Cases per Day")
total_cases_patientinfo

# Graph showing total daily cases over time, time database, total cases in Korea per day
time_02[,confirmed_date:=as.Date(date,"%d/%m/%y")]
time_02[, confirmed_date:= as.IDate(confirmed_date)]
total_cases_time <- ggplot(time_02, aes(x=confirmed_date, y=daily_cases)) + geom_line(colour="black")+
  labs(x="Time", y="Cases per Day")
total_cases_time

#Let's put both graphs next to each other
library(ggpubr)
ggarrange(total_cases_patientinfo, total_cases_time, ncol=2, nrow=1)

# Due to the quite similar progression of the two graphs, we will assume that we can treat
# patientinfo as a valid, representable subset of the time datatable
###DISCLAIMER END###

# Alright, seems like we actually found a bad guy here. Infection cases related to the Shincheonji
# Church are in fact pretty high. Even higher as "Contact with patient" and "Oversees inflow"
# Combined. Let's have a closer look at the Shincheonji Church Events. How did it start? And
# how did it effect the following daily cases?

# Plotting Daily Cases related to the Church together with overall Daily Cases
ggplot(data = patientinfo_02, aes(x=confirmed_date, y=daily_cases)) + geom_line(aes(colour="black")) +
  geom_line(data = subset(patientinfo_02, infection_case=="Shincheonji Church"), 
            aes(colour = "red")) +
  labs(x="Time", y="Cases per Day", title = "Patient 31 and the Shincheonji Covid-Outbreak") +
  scale_color_discrete(name = "Legend", labels = c("Overall Daily Cases", "Church related Cases")) +
  theme(legend.position="bottom", legend.title = element_blank())

# Ok, seems like the church actually had a serious impact especially at the beginning.
# But why did it start in the church in the first place? Was there one certain trigger?

# Look for the first patient infected by Covid-19 that was associated with the Shincheonji Church
df_church = subset(patientinfo_02, select = -c(country, province))
latest_dates <- df_church %>% group_by(infection_case) %>% top_n(1, confirmed_date)
earliest_dates <- setDT(df_church)[, .SD[which.min(confirmed_date)], by = infection_case]

# In the news everyone is talking about this mysterious patient 31, are we able to find her?
# Let's look for the row with the min value for the Shincheonji church
earliest_dates %>% filter(infection_case == "Shincheonji Church")

# Turns out we are! Patient Number 31, in her 60s was diagnosed with Covid on Tuesday, 18th Feb 2020
# and she has an insane amount of contact numbers! No wonder Shincheonji Church cases were so
# high... imagine those 1160 contacts infecting even more people!

# Let's quickly plot this over time and see when patient 31 was going to the church and at what point
# she was diagnosed.

# Create a data table with the main spreader event: Shincheonji Church.
# According to the New York times the event in question took place on Feb 16th 2020
super_spreader_church <- data.table(Event_Name= c("Shincheonji Church"), 
                                    Date=c("2020-02-16"))

# Convert Date column as date, so we can use it in one graph with our Patientinfo & daily cases
super_spreader_church[, Date:=as.Date(Date)]

# Plot everything over time
ggplot(data = patientinfo_02, aes(x=confirmed_date, y=daily_cases)) + geom_line(colour="black") +
  geom_line(data = subset(patientinfo_02, infection_case=="Shincheonji Church"), colour = "red") +
  geom_vline(data = super_spreader_church,
             aes(xintercept = Date, colour = "Patient 31 visiting Shincheonji Church (Sunday,16th Feb 2020) - acc. to NY Times")) +
  geom_vline(data = subset(earliest_dates, infection_case == "Shincheonji Church"),
             aes(xintercept = confirmed_date, colour = "Patient 31 confirmed as Covid-19 positive (Tuesday, 18th Feb 2020")) +
  labs(x="Time", y="Cases per Day", title = "Patient 31 and the Shincheonji Covid-Outbreak") +
  theme(legend.position="bottom",legend.direction="vertical", legend.title = element_blank())

# Seems like one person can kick-start an enourmus outbreack real quick.. and not in a good way!

# But how can this one event or this one person be responsible for the outbreak in a whole country?
# Like obviously 1160 contacts is HUGE, but if this happens in one city it doesn't necessarily mean
# that the whole outbreak was caused by this one event, does it?

# Well, people travel, you know? Let's have a look how many people related to the Church were 
# tested positive for Covid in regions far away from the Church

# Filter case datatable for Church cases
church_cases_around_SK <- case_02%>% filter(infection_case=="Shincheonji Church")

#loose the ID column 
church_cases_around_SK <- subset(church_cases_around_SK, select = -c(case_id))

# Loose the row with the church itself
church_cases_outside_daegu <- church_cases_around_SK[!(church_cases_around_SK$province=="Daegu"),]

# Now plot an overview of these cases
# Look for a cool red color with grep("red", colors(), value=T)
ggplot(data = church_cases_outside_daegu,
       aes(x=confirmed,
           y=reorder(province,confirmed))) +
  geom_bar(stat="identity", width = 0.7, fill = "mediumvioletred") +
  geom_text(aes(label=confirmed), hjust=-.1, color="black", size=3) +
  labs(x="Number of Cases", y="Region", 
       title = "Shincheonji Church related Cases OUTSIDE of Daegu")

# Alright, indeed a couple of people. Gyeongsangbuk-do is probably close to Daegu though, right?
# Are we able to plot this on a map showing where exactly this occured?

# Create a table where region and city are the same in order to get coordinates for regions
church_regions <- region_02%>% filter(province==city)

# Now loose all the columns that we don't need
church_regions <- subset(church_regions, select = -c(code, elementary_school_count, kindergarten_count, university_count, academy_ratio, elderly_population_ratio, elderly_alone_ratio, nursing_home_count))
church_regions_without_city <- subset(church_regions, select = -c(city))
church_cases_around_SK

# Loose the columns we don't need in the church_cases table to prep for full join
church_cases_for_join <- subset(church_cases_around_SK, select = -c(city, group, infection_case, longitude, latitude))

# Merging the two tables with Inner join to only get the regions we actually have confirmed cases at
plotting_table <- merge(church_regions_without_city, church_cases_for_join, by = "province")
plotting_table

# Change data type to double
plotting_table[, latitude := as.double(latitude)][, longitude := as.double(longitude)]

# Plotting the graph for the first time
ggplot() +
  geom_polygon(data = south_korea.f, aes(x = long, y = lat, group = group), fill = "lightgrey",
               colour = "grey", size = 0.25) +
  geom_point(data = plotting_table, aes(x=longitude, y=latitude, size = confirmed, color = "Covid Cases related to the Church")) + 
  coord_map() + labs(title = "Church-Cases spread over South Korea")

# Doing that again but withouht Daegu bc it fucks up the point sizes
plotting_table_witout_daegu <- plotting_table[!(plotting_table$province=="Daegu")]

# Plotting map again and making it a bit more aestetic at the same time
# DISCLAIMER #
# In order to be able to map the geom_points onto the map, we merged the confirmed cases for the 
# whole region with the main city (so for city Seoul the total number actually relate to the whole
# Seoul region.)
ggplot() +
  geom_polygon(data = south_korea.f, aes(x = long, y = lat, group = group), fill = "lightgrey",
               colour = "grey", size = 0.25) +
  geom_point(data = plotting_table_witout_daegu, aes(x=longitude, y=latitude, 
                                                     size = confirmed),
             color = "darkred",
             alpha = 0.7,) +
  scale_size_continuous(range = c(3, 7)) +
  geom_text_repel(data = locations_seoul_church, 
                  aes(x = Longitude, y = Latitude, label = Location), 
                  fontface = "bold", nudge_x = c(0.1, -2), 
                  nudge_y = c(0.4, -0.4)) +
  coord_map() + labs(title = "Church-related-Cases spread over South Korea (excl. Daegu)")

### ANALYSIS ###
# It seems like the Event in the church indeed has huge effects on the Covid spread throughout South
# Korea and it highlights the importance of policies such as travel restrictions making sure that
# in case such an event happens, the infected people do not travel to a different location, taking
# the virus with them.
# However in terms of the claim one cannot say for sure, that the Church was the main reason for 
# the severe Covid outbreak in South Korea there are simply too many other variables that influenced
# this. For one, there were multiple spreader events, not only the Church one (see the Bar Chart "Top
# 10 Infection Reasons"). Additionally, we do not know for sure, that Patient Number 31 was the 
# only person infected at that Church event. While she was the first one to be tested positive
# we do not know where there was a 2nd, 3rd or 4th person infected at that church event that 
# simply wasn't tested positive or didn't get tested at all. And while the case datatable states
# that Patient 31 hat a total of 1160 contacts, we are only able to trace back a total of 17 direct
# connections via the patientinfo datatable.


# To sum it up, YES, the Church Event had a serious impact on the Covid spread in South Korea.
# However we are not able to confirm the claim 100% stating that it was the number one reason
# the outbreak was so severe. 
###############################
