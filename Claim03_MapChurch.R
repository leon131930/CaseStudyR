#Goal: Show a map that displays the different Church related cases
library(magrittr)
library(data.table)
library(ggplot2)
library(maps)
library(ggrepel)
library(raster)
library(rgeos)
library(maptools)
library(scales)
library(dplyr)
library(scales)
library(ggmap)

# importing Case
case <- fread("./extData/Case.csv")
patientinfo <- fread("./extData/PatientInfo.csv")
time <- fread("./extData/time.csv")
case_old <- fread("./extData/Case.csv")
region <- fread("./extData/Region.csv")

# Loading South Korea Data
south_korea <- getData("GADM", country = "South Korea", level = 1)
class(south_korea)

# Fortify shape file to dataframe format
south_korea.f <- fortify(south_korea, region = "NAME_1")
class(south_korea.f)
head(south_korea.f)

########################
# PLAYING IT SAFE - SHOWING COVID CASES THROUGHOUT SOUTH KOREA - CHURCH CASES ARE HIGH!

# Changing Case Data so we can map the longitudes and latitudes
#subsetting to remove empty values (no coordinates)
case <- case[longitude != "-"]

# change data type from character to double
case[, latitude := as.double(latitude)][, longitude := as.double(longitude)]
head(case)

# Adding the locations from the different spreader events
spreader_event_locations <- data.frame(Event_Name = c("Shincheonji Church", "Itaewon Clubs", "Richway", "Guro-gu Call Center", "Coupang Logistics Center"), 
                                       City = c("Daegu", "Seoul", "Seoul", "Guro-gu", "Gyeonggi"), 
                                       Longitude = c(128.600006, 127.024612, 127.024612, 126.8502, 127.2500), 
                                       Latitude = c(35.866669, 37.532600, 37.532600 ,37.49447,37.5000))

# Plotting map of South Korea with different sized dots according to number of cases
ggplot() +
  geom_polygon(data = south_korea.f, aes(x = long, y = lat, group = group), fill = "lightgrey",
               colour = "grey", size = 0.25) +
  geom_point(data = case, aes(x=longitude, y=latitude, size = confirmed, color = "Covid Cases")) + 
  geom_text_repel(data = spreader_event_locations, aes(x = Longitude, y = Latitude, label = Event_Name), 
                fontface = "bold", nudge_x = c(1, -2, 2, 2, -1), 
                nudge_y = c(0.5, -0.5, 0.5, 0.5, -0.5)) +
  coord_map() + labs(title = "Shincheonji Church Outbreak")

##########################
# ARE WE ABLE TO CREATE SOME KIND OF "HEAT MAP" THAT SHOWS CHURCH RELATED CASES THROUGHOUT SOUTH KOREA?

# Delete all rows that have nothing to do with the Church case
cases_church <- case[!(case$infection_case!="Shincheonji Church")]

# Loose all columns that have nothing to do with the Church case
cases_church_sm <- subset(cases_church, select = -c(case_id, city, group, infection_case, latitude, longitude))
class(cases_church_sm$province)
class(sk_region.shp.f$id)

# Rename column name in cases_church_sm so we can merge both dataframes
cases_church_sm <- cases_church_sm %>% rename(id = province)

# Merging both dataframes
merge.south_korea_zwei <- merge(south_korea.f, cases_church_sm, by = "id")
head(merge.south_korea_zwei)

# Trying a basic plot, fingers crossed
ggplot() +
  geom_polygon(data = merge.south_korea_zwei, aes(x = long, y = lat, group = group, 
                                        fill = confirmed),
               color = "black", size = 0.25) +
  coord_map()

# Trying nicer plot
ggplot() + geom_polygon(data = merge.south_korea_zwei, 
                        aes(x = long, y = lat, group = group, 
                                                   fill = confirmed), 
                        color = "black", size = 0.25) + coord_map() + 
  scale_fill_distiller(name = "confirmed",palette = "YlOrRd", breaks = pretty_breaks(n = 5)) +
  theme_nothing(legend = TRUE) + labs(title = "Shincheonji Church Outbreak")

#######################
