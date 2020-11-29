### Region visualization ###

install.packages("maps")
library(magrittr)
library(data.table)
library(ggplot2)
library(maps)

# importing Case
case <- fread("./extData/Case.csv")

#subsetting to remove empty values (no coordinates)
case <- case[longitude != "-"]

# change data type from character to double
case[, latitude := as.double(latitude)][, longitude := as.double(longitude)]
head(case)

# plot coordinates with cases, different size
ggplot(case, aes(x=latitude, y=longitude)) +
  geom_point(aes(size=confirmed, col=province)) +
  scale_x_continuous(labels = scales::comma)+  #to show comma numbers
  scale_y_continuous(labels = scales::comma)+  # to show comma numbers
  labs(title = "Confirmed Covid-cases mapped")

# Doing plot again with y as latitude and x as longitude
ggplot(case, aes(x=longitude, y=latitude)) +
  geom_point(aes(size=confirmed, col=province)) +
  scale_x_continuous(labels = scales::comma)+  #to show comma numbers
  scale_y_continuous(labels = scales::comma)+  # to show comma numbers
  labs(title = "Confirmed Covid-cases mapped")

# to add here: Map of SouthKorea -> Library to import Map Data? -> Maybe just use Region table for coordinates of cities, and display them on top?
# Possible to increase dot size?

# Loading some additional packages that we might need
library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")

# Displaying world map with ggplot2
ggplot(data = world) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map")
# How can we zoom this in on South Korea in a way that it matches the dots we created above?
# Trying an example from the gulf of Mexico
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97), expand = FALSE)

# Now transferring this to South Korea - exact longitude and latidude coordinates to be found
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(125.14, 130.15), ylim = c(33.02, 38.98), expand = FALSE)
# To be continued --> we still have to look up how we add the layer with our dots from above

