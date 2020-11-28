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

# to add here: Map of SouthKorea -> Library to import Map Data? -> Maybe just use Region table for coordinates of cities, and display them on top?
# Possible to increase dot size?


