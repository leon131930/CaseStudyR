### Region visualization ###

install.packages("maps")
library(magrittr)
library(data.table)
library(ggplot2)
library(maps)

# importing Case
case <- fread("./extData/Case.csv")

#subsetting to remove empty values
case <- case[longitude != "-"]

# change data type to double
case[, latitude := as.double(latitude)][, longitude := as.double(longitude)]


# plot coordinates with cases, different size
ggplot(case, aes(x=latitude, y=longitude)) +
  geom_point(aes(size=confirmed))

#use scale_y_continuous() to display coordinates better??