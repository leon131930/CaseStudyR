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

# change data type to integer
case[, latitude := as.double(latitude)][, longitude := as.double(longitude)]



# plot
ggplot(case, aes(x=latitude, y=longitude)) +
  geom_point(aes(size=confirmed))

  
