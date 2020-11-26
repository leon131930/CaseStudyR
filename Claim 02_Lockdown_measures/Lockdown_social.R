library(magrittr)
library(data.table)
library(ggplot2)


# import files: Time, policy
time <- fread("./extData/Time.csv")
policy <- fread("./extData/Policy.csv")

#total cases in Korea per day
time[,confirmed_date:=as.Date(date,"%d/%m/%y")]




ggplot(time, aes(x=confirmed_date, y=daily_cases)) + geom_line() +
  geom_rect(aes(xmin=as.Date('2020-02-29'),xmax=as.Date('2020-04-19'),ymin=840,ymax=843,color="Strong social\ndistancing campaign"))+
  geom_rect(aes(xmin=as.Date('2020-04-19'),xmax=as.Date('2020-06-30'),ymin=840,ymax=843,color="Weak social\ndistancing campaign"))+
  labs(x = "confirmed date", y = "daily cases")+
  theme(legend.position="bottom")

