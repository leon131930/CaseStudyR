library(magrittr)
library(data.table)
library(ggplot2)


# import files: Time, policy
time <- fread("./extData/Time.csv")
policy <- fread("./extData/Policy.csv")

#total cases in Korea per day
time[,confirmed_date:=as.Date(date,"%d/%m/%y")]




ggplot(time, aes(x=confirmed_date, y=daily_cases)) + geom_line() +
  geom_rect(aes(xmin=as.Date('2020-05-08'),xmax=as.Date('2020-06-07'),ymin=110,ymax=113,color="Close bars\nand clubs"))+
  geom_rect(aes(xmin=as.Date('2020-05-16'),xmax=as.Date('2020-06-30'),ymin=120,ymax=123,color="Local government\nadministrative orders"))+
  geom_rect(aes(xmin=as.Date('2020-05-21'),xmax=as.Date('2020-06-03'),ymin=130,ymax=133,color="Close karaoke"))+
  labs(x = "confirmed date", y = "daily cases")+
  theme(legend.position="bottom")