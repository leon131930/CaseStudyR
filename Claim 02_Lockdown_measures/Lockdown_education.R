#Education policies 

#import libraries
library(magrittr)
library(data.table)
library(ggplot2)

# import files: Time, RKI data
time <- fread("./extData/Time.csv")
policy <- fread("./extData/Policy.csv")


#total cases in Korea per day
time[,confirmed_date:=as.Date(date,"%d/%m/%y")]
time[, confirmed_date:= as.IDate(confirmed_date)]


#extract education measures from table policy
date_education <- policy[type == "Education"]


#plot start of education policies +
#administration policies +
#social policies
#on infection cases per day within the time frame 

ggplot(time, aes(x=confirmed_date, y=daily_cases)) + geom_line() +
  
  #education policies
  
  geom_rect(aes(xmin=as.Date('2020-03-02'),xmax=as.Date('2020-04-06'),
                ymin=740,ymax=743,color="School delay"))+
  geom_text(aes(x = as.Date('2020-03-02'), y = 760, label = "School delay"),
    size = 3, vjust = 0, hjust = 0, color = "forestgreen")+
  
  geom_rect(aes(xmin=as.Date('2020-04-09'),xmax=as.Date('2020-06-08'),
                ymin=740,ymax=743,color="Online Classes"))+  
  geom_text(aes(x = as.Date('2020-04-09'), y = 760, label = "Online Classes"),
            size = 3, vjust = 0, hjust = 0, color = "red")+
  
  #administation policies 
  
  geom_rect(aes(xmin=as.Date('2020-02-29'),xmax=as.Date('2020-04-19'),
                ymin=840,ymax=843,color="Strong social\ndistancing campaign"))+
  geom_text(aes(x = as.Date('2020-02-29'), y = 860, label = "Strong Social distancing campaign"),
            size = 3, vjust = 0, hjust = 0, color = "blue")+
  
  geom_rect(aes(xmin=as.Date('2020-04-19'),xmax=as.Date('2020-06-30'),
                ymin=840,ymax=843,color="Weak social\ndistancing campaign"))+
  geom_text(aes(x = as.Date('2020-05-01'), y = 860, label = "Weak social distancing campaign"),
            size = 3, vjust = 0, hjust = 0, color = "purple")+
  
  #social policies
  
  geom_rect(aes(xmin=as.Date('2020-05-08'),xmax=as.Date('2020-06-07'),
                ymin=110,ymax=113,color="Close bars\nand clubs"))+
  geom_rect(aes(xmin=as.Date('2020-05-16'),xmax=as.Date('2020-06-30'),
                ymin=120,ymax=123,color="Local government\nadministrative orders"))+
  geom_rect(aes(xmin=as.Date('2020-05-21'),xmax=as.Date('2020-06-03'),
                ymin=130,ymax=133,color="Close karaoke"))+
  
  labs(x = "confirmed date", y = "daily cases")+
  theme(legend.position="bottom")