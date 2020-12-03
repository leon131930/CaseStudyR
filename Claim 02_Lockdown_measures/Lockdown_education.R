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
  geom_text(aes(x = as.Date('2020-03-02'), y = 750, label = "School delay"),
    size = 3, vjust = 0, hjust = 0, color = "black")+
  
  geom_rect(aes(xmin=as.Date('2020-04-09'),xmax=as.Date('2020-06-08'),
                ymin=740,ymax=743,color="Online classes"))+  
  geom_text(aes(x = as.Date('2020-04-09'), y = 750, label = "Online classes"),
            size = 3, vjust = 0, hjust = 0, color = "black")+
  
  #social policies 
  
  geom_rect(aes(xmin=as.Date('2020-02-29'),xmax=as.Date('2020-04-19'),
                ymin=840,ymax=843,color="Strong social\ndistancing campaign"))+
  geom_text(aes(x = as.Date('2020-02-29'), 
                y = 850, label = "Strong Social distancing campaign"),
            size = 3, vjust = 0, hjust = 0, color = "black")+
  
  geom_rect(aes(xmin=as.Date('2020-04-19'),xmax=as.Date('2020-06-30'),
                ymin=840,ymax=843,color="Weak social\ndistancing campaign"))+
  geom_text(aes(x = as.Date('2020-05-01'), 
                y = 850, label = "Weak social distancing campaign"),
            size = 3, vjust = 0, hjust = 0, color = "black")+
  
  #administrative policies
  
  geom_rect(aes(xmin=as.Date('2020-05-08'),xmax=as.Date('2020-06-07'),
                ymin=110,ymax=113,color="Close bars\nand clubs"))+
  geom_text(aes(x = as.Date('2020-05-08'), 
                y = 120, label = "Close bars and clubs"),
            size = 3, vjust = 0, hjust = 0, color = "black")+
  geom_rect(aes(xmin=as.Date('2020-05-16'),xmax=as.Date('2020-06-30'),
                ymin=170,ymax=173,color="Local government\nadministrative orders"))+
  geom_text(aes(x = as.Date('2020-05-16'), 
                y = 180, label = "Local government administrative orders"),
            size = 3, vjust = 0, hjust = 0, color = "black")+
  geom_rect(aes(xmin=as.Date('2020-05-21'),xmax=as.Date('2020-06-03'),
                ymin=230,ymax=233,color="Close karaoke"))+
  geom_text(aes(x = as.Date('2020-05-21'), 
                y = 240, label = "Close karaoke"),
            size = 3, vjust = 0, hjust = 0, color = "black")+
  
  labs(x = "confirmed date", y = "daily cases",
       title="Administrative / Education / Social Measures")+
  theme(legend.position="none",plot.title= element_text(hjust=0.5)) 
  