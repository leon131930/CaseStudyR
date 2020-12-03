#Technology Policy

#if(!require(devtools)) install.packages("devtools")
#devtools::install_github("kassambara/ggpubr")

#import libraries
library(magrittr)
library(data.table)
library(ggplot2)
library(ggpubr)

# import files: Time, RKI data
time <- fread("./extData/Time.csv")
policy <- fread("./extData/Policy.csv")


#total cases in Korea per day
time[,confirmed_date:=as.Date(date,"%d/%m/%y")]
time[, confirmed_date:= as.IDate(confirmed_date)]



#extract education measures from table policy
tech_start<- policy[type == "Technology"]
tech_start <- tech_start[, c("policy_id", "country","type","end_date", "detail") := NULL]

tech_start_cast <- dcast(tech_start, ... ~ gov_policy, value.var = "start_date")

#plot start of education policies on infection cases per day within
#the time frame 


tech_plot <- ggplot(time, aes(x=confirmed_date, y=daily_cases)) + geom_line() +
  
  geom_vline(data = tech_start_cast, 
             aes(xintercept = `Electronic Wristbands`,
                 color = "Electronic\nWristbands"), 
             linetype = "longdash", show.legend = TRUE)+
  geom_vline(data = tech_start_cast, 
             aes(xintercept = `KI-Pass: Korea Internet - Pass`
                 , color = "KI-Pass"), 
             linetype = "longdash", show.legend = TRUE)+
  geom_vline(data = tech_start_cast, 
             aes(xintercept = `Self-Quarantine Safety Protection App`,
                 color = "Self-Quar.\nApp"), 
             linetype = "longdash", show.legend = TRUE) +

  
  labs(x = "confirmed date", y = "daily cases", 
       title = "Technology measures") +
  theme(plot.title = element_text(hjust=0.5)) +
  labs(x = "confirmed date", y = "daily cases")+
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) 



#1. Run plot in file Lockdown_health
#2. Run plot in Lockdown_technology
#Execute code below to get both plots next to each other

plot_techAndHealth <- ggarrange(tech_plot, health_plot,
          ncol = 2, nrow = 1) 


ggarrange(tech_plot, health_plot)

