library(magrittr)
library(data.table)
library(ggplot2)


# import files: Case, PatientInfo, Time
policy <- fread("./extData/Policy.csv")
patientinfo <- fread("./extData/PatientInfo.csv")
time <- fread("./extData/Time.csv")


#get total number of infection cases due to immigration
contact_overseas <- patientinfo[infection_case == "overseas inflow", 
                                 .N, by = "confirmed_date"]

#get subsets only for 3 different immigration policies 
#IP_Special, IP_14Day, IP_ManTest
immigration_policy <- policy[type == "Immigration"]

IP_Special <- immigration_policy[gov_policy == 
                                   "Special Immigration Procedure"]
IP_14Day <- immigration_policy[gov_policy == 
                                 "Mandatory 14-day Self-Quarantine"]
IP_ManTest <- immigration_policy[gov_policy == 
                                   "Mandatory Self-Quarantine & Diagonostic Tests"]

#plot nb infection cases of immigration per day + start date of 
#immigration policies

ggplot(contact_overseas, aes(x=confirmed_date, y=N)) + geom_line() +
  geom_vline(data = IP_Special, 
             aes(xintercept = start_date, color = "Immigration\nprocedure"), 
             linetype = "longdash", show.legend = TRUE)+
  geom_vline(data = IP_14Day, 
             aes(xintercept = start_date, color = "Self quarantine"), 
             linetype = "longdash", show.legend = TRUE)+
  geom_vline(data = IP_ManTest, 
             aes(xintercept = start_date, color = "Diagnostic Test\nUS"), 
             linetype = "longdash", show.legend = TRUE) +
  labs(x = "confirmed date", y = "nb. infection cases (immigration)")+
  theme(legend.position="bottom")
