library(magrittr)
library(data.table)
library(ggplot2)


# import files: Case, PatientInfo, Time
policy <- fread("./extData/Policy.csv")
patientinfo <- fread("./extData/PatientInfo.csv")
time <- fread("./extData/Time.csv")

#-----------------------inspect data-------------------------------------------

#total number of infection cases due to immigration
contact_overseas <- patient_Info[infection_case == "overseas inflow", 
                                 .N, by = "confirmed_date"]
contact_overseas[1:10]

#get subsets only for 3 different immigration policies 
#IP_Special, IP_14Day, IP_ManTest
immigration_policy <- policy[type == "Immigration"]

IP_Special <- immigration_policy[gov_policy == "Special Immigration Procedure"]
IP_14Day <- immigration_policy[gov_policy == "Mandatory 14-day Self-Quarantine"]
IP_ManTest <- immigration_policy[gov_policy == "Mandatory Self-Quarantine & Diagonostic Tests"]

#plot nb infection cases of immigration per day + start date of immigration policies

ggplot(contact_overseas, aes(x=confirmed_date, y=N)) + geom_line() +
  geom_vline(data = IP_Special, aes(xintercept = start_date, color = "Immigration\nprocedure"), 
             linetype = "longdash", show.legend = TRUE)+
  geom_vline(data = IP_14Day, aes(xintercept = start_date, color = "Self quarantine"), 
             linetype = "longdash", show.legend = TRUE)+
  geom_vline(data = IP_ManTest, aes(xintercept = start_date, color = "Diagnostic Test\nUS"), 
             linetype = "longdash", show.legend = TRUE) +
  labs(x = "confirmed date", y = "nb. infection cases (immigration)")


#Germany
patientinfo2 <- patientinfo[, .N, by = confirmed_date]
patientinfo2[1:100]

RKI_data <- fread("./extData/DE_InfectionCases.csv")
RKI_data1 <- RKI_data[,c("Berichtsdatum", "Anzahl COVID-19-Fälle")]

RKI_data1 <- RKI_data1[, confirmed_date := as.Date(Berichtsdatum, format ="%d.%m.%y")]

comp_Germany <- merge(RKI_data1, patientinfo2, by = "confirmed_date", all = FALSE)







