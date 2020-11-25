library(magrittr)
library(data.table)
library(ggplot2)


# importing Case, PatientInfo, Time
case <- fread("./extData/Policy.csv")
patientinfo <- fread("./extData/PatientInfo.csv")
time <- fread("./extData/Time.csv")


#patient info 
nb_infectionCases <- patientinfo[, .N, by = "infection_case"]

setnames(nb_infectionCases, "N", "sum")


#plot histogram 
ggplot(nb_infectionCases[1:10], aes(x = sum , y = reorder(infection_case,sum))) + 
  geom_bar(stat= "identity", width = .9) 

nb_infectionCases
