#Inspect data for lockdown measures

#patient info: total nb of infection cases per type of infection
nb_infectionCases <- patientinfo[, .N, by = "infection_case"]
setnames(nb_infectionCases, "N", "sum")
nb_infectionCases

#plot bar chart of total infection cases per type of infection in descending order 
ggplot(nb_infectionCases[1:10], aes(x = sum , y = reorder(infection_case,sum))) + 
  geom_bar(stat= "identity", width = .9) 


#Get the total number of measures per policy type + plot it into a bar chart
amount_perPolicy <- policy[ , .N, by = "type"]

ggplot(amount_perPolicy, aes(x = N , y = reorder(type,N))) + 
  geom_bar(stat= "identity", width = .9) 


# plot total number of infection cases from Jan - Jul per day 
patientinfo <- patientinfo[, daily_cases := .N, by = confirmed_date]
ggplot(patientinfo, aes(x=confirmed_date, y=daily_cases)) +
  geom_line()



#Further hot spots to inspect in terms of effect of different domestic policies
#+plot: Club, Call Center, Church

contact_club <- patient_Info[infection_case == "Itaewon Clubs", 
                             .N, by = "confirmed_date"]
contact_club[1:10]
ggplot(contact_club, aes(x=confirmed_date, y=N)) + geom_line()

contact_callCenter <- patient_Info[infection_case == "Guro-gu Call Center", 
                                   .N, by = "confirmed_date"]
contact_callCenter[1:10]
ggplot(contact_callCenter, aes(x=confirmed_date, y=N)) + geom_line()

contact_church <- patient_Info[infection_case == "Dongan Church", 
                               .N, by = "confirmed_date"]
contact_church[1:10]
ggplot(contact_church, aes(x=confirmed_date, y=N)) + 
  geom_line()


#plot total cases of type "contact with patient" by date 
contact_patient <- patient_Info[infection_case == "contact with patient", 
                                .N, by = "confirmed_date"]
contact_patient[1:10]
ggplot(contact_patient, aes(x=confirmed_date, y=N)) +
  geom_line()
