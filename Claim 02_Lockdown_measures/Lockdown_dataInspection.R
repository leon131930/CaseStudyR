#Inspect data for lockdown measures


#Get total nb of measures per policy type + plot it into a bar chart
amount_perPolicy <- policy[ , .N, by = "type"]

ggplot(amount_perPolicy, aes(x = N , y = reorder(type,N))) + 
  geom_bar(stat= "identity", width = .9) 


# plot total number of infection cases from Jan - Jul per day 
time[,confirmed_date:=as.Date(date,"%d/%m/%y")]
ggplot(time, aes(x=confirmed_date, y=daily_cases)) +
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

