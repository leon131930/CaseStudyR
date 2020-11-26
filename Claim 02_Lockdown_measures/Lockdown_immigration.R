library(magrittr)
library(data.table)
library(ggplot2)


# import files: Time, policy
time <- fread("./extData/Time.csv")
policy <- fread("./extData/Policy.csv")

#total cases in Korea per day
time[,confirmed_date:=as.Date(date,"%d/%m/%y")]



immigration_policy <- policy[type == "Immigration"]

China <- immigration_policy[detail == "from China"]
All_countries <- immigration_policy[detail == "from all the countries" & gov_policy == "Special Immigration Procedure"]
Days_14 <- immigration_policy[gov_policy == "Mandatory 14-day Self-Quarantine"]
Diagnostic_Tests <- immigration_policy[gov_policy == "Mandatory Self-Quarantine & Diagonostic Tests"]

ggplot(time, aes(x=confirmed_date, y=daily_cases)) + geom_line() +
  geom_vline(data = China, aes(xintercept = start_date, color = "Special immigration\nprocedure (China)"), 
             linetype = "longdash", show.legend = TRUE)+
  geom_vline(data = All_countries, aes(xintercept = start_date, color = "Special immigration\nprocedure (all countries)"), 
             linetype = "longdash", show.legend = TRUE)+
  geom_vline(data = Days_14, aes(xintercept = start_date, color = "Mandatory 14-day\nSelf-Quarantine"), 
             linetype = "longdash", show.legend = TRUE) +
  geom_vline(data = Diagnostic_Tests, aes(xintercept = start_date, color = "Diagonostic tests"), 
             linetype = "longdash", show.legend = TRUE) +
  labs(x = "confirmed date", y = "daily cases") + theme(legend.position="bottom")