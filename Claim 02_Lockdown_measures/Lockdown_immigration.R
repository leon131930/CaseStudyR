library(magrittr)
library(data.table)
library(ggplot2)
library(ggpubr)

# import files: Time, Policy
time <- fread("./extData/Time.csv")
policy <- fread("./extData/Policy.csv")

#total cases in Korea per day
time[,confirmed_date:=as.Date(date,"%d/%m/%y")]



immigration_policy <- policy[type == "Immigration"]

IP_Special <- immigration_policy[gov_policy == "Special Immigration Procedure"]
IP_14Day <- immigration_policy[gov_policy == "Mandatory 14-day Self-Quarantine"]
IP_ManTest <- immigration_policy[gov_policy == "Mandatory Self-Quarantine & Diagonostic Tests"]

immigration_plot <- ggplot(time, aes(x=confirmed_date, y=daily_cases)) + geom_line() +
  
  geom_vline(data = IP_Special, aes(xintercept = start_date, 
                                    color = "Immigration\nprocedure"), 
             linetype = "longdash", show.legend = TRUE)+
  geom_vline(data = IP_14Day, aes(xintercept = start_date, 
                                  color = "Self\nquarantine"), 
             linetype = "longdash", show.legend = TRUE)+
  geom_vline(data = IP_ManTest, aes(xintercept = start_date, 
                                    color = "Diagnostic\nTest US"), 
             linetype = "longdash", show.legend = TRUE)  +
  labs(x = "confirmed date", y = "daily cases") + 
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) 

#1. Run plot in file: Lockdown_overseas
#2. Run plot in Lockdown_immigration (current file)
#Execute code below to get both plots next to each other
plot_immigration <- ggarrange(immigration_plot, overseas_plot,
          ncol = 2, nrow = 1)
annotate_figure(plot_immigration,
                top = text_grob("International measures", 
                                color = "black", size = 14))
  