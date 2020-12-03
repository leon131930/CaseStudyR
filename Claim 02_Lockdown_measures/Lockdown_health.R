#health_policies

health_policy <- policy[type == "Health"]

Kit <- health_policy[detail == "1st EUA"]
Center <- health_policy[detail == "by Local Government"]
Mask <- health_policy[detail == "Public-Sale"]
Quarantine <- health_policy[gov_policy == "Extends Tightened Quarantine Measures"]

#1. Run plot and safe it in health_plot
#2. Go to file "Lockdown_technology" and run the code at the very end to get
#the figure


health_plot <- ggplot(time, aes(x=confirmed_date, y=daily_cases)) + geom_line() +
  geom_vline(data = Kit, aes(xintercept = start_date, 
                             color = "Kit"), 
             linetype = "longdash", show.legend = TRUE)+
  geom_vline(data = Center, aes(xintercept = start_date, 
                                color = "Drive-Through\nCenter"), 
             linetype = "longdash", show.legend = TRUE)+
  geom_vline(data = Mask, aes(xintercept = start_date, 
                                color = "Mask\nDistr."), 
             linetype = "longdash", show.legend = TRUE) +
  geom_vline(data = Quarantine, aes(xintercept = start_date, 
                                    color = "Ext. Quar."), 
             linetype = "longdash", show.legend = TRUE) +
  labs(x = "confirmed date", y = "",title="Health measures") + theme(legend.position="bottom") +
  theme(legend.title=element_blank(),plot.title=element_text(hjust=0.5)) 
