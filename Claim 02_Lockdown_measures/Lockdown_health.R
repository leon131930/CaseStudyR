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
                             color = "Authorization of\nDiagnostic Kit"), 
             linetype = "longdash", show.legend = TRUE)+
  geom_vline(data = Center, aes(xintercept = start_date, 
                                color = "Drive-Through\nScreening Center"), 
             linetype = "longdash", show.legend = TRUE)+
  geom_vline(data = Mask, aes(xintercept = start_date, 
                              color = "Mask Distribution"), 
                              color = "Mask\nDistribution"), 
             linetype = "longdash", show.legend = TRUE) +
  geom_vline(data = Quarantine, aes(xintercept = start_date, 
                                    color = "Extends Tightened\nQuarantine Measures"), 
             linetype = "longdash", show.legend = TRUE) +
  labs(x = "confirmed date", y = "") + theme(legend.position="bottom", 
                                             titel ="Health measures") +
  theme(plot.title = element_text(hjust=0.5)) +
  theme(legend.title=element_blank()) 