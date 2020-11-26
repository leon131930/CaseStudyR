

health_policy <- policy[type == "Health"]

Kit <- health_policy[detail == "1st EUA"]
Center <- health_policy[detail == "by Local Government"]
Mask <- health_policy[detail == "Public-Sale"]
Quarantine <- health_policy[gov_policy == "Extends Tightened Quarantine Measures"]

ggplot(time, aes(x=confirmed_date, y=daily_cases)) + geom_line() +
  geom_vline(data = Kit, aes(xintercept = start_date, color = "Authorization of\nDiagnostic Kit"), 
             linetype = "longdash", show.legend = TRUE)+
  geom_vline(data = Center, aes(xintercept = start_date, color = "Drive-Through\nScreening Center"), 
             linetype = "longdash", show.legend = TRUE)+
  geom_vline(data = Mask, aes(xintercept = start_date, color = "Mask Distribution"), 
             linetype = "longdash", show.legend = TRUE) +
  geom_vline(data = Quarantine, aes(xintercept = start_date, color = "Extends Tightened\nQuarantine Measures"), 
             linetype = "longdash", show.legend = TRUE) +
  labs(x = "confirmed date", y = "daily cases") + theme(legend.position="bottom")