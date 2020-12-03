#overview_measures 

#import libraries
library(magrittr)
library(data.table)
library(ggplot2)
library(ggpubr)

# import files: Time, RKI data
policy <- fread("./extData/Policy.csv") 


nbPolicies <- policy[, .N, by=type] %>% setorder(., -N) 


plot_nbPolicies <- ggplot(nbPolicies, aes(x = reorder(type, N) ,y = N)) +
  geom_bar(stat="identity", width = 0.7) + 
  coord_flip() +
  labs(x = "type of policy", y = "nb. of measures",
       title="Policies of the Government")+
  theme(legend.position="none",plot.title= element_text(hjust=0.5)) 


#Policy Starting Day Distribution

policyDay = policy[, c("policy_id", "country", "type", "detail", "end_date") := NULL]

policyDay <- policyDay[, count := .N, by=start_date]

plot_polDistribution <- ggplot(policyDay, aes(x = start_date)) +
  geom_dotplot(dotsize = 2) +
  
  labs(x = "start date", y = "nb. of measures",
       title="Nb. of introduced measures")+
  theme(legend.position="none",plot.title= element_text(hjust=0.5), 
        axis.text.y = element_blank())



#put plots together

ggarrange(plot_nbPolicies, plot_polDistribution, ncol = 2, nrow = 1)



