#overview_measures 

#import libraries
library(magrittr)
library(data.table)
library(ggplot2)
library(ggpubr)

# import files: Time, RKI data
policy <- fread("./extData/Policy.csv") 

policy_copy = copy(policy)
nbPolicies <- policy_copy[, .N, by=type] %>% setorder(., -N) 

plot_nbPolicies <- ggplot(nbPolicies, aes(x = reorder(type, N) ,y = N)) +
  geom_bar(stat="identity", width = 0.7) + 
  coord_flip() +
  labs(x = "Type of Policy", y = "Number of Measures",
       title="Different Policies of the Government")+
  theme(legend.position="none") 


#Policy Starting Day Distribution
policy_copy = copy(policy)
policyDay = policy_copy[, c("policy_id", "country", "type", "detail", "end_date") := NULL]

policyDay <- policyDay[, count := .N, by=start_date]

plot_polDistribution <- ggplot(policyDay, aes(x = start_date)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  geom_bar() +
  labs(x = "Time", y = "Number of Measures",
       title="Number of introduced Measures by Start Date")+
  theme(legend.position="none",plot.title= element_text(hjust=0.5)) +
  geom_text(aes(label=stat(count)), stat = "count", vjust = -0.5)

ggarrange(plot_nbPolicies, plot_polDistribution, ncol=2, nrow=1)


# Policies: cumulative Measures per Day
policy_copy <- fread("./extData/Policy.csv") 

policy_day_start <- policy_copy[,.(start_date)]  %>% .[,.N,by = "start_date"]
colnames(policy_day_start) <- c("date","number")

policy_day_end <- policy_copy[,.(end_date)] %>% .[,.N,by = "end_date"]
colnames(policy_day_end) <- c("date","number")
policy_day_end <- na.omit(policy_day_end, "date")

#Merge data and only retain the relevant ones and replace NAs with 0
merge_StartEnd <- merge(x = policy_day_start, y = policy_day_end, by = "date", all = TRUE)
colnames(merge_StartEnd) <- c("date","newMeasures","stoppedMeasures")
merge_StartEnd[is.na(merge_StartEnd)] <- 0

#get difference between policies that started and ended at a certain day
merge_StartEnd[, difference := .(newMeasures - stoppedMeasures)]

#Including daily number of infection cases from table "time"
time <- fread("./extData/Time.csv")
time[,confirmed_date:=as.Date(date,"%d/%m/%y")]
time[, confirmed_date:= as.IDate(confirmed_date)]
time <- time[,.(confirmed_date, daily_cases)]
colnames(time) <- c("date", "daily_cases")

#merge number of cum. measures and daily cases
merge_CasesMeasures <- 
  merge(x = time, y = merge_StartEnd, by = "date", all.x = TRUE)
merge_CasesMeasures[is.na(merge_CasesMeasures)] <- 0

merge_CasesMeasures[, cumulativeNumbers := cumsum(difference)]

# Plot "daily cases" and "cum. number of measures" on two different y axis
ggplot(merge_CasesMeasures, aes(x = date)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  geom_line(aes(y=`daily_cases`, col = "Daily Cases")) +
  geom_line(aes(y=`cumulativeNumbers` * 8,col = "Cum. Measures")) + 
  labs(title="Daily Cases and Number of Measures") +
  scale_y_continuous(name = "Daily Cases",
  # Add a second axis, with a scale / 8
  sec.axis = sec_axis(~./ 8, name="Number of cum. Measures")) +
  theme(legend.position="right", legend.title = element_blank(), 
        axis.title.y.right = element_text(angle=90))


#Calculate statistics
#Correlation 
cor_CasesMeasures <- cor(merge_CasesMeasures$cumulativeNumbers, merge_CasesMeasures$daily_cases, method = 'spearman')
cor_CasesMeasures

#Cross-correlation
ccfValues <- ccf(merge_CasesMeasures$cumulativeNumbers,merge_CasesMeasures$daily_cases, 50)

#Interpretation
"
ccf(x,y)
x = number of measures at time xt+h with h = -30
y = daily cases at time yt

The correlations in this region are negative, indicating that an above 
average value of measures is likely to lead to a below average value of daily cases 
about 30 days later. 
And, a below average of measures is associated with a likely above average daily cases
value about 6 months later.
"

#Test - Statistics
test <-cor.test(merge_CasesMeasures$cumulativeNumbers, 
                merge_CasesMeasures$daily_cases,method="spearman")
test

p_value <- test$p.value
p_value


