install.packages("patchwork")
install.packages("hrbrthemes")
library(magrittr)
library(data.table)
library(ggplot2)
library(dplyr)
library(patchwork)
library(hrbrthemes)
library(ggpubr)


# importing PatientInfo, Searchtrend, and time table
patientinfo <- fread("./extData/PatientInfo.csv")
searchtrend <- fread("./extData/SearchTrend.csv")
time <- fread("./extData/Time.csv")


#***Data preparation Time table:***
timecs <- copy(time)
timecs <- timecs[, date:= as.IDate(date, format= "%d/%m/%Y")]

#***Merge merge time table and searchtrend table, full outer join***
merged_dt1 <- merge(timecs, searchtrend, by = "date", all = TRUE)




#set colors for visualization
searchcolour <- "#0000CD"
dailycasecolour <- "#ff0000"
churchcolour <- "#9400D3"

#plot search volume and daily cases, using two y-axis because of relative values of searchtrend table
ggplot(merged_dt1[date %between% c("2020-02-15", "2020-06-29")],aes(x=date)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y")+
  geom_line(aes(y=(coronavirus), col = "relative search volume")) +
  geom_line(aes(y=daily_cases/8, col = "daily confirmed cases")) +
  scale_color_manual(values=c(dailycasecolour,searchcolour))+
  labs(x="Time", title="Daily covid cases & relative search volume for 'coronavirus'")+
  scale_y_continuous(
    name = "relative search volume",
    sec.axis = sec_axis(trans=~.*8, name="daily confirmed cases",labels = scales::comma))+
  theme(legend.position="bottom", legend.title = element_blank(), 
        axis.title.y.right = element_text(angle=90, color = dailycasecolour),
        axis.title.y.left = element_text(color = searchcolour))

#calculate pearson correlation (daily cases vs search) for date range Mid February - End of June
merged_dt1 <- merged_dt1[date %between% c("2020-02-15", "2020-06-29")]
corr1 <- cor(merged_dt1$daily_cases, merged_dt1$coronavirus, method = 'pearson')
corr1

#Pearson's correlation test
cor.test(merged_dt1$daily_cases, merged_dt1$coronavirus, method="pearson")

#Using Cross correlation to show that search cases can indicate covid cases
# -> Shows high correlation (0.92) for a time lag of 6 days
ccfvalues1 <- ccf(merged_dt1$coronavirus,merged_dt1$daily_cases, main ="Cross-correlation between search volume and daily cases", plot = TRUE)
ccfvalues1


#***apply permutation testing to test whether this correlation could have arisen by chance***
dt_permuted <- copy(merged_dt1)
set.seed(2)

#function to calculate correlation
correlation <- function(dt_permuted){
  corr_p <- cor(dt_permuted$daily_cases, dt_permuted$coronavirus_sampled, method = 'pearson')}

# create list with 1000 NA-values
m <- 1000
T_permuted <- rep(NA, m)

#repeat 1000 times: sample the column "coronavirus", calculate correlation and store variable in T_permuted
for(i in 1:m){
  dt_permuted[, coronavirus_sampled := sample(coronavirus)]
  T_permuted[i] <- correlation(dt_permuted)}

#plot T_permuted as histogram
ggplot(data.table(T_permuted), aes(x = T_permuted)) +
  geom_histogram() +
  geom_vline(aes(xintercept= corr1, color = "oberserved correlation"))



#***check influence of church superspreader event as 3rd confounding variable influencing daily cases***
#***Data preparation Patientinfo table:***
patientinfocs <- copy(patientinfo)
#filter for Shincheeonji Curch event
patientinfocs <- patientinfocs[grep("Shincheonji",infection_case)]

#create new column for number of daily cases, based on count of confirmed_date
patientinfocs <- patientinfocs[, dailychurchcases := .N, by = confirmed_date]

#new column/rename "confirmed_date" to "date" (otherwise not able to join)
patientinfocs[, date := sub("confirmed_date", "date", confirmed_date)]
patientinfocs[, date:= as.IDate(date)]

#***merge time table with Patientinfo table, full outer join***
merged_dt2 <- merge(timecs, patientinfocs, by = "date", all = TRUE)


#plot Shincheonji church related cases and (total) daily cases, using two y-axis to better visualize the course
ggplot(merged_dt2[date %between% c("2020-02-15", "2020-03-08")],aes(x=date)) +
  scale_x_date(date_breaks = "2 day", date_labels = "%d.%m")+
  geom_line(aes(y=daily_cases/20, col = "daily cases")) +
  geom_line(aes(y=(dailychurchcases), col = "Shincheonji church daily cases")) +
  scale_color_manual(values=c(dailycasecolour, churchcolour))+
  labs(x="Time", title="Shincheonji church daily cases vs. total daily cases")+
  scale_y_continuous(
    name = "Shincheonji church daily cases",
    sec.axis = sec_axis(trans=~.*20, name="daily cases",labels = scales::comma))+
  theme(legend.position="bottom", legend.title = element_blank(), 
        axis.title.y.right = element_text(angle=90, color = dailycasecolour),
        axis.title.y.left = element_text(color = churchcolour))


#calculate observed correlation (daily cases vs churchcases), for a time period of start of church-cases until 10 days later
merged_dt2 <- merged_dt2[date %between% c("2020-02-18", "2020-02-28")]

corr2 <- cor(merged_dt2$daily_cases, merged_dt2$dailychurchcases, method = 'pearson')
corr2

#Pearson's correlation test
cor.test(merged_dt2$daily_cases, merged_dt2$dailychurchcases, method="pearson")






