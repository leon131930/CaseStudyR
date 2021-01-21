install.packages("patchwork")
install.packages("hrbrthemes")
library(magrittr)
library(data.table)
library(ggplot2)
library(dplyr)
library(patchwork)
library(hrbrthemes)
library(ggpubr)

######## DATA Preparation ############
# importing PatientInfo, Searchtrend, and time table

patientinfo <- fread("./extData/PatientInfo.csv")
searchtrend <- fread("./extData/SearchTrend.csv")
time <- fread("./extData/Time.csv")


#check infection reasons with case table
case <- fread("./extData/Case.csv")
case[, .N, by=infection_case][(order(-N))]
case[grep("Church|Christ",infection_case)][order(-confirmed)]


############ Exploration influence of church event############
patientinfocs <- copy(patientinfo)
#check top infection cases
patientinfocs[, .N, by=infection_case][(order(-N))]
#filter for church related infection cases

#patientinfocs <- patientinfocs[grep("Church|Christ",infection_case)]

#comparison if only use Shincheeonji Curch data
patientinfocs <- patientinfocs[grep("Shincheonji",infection_case)]

#create new column for number of daily cases, based on count of confirmed_date
patientinfocs <- patientinfocs[, dailychurchcases := .N, by = confirmed_date]
#new column/rename "confirmed_date" to "date" (otherwise not able to join)
patientinfocs[, date := sub("confirmed_date", "date", confirmed_date)]
#change date class to "IDate"
patientinfocs[, date:= as.IDate(date)]

#plot all church cases
ggplot(patientinfocs[date %between% c("2020-02-18", "2020-02-28")], aes(x = date)) +
  geom_line(aes(y=dailychurchcases))+
  geom_point(aes(y=dailychurchcases))+
  scale_x_date(date_breaks = "1 day", date_labels = "%d-%m")

#checking dates with most church cases
patientinfocs[order(-dailychurchcases)]%>%head(30)



#set colors for visualization
searchcolour <- "#0000CD"
dailycasecolour <- "#ff0000"
churchcolour <- "#9400D3"


#########Visualization with time table############
timecs <- copy(time)
timecs <- timecs[, date:= as.IDate(date, format= "%d/%m/%Y")]

#merge time table and searchtrend table, full outer join
merged_dt1 <- merge(timecs, searchtrend, by = "date", all = TRUE)

#merge timecs with patientinfocs, full outer join
merged_dt2 <- merge(timecs, patientinfocs, by = "date", all = TRUE)


#plot search and daily cases with search volume on left y axis
ggplot(merged_dt1[date %between% c("2020-02-15", "2020-06-29")],aes(x=date)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y")+
  geom_line(aes(y=(coronavirus), col = "relative search volume")) +
  geom_line(aes(y=daily_cases/8, col = "daily confirmed cases")) +
  scale_color_manual(values=c(dailycasecolour,searchcolour))+
  labs(x="Calendar week 2020", title="Daily covid cases & relative search volume for 'coronavirus'")+
  scale_y_continuous(
    name = "relative search volume",
    sec.axis = sec_axis(trans=~.*8, name="daily confirmed cases",labels = scales::comma))+
  theme(legend.position="bottom", legend.title = element_blank(), 
        axis.title.y.right = element_text(angle=90, color = dailycasecolour),
        axis.title.y.left = element_text(color = searchcolour))


#plot churchcases against total daily cases 
ggplot(merged_dt2[date %between% c("2020-02-15", "2020-03-10")],aes(x=date)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%m-%d")+
  geom_line(aes(y=daily_cases/17, col = "total daily confirmed cases")) +
  geom_line(aes(y=(dailychurchcases), col = "daily cases from churches")) +
  scale_color_manual(values=c(churchcolour, dailycasecolour))+
  labs(x="Calendar week 2020", title="Daily covid cases in churches vs total")+
  scale_y_continuous(
    name = "daily cases from churches",
    sec.axis = sec_axis(trans=~.*17, name="total daily confirmed cases",labels = scales::comma))+
  theme(legend.position="bottom", legend.title = element_blank(), 
        axis.title.y.right = element_text(angle=90, color = dailycasecolour),
        axis.title.y.left = element_text(color = churchcolour))


#calculate observed correlation (daily cases vs churchcases) 
merged_dt2 <- merged_dt2[date %between% c("2020-02-18", "2020-02-28")]

corr <- cor(merged_dt2$daily_cases, merged_dt2$dailychurchcases, method = 'spearman')
corr

#checking cross correlation
ccf(merged_dt2$dailychurchcases, merged_dt2$daily_cases, na.action = na.pass)

?ccf()
merged_dt2

#calculate observed correlation (daily cases vs search) for date range Mid February - End of June
merged_dt1 <- merged_dt[date %between% c("2020-02-15", "2020-06-29")]
corr <- cor(merged_dt1$daily_cases, merged_dt1$coronavirus, method = 'pearson')
corr


############ Test: Correlation search volume and daily cases########################
# Nullhypothesis: No Correlation
ho_0 <- 0
# Na's in data?
anyNA(merged_dt1$daily_cases, merged_dt1$coronavirus)
n <- nrow(merged_dt1)


#test statistic
t <- corr/ sqrt(1-(corr)^2)*sqrt(n-2)
t
# critical value
alpha <- 0.05
z_krit <- qnorm(1-alpha/2)
z_krit
# Lehne H0 ab, wenn der Absolutwert (Betrag) der Teststatistik größer ist als der kritische Wert:
abs(t) > z_krit

cor.test(merged_dt1$daily_cases, merged_dt1$coronavirus, method="pearson")

################## Using Cross correlation to show that search cases can indicate covid cases######################
# -> Shows high correlation (0.92) for a time lag of 6 days
ccf(merged_dt1$coronavirus,merged_dt1$daily_cases, main ="Cross-correlation between search volume and daily # of cases")
ccfvalues <- ccf(merged_dt1$coronavirus,merged_dt1$daily_cases, plot = FALSE)
ccfvalues
#storing ccfvalues in data table (sorted by correlation)
cor <- ccfvalues$acf[,,1]
lag <- ccfvalues$lag[,,1]
dt_ccfvalues <- data.table(cor, lag)
dt_ccfvalues <- dt_ccfvalues[order(-cor)]
dt_ccfvalues


#####################permutation testing#######################
dt_permuted <- copy(merged_dt1)
set.seed(0)

#function to calculate correlation
correlation <- function(dt_permuted){
  corr_p <- cor(dt_permuted$daily_cases, dt_permuted$coronavirus_sampled, method = 'pearson')
}

# create list with 1000 NA-values
m <- 1000
T_permuted <- rep(NA, m)

#repeat 1000 times: sample the column "coronavirus", calculate correlation and store variable in T_permuted
for(i in 1:m){
  dt_permuted[, coronavirus_sampled := sample(coronavirus)]
  T_permuted[i] <- correlation(dt_permuted)
}

#plot T_permuted as histogram
ggplot( data.table(T_permuted), aes(x = T_permuted)) +
  geom_histogram(bins = 40) +
  geom_vline(aes(xintercept= corr, color = "oberserved correlation"))

# -> It seems the correlation is not likely to have arisen by chance






