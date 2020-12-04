library(magrittr)
library(data.table)
library(ggplot2)


# import files: Time, RKI data
time <- fread("./extData/Time.csv")
RKI_data <- fread("./extData/DE_InfectionCases.csv")
RKI_data
#total cases in Korea per day
#total cases in Korea per day
time[,confirmed_date:=as.Date(date,"%d/%m/%y")]
time[, confirmed_date:= as.IDate(confirmed_date)]

# total cases Germany

RKI_data_UsFormat <- RKI_data[, confirmed_date := as.Date(`Berichtsdatum`, 
                                                          format ="%d.%m.%y")]
#change date class to "IDate"
RKI_data_UsFormat[, confirmed_date:= as.IDate(confirmed_date)]
RKI_data_UsFormat <- RKI_data_UsFormat[, Berichtsdatum := NULL]



merge_GermanyUS <- merge(RKI_data_UsFormat, time, 
                         by = "confirmed_date", all.y = TRUE)

# plot
plot_germany <- ggplot(merge_GermanyUS, aes(x=confirmed_date)) + 
  geom_line(aes(y=`daily_cases`,color="Germany"))+
  geom_line(aes(y=`Anzahl COVID-19-Faelle pro Tag`,color="South Korea")) + 
  labs(x= "confirmed date", y="daily cases",title="Daily cases: South Korea vs. Germany")+
  geom_rect(aes(xmin=as.Date('2020-02-26'),xmax=as.Date('2020-03-15'),
                ymin=860,ymax=890,color="18 days"))+
  geom_rect(aes(xmin=as.Date('2020-03-10'),xmax=as.Date('2020-05-10'),
                ymin=6400,ymax=6430,color="61 days"))+
  theme(legend.position="bottom") +
  theme(legend.title=element_blank(),plot.title=element_text(hjust=0.5))
