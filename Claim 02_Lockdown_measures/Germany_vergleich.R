library(magrittr)
library(data.table)
library(ggplot2)


# import files: Time, RKI data
time <- fread("./extData/Time.csv")
RKI_data <- fread("./extData/DE_InfectionCases.csv")

#total cases in Korea per day
time[,confirmed_date:=as.Date(date,"%d/%m/%y")]
time[, confirmed_date:= as.IDate(confirmed_date)]
ggplot(time, aes(x=confirmed_date, y=daily_cases)) +
  geom_line()

# total cases Germany
RKI_data <- RKI_data[, "Anzahl COVID-19-Fälle kumuliert" := NULL]
RKI_data

RKI_data_UsFormat <- RKI_data[, confirmed_date := as.Date(Berichtsdatum, 
                                                          format ="%d.%m.%y")]
#change date class to "IDate"
RKI_data_UsFormat[, confirmed_date:= as.IDate(confirmed_date)]
RKI_data_UsFormat <- RKI_data_UsFormat[, Berichtsdatum := NULL]
RKI_data_UsFormat
class(RKI_data_UsFormat$confirmed_date)


merge_GermanyUS <- merge(RKI_data_UsFormat, time, 
                         by = "confirmed_date", all.y = TRUE)


# plot
ggplot(merge_GermanyUS, aes(x=confirmed_date)) + 
  geom_line(aes(y=`daily_cases`,color="Germany"))+
  geom_line(aes(y=`Anzahl Covid-Fälle pro Tag`,color="South Korea")) + 
  labs(x= "confirmed date", y="daily cases")+
  geom_rect(aes(xmin=as.Date('2020-02-26'),xmax=as.Date('2020-03-15'),
                ymin=860,ymax=890,color="18 days"))+
  geom_rect(aes(xmin=as.Date('2020-03-10'),xmax=as.Date('2020-05-10'),
                ymin=6400,ymax=6430,color="61 days"))+
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) 

