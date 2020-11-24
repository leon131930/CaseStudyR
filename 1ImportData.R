#Install libraries and import data
library(magrittr)
library(data.table)
library(ggplot2)

#import data 
case <- fread("C:/Users/felix/Documents/Data Analysis in R/CaseStudyR/extData/Case.csv")
patient_Info <- fread("C:/Users/felix/Documents/Data Analysis in R/CaseStudyR/extData/PatientInfo.csv")
policy <- fread("C:/Users/felix/Documents/Data Analysis in R/CaseStudyR/extData/Policy.csv")
region <- fread("C:/Users/felix/Documents/Data Analysis in R/CaseStudyR/extData/Region.csv")
search_Trend <- fread("C:/Users/felix/Documents/Data Analysis in R/CaseStudyR/extData/SearchTrend.csv")
seoul_Floating <- fread("C:/Users/felix/Documents/Data Analysis in R/CaseStudyR/extData/SeoulFloating.csv")
time <- fread("C:/Users/felix/Documents/Data Analysis in R/CaseStudyR/extData/Time.csv")
time_Age <- fread("C:/Users/felix/Documents/Data Analysis in R/CaseStudyR/extData/TimeAge.csv")
time_Gender <- fread("C:/Users/felix/Documents/Data Analysis in R/CaseStudyR/extData/TimeGender.csv")
time_Province <- fread("C:/Users/felix/Documents/Data Analysis in R/CaseStudyR/extData/TimeProvince.csv")
weather <- fread("C:/Users/felix/Documents/Data Analysis in R/CaseStudyR/extData/weather.csv")

