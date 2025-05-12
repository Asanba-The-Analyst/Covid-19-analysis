library(tidyverse)


KHR<-read.csv("D:/Covid-19_Nicholas/Data/KINT V-1.csv")
NHR<-read.csv("D:/Covid-19_Nicholas/Data/NAV-V.csv")
DHR<-read.csv("D:/Covid-19_Nicholas/Data/DOD V-1.csv")
#creating a variable called site for the location identity
#kintampo is 1
#Navrongo is 2
#Doddowa is 3
KHR$site <- 1
NHR$site <- 2
DHR$site <- 3

table(duplicated(KHR))
table(duplicated(NHR))
table(duplicated(DHR))


data<-rbind(KHR,NHR,DHR)

table(duplicated(data))


notneeded<-c("consent_form","fname","lname","othname","compoundno","compno_avail","contactno")

data <- data %>% select(-all_of(notneeded))

#othtestspcfy  

sum(is.na(data$compoundno)) # all missing
sum(is.na(data$compno_avail)) # all missing

sum(is.na(data$IgG)) # all missing- they are fro DHR-Dodowa
sum(is.na(data$IgM)) # all missing-they are fro DHR-Dodowa


