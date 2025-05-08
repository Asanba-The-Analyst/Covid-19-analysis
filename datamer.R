library(tidyverse)


KHR<-read.csv("D:/Covid-19_Nicholas/Data/KINT V-1.csv")
NHR<-read.csv("D:/Covid-19_Nicholas/Data/NAV-V.csv")
DHR<-read.csv("D:/Covid-19_Nicholas/Data/DOD V-1.csv")

table(duplicated(KHR))
table(duplicated(NHR))
table(duplicated(DHR))


data<-rbind(KHR,NHR,DHR)

table(duplicated(data))


notneeded<-c("consent_form","fname","lname","othname","compno_avail","contactno")
data <- data %>% select(-notneeded)
