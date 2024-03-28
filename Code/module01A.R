library(readr)
library(tidyverse)
library(ggplot2)
Xtr = read.csv("Xtr.csv")
Xte = read.csv("Xte.csv")
Ytr = read.csv("Ytr.csv")
Xtr_mod = read.csv("Xtr_new2.csv")

#############
Xtr_1 <- Xtr[c("List_Year","Date_Recorded","Town","Address","Assessed_Value","Property_Type","Residential_Type")]
Xte_1 <- Xte[c("Date_Recorded","Town","Assessed_Value","Property_Type","Residential_Type","Address")]

Xtr_1$Date_Recorded <- as.Date(Xtr_1$Date_Recorded,format = "%m/%d/%Y")
Xtr_1$Month <- month(Xtr_1$Date_Recorded)
Xtr_1$Year <- year(Xtr_1$Date_Recorded)

Xtr_1$Property_Type <- gsub("Condo|Two Family|Single Family|Three Family|Four Family", "Residential", Xtr_1$Property_Type)
Xtr_1$Town <- as.factor(Xtr_1$Town)
Xtr_1$Month <- as.factor(Xtr_1$Month)
Xtr_1$Year <- as.factor(Xtr_1$Year)
Xtr_1$Type <- ifelse(Xtr_1$Property_Type != "Residential", Xtr_1$Property_Type, Xtr_1$Residential_Type)
Xtr_1$Type <- as.factor(Xtr_1$Type)
drop <-c("Date_Recorded","Property_Type","Residential_Type")
Xtr_1 <- Xtr_1[, !names(Xtr_1) %in% drop]
Xtr_1$Type[Xtr_1$Type == ""] <- NA
Xtr_1 = mutate(Xtr_1,log10Value = log10(Xtr_1$Assessed_Value))
Xte_2 = mutate(Xte,log10Value = log10(Xte_1$Assessed_Value))
Xtr_lm_log2 <- Xtr_2$log10Value[!is.infinite(Xtr_2$log10Value)]
Xte_lm_log2 <- Xte_2$log10Value[!is.infinite(Xte_2$log10Value)]
mean_log = mean(c(Xtr_lm_log2,Xte_lm_log2),na.rm=T)


Xtr_2$log10Value[is.infinite(Xtr_2$log10Value)] = mean_log

Xte_2$log10Value[is.infinite(Xte_2$log10Value)] = mean_log
