library(GISTools)
library(sp)
library(rgdal)
library(tmap)
library(tbart)
library(scales)
library(ggplot2)
library(sf)
library(repmis)

## Leeds case
# Load Data
source_data("https://github.com/Wen-Zeng/Areal-Interpolation/blob/master/DataHPsales.RData?raw=True")
# or if saved locally
# load("DataHPsales.RData")

#MSOA to LSOA 
ina <- st_intersection(Leeds_house, Leeds_MSOA)
ina$no. <- seq(1,length(ina$household),1)
zone.list <- sort(unique(array(ina$geo_code)))
ina$pop_hou <- 0
for (i in 1:length(zone.list)) { 
  item <- zone.list[i]
  zone.set <- (ina$geo_code == item) 
  inano.list <- ina$no.[zone.set]
  for (j in 1:length(inano.list)) {
    item1 <- inano.list[j]
    ina$pop_hou[which(ina$no. == item1)] <- (ina$Population[which(ina$no. == item1)])*(ina$household[which(ina$no. == item1)]/sum(ina$household[zone.set], na.rm = TRUE))
    cat(i,":",j,"/",length(inano.list), "\t")
    }
}

sum(ina$pop_hou,na.rm = TRUE)
sum(Leeds_MSOA$Population)

res <- st_intersection(ina,Leeds_LSOA)
zone.list1 <- sort(unique(array(res$code)))
Leeds_LSOA$Pop_estimate <- 0
for (i in 1:length(zone.list1)) { 
  item <- zone.list1[i]
  zone.set <- (res$code == item) 
  Leeds_LSOA$Pop_estimate[which(Leeds_LSOA$code == item)] <- sum(res$pop_hou[zone.set],na.rm = TRUE)
  cat(i,"/",length(zone.list1), "\t")
  }

sum(Leeds_LSOA$Pop_estimate)
sum(Leeds_LSOA$pop)

cor.test(Leeds_LSOA$pop,Leeds_LSOA$Pop_estimate)

Leeds_LSOA$Household_sales_error_MSOA_to_LSOA <- Leeds_LSOA$Pop_estimate - Leeds_LSOA$pop

table <- cbind(Leeds_LSOA$code, Leeds_LSOA$pop, Leeds_LSOA$Pop_estimate,Leeds_LSOA$Household_sales_error_MSOA_to_LSOA)
colnames(table) <- c("code", "pop", "estimate", "error")

write.csv(table, file = "Household_sales_error_MSOA_to_LSOA.csv")

#MSOA to OA 
res <- st_intersection(ina, Leeds_OA)
zone.list1 <- sort(unique(array(res$code)))
Leeds_OA$Pop_estimate <- 0
for (i in 1:length(zone.list1)) { 
  item <- zone.list1[i]
  zone.set <- (res$code == item) 
  Leeds_OA$Pop_estimate[which(Leeds_OA$code == item)] <- sum(res$pop_hou[zone.set],na.rm = TRUE)
  cat(i,"/",length(zone.list1), "\t")
}

sum(Leeds_OA$Pop_estimate)
sum(Leeds_OA$pop)

cor.test(Leeds_OA$pop,Leeds_OA$Pop_estimate)

Leeds_OA$Household_sales_error_MSOA_to_OA <- Leeds_OA$Pop_estimate - Leeds_OA$pop

table <- cbind(Leeds_OA$code, Leeds_OA$pop, Leeds_OA$Pop_estimate,Leeds_OA$Household_sales_error_MSOA_to_OA)
colnames(table) <- c("code", "pop", "estimate", "error")

write.csv(table, file = "Household_sales_error_MSOA_to_OA.csv")

#LSOA to OA 
ina <- st_intersection(Leeds_house, Leeds_LSOA)
ina$no. <- seq(1,length(ina$household),1)
zone.list <- sort(unique(array(ina$code)))
ina$pop_hou <- 0
for (i in 1:length(zone.list)) { 
  item <- zone.list[i]
  zone.set <- (ina$code == item) 
  inano.list <- ina$no.[zone.set]
  for (j in 1:length(inano.list)) {
    item1 <- inano.list[j]
    ina$pop_hou[which(ina$no. == item1)] <- (ina$pop[which(ina$no. == item1)])*(ina$household[which(ina$no. == item1)]/sum(ina$household[zone.set], na.rm = TRUE))
    cat(i,":",j,"/",length(inano.list), "\t")
    }
}

sum(ina$pop_hou,na.rm = TRUE)
sum(Leeds_LSOA$pop)

res <- st_intersection(ina, Leeds_OA)
zone.list1 <- sort(unique(array(res$code.1)))
Leeds_OA$Pop_estimate <- 0
for (i in 1:length(zone.list1)) { 
  item <- zone.list1[i]
  zone.set <- (res$code.1 == item) 
  Leeds_OA$Pop_estimate[which(Leeds_OA$code == item)] <- sum(res$pop_hou[zone.set],na.rm = TRUE)
  cat(i,"/",length(zone.list1), "\t")
  }

sum(Leeds_OA$Pop_estimate)
sum(Leeds_OA$pop)

cor.test(Leeds_OA$pop,Leeds_OA$Pop_estimate)

Leeds_OA$Household_sales_error_LSOA_to_OA <- Leeds_OA$Pop_estimate - Leeds_OA$pop

table <- cbind(Leeds_OA$code, Leeds_OA$pop, Leeds_OA$Pop_estimate,Leeds_OA$Household_sales_error_LSOA_to_OA)
colnames(table) <- c("code", "pop", "estimate", "error")

write.csv(table, file = "Household_sales_error_LSOA_to_OA.csv")

## Qingdao case
ina <- intersect(Qingdao_house, District_Qingdao)
ina$no. <- seq(1,length(ina$House_num),1)
ina$code <- as.character(ina$District) #District no.
zone.list <- sort(unique(array(ina$code)))
ina$pop_hou <- 0
for (item in zone.list) { 
  zone.set <- (ina$code == item) 
  inano.list <- ina$no.[zone.set]
  for (item1 in inano.list) {
    ina$pop_hou[which(ina$no. == item1)] <- (ina$Population[which(ina$no. == item1)])*(ina$House_num[which(ina$no. == item1)]/sum(ina$House_num[zone.set], na.rm = TRUE))
  }
}

sum(ina$pop_hou)
sum(District_Qingdao$Population)
sum(ina$House_num)
sum(Qingdao_house$House_num)

res <- intersect(ina, Subdistrict_Qingdao)
zone.list1 <- sort(unique(array(res$Subdistric))) 
Subdistrict_Qingdao$Pop_estimate <- 0
for (item in zone.list1) { 
  zone.set1 <- (res$Subdistric == item) 
  Subdistrict_Qingdao$Pop_estimate[which(Subdistrict_Qingdao$Subdistric == item)] <- sum(ina$pop_hou[zone.set1], na.rm = TRUE)
}

sum(Subdistrict_Qingdao$Pop_estimate)
sum(Subdistrict_Qingdao$Population)

cor.test(Subdistrict_Qingdao$Population,Subdistrict_Qingdao$Pop_estimate)

Subdistrict_Qingdao$Household_sales_error_Dis_to_Subdis <- Subdistrict_Qingdao$Pop_estimate - Subdistrict_Qingdao$Population

table <- cbind(Subdistrict_Qingdao$Subdi_code, Subdistrict_Qingdao$Population, Subdistrict_Qingdao$Pop_estimate,Subdistrict_Qingdao$Household_sales_error_Dis_to_Subdis)
colnames(table) <- c("code", "pop", "estimate", "error")

write.csv(table, file = "Household_sales_error_Dis_to_Subdis.csv")

