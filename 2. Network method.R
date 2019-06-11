library(GISTools)
library(sp)
library(rgdal)
library(rgeos) 
library(tmap)
library(scales)
library(ggplot2)
library(rtop)
library(gstat)
library(automap)
library(pycno)
library(deldir)
library(raster)
library(sf)

## Leeds case
#Load data
# Load Data
source_data("https://github.com/Wen-Zeng/Areal-Interpolation/blob/master/DataNet.RData?raw=True")
# or if saved locally
# load("DataNet.RData")

#check the population
sum(Leeds_MSOA$Population, na.rm = T)
sum(Leeds_LSOA$pop, na.rm = T)
sum(Leeds_OA$pop, na.rm = T)

par(mar=c(1,1,1,1))

#MSOA to LSOA
ina <- st_intersection(Road_Leeds, Leeds_MSOA)
ina$Length <- st_length(ina)
ina$Length <- as.numeric(ina$Length)
ina$ID <- seq(1,length(ina$Length),1)
zone.list <- sort(unique(array(ina$geo_code)))
ina$pop <- 0
for (item in zone.list) { 
  zone.set <- (ina$geo_code == item) 
  inano.list <- ina$ID[zone.set]
  for (item1 in inano.list) {
    ina$pop[which(ina$ID == item1)] <- (ina$Population[which(ina$ID == item1)])*(ina$Length[which(ina$ID == item1)]/sum(ina$Length[zone.set], na.rm = TRUE))
   }
}

sum(ina$pop,na.rm = TRUE)
sum(Leeds_MSOA$Population)

res <- st_intersection(ina, Leeds_LSOA)
res$RLength <- st_length(res)
res$RLength <- as.numeric(res$RLength)
res$RID <- seq(1,length(res$RLength),1)
zone.list1 <- sort(unique(array(res$code)))
Leeds_LSOA$Popu_estimate <- 0
for (item in zone.list1) { 
  zone.set1 <- (res$code == item) 
  resno.list <- res$RID[zone.set1]
  pps <- 0
  pp <- 0
  for (item1 in resno.list) {
    #zone.set2 <- (res$RID == res$RID[which(res$RID == item1)])
    zone.set2 <- (res$ID == res$ID[which(res$RID == item1)])
    pp <- (res$pop[which(res$RID == item1)])*(res$RLength[which(res$RID == item1)]/sum(res$RLength[zone.set2], na.rm = TRUE))
    pps <- pps + pp    
  }
  Leeds_LSOA$Popu_estimate[which(Leeds_LSOA$code == item)] <- pps
}

sum(Leeds_LSOA$Popu_estimate)
sum(Leeds_LSOA$pop)
cor.test(Leeds_LSOA$pop,Leeds_LSOA$Popu_estimate)

Leeds_LSOA$LSOA_network_error_MSOA <- Leeds_LSOA$Popu_estimate - Leeds_LSOA$pop

table <- cbind(Leeds_LSOA$code, Leeds_LSOA$pop, Leeds_LSOA$Popu_estimate,Leeds_LSOA$LSOA_network_error_MSOA)
colnames(table) <- c("code", "pop", "estimate", "error")
write.csv(table, file = "LSOA_network_error_MSOA.csv")


#MSOA to OA
res <- st_intersection(ina, Leeds_OA)
res$RLength <- st_length(res)
res$RLength <- as.numeric(res$RLength)
res$RID <- seq(1,length(res$RLength),1)
zone.list1 <- sort(unique(array(res$code)))
Leeds_OA$Popu_estimate <- 0
for (item in zone.list1) { 
  zone.set1 <- (res$code == item) 
  resno.list <- res$RID[zone.set1]
  pps <- 0
  pp <- 0
  for (item1 in resno.list) {
    zone.set2 <- (res$ID == res$ID[which(res$RID == item1)])
    pp <- (res$pop[which(res$RID == item1)])*(res$RLength[which(res$RID == item1)]/sum(res$RLength[zone.set2], na.rm = TRUE))
    pps <- pps + pp    
  }
  Leeds_OA$Popu_estimate[which(Leeds_OA$code == item)] <- pps
}

sum(Leeds_OA$Popu_estimate)
sum(Leeds_OA$pop)
cor.test(Leeds_OA$pop,Leeds_OA$Popu_estimate)

Leeds_OA$OA_network_error_MSOA <- Leeds_OA$Popu_estimate - Leeds_OA$pop

table <- cbind(Leeds_OA$code, Leeds_OA$pop, Leeds_OA$Popu_estimate,Leeds_OA$OA_network_error_MSOA)
colnames(table) <- c("code", "pop", "estimate", "error")
write.csv(table, file = "OA_network_error_MSOA.csv")


#LSOA to OA
ina <- st_intersection(Road_Leeds, Leeds_LSOA)
ina$Length <- st_length(ina)
ina$Length <- as.numeric(ina$Length)
ina$ID <- seq(1,length(ina$Length),1)
zone.list <- sort(unique(array(ina$code)))
ina$pop1 <- 0
for (item in zone.list) { 
  zone.set <- (ina$code == item) 
  inano.list <- ina$ID[zone.set]
  for (item1 in inano.list) {
    ina$pop1[which(ina$ID == item1)] <- (ina$pop[which(ina$ID == item1)])*(ina$Length[which(ina$ID == item1)]/sum(ina$Length[zone.set], na.rm = TRUE))
  }
}

sum(ina$pop1)
sum(Leeds_LSOA$pop)

res <- st_intersection(ina,Leeds_OA)
res$RLength <- st_length(res)
res$RLength <- as.numeric(res$RLength)
res$RID <- seq(1,length(res$RLength),1)
zone.list1 <- sort(unique(array(res$code.1)))
Leeds_OA$Popu_estimate <- 0
for (item in zone.list1) { 
  zone.set1 <- (res$code.1 == item) 
  resno.list <- res$RID[zone.set1]
  pps <- 0
  pp <- 0
  for (item1 in resno.list) {
    zone.set2 <- (res$ID == res$ID[which(res$RID == item1)])
    pp <- (res$pop1[which(res$RID == item1)])*(res$RLength[which(res$RID == item1)]/sum(res$RLength[zone.set2], na.rm = TRUE))
    pps <- pps + pp    
  }
  Leeds_OA$Popu_estimate[which(Leeds_OA$code == item)] <- pps
}

sum(Leeds_OA$Popu_estimate)
sum(Leeds_OA$pop)
cor.test(Leeds_OA$pop,Leeds_OA$Popu_estimate)

Leeds_OA$OA_network_error_LSOA <- Leeds_OA$Popu_estimate - Leeds_OA$pop

table <- cbind(Leeds_OA$code, Leeds_OA$pop, Leeds_OA$Popu_estimate,Leeds_OA$OA_network_error_LSOA)
colnames(table) <- c("code", "pop", "estimate", "error")
write.csv(table, file = "OA_network_error_LSOA.csv")

##Qingdao case
#check the population
sum(Subdistrict_Qingdao$Population, na.rm = T)
sum(District_Qingdao$Population, na.rm = T)

par(mar=c(1,1,1,1))

ina <-  st_intersection(Road_Qingdao, District_Qingdao)
ina$Length <- st_length(ina)
ina$Length <- as.numeric(ina$Length)
ina$ID <- seq(1,length(ina$Length),1)
zone.list <- sort(unique(array(ina$District)))
ina$pop <- 0
for (item in zone.list) { 
  zone.set <- (ina$District == item) 
  inano.list <- ina$ID[zone.set]
  for (item1 in inano.list) {
    ina$pop[which(ina$ID == item1)] <- (ina$Population[which(ina$ID == item1)])*(ina$Length[which(ina$ID == item1)]/sum(ina$Length[zone.set], na.rm = TRUE))
    
  }
}

sum(ina$pop)
sum(District_Qingdao$Population)

res <- st_intersection(ina, Subdistrict_Qingdao)
res$RLength <- st_length(res)
res$RLength <- as.numeric(res$RLength)
res$RID <- seq(1,length(res$RLength),1)
zone.list1 <- sort(unique(array(res$Subdistric)))
Subdistrict_Qingdao$Popu_estimate <- 0
for (item in zone.list1) { 
  zone.set1 <- (res$Subdistric == item) 
  resno.list <- res$RID[zone.set1]
  pps <- 0
  pp <- 0
  for (item1 in resno.list) {
    zone.set2 <- (res$ID == res$ID[which(res$RID == item1)])
    pp <- (res$pop[which(res$RID == item1)])*(res$RLength[which(res$RID == item1)]/sum(res$RLength[zone.set2], na.rm = TRUE))
    pps <- pps + pp    
  }
  Subdistrict_Qingdao$Popu_estimate[which(Subdistrict_Qingdao$Subdistric == item)] <- pps
}

sum(Subdistrict_Qingdao$Popu_estimate)
sum(Subdistrict_Qingdao$Population)

cor.test(Subdistrict_Qingdao$Population,Subdistrict_Qingdao$Popu_estimate)

Subdistrict_Qingdao$Subdis_network_error_dis <- Subdistrict_Qingdao$Popu_estimate - Subdistrict_Qingdao$Population

table <- cbind(Subdistrict_Qingdao$Dist_code, Subdistrict_Qingdao$Population, Subdistrict_Qingdao$Popu_estimate,Subdistrict_Qingdao$Subdis_network_error_dis)
colnames(table) <- c("code", "pop", "estimate", "error")
write.csv(table, file = "Subdis_network_error_dis.csv")
