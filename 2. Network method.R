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

#Load data
# Load Data
source_data("https://github.com/Wen-Zeng/Areal-Interpolation/blob/master/DataNet.RData?raw=True")
# or if saved locally
# load("DataNet.RData")

## Leeds case
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

Leeds_LSOA$Network_error_MSOA_to_LSOA <- Leeds_LSOA$Popu_estimate - Leeds_LSOA$pop

table <- cbind(Leeds_LSOA$code, Leeds_LSOA$pop, Leeds_LSOA$Popu_estimate,Leeds_LSOA$Network_error_MSOA_to_LSOA)
colnames(table) <- c("code", "pop", "estimate", "error")
write.csv(table, file = "Network_error_MSOA_to_LSOA.csv")


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

Leeds_OA$Network_error_MSOA_to_OA <- Leeds_OA$Popu_estimate - Leeds_OA$pop

table <- cbind(Leeds_OA$code, Leeds_OA$pop, Leeds_OA$Popu_estimate,Leeds_OA$Network_error_MSOA_to_OA)
colnames(table) <- c("code", "pop", "estimate", "error")
write.csv(table, file = "Network_error_MSOA_to_OA.csv")


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

Leeds_OA$Network_error_LSOA_to_OA <- Leeds_OA$Popu_estimate - Leeds_OA$pop

table <- cbind(Leeds_OA$code, Leeds_OA$pop, Leeds_OA$Popu_estimate,Leeds_OA$Network_error_LSOA_to_OA)
colnames(table) <- c("code", "pop", "estimate", "error")
write.csv(table, file = "Network_error_LSOA_to_OA.csv")

## The data in Qingdao case is not all open. 
## If you are interested, you can contact me. Email: Alvin_z@163.com
