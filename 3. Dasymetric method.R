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
library(repmis)

# Load Data
source_data("https://github.com/Wen-Zeng/Areal-Interpolation/blob/master/DataDasy.RData?raw=True")
# or if saved locally
# load("DataDasy.RData")

## Leeds case
#MSOA to LSOA
par(mar=c(1,1,1,1))
ina <- st_intersection(SE_Building_Leeds, Leeds_MSOA)
ina$Area <- st_area(ina)
ina$Area <- as.numeric(ina$Area)
ina$ID <- seq(1,length(ina$Area),1)
zone.list <- sort(unique(array(ina$geo_code)))
ina$pop <- 0
for (i in 1:length(zone.list)) { 
  item <- zone.list[i]
  zone.set <- (ina$geo_code == item) 
  inano.list <- ina$ID[zone.set]
  for (j in 1:length(inano.list)) {
    item1 <- inano.list[j]
    ina$pop[which(ina$ID == item1)] <- (ina$Population[which(ina$ID == item1)])*(ina$Area[which(ina$ID == item1)]/sum(ina$Area[zone.set], na.rm = TRUE))
    cat(i,":",j,"/",length(inano.list), "\t")
    }
}

sum(ina$pop,na.rm = TRUE)
sum(Leeds_MSOA$Population)

res <- st_intersection(ina, Leeds_LSOA)
res$RArea <- st_area(res)
res$RArea <- as.numeric(res$RArea)
res$RID <- seq(1,length(res$RArea),1)
zone.list1 <- sort(unique(array(res$code.1)))
Leeds_LSOA$Pop_estimate <- 0
for (i in 1:length(zone.list1)) { 
  item <- zone.list1[i]
  zone.set1 <- (res$code.1 == item) 
  resno.list <- res$RID[zone.set1]
  pps <- 0
  pp <- 0
  for (j in 1:length(resno.list)) {
    item1 <- resno.list[j]
    zone.set2 <- (res$ID == res$ID[which(res$RID == item1)])
    pp <- (res$pop[which(res$RID == item1)])*(res$RArea[which(res$RID == item1)]/sum(res$RArea[zone.set2], na.rm = TRUE))
    pps <- pps + pp
    cat(i,":",j,"/",length(resno.list), "\t")
  }
  Leeds_LSOA$Pop_estimate[which(Leeds_LSOA$code == item)] <- pps
}

sum(Leeds_LSOA$Pop_estimate)
sum(Leeds_LSOA$pop)

cor.test(Leeds_LSOA$pop,Leeds_LSOA$Pop_estimate)

Leeds_LSOA$Dasy_error_MSOA_to_LSOA <- Leeds_LSOA$Pop_estimate - Leeds_LSOA$pop

table <- cbind(Leeds_LSOA$code, Leeds_LSOA$pop, Leeds_LSOA$Pop_estimate,Leeds_LSOA$Dasy_error_MSOA_to_LSOA)
colnames(table) <- c("code", "pop", "estimate", "error")

write.csv(table, file = "Dasy_error_MSOA_to_LSOA.csv")


#MSOA to OA
res <- st_intersection(ina,Leeds_OA)
res$RArea <- st_area(res)
res$RArea <- as.numeric(res$RArea)
res$RID <- seq(1,length(res$RArea),1)
zone.list1 <- sort(unique(array(res$code.1)))
Leeds_OA$Pop_estimate <- 0
for (i in 1:length(zone.list1)) { 
  item <- zone.list1[i]
  zone.set1 <- (res$code.1 == item) 
  resno.list <- res$RID[zone.set1]
  pps <- 0
  pp <- 0
  for (j in 1:length(resno.list)) {
    item1 <- resno.list[j]
    zone.set2 <- (res$ID == res$ID[which(res$RID == item1)])
    pp <- (res$pop[which(res$RID == item1)])*(res$RArea[which(res$RID == item1)]/sum(res$RArea[zone.set2], na.rm = TRUE))
    pps <- pps + pp
    cat(i,":",j,"/",length(resno.list), "\t")
  }
  Leeds_OA$Pop_estimate[which(Leeds_OA$code == item)] <- pps
}

sum(Leeds_OA$Pop_estimate)
sum(Leeds_OA$pop)

cor.test(Leeds_OA$pop,Leeds_OA$Pop_estimate)

Leeds_OA$Dasy_error_MSOA_to_OA <- Leeds_OA$Pop_estimate - Leeds_OA$pop

table <- cbind(Leeds_OA$code, Leeds_OA$pop, Leeds_OA$Pop_estimate,Leeds_OA$Dasy_error_MSOA_to_OA)
colnames(table) <- c("code", "pop", "estimate", "error")

write.csv(table, file = "Dasy_error_MSOA_to_OA.csv")


#LSOA to OA
ina <- st_intersection(SE_Building_Leeds, Leeds_LSOA)
ina$Area <- st_area(ina)
ina$Area <- as.numeric(ina$Area)
ina$ID <- seq(1,length(ina$Area),1)
zone.list <- sort(unique(array(ina$code.1)))
ina$pop1 <- 0
for (i in 1:length(zone.list)) { 
  item <- zone.list[i]
  zone.set <- (ina$code.1 == item) 
  inano.list <- ina$ID[zone.set]
  for (j in 1:length(inano.list)) {
    item1 <- inano.list[j]
    ina$pop1[which(ina$ID == item1)] <- (ina$pop[which(ina$ID == item1)])*(ina$Area[which(ina$ID == item1)]/sum(ina$Area[zone.set], na.rm = TRUE))
    cat(i,":",j,"/",length(inano.list), "\t")
    }
}

sum(ina$pop1)
sum(Leeds_LSOA$pop)

res <- st_intersection(ina,Leeds_OA)
res$RArea <- st_area(res)
res$RArea <- as.numeric(res$RArea)
res$RID <- seq(1,length(res$RArea),1)
zone.list1 <- sort(unique(array(res$code.2)))
Leeds_OA$Pop_estimate <- 0
for (i in 1:length(zone.list1)) { 
  item <- zone.list1[i]
  zone.set1 <- (res$code.2 == item) 
  resno.list <- res$RID[zone.set1]
  pps <- 0
  pp <- 0
  for (j in 1:length(resno.list)) {
    item1 <- resno.list[j]
    zone.set2 <- (res$ID == res$ID[which(res$RID == item1)])
    pp <- (res$pop1[which(res$RID == item1)])*(res$RArea[which(res$RID == item1)]/sum(res$RArea[zone.set2], na.rm = TRUE))
    pps <- pps + pp
    cat(i,":",j,"/",length(resno.list), "\t")
  }
  Leeds_OA$Pop_estimate[which(Leeds_OA$code == item)] <- pps
}

sum(Leeds_OA$Pop_estimate)
sum(Leeds_OA$pop)

cor.test(Leeds_OA$pop,Leeds_OA$Pop_estimate)

Leeds_OA$Dasy_error_LSOA_to_OA <- Leeds_OA$Pop_estimate - Leeds_OA$pop

table <- cbind(Leeds_OA$code, Leeds_OA$pop, Leeds_OA$Pop_estimate,Leeds_OA$Dasy_error_LSOA_to_OA)
colnames(table) <- c("code", "pop", "estimate", "error")

write.csv(table, file = "Dasy_error_LSOA_to_OA.csv")

## The data in Qingdao case is not all open. 
## If you are interested, you can contact me. Email: Alvin_z@163.com
