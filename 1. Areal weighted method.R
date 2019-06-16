library(GISTools)
library(sp)
library(rgdal)
library(tmap)
library(scales)
library(ggplot2)
library(rtop)
library(deldir)
library(raster)
library(sf)
library(repmis)

# Load Data
source_data("https://github.com/Wen-Zeng/Areal-Interpolation/blob/master/DataAW.RData?raw=True")
# or if saved locally
# load("DataAW.RData")

## Leeds case
#check the population and household count
sum(Leeds_MSOA$Population, na.rm = T)
sum(Leeds_LSOA$pop, na.rm = T)
sum(Leeds_OA$pop, na.rm = T)
sum(Leeds_MSOA$hou_num)
sum(Leeds_LSOA$hou_num)
sum(Leeds_OA$hou_num)

#MSOA to LSOA
# intersect the two layers
ina <- intersect(Leeds_LSOA, Leeds_MSOA)
# calculate the area of each intersected area
ina$Area <- 0
ina$Area <- area(ina)
# generate a new field named"no." representing the serial number of each intersected section
# each no. represents one intersected section in "ina"
ina$no. <- seq(1,length(ina$pop),1)
# summarise the codes in MSOA, different codes represent different MSOA areas
zone.list <- sort(unique(array(ina$geo_code)))
ina$pops <- 0
ina$area_weight <- 0
# calculate the population in each intersected zone
for (item in zone.list) { 
  zone.set <- (ina$geo_code == item) 
  inano.list <- ina$no.[zone.set]
  for (item1 in inano.list) {
    # calculate the area proportion of each intersected zone in the same MSOA
    ina$area_weight[which(ina$no. == item1)] <- ina$Area[which(ina$no. == item1)]/sum(ina$Area[zone.set], na.rm = TRUE)
  }
  for (item2 in inano.list) {
    ina$pops[which(ina$no. == item2)] = ina$Population[which(ina$no. == item2)]*ina$area_weight[which(ina$no. == item2)]
  }
}

# check whether the total populaiton is preserved
sum(ina$pops)
sum(Leeds_MSOA$Population)

# calculate the estimated population of the target zones
Leeds_LSOA$pop_estimate <-0
zone.list <- sort(unique(array(ina$code)))
for (item in zone.list){
  Leeds_LSOA$pop_estimate[which(Leeds_LSOA$code == item)] <- ina$pops[which(ina$code == item)]
}

# check whether the total populaiton is preserved
sum(Leeds_LSOA$pop_estimate)
sum(Leeds_LSOA$pop)

# calculate the difference between estimated population and actual population
Leeds_LSOA$Areal_weight_error_MSOA_to_LSOA <- Leeds_LSOA$pop_estimate - Leeds_LSOA$pop

# convert the results into csv file
table <- cbind(Leeds_LSOA$code, Leeds_LSOA$pop, Leeds_LSOA$pop_estimate,Leeds_LSOA$Areal_weight_error_MSOA_to_LSOA)
colnames(table) <- c("code", "pop", "estimate", "error")
write.csv(table, file = "Areal_weight_error_MSOA_to_LSOA.csv")

#MSOA to OA
# The route is similar to MSOA to LSOA
ina <- intersect(Leeds_OA, Leeds_MSOA)
ina$Area <- 0
ina$Area <- area(ina)
ina$no. <- seq(1,length(ina$pop),1)
zone.list <- sort(unique(array(ina$geo_code)))
ina$pops <- 0
ina$area_weight <- 0
for (item in zone.list) { 
  zone.set <- (ina$geo_code == item) 
  inano.list <- ina$no.[zone.set]
  for (item1 in inano.list) {
    ina$area_weight[which(ina$no. == item1)] <- ina$Area[which(ina$no. == item1)]/sum(ina$Area[zone.set], na.rm = TRUE)
  }
  for (item2 in inano.list) {
    ina$pops[which(ina$no. == item2)] = ina$Population[which(ina$no. == item2)]*ina$area_weight[which(ina$no. == item2)]
  }
}

sum(ina$pops)
sum(Leeds_MSOA$Population)

Leeds_OA$pop_estimate <-0
zone.list <- sort(unique(array(ina$code)))
for (item in zone.list){
  Leeds_OA$pop_estimate[which(Leeds_OA$code == item)] <- ina$pops[which(ina$code == item)]
}

sum(Leeds_OA$pop_estimate)
sum(Leeds_OA$pop)

Leeds_OA$Areal_weight_error_MSOA_to_OA <- Leeds_OA$pop_estimate - Leeds_OA$pop

table <- cbind(Leeds_OA$code, Leeds_OA$pop, Leeds_OA$pop_estimate,Leeds_OA$Areal_weight_error_MSOA_to_OA)
colnames(table) <- c("code", "pop", "estimate", "error")
write.csv(table, file = "Areal_weight_error_MSOA_to_OA.csv")

#LSOA to OA
# The route is similar to MSOA to LSOA
ina <- intersect(Leeds_OA, Leeds_LSOA)
ina$Area <- 0
ina$Area <- area(ina)
ina$no. <- seq(1,length(ina$pop.1),1)
zone.list <- sort(unique(array(ina$code.2)))
ina$pops <- 0
ina$area_weight <- 0
for (item in zone.list) { 
  zone.set <- (ina$code.2 == item) 
  inano.list <- ina$no.[zone.set]
  for (item1 in inano.list) {
    ina$area_weight[which(ina$no. == item1)] <- ina$Area[which(ina$no. == item1)]/sum(ina$Area[zone.set], na.rm = TRUE)
  }
  for (item2 in inano.list) {
    ina$pops[which(ina$no. == item2)] = ina$pop.2[which(ina$no. == item2)]*ina$area_weight[which(ina$no. == item2)]
  }
}

sum(ina$pops)
sum(Leeds_MSOA$Population)

Leeds_OA$pop_estimate <-0
zone.list <- sort(unique(array(ina$code.1)))
for (item in zone.list){
  Leeds_OA$pop_estimate[which(Leeds_OA$code == item)] <- ina$pops[which(ina$code.1 == item)]
}

sum(Leeds_OA$pop_estimate)
sum(Leeds_OA$pop)

Leeds_OA$Areal_weight_error_LSOA_to_OA <- Leeds_OA$pop_estimate - Leeds_OA$pop

table <- cbind(Leeds_OA$code, Leeds_OA$pop, Leeds_OA$pop_estimate,Leeds_OA$Areal_weight_error_LSOA_to_OA)
colnames(table) <- c("code", "pop", "estimate", "error")
write.csv(table, file = "Areal_weight_error_LSOA_to_OA.csv")

## The data in the Qingdao case are not all open, but the codes and methods are similiar with the Leeds case.
## If you are interested, you can contact me. Email: Alvin_z@163.com

