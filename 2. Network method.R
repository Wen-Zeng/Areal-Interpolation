library(GISTools)
library(sp)
library(rgdal)
library(rgeos) 
library(tmap)
library(scales)
library(ggplot2)
library(rtop)
library(deldir)
library(raster)
library(sf)
library(repmis)

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

#MSOA to LSOA
# intersect the road layer with MSOA layer
# the intersected layer is a line layer
ina <- st_intersection(Road_Leeds, Leeds_MSOA)
# calculate the length of each intersected road
ina$Length <- st_length(ina)
ina$Length <- as.numeric(ina$Length)
# generate a new field named"ID" representing the serial number of each intersected section
# each ID represents one intersected section in "ina"
ina$ID <- seq(1,length(ina$Length),1)
# summarise the codes in MSOA, different codes represent different MSOA areas
zone.list <- sort(unique(array(ina$geo_code)))
ina$pop <- 0
# calculate the population in each intersected section in "ina"
# in this case, we can consider it as the populaiotn in each road section
for (item in zone.list) { 
  # select the objects in "ina", the code of which is "item" 
  # This can select all the objects within the same MSOA area
  zone.set <- (ina$geo_code == item) 
  # find the ID list for the objects in "ina", which are in the same MSOA area
  inano.list <- ina$ID[zone.set]
  # use the road length proportion as the weight to calculate the populaiton in each intersected section
  for (item1 in inano.list) {
    ina$pop[which(ina$ID == item1)] <- (ina$Population[which(ina$ID == item1)])*(ina$Length[which(ina$ID == item1)]/sum(ina$Length[zone.set], na.rm = TRUE))
   }
}

# check whether the total populaiton is preserved
sum(ina$pop,na.rm = TRUE)
sum(Leeds_MSOA$Population)

# intersect above intersected layer with LSOA layer
# this is for estimating the population in each LSOA area
res <- st_intersection(ina, Leeds_LSOA)
# calculate the length of each intersected road
res$RLength <- st_length(res)
res$RLength <- as.numeric(res$RLength)
# generate a new field named"RID" representing the serial number of each intersected road
# each RID represents one intersected area in "res"
res$RID <- seq(1,length(res$RLength),1)
# summarise the codes in LSOA, different codes represent different LSOA areas
zone.list1 <- sort(unique(array(res$code)))
Leeds_LSOA$Popu_estimate <- 0
# calculate the estimated population in each LSOA area, whose code is "item"
for (item in zone.list1) { 
  # select the objects in "res", the code of which is "item" 
  # This can select all the objects within the same LSOA area
  zone.set1 <- (res$code == item) 
  # find the RID list for the objects in "res", the code of which is "item"
  resno.list <- res$RID[zone.set1]
  pps <- 0
  pp <- 0
  # calculate the population in each LSOA, whose code is "item"
  for (item1 in resno.list) {
    # select the objects in "ina" layer, the ID of which is the same with "item1"
    zone.set2 <- (res$ID == res$ID[which(res$RID == item1)])
    # use the road length proportion as the weight to calculate the populaiton
    # the road length proportion is the road length of the object "item1" in "res" divides the total road length in "ina", where "item1" belongs to
    pp <- (res$pop[which(res$RID == item1)])*(res$RLength[which(res$RID == item1)]/sum(res$RLength[zone.set2], na.rm = TRUE))
    # calculate the total population in the LSOA area whose code is "item"
    # This is an iteration, because everytime one object in "res" is added
    pps <- pps + pp    
  }
  # calculate the estimated population in the LSOA area whose code is "item"
  Leeds_LSOA$Popu_estimate[which(Leeds_LSOA$code == item)] <- pps
}

# check whether the total populaiton is preserved
sum(Leeds_LSOA$Popu_estimate)
sum(Leeds_LSOA$pop)

# calculate the difference between estimated population and actual population
Leeds_LSOA$Network_error_MSOA_to_LSOA <- Leeds_LSOA$Popu_estimate - Leeds_LSOA$pop

# convert the results into csv file
table <- cbind(Leeds_LSOA$code, Leeds_LSOA$pop, Leeds_LSOA$Popu_estimate,Leeds_LSOA$Network_error_MSOA_to_LSOA)
colnames(table) <- c("code", "pop", "estimate", "error")
write.csv(table, file = "Network_error_MSOA_to_LSOA.csv")


#MSOA to OA
# The route is similar to MSOA to LSOA
# "ina" is the same layer with that in above process
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

Leeds_OA$Network_error_MSOA_to_OA <- Leeds_OA$Popu_estimate - Leeds_OA$pop

table <- cbind(Leeds_OA$code, Leeds_OA$pop, Leeds_OA$Popu_estimate,Leeds_OA$Network_error_MSOA_to_OA)
colnames(table) <- c("code", "pop", "estimate", "error")
write.csv(table, file = "Network_error_MSOA_to_OA.csv")


#LSOA to OA
# The route is similar to MSOA to LSOA
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

Leeds_OA$Network_error_LSOA_to_OA <- Leeds_OA$Popu_estimate - Leeds_OA$pop

table <- cbind(Leeds_OA$code, Leeds_OA$pop, Leeds_OA$Popu_estimate,Leeds_OA$Network_error_LSOA_to_OA)
colnames(table) <- c("code", "pop", "estimate", "error")
write.csv(table, file = "Network_error_LSOA_to_OA.csv")

## The data in the Qingdao case are not all open, but the codes and methods are similiar with the Leeds case.
## If you are interested, you can contact me. Email: Alvin_z@163.com
