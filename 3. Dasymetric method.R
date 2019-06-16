# Check and intstall packages
if (!is.element("GISTools", installed.packages()))
  install.packages("GISTools", dep = T)
if (!is.element("sp", installed.packages()))
  install.packages("sp", dep = T)
if (!is.element("rgdal", installed.packages()))
  install.packages("rgdal", dep = T)
if (!is.element("rgeos", installed.packages()))
  install.packages("rgeos", dep = T)
if (!is.element("tmap", installed.packages()))
  install.packages("tmap", dep = T)
if (!is.element("scales", installed.packages()))
  install.packages("scales", dep = T)
if (!is.element("ggplot2", installed.packages()))
  install.packages("ggplot2", dep = T)
if (!is.element("rtop", installed.packages()))
  install.packages("rtop", dep = T)
if (!is.element("deldir", installed.packages()))
  install.packages("deldir", dep = T)
if (!is.element("raster", installed.packages()))
  install.packages("raster", dep = T)
if (!is.element("sf", installed.packages()))
  install.packages("sf", dep = T)
if (!is.element("repmis", installed.packages()))
  install.packages("repmis", dep = T)
# load packages
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

# Load Data
source_data("https://github.com/Wen-Zeng/Areal-Interpolation/blob/master/DataDasy.RData?raw=True")
# or if saved locally
# load("DataDasy.RData")

## Leeds case
#MSOA to LSOA
# intersect the land use layer with MSOA layer
ina <- st_intersection(SE_Building_Leeds, Leeds_MSOA)
# calculate the area of each intersected area
ina$Area <- st_area(ina)
ina$Area <- as.numeric(ina$Area)
# generate a new field named"ID" representing the serial number of each intersected section
# each ID represents one intersected area in "ina"
ina$ID <- seq(1,length(ina$Area),1)
# summarise the codes in MSOA, different codes represent different MSOA areas
zone.list <- sort(unique(array(ina$geo_code)))
ina$pop <- 0
# calculate the population in each intersected area in "ina"
# this may take a long time
for (i in 1:length(zone.list)) { 
  item <- zone.list[i]
  # select the objects in "ina", the code of which is "item" 
  # This can select all the objects within the same MSOA area
  zone.set <- (ina$geo_code == item) 
  # find the ID list for the objects in "ina", which are in the same MSOA area
  inano.list <- ina$ID[zone.set]
  # use the area proportion as the weight to calculate the populaiton in each intersected zone
  for (j in 1:length(inano.list)) {
    item1 <- inano.list[j]
    ina$pop[which(ina$ID == item1)] <- (ina$Population[which(ina$ID == item1)])*(ina$Area[which(ina$ID == item1)]/sum(ina$Area[zone.set], na.rm = TRUE))
    cat(i,":",j,"/",length(inano.list), "\t")
    }
}

# check whether the total populaiton is preserved
sum(ina$pop,na.rm = TRUE)
sum(Leeds_MSOA$Population)

# intersect the "ina" layer with LSOA layer
# this is for estimating the population in each LSOA area
res <- st_intersection(ina, Leeds_LSOA)
# calculate the area of each intersected zone in "res"
res$RArea <- st_area(res)
res$RArea <- as.numeric(res$RArea)
# generate a new field named"RID" representing the serial number of each intersected road
# each RID represents one intersected area in "res"
res$RID <- seq(1,length(res$RArea),1)
# summarise the codes in LSOA, different codes represent different LSOA areas
zone.list1 <- sort(unique(array(res$code)))
Leeds_LSOA$Pop_estimate <- 0
# calculate the estimated population in each LSOA area, whose code is "item"
# this may take a long time
for (i in 1:length(zone.list1)) { 
  item <- zone.list1[i]
  # select the objects in "res", the code of which is "item" 
  # This can select all the objects within the same LSOA area
  zone.set1 <- (res$code == item) 
  # find the RID list for the objects in "res", the code of which is "item"
  resno.list <- res$RID[zone.set1]
  pps <- 0
  pp <- 0
  for (j in 1:length(resno.list)) {
    item1 <- resno.list[j]
    # select the objects in "ina" layer, the ID of which is the same with "item1"
    zone.set2 <- (res$ID == res$ID[which(res$RID == item1)])
    # use the area proportion as the weight to calculate the populaiton
    # the area proportion is area of the object "item1" in "res" divides the total area in "ina", where "item1" belongs to
    pp <- (res$pop[which(res$RID == item1)])*(res$RArea[which(res$RID == item1)]/sum(res$RArea[zone.set2], na.rm = TRUE))
    # calculate the total population in the LSOA area whose code is "item"
    # This is an iteration, because everytime one object in "res" is added
    pps <- pps + pp
    cat(i,":",j,"/",length(resno.list), "\t")
  }
  Leeds_LSOA$Pop_estimate[which(Leeds_LSOA$code == item)] <- pps
}

# check whether the total populaiton is preserved
sum(Leeds_LSOA$Pop_estimate)
sum(Leeds_LSOA$pop)

# calculate the difference between estimated population and actual population
Leeds_LSOA$Dasy_error_MSOA_to_LSOA <- Leeds_LSOA$Pop_estimate - Leeds_LSOA$pop

# convert the results into csv file
table <- cbind(Leeds_LSOA$code, Leeds_LSOA$pop, Leeds_LSOA$Pop_estimate,Leeds_LSOA$Dasy_error_MSOA_to_LSOA)
colnames(table) <- c("code", "pop", "estimate", "error")
write.csv(table, file = "Dasy_error_MSOA_to_LSOA.csv")


#MSOA to OA
# The route is similar to MSOA to LSOA
# "ina" is the same layer with that in above process
res <- st_intersection(ina,Leeds_OA)
res$RArea <- st_area(res)
res$RArea <- as.numeric(res$RArea)
res$RID <- seq(1,length(res$RArea),1)
zone.list1 <- sort(unique(array(res$code)))
Leeds_OA$Pop_estimate <- 0
for (i in 1:length(zone.list1)) { 
  item <- zone.list1[i]
  zone.set1 <- (res$code == item) 
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

Leeds_OA$Dasy_error_MSOA_to_OA <- Leeds_OA$Pop_estimate - Leeds_OA$pop

table <- cbind(Leeds_OA$code, Leeds_OA$pop, Leeds_OA$Pop_estimate,Leeds_OA$Dasy_error_MSOA_to_OA)
colnames(table) <- c("code", "pop", "estimate", "error")

write.csv(table, file = "Dasy_error_MSOA_to_OA.csv")


#LSOA to OA
# The route is similar to MSOA to LSOA
ina <- st_intersection(SE_Building_Leeds, Leeds_LSOA)
ina$Area <- st_area(ina)
ina$Area <- as.numeric(ina$Area)
ina$ID <- seq(1,length(ina$Area),1)
zone.list <- sort(unique(array(ina$code)))
ina$pop1 <- 0
for (i in 1:length(zone.list)) { 
  item <- zone.list[i]
  zone.set <- (ina$code == item) 
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
    pp <- (res$pop1[which(res$RID == item1)])*(res$RArea[which(res$RID == item1)]/sum(res$RArea[zone.set2], na.rm = TRUE))
    pps <- pps + pp
    cat(i,":",j,"/",length(resno.list), "\t")
  }
  Leeds_OA$Pop_estimate[which(Leeds_OA$code == item)] <- pps
}

sum(Leeds_OA$Pop_estimate)
sum(Leeds_OA$pop)

Leeds_OA$Dasy_error_LSOA_to_OA <- Leeds_OA$Pop_estimate - Leeds_OA$pop

table <- cbind(Leeds_OA$code, Leeds_OA$pop, Leeds_OA$Pop_estimate,Leeds_OA$Dasy_error_LSOA_to_OA)
colnames(table) <- c("code", "pop", "estimate", "error")

write.csv(table, file = "Dasy_error_LSOA_to_OA.csv")

## The data in the Qingdao case are not all open, but the codes and methods are similiar with the Leeds case.
## If you are interested, you can contact me. Email: Alvin_z@163.com
