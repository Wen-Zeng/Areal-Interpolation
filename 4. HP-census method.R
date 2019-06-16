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
source_data("https://github.com/Wen-Zeng/Areal-Interpolation/blob/master/DataHPcensus.RData?raw=True")
# or if saved locally
# load("DataHPcensus.RData")

## Leeds case
#check the population and household count
sum(Leeds_MSOA$Population, na.rm = T)
sum(Leeds_LSOA$pop, na.rm = T)
sum(Leeds_OA$pop, na.rm = T)
sum(Leeds_MSOA$hou_num)
sum(Leeds_LSOA$hou_num)
sum(Leeds_OA$hou_num)

#MSOA to LSOA
# convert the LSOA layer into point layer for intersection
Leeds_LSOA$no. <- seq(1,length(Leeds_LSOA$code),1)
px <- CRS(proj4string(Leeds_LSOA)) 
res <- SpatialPointsDataFrame(coordinates(Leeds_LSOA),proj4string=px,data.frame(pop=Leeds_LSOA$pop,hou_num=Leeds_LSOA$hou_num,no.=Leeds_LSOA$no.))
# intersect the two layers
ina <- intersect(res, Leeds_MSOA)
# summarise the codes in MSOA, different codes represent different MSOA areas
ina$code <- as.character(ina$geo_code) 
zone.list <- sort(unique(array(ina$code)))
Leeds_LSOA$Pop_estimate <- 0
ina$pop_hou <- 0
# calculate the estimated population in each LSOA area, whose code is "item"
for (item in zone.list) { 
  # select the objects in "ina", the code of which is "item" 
  # This can select all the objects within the same MSOA area
  zone.set <- (ina$code == item) 
  # find the ID list for the objects in "ina", the code of which is "item"
  inano.list <- ina$no.[zone.set]
  # use the household count proportion as the weight to calculate the populaiton
  # the household count proportion is household count of the object "item1" in "ina" divides the total household in the MSOA area, where "item1" belongs to
  for (item1 in inano.list) {
    ina$pop_hou[which(ina$no. == item1)] <- (ina$Population[item1])*(ina$hou_num[item1]/sum(ina$hou_num[zone.set], na.rm = TRUE))
    Leeds_LSOA$Pop_estimate[which(Leeds_LSOA$no. == item1)] <- ina$pop_hou[which(ina$no. == item1)]
  }
}

# check whether the total populaiton is preserved
sum(Leeds_LSOA$Pop_estimate)
sum(Leeds_LSOA$pop)

# calculate the difference between estimated population and actual population
Leeds_LSOA$Household_census_error_MSOA_to_LSOA <- Leeds_LSOA$Pop_estimate - Leeds_LSOA$pop

# convert the results into csv file
table <- cbind(Leeds_LSOA$code, Leeds_LSOA$pop, Leeds_LSOA$Pop_estimate,Leeds_LSOA$Household_census_error_MSOA_to_LSOA)
colnames(table) <- c("code", "pop", "estimate", "error")
write.csv(table, file = "Household_census_error_MSOA_to_LSOA.csv")

#MSOA to OA
# The route is similar to MSOA to LSOA
Leeds_MSOA$fig <- seq(1,length(Leeds_MSOA$Population),1)
Leeds_OA$no. <- seq(1,length(Leeds_OA$code),1)
px <- CRS(proj4string(Leeds_OA)) 
res <- SpatialPointsDataFrame(coordinates(Leeds_OA),proj4string=px,data.frame(pop=Leeds_OA$pop,hou_num=Leeds_OA$hou_num,no.=Leeds_OA$no.))
ina <- intersect(res, Leeds_MSOA)
ina$code <- as.character(ina$geo_code) #MSOA no.
zone.list <- sort(unique(array(ina$code)))
Leeds_OA$Pop_estimate <- 0
ina$pop_hou <- 0
for (item in zone.list) { 
  zone.set <- (ina$code == item) 
  inano.list <- ina$no.[zone.set]
  for (item1 in inano.list) {
    ina$pop_hou[which(ina$no. == item1)] <- (ina$Population[item1])*(ina$hou_num[item1]/sum(ina$hou_num[zone.set], na.rm = TRUE))
    Leeds_OA$Pop_estimate[which(Leeds_OA$no. == item1)] <- ina$pop_hou[which(ina$no. == item1)]
  }
}

sum(Leeds_OA$Pop_estimate)
sum(Leeds_OA$pop)

Leeds_OA$Household_census_error_MSOA_to_OA <- Leeds_OA$Pop_estimate - Leeds_OA$pop

table <- cbind(Leeds_OA$code, Leeds_OA$pop, Leeds_OA$Pop_estimate,Leeds_OA$Household_census_error_MSOA_to_OA)
colnames(table) <- c("code", "pop", "estimate", "error")

write.csv(table, file = "Household_census_error_MSOA_to_OA.csv")

#LSOA to OA
# The route is similar to MSOA to LSOA
Leeds_LSOA$fig <- seq(1,length(Leeds_LSOA$pop),1)
Leeds_OA$no. <- seq(1,length(Leeds_OA$code),1)
px <- CRS(proj4string(Leeds_OA)) 
res <- SpatialPointsDataFrame(coordinates(Leeds_OA),proj4string=px,data.frame(pop=Leeds_OA$pop,hou_num=Leeds_OA$hou_num,no.=Leeds_OA$no.))
ina <- intersect(res, Leeds_LSOA)
ina$code <- as.character(ina$code) #LSOA no.
zone.list <- sort(unique(array(ina$code)))
ina$pop_hou <- 0
Leeds_OA$Pop_estimate <- 0
for (item in zone.list) { 
  zone.set <- (ina$code == item) 
  inano.list <- ina$no.[zone.set]
  for (item1 in inano.list) {
    ina$pop_hou[which(ina$no. == item1)] <- (ina$pop.1[item1])*(ina$hou_num[item1]/sum(ina$hou_num[zone.set], na.rm = TRUE))
    Leeds_OA$Pop_estimate[which(Leeds_OA$no. == item1)] <- ina$pop_hou[which(ina$no. == item1)]
  }
}

sum(Leeds_OA$Pop_estimate)
sum(Leeds_OA$pop)

Leeds_OA$Household_census_error_LSOA_to_OA <- Leeds_OA$Pop_estimate - Leeds_OA$pop

table <- cbind(Leeds_OA$code, Leeds_OA$pop, Leeds_OA$Pop_estimate,Leeds_OA$Household_census_error_LSOA_to_OA)
colnames(table) <- c("code", "pop", "estimate", "error")

write.csv(table, file = "Household_census_error_LSOA_to_OA.csv")

## The data in the Qingdao case are not all open, but the codes and methods are similiar with the Leeds case.
## If you are interested, you can contact me. Email: Alvin_z@163.com
