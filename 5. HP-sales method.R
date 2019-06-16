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
library(tmap)
library(scales)
library(ggplot2)
library(sf)
library(repmis)

# Load Data
source_data("https://github.com/Wen-Zeng/Areal-Interpolation/blob/master/DataHPsales.RData?raw=True")
# or if saved locally
# load("DataHPsales.RData")

## Leeds case
#MSOA to LSOA 
# intersect the house sales data layer with MSOA layer
# the intersected layer is a point layer
ina <- st_intersection(Leeds_house, Leeds_MSOA)
# generate a new field named"no." representing the serial number of each intersected point
# each no. represents one intersected point in "ina"
ina$no. <- seq(1,length(ina$household),1)
# summarise the codes in MSOA, different codes represent different MSOA areas
zone.list <- sort(unique(array(ina$geo_code)))
ina$pop_hou <- 0
# calculate the population in each intersected point in "ina"
# this may take a long time
for (i in 1:length(zone.list)) { 
  item <- zone.list[i]
  # select the objects in "ina", the code of which is "item" 
  # This can select all the objects within the same MSOA area
  zone.set <- (ina$geo_code == item) 
  # find the no. list for the objects in "ina", which are in the same MSOA area
  inano.list <- ina$no.[zone.set]
  # use the household count proportion as the weight to calculate the populaiton in each intersected point
  for (j in 1:length(inano.list)) {
    item1 <- inano.list[j]
    ina$pop_hou[which(ina$no. == item1)] <- (ina$Population[which(ina$no. == item1)])*(ina$household[which(ina$no. == item1)]/sum(ina$household[zone.set], na.rm = TRUE))
    cat(i,":",j,"/",length(inano.list), "\t")
    }
}

# check whether the total populaiton is preserved
sum(ina$pop_hou,na.rm = TRUE)
sum(Leeds_MSOA$Population)

# intersect the "ina" layer with LSOA layer
# this is for estimating the population in each LSOA area
res <- st_intersection(ina,Leeds_LSOA)
# summarise the codes in LSOA, different codes represent different LSOA areas
zone.list1 <- sort(unique(array(res$code)))
Leeds_LSOA$Pop_estimate <- 0
# calculate the estimated population in each LSOA area, whose code is "item"
# this may take a long time
for (i in 1:length(zone.list1)) { 
  item <- zone.list1[i]
  zone.set <- (res$code == item) 
  # sum the population of the points in "res", which falls in the same LSOA
  Leeds_LSOA$Pop_estimate[which(Leeds_LSOA$code == item)] <- sum(res$pop_hou[zone.set],na.rm = TRUE)
  cat(i,"/",length(zone.list1), "\t")
  }

# check whether the total populaiton is preserved
sum(Leeds_LSOA$Pop_estimate)
sum(Leeds_LSOA$pop)

# calculate the difference between estimated population and actual population
Leeds_LSOA$Household_sales_error_MSOA_to_LSOA <- Leeds_LSOA$Pop_estimate - Leeds_LSOA$pop

# convert the results into csv file
table <- cbind(Leeds_LSOA$code, Leeds_LSOA$pop, Leeds_LSOA$Pop_estimate,Leeds_LSOA$Household_sales_error_MSOA_to_LSOA)
colnames(table) <- c("code", "pop", "estimate", "error")
write.csv(table, file = "Household_sales_error_MSOA_to_LSOA.csv")

#MSOA to OA 
# The route is similar to MSOA to LSOA
# "ina" is the same layer with that in above process
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

Leeds_OA$Household_sales_error_MSOA_to_OA <- Leeds_OA$Pop_estimate - Leeds_OA$pop

table <- cbind(Leeds_OA$code, Leeds_OA$pop, Leeds_OA$Pop_estimate,Leeds_OA$Household_sales_error_MSOA_to_OA)
colnames(table) <- c("code", "pop", "estimate", "error")

write.csv(table, file = "Household_sales_error_MSOA_to_OA.csv")

#LSOA to OA 
# The route is similar to MSOA to LSOA
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

Leeds_OA$Household_sales_error_LSOA_to_OA <- Leeds_OA$Pop_estimate - Leeds_OA$pop

table <- cbind(Leeds_OA$code, Leeds_OA$pop, Leeds_OA$Pop_estimate,Leeds_OA$Household_sales_error_LSOA_to_OA)
colnames(table) <- c("code", "pop", "estimate", "error")

write.csv(table, file = "Household_sales_error_LSOA_to_OA.csv")

## The data in the Qingdao case are not all open, but the codes and methods are similiar with the Leeds case.
## If you are interested, you can contact me. Email: Alvin_z@163.com
