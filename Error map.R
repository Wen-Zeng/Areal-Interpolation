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
if (!is.element("Rmisc", installed.packages()))
  install.packages("Rmisc", dep = T)
if (!is.element("car", installed.packages()))
  install.packages("car", dep = T)
if (!is.element("GGally", installed.packages()))
  install.packages("GGally", dep = T)
# load packages
library(GISTools)
library(sp)
library(rgdal)
library(tmap)
library(scales)
library(ggplot2)
library(rtop)
library(automap)
library(deldir)
library(raster)
library(sf)
library(repmis)

# This code is for the Error distribution Figure "The error distributions of results for different interpolation approaches"

# Load Data
source_data("https://github.com/Wen-Zeng/Areal-Interpolation/blob/master/DataErrormap.RData?raw=True")
# or if saved locally
# load("DataDasy.RData")

par(mar=c(0,0,0,0))

#MSOA to LSOA
tm_shape(Error_MSOA_to_LSOA) +
  tm_fill(col='HP_census',,title='Error',palette='-RdBu', breaks = c(-2200,-500,-100,0,100,500,4700), legend.is.portrait=TRUE, midpoint = NA) + 
  tm_layout(frame = F,legend.show = FALSE)
tm_shape(Error_MSOA_to_LSOA) +
  tm_fill(col='HP_sales',title='Error',palette='-RdBu', breaks = c(-2200,-500,-100,0,100,500,4700), legend.is.portrait=TRUE, midpoint = NA) +
  tm_layout(frame = F,legend.show = FALSE)
tm_shape(Error_MSOA_to_LSOA) +
  tm_fill(col='Dasymetric',title='Error',palette='-RdBu', breaks = c(-2200,-500,-100,0,100,500,4700), legend.is.portrait=TRUE, midpoint = NA) +
  tm_layout(frame = F,legend.show = FALSE)
tm_shape(Error_MSOA_to_LSOA) +
  tm_fill(col='Network',title='Error',palette='-RdBu', breaks = c(-2200,-500,-100,0,100,500,4700), legend.is.portrait=TRUE, midpoint = NA) +
  tm_layout(frame = F,legend.show = FALSE)
tm_shape(Error_MSOA_to_LSOA) +
  tm_fill(col='Areal_weighted',title='Error',palette='-RdBu', breaks = c(-2200,-500,-100,0,100,500,4700), legend.is.portrait=TRUE, midpoint = NA) +
  tm_layout(frame = F,legend.show = FALSE)

#LSOA to OA
tm_shape(Error_LSOA_to_OA) +
  tm_fill(col='HP_census',,title='Error',palette='-RdBu', breaks = c(-1300,-100,-30,0,30,100,1650), legend.is.portrait=TRUE, midpoint = NA) + 
  tm_layout(frame = F,legend.show = FALSE)
tm_shape(Error_LSOA_to_OA) +
  tm_fill(col='HP_sales',title='Error',palette='-RdBu', breaks = c(-1300,-100,-30,0,30,100,1650), legend.is.portrait=TRUE, midpoint = NA) +
  tm_layout(frame = F,legend.show = FALSE)
tm_shape(Error_LSOA_to_OA) +
  tm_fill(col='Dasymetric',title='Error',palette='-RdBu', breaks = c(-1300,-100,-30,0,30,100,1650), legend.is.portrait=TRUE, midpoint = NA) +
  tm_layout(frame = F,legend.show = FALSE)
tm_shape(Error_LSOA_to_OA) +
  tm_fill(col='Network',title='Error',palette='-RdBu', breaks = c(-1300,-100,-30,0,30,100,1650), legend.is.portrait=TRUE, midpoint = NA) +
  tm_layout(frame = F,legend.show = FALSE)
tm_shape(Error_LSOA_to_OA) +
  tm_fill(col='Areal_weighted',title='Error',palette='-RdBu', breaks = c(-1300,-100,-30,0,30,100,1650), legend.is.portrait=TRUE, midpoint = NA) +
  tm_layout(frame = F,legend.show = FALSE)

#MSOA to OA
tm_shape(Error_MSOA_to_OA) +
  tm_fill(col='HP_census',,title='Error',palette='-RdBu', breaks = c(-2100,-100,-30,0,30,100,4200), legend.is.portrait=TRUE, midpoint = NA) + 
  tm_layout(frame = F,legend.show = FALSE)
tm_shape(Error_MSOA_to_OA) +
  tm_fill(col='HP_sales',title='Error',palette='-RdBu', breaks = c(-2100,-100,-30,0,30,100,4200), legend.is.portrait=TRUE, midpoint = NA) +
  tm_layout(frame = F,legend.show = FALSE)
tm_shape(Error_MSOA_to_OA) +
  tm_fill(col='Dasymetric',title='Error',palette='-RdBu', breaks = c(-2100,-100,-30,0,30,100,4200), legend.is.portrait=TRUE, midpoint = NA) +
  tm_layout(frame = F,legend.show = FALSE)
tm_shape(Error_MSOA_to_OA) +
  tm_fill(col='Network',title='Error',palette='-RdBu', breaks = c(-2100,-100,-30,0,30,100,4200), legend.is.portrait=TRUE, midpoint = NA) +
  tm_layout(frame = F,legend.show = FALSE)
tm_shape(Error_MSOA_to_OA) +
  tm_fill(col='Areal_weighted',title='Error',palette='-RdBu', breaks = c(-2100,-100,-30,0,30,100,4200), legend.is.portrait=TRUE, midpoint = NA) +
  tm_layout(frame = F,legend.show = FALSE)

#Legend
tm_shape(Error_MSOA_to_LSOA) +
  tm_fill(col='HP_census',,title='Error',palette='-RdBu', breaks = c(-2200,-500,-100,0,100,500,4700), legend.is.portrait=TRUE, midpoint = NA) + 
  tm_layout(frame = F,legend.only=T)

tm_shape(Error_LSOA_to_OA) +
  tm_fill(col='HP_census',,title='Error',palette='-RdBu', breaks = c(-1300,-100,-30,0,30,100,1650), legend.is.portrait=TRUE, midpoint = NA) + 
  tm_layout(frame = F,legend.only=T)

tm_shape(Error_MSOA_to_OA) +
  tm_fill(col='HP_census',,title='Error',palette='-RdBu', breaks = c(-2100,-100,-30,0,30,100,4200), legend.is.portrait=TRUE, midpoint = NA) + 
  tm_layout(frame = F,legend.only=T)

## The data in the Qingdao case are not all open, but the codes and methods are similiar with the Leeds case.
## If you are interested, you can contact me. Email: Alvin_z@163.com
