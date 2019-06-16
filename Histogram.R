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
library(Rmisc)
library(sf)
library(repmis)
library(GGally)

# This code is for the Histogram Figure "The estimation error for different interpolation approaches in different cases"

# Load Data
source_data("https://github.com/Wen-Zeng/Areal-Interpolation/blob/master/DataHist.RData?raw=True")
# or if saved locally
# load("DataHist.RData")

# MSOA to LSOA
h1 <- ggplot(MSOA_to_LSOA, aes(error)) +
  geom_histogram(fill="firebrick3", bins = 30, col = "white") +
  facet_wrap( vars(method), ncol = 5)+ 
  #ylab("Count") +
  #theme_minimal() +
  ylab("A. MSOA to LSOA, Leeds")+
  #theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))+ 
  #ggtitle("A. MSOA to LSOA, Leeds") +
  scale_x_continuous(breaks = 0) +
  theme(title = element_text(size = 8, face = "bold"))+
  theme(axis.line=element_blank(),
        #axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title = element_blank())+
  theme(axis.title.y = element_text(angle = 90, size = 8, hjust = 0.5, vjust = 0.5, face = "bold")) 
  
# LSOA to OA
h2 <- ggplot(LSOA_to_OA, aes(error)) +
  geom_histogram(fill="firebrick3", bins = 30, col = "white") +
  facet_wrap( vars(method), ncol = 5)+
  #theme_minimal() +
  ylab("B. LSOA to OA, Leeds")+
  #ggtitle("B. LSOA to OA, Leeds") +
  #theme(title = element_text(size = 8, face = "bold"))+
  scale_x_continuous(breaks = 0) +
  theme(axis.line=element_blank(),
        #axis.text=element_blank(),
        strip.text.x = element_blank(),
        axis.ticks=element_blank(),
        axis.title = element_blank())+
  theme(axis.title.y = element_text(angle = 90, size = 8, hjust = 0.5, vjust = 0.5, face = "bold"))
        
# MSOA to OA
h3 <- ggplot(MSOA_to_OA, aes(error)) +
  geom_histogram(fill="firebrick3", bins = 30, col = "white") +
  facet_wrap( vars(method), ncol = 5)+
  #theme_minimal() +
  ylab("C. MSOA to OA, Leeds")+
  #ggtitle("C. MSOA to OA, Leeds") +
  #theme(title = element_text(size = 8, face = "bold"))+
  scale_x_continuous(breaks = 0) +
  theme(axis.line=element_blank(),
        #axis.text=element_blank(),
        strip.text.x = element_blank(),
        axis.ticks=element_blank(),
        axis.title = element_blank())+
 theme(axis.title.y = element_text(angle = 90, size = 8, hjust = 0.5, vjust = 0.5, face = "bold"))

# District to Subdistrict
h4 <- ggplot(Dis_to_Subdis, aes(error)) +
  geom_histogram(fill="firebrick3", bins = 30, col = "white") +
  facet_wrap( vars(method), ncol = 5)+
  #theme_minimal() +
  ylab("D. District to Subdistrict, Qingdao")+
  #ggtitle("D. District to Subdistrict, Qingdao") +
  scale_x_continuous(breaks = 0) +
  theme(title = element_text(size = 8, face = "bold"))+
  theme(axis.line=element_blank(),
        #axis.text=element_blank(),
        axis.ticks=element_blank(),
        strip.text.x = element_blank(),
        axis.title = element_blank())+
theme(axis.title.y = element_text(angle = 90, size = 8, hjust = 0.5, vjust = 0.5, face = "bold"))

# draw the four plots on the same figure
multiplot(h1, h2, h3, h4, cols = 1)

