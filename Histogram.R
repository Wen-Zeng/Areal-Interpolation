library(GISTools)
library(sp)
library(rgdal)
library(tmap)
library(scales)
library(ggplot2)
library(rtop)
library(gstat)
library(Rmisc)
library(sf)
library(repmis)
library(GGally)

# This code is for the Figure "The estimation error for different interpolation approaches in different cases"

# Load Data
source_data("https://github.com/Wen-Zeng/Areal-Interpolation/blob/master/DataHist.RData?raw=True")
# or if saved locally
# load("DataHist.RData")

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
  

h2<- ggplot(LSOA_to_OA, aes(error)) +
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

multiplot(h1, h2, h3, h4, cols = 1)

