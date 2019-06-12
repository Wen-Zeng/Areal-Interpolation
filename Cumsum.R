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
library(car)
library(Rmisc)
library(repmis)

# This code is for the Figure "The cumulative percentage of absolute error for the different interpolation approaches"

# Load Data
source_data("https://github.com/Wen-Zeng/Areal-Interpolation/blob/master/DataCumsum.RData?raw=True")
# or if saved locally
# load("DataCumsum.RData")

g1 <- ggplot()+
  geom_line(data = Cumsum_LSOA_to_OA,aes(x = Cumsum_LSOA_to_OA$Household_sales_abs,y = Cumsum_LSOA_to_OA$Household_sales.1, colour  = "HP_sales"),size=1) +
  geom_point(data = Cumsum_LSOA_to_OA,aes(x = Cumsum_LSOA_to_OA$Household_sales_abs,y = Cumsum_LSOA_to_OA$Household_sales.1, colour  = "HP_sales"),size=3,alpha = 0.2) +
  geom_line(data = Cumsum_LSOA_to_OA,aes(x = Cumsum_LSOA_to_OA$Household_census_abs,y = Cumsum_LSOA_to_OA$Household_census.1,colour  = "HP_census"),size=1) +
  geom_point(data = Cumsum_LSOA_to_OA,aes(x = Cumsum_LSOA_to_OA$Household_census_abs,y = Cumsum_LSOA_to_OA$Household_census.1,colour  = "HP_census"),size=3,alpha = 0.2) +
  geom_line(data = Cumsum_LSOA_to_OA,aes(x = Cumsum_LSOA_to_OA$Dasymetric_abs,y = Cumsum_LSOA_to_OA$Dasymetric.1, colour = "Dasymetric"),size=1) +
  geom_point(data = Cumsum_LSOA_to_OA,aes(x = Cumsum_LSOA_to_OA$Dasymetric_abs,y = Cumsum_LSOA_to_OA$Dasymetric.1, colour = "Dasymetric"),size=3,alpha = 0.2) +
  geom_line(data = Cumsum_LSOA_to_OA,aes(x = Cumsum_LSOA_to_OA$Network_abs,y = Cumsum_LSOA_to_OA$Network.1, colour = "Network"),size=1) +
  geom_point(data = Cumsum_LSOA_to_OA,aes(x = Cumsum_LSOA_to_OA$Network_abs,y = Cumsum_LSOA_to_OA$Network.1, colour = "Network"),size=3,alpha = 0.2) +
  geom_line(data = Cumsum_LSOA_to_OA,aes(x = Cumsum_LSOA_to_OA$Areal_weight_abs,y = Cumsum_LSOA_to_OA$Areal_weight.1, colour = "Areal_weight"),size=1) +
  geom_point(data = Cumsum_LSOA_to_OA,aes(x = Cumsum_LSOA_to_OA$Areal_weight_abs,y = Cumsum_LSOA_to_OA$Areal_weight.1, colour = "Areal_weight"),size=3,alpha = 0.2) +
  xlab("Absolute Error") + theme(axis.title.x = element_text(size = 15, color = "black", face = "bold", vjust = 0.5, hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 10, color = "black", face = "bold")) +
  ylab("Cumulative Percentage") + theme(axis.title.y = element_text(size = 15, color = "black", face = "bold", vjust = 0.5, hjust = 0.5)) +
  scale_x_continuous(breaks=seq(0, 1500, 300)) +
    scale_y_continuous(labels=percent, breaks=seq(0, 1, 0.2)) +
  theme(axis.text.y = element_text(size = 10, color = "black", face = "bold")) +
  theme(axis.line = element_line(colour = "black")) +
  geom_hline(aes(yintercept = 0.8), colour="#BB0000", linetype="dashed") +
  scale_color_brewer(palette = 'Set2', direction = 1, breaks = c('HP_sales','HP_census','Dasymetric','Network','Areal_weight')) +
  theme(legend.title = element_text(color="black", size=16, face="bold"),legend.position = 'bottom') +
  labs(color='Methods: ')   +
  theme(legend.text = element_text(color="black", size = 10, face = "bold")) +   
  theme(panel.grid =element_blank(), panel.background = element_blank()) +  
  ggtitle('C. LSOA to OA in Leeds, UK') +
  guides(colour = guide_legend(nrow = 2)) 


g2 <- ggplot()+
  geom_line(data = Cumsum_MSOA_to_LSOA,aes(x = Cumsum_MSOA_to_LSOA$Household_sales_abs,y = Cumsum_MSOA_to_LSOA$Household_sales.1, colour  = "HP_sales"),size=1) +
  geom_point(data = Cumsum_MSOA_to_LSOA,aes(x = Cumsum_MSOA_to_LSOA$Household_sales_abs,y = Cumsum_MSOA_to_LSOA$Household_sales.1, colour  = "HP_sales"),size=3,alpha = 0.2) +
  geom_line(data = Cumsum_MSOA_to_LSOA,aes(x = Cumsum_MSOA_to_LSOA$Household_census_abs,y = Cumsum_MSOA_to_LSOA$Household_census.1,colour  = "HP_census"),size=1) +
  geom_point(data = Cumsum_MSOA_to_LSOA,aes(x = Cumsum_MSOA_to_LSOA$Household_census_abs,y = Cumsum_MSOA_to_LSOA$Household_census.1,colour  = "HP_census"),size=3,alpha = 0.2) +
  geom_line(data = Cumsum_MSOA_to_LSOA,aes(x = Cumsum_MSOA_to_LSOA$Dasymetric_abs,y = Cumsum_MSOA_to_LSOA$Dasymetric.1, colour = "Dasymetric"),size=1) +
  geom_point(data = Cumsum_MSOA_to_LSOA,aes(x = Cumsum_MSOA_to_LSOA$Dasymetric_abs,y = Cumsum_MSOA_to_LSOA$Dasymetric.1, colour = "Dasymetric"),size=3,alpha = 0.2) +
  geom_line(data = Cumsum_MSOA_to_LSOA,aes(x = Cumsum_MSOA_to_LSOA$Network_abs,y = Cumsum_MSOA_to_LSOA$Network.1, colour = "Network"),size=1) +
  geom_point(data = Cumsum_MSOA_to_LSOA,aes(x = Cumsum_MSOA_to_LSOA$Network_abs,y = Cumsum_MSOA_to_LSOA$Network.1, colour = "Network"),size=3,alpha = 0.2) +
  geom_line(data = Cumsum_MSOA_to_LSOA,aes(x = Cumsum_MSOA_to_LSOA$Areal_weight_abs,y = Cumsum_MSOA_to_LSOA$Areal_weight.1, colour = "Areal_weight"),size=1) +
  geom_point(data = Cumsum_MSOA_to_LSOA,aes(x = Cumsum_MSOA_to_LSOA$Areal_weight_abs,y = Cumsum_MSOA_to_LSOA$Areal_weight.1, colour = "Areal_weight"),size=3,alpha = 0.2) +
  xlab("Absolute Error") + theme(axis.title.x = element_text(size = 15, color = "black", face = "bold", vjust = 0.5, hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 10, color = "black", face = "bold")) +
  ylab("Cumulative Percentage") + theme(axis.title.y = element_text(size = 15, color = "black", face = "bold", vjust = 0.5, hjust = 0.5)) +
  scale_x_continuous(breaks=seq(0, 4000, 800)) +
  scale_y_continuous(labels=percent, breaks=seq(0, 1, 0.2)) +
  theme(axis.text.y = element_text(size = 10, color = "black", face = "bold")) +
  theme(axis.line = element_line(colour = "black")) +
  geom_hline(aes(yintercept = 0.8), colour="#BB0000", linetype="dashed") +
  scale_color_brewer(palette = 'Set2', direction = 1, breaks = c('HP_sales','HP_census','Dasymetric','Network','Areal_weight')) +
  theme(legend.title = element_text(color="black", size=16, face="bold"),legend.position = 'bottom') +
  labs(color='Methods: ')   +
  theme(legend.text = element_text(color="black", size = 10, face = "bold")) +   
  theme(panel.grid =element_blank(), panel.background = element_blank()) +   
  ggtitle('A. MSOA to LSOA in Leeds, UK') +
  guides(colour = guide_legend(nrow = 2))

g3 <- ggplot()+
  geom_line(data = Cumsum_MSOA_to_OA,aes(x = Cumsum_MSOA_to_OA$Household_sales_abs,y = Cumsum_MSOA_to_OA$Household_sales.1, colour  = "HP_sales"),size=1) +
  geom_point(data = Cumsum_MSOA_to_OA,aes(x = Cumsum_MSOA_to_OA$Household_sales_abs,y = Cumsum_MSOA_to_OA$Household_sales.1, colour  = "HP_sales"),size=3,alpha = 0.2) +
  geom_line(data = Cumsum_MSOA_to_OA,aes(x = Cumsum_MSOA_to_OA$Household_census_abs,y = Cumsum_MSOA_to_OA$Household_census.1,colour  = "HP_census"),size=1) +
  geom_point(data = Cumsum_MSOA_to_OA,aes(x = Cumsum_MSOA_to_OA$Household_census_abs,y = Cumsum_MSOA_to_OA$Household_census.1,colour  = "HP_census"),size=3,alpha = 0.2) +
  geom_line(data = Cumsum_MSOA_to_OA,aes(x = Cumsum_MSOA_to_OA$Dasymetric_abs,y = Cumsum_MSOA_to_OA$Dasymetric.1, colour = "Dasymetric"),size=1) +
  geom_point(data = Cumsum_MSOA_to_OA,aes(x = Cumsum_MSOA_to_OA$Dasymetric_abs,y = Cumsum_MSOA_to_OA$Dasymetric.1, colour = "Dasymetric"),size=3,alpha = 0.2) +
  geom_line(data = Cumsum_MSOA_to_OA,aes(x = Cumsum_MSOA_to_OA$Network_abs,y = Cumsum_MSOA_to_OA$Network.1, colour = "Network"),size=1) +
  geom_point(data = Cumsum_MSOA_to_OA,aes(x = Cumsum_MSOA_to_OA$Network_abs,y = Cumsum_MSOA_to_OA$Network.1, colour = "Network"),size=3,alpha = 0.2) +
  geom_line(data = Cumsum_MSOA_to_OA,aes(x = Cumsum_MSOA_to_OA$Areal_weight_abs,y = Cumsum_MSOA_to_OA$Areal_weight.1, colour = "Areal_weight"),size=1) +
  geom_point(data = Cumsum_MSOA_to_OA,aes(x = Cumsum_MSOA_to_OA$Areal_weight_abs,y = Cumsum_MSOA_to_OA$Areal_weight.1, colour = "Areal_weight"),size=3,alpha = 0.2) +
  xlab("Absolute Error") + theme(axis.title.x = element_text(size = 15, color = "black", face = "bold", vjust = 0.5, hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 10, color = "black", face = "bold")) +
  ylab("Cumulative Percentage") + theme(axis.title.y = element_text(size = 15, color = "black", face = "bold", vjust = 0.5, hjust = 0.5)) +
  scale_x_continuous(breaks=seq(0, 5000, 500)) +
  scale_y_continuous(labels=percent, breaks=seq(0, 1, 0.2)) +
  theme(axis.text.y = element_text(size = 10, color = "black", face = "bold")) +
  theme(axis.line = element_line(colour = "black")) +
  geom_hline(aes(yintercept = 0.8), colour="#BB0000", linetype="dashed") +
  scale_color_brewer(palette = 'Set2', direction = 1, breaks = c('HP_sales','HP_census','Dasymetric','Network','Areal_weight')) +
  theme(legend.title = element_text(color="black", size=16, face="bold"),legend.position = 'bottom') +
  labs(color='Methods: ')   +
  theme(legend.text = element_text(color="black", size = 10, face = "bold")) +  
  theme(panel.grid =element_blank(), panel.background = element_blank())+   
  ggtitle('B. MSOA to OA in Leeds, UK') +
  guides(colour = guide_legend(nrow = 2))


g4 <- ggplot()+
  geom_line(data = Cumsum_Dis_to_Subdis,aes(x = Cumsum_Dis_to_Subdis$Household_sales_abs,y = Cumsum_Dis_to_Subdis$Household_sales.1, colour  = "HP_sales"),size=1) +
  geom_point(data = Cumsum_Dis_to_Subdis,aes(x = Cumsum_Dis_to_Subdis$Household_sales_abs,y = Cumsum_Dis_to_Subdis$Household_sales.1, colour  = "HP_sales"),size=3,alpha = 0.2) +
  geom_line(data = Cumsum_Dis_to_Subdis,aes(x = Cumsum_Dis_to_Subdis$Household_census_abs,y = Cumsum_Dis_to_Subdis$Household_census.1,colour  = "HP_census"),size=1) +
  geom_point(data = Cumsum_Dis_to_Subdis,aes(x = Cumsum_Dis_to_Subdis$Household_census_abs,y = Cumsum_Dis_to_Subdis$Household_census.1,colour  = "HP_census"),size=3,alpha = 0.2) +
  geom_line(data = Cumsum_Dis_to_Subdis,aes(x = Cumsum_Dis_to_Subdis$Dasymetric_abs,y = Cumsum_Dis_to_Subdis$Dasymetric.1, colour = "Dasymetric"),size=1) +
  geom_point(data = Cumsum_Dis_to_Subdis,aes(x = Cumsum_Dis_to_Subdis$Dasymetric_abs,y = Cumsum_Dis_to_Subdis$Dasymetric.1, colour = "Dasymetric"),size=3,alpha = 0.2) +
  geom_line(data = Cumsum_Dis_to_Subdis,aes(x = Cumsum_Dis_to_Subdis$Network_abs,y = Cumsum_Dis_to_Subdis$Network.1, colour = "Network"),size=1) +
  geom_point(data = Cumsum_Dis_to_Subdis,aes(x = Cumsum_Dis_to_Subdis$Network_abs,y = Cumsum_Dis_to_Subdis$Network.1, colour = "Network"),size=3,alpha = 0.2) +
  geom_line(data = Cumsum_Dis_to_Subdis,aes(x = Cumsum_Dis_to_Subdis$Areal_weight_abs,y = Cumsum_Dis_to_Subdis$Areal_weight.1, colour = "Areal_weight"),size=1) +
  geom_point(data = Cumsum_Dis_to_Subdis,aes(x = Cumsum_Dis_to_Subdis$Areal_weight_abs,y = Cumsum_Dis_to_Subdis$Areal_weight.1, colour = "Areal_weight"),size=3,alpha = 0.2) +
  xlab("Absolute Error") + theme(axis.title.x = element_text(size = 15, color = "black", face = "bold", vjust = 0.5, hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 10, color = "black", face = "bold")) +
  ylab("Cumulative Percentage") + theme(axis.title.y = element_text(size = 15, color = "black", face = "bold", vjust = 0.5, hjust = 0.5)) +
  scale_y_continuous(labels=percent, breaks=seq(0, 1, 0.2)) +
  theme(axis.text.y = element_text(size = 10, color = "black", face = "bold")) +
  theme(axis.line = element_line(colour = "black")) +
  geom_hline(aes(yintercept = 0.8), colour="#BB0000", linetype="dashed") +
  scale_color_brewer(palette = 'Set2', direction = 1, breaks = c('HP_sales','HP_census','Dasymetric','Network','Areal_weight')) +
  theme(legend.title = element_text(color="black", size=16, face="bold"),legend.position = 'bottom') +
  labs(color='Methods: ')   +
  theme(legend.text = element_text(color="black", size = 10, face = "bold")) +  
  theme(panel.grid =element_blank(), panel.background = element_blank())+   
  ggtitle('D. District to Subdistrict in Qingdao, China') +
  guides(colour = guide_legend(nrow = 2))

multiplot(g2, g1, g3, g4, cols = 2)
