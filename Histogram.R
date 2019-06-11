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


#png(filename = "F1.png", w = 15/1.5, h = 15/1.5, units = "in", res = 300)
pushViewport(viewport(layout=grid.layout(4,1)))
print(h1, vp=viewport(layout.pos.col = 1, layout.pos.row = 1, height = 5))
print(h2, vp=viewport(layout.pos.col = 1, layout.pos.row = 2, height = 5))
print(h3, vp=viewport(layout.pos.col = 1, layout.pos.row = 3, height = 5))
print(h4, vp=viewport(layout.pos.col = 1, layout.pos.row = 4, height = 5))
#dev.off()


### alternative
df1 = data.frame(Scale = rep("MSOA to LSOA", length(MSOA_to_LSOA$error)), 
				Error = MSOA_to_LSOA$error, 
				Method = MSOA_to_LSOA$method)
df2 = data.frame(Scale = rep("LSOA to OA", length(LSOA_to_OA$error)),
				Error = LSOA_to_OA$error,
				Method = LSOA_to_OA$method)
df3 = data.frame(Scale = rep("MSOA to OA", length(MSOA_to_OA$error)),
				Error = MSOA_to_OA$error,
			Method = MSOA_to_OA$method)
df4 = data.frame(Scale = rep("District to Subdistrict", length(Dis_to_Subdis$error)),
				Error = Dis_to_Subdis$error, 
				Method = Dis_to_Subdis$method)

df = data.frame(rbind(df1, rbind(df2, rbind(df3, df4))))
names(df) = c("Scale", "Error", "Method")
head(df)
png(filename = "F1.png", w = 12, h = 8, units = "in", res = 300)
ggplot(df, aes(x = Error)) +
  geom_histogram(fill="firebrick3", bins = 20, col = "white") +
  facet_wrap(~Scale+Method, ncol = 5, scales = "free")+
  scale_x_continuous(breaks = 0) +
  theme(axis.line=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title = element_blank())
dev.off()



