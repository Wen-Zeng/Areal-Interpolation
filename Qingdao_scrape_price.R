library(RCurl)
library(jsonlite)
library('rvest')
library(tidyverse)
library(GISTools)
library(sf)
library(stringr)
library(RSelenium)
library(cURL)

#extract the last charactor in the string
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))}

# see https://cfss.uchicago.edu/webdata005_scraping.html
# done at "Mon Jan 14 21:54:27 2019"

# 1.  Create list of property IDs from the website
zpid_list_3 <- vector()
for (i in 1:100) {
  url <- paste0("https://qd.lianjia.com/xiaoqu/pg", i)
  webpage <- read_html(url)
  data_html <- html_nodes(webpage,'.title a')
  data_html <- as.character(data_html)
    if(length(data_html) > 1){
      for(j in 1:(length(data_html)-1)){
        data_htmls <- strsplit(data_html, '<a href=\"')[[j]][2]
        data_htmls <- strsplit(data_htmls, '\" target')[[1]][1]
        zpid_list_3 <- append(zpid_list_3, data_htmls)}
    }
  cat(i, "\t")
  }
  
length(zpid_list_3)

# 2. use this to download details for each property
p.list <- sort(unique(zpid_list_3))
length(p.list)
title_list <- vector()
building_list <- vector()
house_list <- vector()
Property_value_list <- vector()
latitude_list <- vector()
longitude_list <- vector()
for (i in 1:length(p.list)) {
  url <- p.list[i]
  if(url.exists(url)){
    webpage <- read_html(url)		
    #scrape the title
    title_data_html <- html_nodes(webpage,'.detailTitle')
    title_data_html <- html_text(title_data_html)
    title_data <- title_data_html
    if(length(title_data) == 0){title_data <- "no data"}
    #scrape the building number
    building_data_html <- html_nodes(webpage,'.xiaoquInfoItem:nth-child(6) .xiaoquInfoContent')
    building_data_html <- html_text(building_data_html)
    building_data <- gsub("栋","",building_data_html)
    if(length(building_data) == 0){building_data <- "no data"}
    #scrape the house number
    house_data_html <- html_nodes(webpage,'.xiaoquInfoItem:nth-child(7) .xiaoquInfoContent')
    house_data_html <- html_text(house_data_html)
    house_data <- gsub("户","",house_data_html)
    if(length(house_data) == 0){house_data <- "no data"}
    #scrape the Property_value
    Property_value_data_html <- html_nodes(webpage,'.xiaoquUnitPrice')
    Property_value_data_html <- html_text(Property_value_data_html)
    Property_value_data      <- Property_value_data_html
    if(length(Property_value_data) == 0){Property_value_data <- "no data"}
    #scrape the location
    rD <- rsDriver(verbose = FALSE)
    remDr <- rD$client
    remDr$navigate("http://www.r-project.org")
    XML::htmlParse(remDr$getPageSource()[[1]])
    
    location_data_html <- html_nodes(webpage,'#map .name')
    location_data_html <- html_text(location_data_html)
    location_data <- gsub("户","",house_data_html)
    
    url1 <- paste0("view-source:", url)
    webpage1 <- read_html(url1)
    data_html1 <- html_text(webpage1)
    
    html_attrs(webpage)
    
    data_html <- html_text(webpage)
    location_data <- strsplit(data_html,"\"latitude\":\"")[[1]][2]
    location_data <- strsplit(location_data,"name")[[1]][1]
    location_data <- gsub("\",\n            \"","",location_data)
    location_data <- strsplit(location_data,"\"},\"")[[1]][1]
    location_data <- gsub("\",\"longitude\":\""," ",location_data)
    latitude <- strsplit(location_data," ")[[1]][1]
    longitude <- strsplit(location_data," ")[[1]][2]
    if(length(latitude) == 0){latitude <- "no data"}
    if(length(latitude) == 0){latitude <- "no data"}
    #save data 
    title_list    <- append(title_list, title_data)
    bedroom_list  <- append(bedroom_list, bedroom_data)
    bathroom_list <- append(bathroom_list, bathroom_data)
    Property_value_list <- append(Property_value_list, Property_value_data)
    Rental_value_list <- append(Rental_value_list, Rental_value_data)
    latitude_list <- append(latitude_list, latitude) 
    longitude_list <- append(longitude_list, longitude)   
    cat("1:",i, "\t")
    }
  }

#save data and outpt
Leeds_housesales <- cbind(title_list,bedroom_list,bathroom_list,Property_value_list,Rental_value_list,latitude_list,longitude_list)
Leeds_housesales <- data.frame(Leeds_housesales)

write.csv(Leeds_housesales, file = "Leeds_houseprice_LS27.csv")
getwd()
