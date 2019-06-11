library(RCurl)
library(jsonlite)
library('rvest')
library(tidyverse)
library(GISTools)
library(sf)
library(stringr)

#extract the last charactor in the string
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))}

# see https://cfss.uchicago.edu/webdata005_scraping.html
# done at "Tue Jan 29 22:00:15 2019"

# 1.  Create list of property IDs from the website
zpid_list <- vector()
# Since the data is big, I suggest you scrape the data through each "ls" number one by one.
# For example, this time just scrape the data on LS1, next time scrape the data on LS2. Every time just change "i".
for (i in 1:29){	
  url <- paste0("https://www.zoopla.co.uk/house-prices/browse/ls", i)
  webpage <- read_html(url)
  #test the webpage if have subsequent pages
  page_test <- html_nodes(webpage,'.bg-muted')
  if(length(page_test)!= 0){
    page_test <- as.character(page_test)
    page_test <- str_extract_all(page_test,"[0-9]+") #select the numbers 
    page_test <- max(as.numeric(page_test[[1]]))
    } else{page_test <- 1}
  cat("LS",i, "\t")
  for(i in 1:page_test){
    url_1 <- paste0(url,"/?pn=", i)
    webpage <- read_html(url_1)
    #Using CSS selectors to scrap the link section
    data_html <- html_nodes(webpage,'.browse-cell-address')
    data_html <- as.character(data_html)
    if(length(data_html) > 1){
    for(j in 1:length(data_html)){
      data_htmls <- strsplit(data_html, 'href=\"/')[[j]][2]
      data_htmls <- strsplit(data_htmls, '/\">')[[1]][1]
      #data_html <- substring(data_html,65,155)
      zpid_list <- append(zpid_list, data_htmls)}
    }
  }
}
length(unique(zpid_list))

zpid_list_2 <- sort(unique(zpid_list))
zpid_list_3 <- vector()
for (i in 1:length(zpid_list_2)) {
  pid.i <- zpid_list_2[i]
  url <- paste0("https://www.zoopla.co.uk/", pid.i)
  webpage <- read_html(url)
  #test the webpage if have subsequent pages
  page_test <- html_nodes(webpage,'.bg-muted')
  if(length(page_test)!= 0){
    page_test <- as.character(page_test)
    page_test <- str_extract_all(page_test,"[0-9]+") #select the numbers 
    page_test <- max(as.numeric(page_test[[1]]))
    } else{page_test <- 1}
  for(j in 1:page_test){
    url_1 <- paste0(url, "/?pn=",j)
    webpage <- read_html(url_1)		
    data_html <- html_nodes(webpage,'.browse-cell-address')
    data_html <- as.character(data_html)
    if(length(data_html) > 1){
      for(j in 2:length(data_html)){
        data_htmls <- strsplit(data_html, '<a href=\"/')[[j]][2]
        data_htmls <- strsplit(data_htmls, '\">\n')[[1]][1]
        zpid_list_3 <- append(zpid_list_3, data_htmls)}
    }
  }
  cat(i, "\t")
}

length(zpid_list_3)

# 2. use this to download details for each property
p.list <- sort(unique(zpid_list_3))
length(p.list)
title_list <- vector()
bedroom_list <- vector()
bathroom_list <- vector()
Property_value_list <- vector()
Rental_value_list <- vector()
latitude_list <- vector()
longitude_list <- vector()
for (i in 1:length(p.list)) {
  pid.i <- p.list[i]
  url <- paste0("https://www.zoopla.co.uk/", pid.i)
  if(url.exists(url)){
    webpage <- read_html(url)		
    #scrape the title
    title_data_html <- html_nodes(webpage,'.pdp-property-summary__title')
    title_data_html <- html_text(title_data_html)
    title_data <- gsub("\n                        Property details for\n                        ","",title_data_html)
    if(length(title_data) == 0){title_data <- "no data"}
    #scrape the bedroom number
    bedroom_data_html <- html_nodes(webpage,'.ui-property-spec__item:nth-child(1)')
    bedroom_data_html <- html_text(bedroom_data_html)
    bedroom_data <- gsub("\n                                    ","",bedroom_data_html)
    bedroom_data <- gsub("\n                                ","",bedroom_data)
    bedroom_data <- gsub(" bedrooms","",bedroom_data)
    bedroom_data <- gsub(" bedroom","",bedroom_data)
    if(length(bedroom_data) == 0){bedroom_data <- "no data"}
    #scrape the bathroom number
    bathroom_data_html <- html_nodes(webpage,'.ui-property-spec__item:nth-child(2)')
    bathroom_data_html <- html_text(bathroom_data_html)
    bathroom_data <- gsub("\n                                    ","",bathroom_data_html)
    bathroom_data <- gsub("\n                                ","",bathroom_data)
    bathroom_data <- gsub(" bathrooms","",bathroom_data)
    bathroom_data <- gsub(" bathroom","",bathroom_data)
    if(length(bathroom_data) == 0){bathroom_data <- "no data"}
    #scrape the Property_value
    Property_value_data_html <- html_nodes(webpage,'#sale-estimate~ .pdp-estimate__content .ui-text-t3')
    Property_value_data_html <- html_text(Property_value_data_html)
    Property_value_data      <- gsub("£","",Property_value_data_html)
    Property_value_data      <- gsub(",","",Property_value_data)
    if(length(Property_value_data) == 0){Property_value_data <- "no data"}
    #scrape the Rental value
    Rental_value_data_html <- html_nodes(webpage,'#rent-estimate~ .pdp-estimate__content .ui-text-t3')
    Rental_value_data_html <- html_text(Rental_value_data_html)
    Rental_value_data      <- gsub("£","",Rental_value_data_html)
    Rental_value_data      <- gsub(",","",Rental_value_data)
    if(length(Rental_value_data) == 0){Rental_value_data <- "no data"}
    #scrape the location
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
    cat(length(p.list),":",i, "\t")
    }
  }

#save data and outpt
Leeds_house <- cbind(title_list,bedroom_list,bathroom_list,Property_value_list,Rental_value_list,latitude_list,longitude_list)
Leeds_house <- data.frame(Leeds_house)

write.csv(Leeds_house, file = "Leeds_housedata.csv")
getwd()
