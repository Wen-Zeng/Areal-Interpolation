# Areal-Interpolation

This repository contains data and code to reproduce the analysis described in a paper submitted in Computers, Environment and Urban Systems in June 2019.

Five different areal interpolation methods were used: 1. Areal weighted method, 2. Network method, 3. Dasymetric method, 	4. HP-census method, 5. HP-sales method. The data used in these interpolation methods are respectively in: DataAW.RData, DataNet.RData, DataDasy.RData, DataHPcensus.RData, DataHPsales.RData. The R script used to capture the house sales data from the websites is in ScrapeWeb.R. The codes for three figures in the papers are in: Histogram.R, Cumsum.R and Error map.R. The data used in these figures are in: DataHist.RData, DataCumsum.RData and DataErrormap.RData. 

Please contact Wen Zeng alvin_z@163.com if you have any questions.

# Paper title: Using household count as ancillary information for areal interpolation of population: comparing informal and formal data sources

Wen Zeng12, Alexis Comber2

1Shandong University of Science and Technology, Qingdao, P.R. China. Email: alvin_z@163.com, w.zeng1@leeds.ac.uk
2School of Geography, University of Leeds, Leeds, LS2 9JT, UK. Email: a.comber@leeds.ac.uk

# Abstract

Fine-scale population estimates are needed to support both public and private planning. A number of areal interpolation methods have been proposed using different ancillary data. Many new forms of open and free to access data are available, and as yet little research has evaluated the use of household data in areal interpolation. This study evaluates the effectiveness of household data as ancillary information from two sources: formal census household counts and informal data on residential (house) sales from commercial websites, applied to 2 case studies with different contexts - Leeds in UK and Qingdao in China. A simple Household Percentage method (HP) is proposed to use household data as ancillary information for areal interpolation. The results show that household counts and proportions have a strong correlation with the population and provide a reliable source of ancillary information for guiding the areal interpolation of the population across different zones. A simple household Household percentage Percentage method (HP) yields significantly better results than other interpolation approaches using ancillary data, with lower errors, resulting in better population surfaces. Informal data on house sales and data describing other features captured from websites, frequently have detailed geographical and location information. These can be used to robustly support areal interpolation. This research also demonstrates that such data supports the application of a simpler suite of interpolation methods that make fewer assumptions about underlying spatial processes.

