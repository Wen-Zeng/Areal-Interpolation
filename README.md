# Areal-Interpolation

This repository contains data and code to reproduce the analysis described in a paper published in Computers, Environment and Urban Systems. https://www.sciencedirect.com/science/article/pii/S0198971519303370?dgcid=author

You can download the .R files and run them in R or RStudio. It loads the .RData files and provides links to the .R files which describe how the data were created and assembled. You may need to install some of the packages but the code checks and does this. The script will load up the data (and different forms of ancillary data) and illustrate each of areal interpolation approaches described in the text.

Five different areal interpolation methods were used: 1. Areal weighted method, 2. Network method, 3. Dasymetric method, 	4. HP-census method, 5. HP-sales method. The data used in these interpolation methods are respectively in: DataAW.RData, DataNet.RData, DataDasy.RData, DataHPcensus.RData, DataHPsales.RData. The R script used to capture the house sales data from the websites is in ScrapeWeb.R. The codes for three figures in the papers are in: Histogram.R, Cumsum.R and Error map.R. The data used in these figures are in: DataHist.RData, DataCumsum.RData and DataErrormap.RData. 

Please contact Wen Zeng alvin_z@163.com if you have any questions.

# Paper title: Using household counts as ancillary information for areal interpolation of population: Comparing formal and informal, online data sources

Wen Zeng12, Alexis Comber2

1Shandong University of Science and Technology, Qingdao, P.R. China. Email: alvin_z@163.com, w.zeng1@leeds.ac.uk
2School of Geography, University of Leeds, Leeds, LS2 9JT, UK. Email: a.comber@leeds.ac.uk

# Abstract

Fine-scale population estimates are needed to support both public and private planning. Previous areal interpolation research has used various types and sources of data as ancillary information to guide and constrain the disaggregation from (usually) larger source zones to (usually) smaller target zones. Many new forms of open and free to access geo-located data are available, and as yet little research has evaluated the use of these data in areal interpolation. This study evaluates the effectiveness of household data as ancillary information from two sources: formal census household counts and informal data on residential (house) sales from commercial websites, applied to 2 case studies with different contexts - Leeds in UK and Qingdao in China. The proposed Household Proportion method uses household counts as ancillary information for areal interpolation of population. It is compared with other interpolation and the results show that HP method yields significantly better results than other interpolation approaches using ancillary data, with lower errors. This research also demonstrates that such data support the application of a suite of interpolation methods that make fewer assumptions about underlying spatial processes. The need to examine issues of representativeness and data coverage are identified and discussed, but the study demonstrates the opportunities for including freely available geo-located data to inform geographic analyses.

# Acknowledgements

This work was supported by the Natural Science Foundation of Shandong Province [grant numbers ZR201702170310]; the State Scholarship Fund of China Scholarship Council [grant numbers 201808370092]; and the Scientific Research Foundation of Shandong University of Science and Technology for Recruited Talents [grant numbers 2016RCJJ003]. All of the data analyses and mappings were undertaken in R 3.5.1, the open source software.

