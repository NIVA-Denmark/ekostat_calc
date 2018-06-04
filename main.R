library(tidyverse)
library(haven)
library(lme4)
library(lubridate)
library(prodlim)
library(matrixStats)

library(extrafont)
loadfonts(quiet = TRUE)
theme_set(theme_bw(base_size = 12, base_family = "Open Sans"))

# source("R/extract_modis.R") # ATTENTION: takes few minutes

# Clean the workspace
rm(list = ls())

source("ReadMonitoringData.R")
source("ReadIndicatorParms.R")
#source("CalculateIndicator.R")

source("IndicatorFunctions.R")

# Read data set for specific waterbody and period
df <- ReadMonitoringDataSMHI("data/Gullmarn_2007_2012.sas7bdat")
df <- ReadMonitoringDataSMHI("data/danafjord_2013_2016.sas7bdat")
df <- ReadMonitoringDataSMHI("data/danafjord_2001_2006.sas7bdat")
df <- ReadMonitoringDataSMHI("data/byfjorden_2007_2012.sas7bdat")
# Read covariance parameters for the indicator
parmlist <- ReadParms_chla()
variance_list <- list(V_station=parmlist$covparams_CumCover$Estimate[parmlist$covparams_CumCover$CovParm == "stati(vandom*period)"],V_obspoint=0,
                      V_year=parmlist$covparams_CumCover$Estimate[parmlist$covparams_CumCover$CovParm == "year(vandomr*period)"],V_yearmonth=0,
                      V_stationdate=parmlist$covparams_CumCover$Estimate[parmlist$covparams_CumCover$CovParm == "Residual"],
                      V_stationyear=parmlist$covparams_CumCover$Estimate[parmlist$covparams_CumCover$CovParm == "stat*year(vand*peri)"],V_stationmonth=0,
                      V_institution=parmlist$covparams_CumCover$Estimate[parmlist$covparams_CumCover$CovParm == "proevetager"],V_replication=0)
# Calculate the indicator
MonthInclude <- c(6,7,8)
CalculateIndicator("CoastChla",df,RefCond_sali,variance_list,MonthInclude,2007,2012)

# type with no salinity correction, e.g. type=6
RefCond_sali <- c(rep(0.9,36))
# type with salinity correction, e.g. type=8
RefCond_sali <- c(15.7,12.4,9.5,6.9,4.8,3.0,1.7,rep(1.29,29))
MonthInclude <- c(6,7,8)
CalculateIndicator("CoastChlaEQR",df,RefCond_sali,variance_list,MonthInclude,2007,2012)

# type with salinity correction, e.g. type=8 in summer
RefCond_sali <- c(56,50,43,37,31,24,18,rep(15,29))
MonthInclude <- c(6,7,8)
CalculateIndicator("CoastTNsummer",df,RefCond_sali,variance_list,MonthInclude,2007,2012)

# type with salinity correction, e.g. type=8 in winter
RefCond_sali <- c(56,50,44,38,32,26,20,rep(17,29))
MonthInclude <- c(11,12,1,2)
CalculateIndicator("CoastTNwinter",df,RefCond_sali,variance_list,MonthInclude,2007,2012)

# Calculate the indicator for Secchi depth
MonthInclude <- c(6,7,8)
CalculateIndicator("CoastSecchi",df,RefCond_sali,variance_list,MonthInclude,2007,2012)

# type with salinity correction, e.g. type=8
RefCond_sali <- c(1.1,1.4,1.7,2.2,3.1,4.5,7.5,rep(10,29))
MonthInclude <- c(6,7,8)
CalculateIndicator("CoastSecchiEQR",df,RefCond_sali,variance_list,MonthInclude,2007,2012)


RefCond_sali <- c(15.7,12.4,9.5,6.9,4.8,3.0,1.7,rep(1.29,29))
MonthInclude <- c(6,7,8)
CalculateIndicator("CoastChlaEQR",df,RefCond_sali,variance_list,MonthInclude,2007,2012)

RefCond_sali <- c(56,50,44,38,32,26,20,rep(17,29))
MonthInclude <- c(11,12,1,2)
CalculateIndicator("CoastTNwinter",df,RefCond_sali,variance_list,MonthInclude,2007,2012)
CalculateIndicator("CoastTNwinterEQR",df,RefCond_sali,variance_list,MonthInclude,2007,2012)

# Oxygen indicator - first version
variance_list <- list(V_station=0.5,V_obspoint=0,
                      V_year=1.5,V_yearmonth=1.4,
                      V_stationyear=0.4,V_stationmonth=0.1,V_stationdate=1.0,
                      V_institution=0.0,V_replication=0)
df <- ReadMonitoringDataSMHI("data/GullmarnO2_2007_2012.sas7bdat")
df <- ReadMonitoringDataSMHI("data/ByfjordenO2_2007_2012.sas7bdat")
MonthInclude <- c(1,2,3,4,5,6,7,8,9,10,11,12)
CalculateIndicator("CoastOxygen",df,RefCond_sali,variance_list,MonthInclude,2007,2012,n_iter=10)

# Testing full oxygen indicator
MonthInclude <- c(1,2,3,4,5,6,7,8,9,10,11,12)
df <- ReadMonitoringDataSMHI("data/ByfjordenO2x_2007_2012.sas7bdat")
WB_bathymetry <- data.frame(area_pct = 1:100, depth = c(1:40/4,10+1:20/2,20+1:30/3,30+1:10))
BoundariesHypoxicArea <- c(100,68,64,60,40,0)
df <- mutate(df,xvar=O2)
CalculateIndicator("CoastOxygen",filter(df,year>2009),RefCond_sali,variance_list,MonthInclude,2007,2012)
df <- ReadMonitoringDataSMHI("data/GullmarnO2x_2007_2012.sas7bdat")
WB_bathymetry <- data.frame(area_pct = 1:100, depth = c(1:40*2,80+1:20,100+1:30,130+1:10))
BoundariesHypoxicArea <- c(100,82,53,24,16,0)
df <- mutate(df,xvar=O2)
CalculateIndicator("CoastOxygen",df,RefCond_sali,variance_list,MonthInclude,2007,2012)

# BQI indicator
# Read data set for specific waterbody and period
variance_list <- list(V_station=2.5,V_obspoint=0.64,
                      V_year=0.10,V_yearmonth=0,
                      V_stationyear=0.63,V_stationmonth=0,V_stationdate=0.5,
                      V_institution=0.5,V_replication=0)
df <- ReadMonitoringDataSMHI("data/halse_2007_2012.sas7bdat")
df <- ReadMonitoringDataSMHI("data/marstrand_2007_2012.sas7bdat")
MonthInclude <- c(1,2,3,4,5,6,7,8,9,10,11,12)
CalculateIndicator("CoastBQI",df,RefCond_sali,variance_list,MonthInclude,2007,2012)

# MSMDI indicator
# Read data set for specific waterbody and period
variance_list <- list(V_station=0.5211,V_obspoint=0,
                      V_year=0.1911,V_yearmonth=0,
                      V_stationdate=0.3301,
                      V_stationyear=0.3927,V_stationmonth=0,
                      V_institution=0,V_replication=0)
df <- ReadMonitoringDataSMHI("data/MBlekinge_2007_2012.sas7bdat")
df <- ReadMonitoringDataSMHI("data/VBlekinge_2007_2012.sas7bdat")
MonthInclude <- c(1,2,3,4,5,6,7,8,9,10,11,12)
CalculateIndicator("CoastMSMDI",df,RefCond_sali,variance_list,MonthInclude,2007,2012)

# Testing indicator calculation for non-monitored water bodies
variance_list <- list(V_station=0.0909487321117623,V_obspoint=0,
                      V_year=0.001168302,V_yearmonth=0,
                      V_stationdate=0.278888006,
                      V_stationyear=0,V_stationmonth=0.092962407,
                      V_institution=0.062103034,V_replication=0)
MonthInclude <- c(11,12,1,2)
RefCond_sali <- c(rep(0.9,36))
df <- ReadMonitoringDataSMHI("data/Gullmarn_2007_2012.sas7bdat")
un1 <-CalculateIndicator("CoastTNwinter",df,RefCond_sali,variance_list,MonthInclude,2007,2012)
df <- ReadMonitoringDataSMHI("data/danafjord_2007_2012.sas7bdat")
un2 <-CalculateIndicator("CoastTNwinter",df,RefCond_sali,variance_list,MonthInclude,2007,2012)
df <- ReadMonitoringDataSMHI("data/koljoefjord_2007_2012.sas7bdat")
un3 <-CalculateIndicator("CoastTNwinter",df,RefCond_sali,variance_list,MonthInclude,2007,2012)
df <- ReadMonitoringDataSMHI("data/byfjorden_2007_2012.sas7bdat")
un4 <-CalculateIndicator("CoastTNwinter",df,RefCond_sali,variance_list,MonthInclude,2007,2012)
un5 <-CalculateIndicator("CoastTNwinter",filter(df,year>2009),RefCond_sali,variance_list,MonthInclude,2007,2012)
un <- list(un1,un2,un3,un4,un5)

CalculateIndicatorType("CoastTNwinter",un,list(V_WBperiod=9,V_WBannual=12),30,2007,2012)

