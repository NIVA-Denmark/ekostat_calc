library(tidyverse)
library(haven)
library(lme4)
library(lubridate)
library(prodlim)
library(matrixStats)
library(stats)

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
df <- ReadMonitoringDataSMHI("../Testing/SASdata/Coast_watersamples.sas7bdat") %>% filter(WB_ID=="561400-161201") %>% mutate(month=month(date),year=year(date))
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
RefCond_sali <- c(5.21,3.78,2.62,1.72,1.04,0.56,0.25,rep(0.18,29))
CalculateIndicator("CoastBiovolEQR",df,RefCond_sali,variance_list,MonthInclude,2007,2012)


RefCond_sali <- c(56,50,44,38,32,26,20,rep(17,29))
MonthInclude <- c(11,12,1,2)
CalculateIndicator("CoastTNwinter",df,RefCond_sali,variance_list,MonthInclude,2007,2012)
CalculateIndicator("CoastTNwinterEQR",df,RefCond_sali,variance_list,MonthInclude,2007,2012)
CalculateIndicator("CoastDINwinterEQR",df,RefCond_sali,variance_list,MonthInclude,2007,2012)
CalculateIndicator("CoastDIPwinterEQR",df,RefCond_sali,variance_list,MonthInclude,2007,2012)

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
variance_list <- list(V_station=0.0472620391974166,V_obspoint=0,
                      V_year=0.000240047417516435,V_yearmonth=0.0192565837622898,
                      V_stationdate=0.0403816847055466,
                      V_stationyear=0,V_stationmonth=0.0205176400753212,
                      V_institution=0.0160297370140365,V_replication=0)
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


# Testing of freshwater indicators in lakes
variance_list <- list(V_station=0.5211,V_obspoint=0,
                      V_year=0.1911,V_yearmonth=0,
                      V_stationdate=0.3301,
                      V_stationyear=0.3927,V_stationmonth=0,
                      V_institution=0,V_replication=0)
# Phytoplankton
MonthInclude <- c(7,8)
df <- filter(ReadMonitoringDataSMHI("data/lake_biolindex.sas7bdat"),WB_ID == 'SE615375-137087' & year %in% c(2007,2008,2009,2010,2011,2012))
df <- filter(ReadMonitoringDataSMHI("data/lake_biolindex.sas7bdat"),WB_ID == 'SE638317-138010' & year %in% c(2007,2008,2009,2010,2011,2012))
CalculateIndicator("LakeBiovol",df,RefCond_sali,variance_list,MonthInclude,2007,2012)
RefCond_sali <- c(rep(400,36))
CalculateIndicator("LakeBiovolEQR",df,RefCond_sali,variance_list,MonthInclude,2007,2012)
CalculateIndicator("LakePropCyano",df,RefCond_sali,variance_list,MonthInclude,2007,2012)
RefCond_sali <- c(rep(0,36))
CalculateIndicator("LakePropCyanoEQR",df,RefCond_sali,variance_list,MonthInclude,2007,2012)
CalculateIndicator("LakeTPI",df,RefCond_sali,variance_list,MonthInclude,2007,2012)
RefCond_sali <- c(-1,-0.5,rep(0.5,34))
CalculateIndicator("LakeTPIEQR",df,RefCond_sali,variance_list,MonthInclude,2007,2012)
CalculateIndicator("LakeNphytspec",df,RefCond_sali,variance_list,MonthInclude,2007,2012)
RefCond_sali <- c(rep(45,36))
CalculateIndicator("LakeNphytspecEQR",df,RefCond_sali,variance_list,MonthInclude,2007,2012)
CalculateIndicator("LakeChla",df,RefCond_sali,variance_list,MonthInclude,2007,2012)
RefCond_sali <- c(rep(3,36))
CalculateIndicator("LakeChlaEQR",df,RefCond_sali,variance_list,MonthInclude,2007,2012)
# Macrophytes
MonthInclude <- c(1,2,3,4,5,6,7,8,9,10,11,12)
df <- filter(ReadMonitoringDataSMHI("data/lake_biolindex.sas7bdat"),WB_ID == 'SE615365-134524' & year %in% c(2007,2008,2009,2010,2011,2012))
df <- filter(ReadMonitoringDataSMHI("data/lake_biolindex.sas7bdat"),WB_ID == 'SE652177-159038' & year %in% c(2007,2008,2009,2010,2011,2012))
RefCond_sali <- c(rep(8.27,36))
CalculateIndicator("LakeTMIEQR",df,RefCond_sali,variance_list,MonthInclude,2007,2012)
# Benthic invertebrates
MonthInclude <- c(1,2,3,4,5,6,7,8,9,10,11,12)
df <- filter(ReadMonitoringDataSMHI("data/lake_biolindex.sas7bdat"),WB_ID == 'SE638665-129243' & year %in% c(2007,2008,2009,2010,2011,2012))
df <- filter(ReadMonitoringDataSMHI("data/lake_biolindex.sas7bdat"),WB_ID == 'SE652177-159038' & year %in% c(2007,2008,2009,2010,2011,2012))
RefCond_sali <- c(rep(5.6,36))
CalculateIndicator("LakeASPTEQR",df,RefCond_sali,variance_list,MonthInclude,2007,2012)
RefCond_sali <- c(rep(3.25,36))
CalculateIndicator("LakeBQIEQR",df,RefCond_sali,variance_list,MonthInclude,2007,2012)
RefCond_sali <- c(rep(77.5,36))
CalculateIndicator("LakeMILAEQR",df,RefCond_sali,variance_list,MonthInclude,2007,2012)
# Fish
MonthInclude <- c(7,8)
df <- filter(ReadMonitoringDataSMHI("data/lake_biolindex.sas7bdat"),WB_ID == 'SE615375-137087' & year %in% c(2007,2008,2009,2010,2011,2012))
df <- filter(ReadMonitoringDataSMHI("data/lake_biolindex.sas7bdat"),WB_ID == 'SE664197-149337' & year %in% c(2007,2008,2009,2010,2011,2012))
CalculateIndicator("LakeEQR8",df,RefCond_sali,variance_list,MonthInclude,2007,2012)
CalculateIndicator("LakeAindexW5",df,RefCond_sali,variance_list,MonthInclude,2007,2012)
CalculateIndicator("LakeEindexW3",df,RefCond_sali,variance_list,MonthInclude,2007,2012)
# Nutrients - Total phosphorus
MonthInclude <- c(7,8)
df <- filter(ReadMonitoringDataSMHI("data/lake_WQdata.sas7bdat"),WB_ID == 'SE655455-136581' & year %in% c(2007,2008,2009,2010,2011,2012))
df <- filter(ReadMonitoringDataSMHI("data/lake_WQdata.sas7bdat"),WB_ID == 'SE670275-146052' & year %in% c(2007,2008,2009,2010,2011,2012))
RefCond_sali <- c(RefCond_LakeTPsummer(mean(df$Abs_F420,na.rm=TRUE),100,10),rep(0,35))
CalculateIndicator("LakeTPsummerEQR",df,RefCond_sali,variance_list,MonthInclude,2007,2012)
# Secchi depth
MonthInclude <- c(1,2,3,4,5,6,7,8,9,10,11,12)
df <- filter(ReadMonitoringDataSMHI("data/lake_WQdata.sas7bdat"),WB_ID == 'SE655455-136581' & year %in% c(2007,2008,2009,2010,2011,2012))
df <- filter(ReadMonitoringDataSMHI("data/lake_WQdata.sas7bdat"),WB_ID == 'SE670275-146052' & year %in% c(2007,2008,2009,2010,2011,2012))
RefCond_sali <- c(RefCond_LakeSecchiDepth(mean(df$Abs_F420,na.rm=TRUE),3),rep(0,35))
CalculateIndicator("LakeSecchiDepthEQR",df,RefCond_sali,variance_list,MonthInclude,2007,2012)
# Oxygen

# pH change

# Testing of freshwater indicators in rivers
variance_list <- list(V_station=0.5211,V_obspoint=0,
                      V_year=0.1911,V_yearmonth=0,
                      V_stationdate=0.3301,
                      V_stationyear=0.3927,V_stationmonth=0,
                      V_institution=0,V_replication=0)
# Benthic diatoms
MonthInclude <- c(1,2,3,4,5,6,7,8,9,10,11,12)
df <- filter(ReadMonitoringDataSMHI("data/river_biolindex.sas7bdat"),WB_ID == 'SE625195-141220' & year %in% c(2007,2008,2009,2010,2011,2012))
df <- filter(ReadMonitoringDataSMHI("data/river_biolindex.sas7bdat"),WB_ID == 'SE654141-124734' & year %in% c(2007,2008,2009,2010,2011,2012))
CalculateIndicator("RiverIPS",df,RefCond_sali,variance_list,MonthInclude,2007,2012)
RefCond_sali <- c(rep(19.6,36))
CalculateIndicator("RiverIPSEQR",df,RefCond_sali,variance_list,MonthInclude,2007,2012)
CalculateIndicator("RiverPctPT",df,RefCond_sali,variance_list,MonthInclude,2007,2012)
CalculateIndicator("RiverTDI",df,RefCond_sali,variance_list,MonthInclude,2007,2012)
CalculateIndicator("RiverACID",df,RefCond_sali,variance_list,MonthInclude,2007,2012)
# Benthic invertebrates
MonthInclude <- c(1,2,3,4,5,6,7,8,9,10,11,12)
df <- filter(ReadMonitoringDataSMHI("data/river_biolindex.sas7bdat"),WB_ID == 'SE632137-147160' & year %in% c(2007,2008,2009,2010,2011,2012))
df <- filter(ReadMonitoringDataSMHI("data/river_biolindex.sas7bdat"),WB_ID == 'SE638475-137575' & year %in% c(2007,2008,2009,2010,2011,2012))
RefCond_sali <- c(rep(6.53,36))
CalculateIndicator("RiverASPTEQR",df,RefCond_sali,variance_list,MonthInclude,2007,2012)
RefCond_sali <- c(rep(14,36))
CalculateIndicator("RiverDJEQR",df,RefCond_sali,variance_list,MonthInclude,2007,2012)
RefCond_sali <- c(rep(47.5,36))
CalculateIndicator("RiverMISAEQR",df,RefCond_sali,variance_list,MonthInclude,2007,2012)
# Fish
MonthInclude <- c(8,9,10)
df <- filter(ReadMonitoringDataSMHI("data/river_biolindex.sas7bdat"),WB_ID == 'SE624900-134390' & year %in% c(2007,2008,2009,2010,2011,2012))
df <- filter(ReadMonitoringDataSMHI("data/river_biolindex.sas7bdat"),WB_ID == 'SE694939-144561' & year %in% c(2007,2008,2009,2010,2011,2012))
CalculateIndicator("RiverVIX",df,RefCond_sali,variance_list,MonthInclude,2007,2012)
CalculateIndicator("RiverVIXh",df,RefCond_sali,variance_list,MonthInclude,2007,2012)
CalculateIndicator("RiverVIXsm",df,RefCond_sali,variance_list,MonthInclude,2007,2012)
# Nutrients - Total phosphorus
MonthInclude <- c(1,2,3,4,5,6,7,8,9,10,11,12)
df <- filter(ReadMonitoringDataSMHI("data/river_WQdata.sas7bdat"),WB_ID == 'SE648930-125964' & year %in% c(2007,2008,2009,2010,2011,2012))
df <- filter(ReadMonitoringDataSMHI("data/river_WQdata.sas7bdat"),WB_ID == 'SE632601-145366' & year %in% c(2007,2008,2009,2010,2011,2012))
RefCond_sali <- c(RefCond_RiverTP(mean(df$Abs_F420,na.rm=TRUE),100,1),rep(0,35))
CalculateIndicator("RiverTPEQR",df,RefCond_sali,variance_list,MonthInclude,2007,2012)





