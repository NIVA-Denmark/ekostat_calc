library(tidyverse)
library(haven)
library(lme4)
library(lubridate)
library(prodlim)
library(matrixStats)


# Clean the workspace
rm(list = ls())

source("IndicatorFunctions.R")
source("ReadMonitoringData.R")

variance_list <- list(V_station=0.5,V_obspoint=0,
                      V_year=1.5,V_yearmonth=1.4,
                      V_stationyear=0.4,V_stationmonth=0.1,V_stationdate=1.0,
                      V_institution=0.0,V_replication=0)

# Testing full oxygen indicator
MonthInclude <- c(1,2,3,4,5,6,7,8,9,10,11,12)
df <- ReadMonitoringDataSMHI("data/ByfjordenO2x_2007_2012.sas7bdat")
WB_bathymetry <- data.frame(area_pct = 1:100, depth = c(1:40/4,10+1:20/2,20+1:30/3,30+1:10))
BoundariesHypoxicArea <- c(100,68,64,60,40,0)
df <- mutate(df,xvar=O2)
#CalculateIndicator("CoastOxygen",filter(df,year>2009),RefCond_sali,variance_list,MonthInclude,2007,2012,n_iter=100)
df <- ReadMonitoringDataSMHI("data/GullmarnO2x_2007_2012.sas7bdat")
WB_bathymetry <- data.frame(area_pct = 1:100, depth = c(1:40*2,80+1:20,100+1:30,130+1:10))
BoundariesHypoxicArea <- c(100,82,53,24,16,0)
df <- mutate(df,xvar=O2)
#CalculateIndicator("CoastOxygen",df,RefCond_sali,variance_list,MonthInclude,2007,2012,n_iter=100)

# Test using other stations

load("data/SASdata.Rda")
dfall<-df

WBselect<-"SE563000-123351"
df <- dfall %>% filter(WB_ID==WBselect) %>% 
  mutate(xvar=O2) %>% filter(!is.na(xvar))
#CalculateIndicator("CoastOxygen",df,RefCond_sali,variance_list,MonthInclude,2004,2009,n_iter=100)
#CalculateIndicator("CoastOxygen",df,RefCond_sali,variance_list,MonthInclude,2010,2015,n_iter=100)


df.bounds<-ReadBounds()
WBselect<-"SE572135-120141"
df <- dfall %>% filter(WB_ID==WBselect) %>% 
  mutate(xvar=secchi) %>% filter(!is.na(xvar))
CalculateIndicator("CoastSecchiEQR",df,RefCond_sali,variance_list,MonthInclude,2007,2012,n_iter=100)


