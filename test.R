library(tidyverse)
library(haven)
library(lme4)
library(lubridate)
library(prodlim)
library(matrixStats)
library(ggplot2)

# Clean the workspace
rm(list = ls())

source("IndicatorFunctions.R")
source("ReadMonitoringData.R")
source("ReadBounds.R")
source("ReadIndicatorType.R")
source("ReadVariances.R")
source("Assessment.R")

load("data/SASdata.Rda")
dfall<-df

df.bounds<-ReadBounds()
df.indicators<-ReadIndicatorType()
df.variances<-ReadVariances()

WBselect<-"SE572135-120141"
type<-"1s"
varname<-"CoastSecchiEQR"

varlist<- VarianceComponents(df.indicators,df.variances,type,varname)
RefCond_sali<-SalinityReferenceValues(df.bounds,"1s",varname,missing=1)

MonthInclude <- c(6,7,8)

df <- dfall %>% filter(WB_ID==WBselect) %>% 
  mutate(xvar=secchi) %>% filter(!is.na(xvar))
res<-CalculateIndicator(varname,df,RefCond_sali,varlist,MonthInclude,2007,2012,n_iter=200)

Value<-res$indicator_sim
dfres<-as.data.frame(Value) %>% mutate(Type=type,Indicator=varname)

bounds <- df.bounds %>% filter(Indicator==varname,Type==type) %>%
  select(Indicator,Type,RefCond,H.G,G.M,M.P,P.B,Worst)

dfres<-dfres %>% left_join(bounds,by=c("Type","Indicator"))
dfres<-GetClass(dfres)

ggplot(dfres,aes(Value)) + geom_histogram()
ggplot(dfres,aes(ClassID)) + geom_histogram()

#dfresx<-dfres
