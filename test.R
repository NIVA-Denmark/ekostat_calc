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
source("ReadBathymetry.R")

load("data/SASdata.Rda")
dfall<-df

df.bounds<-ReadBounds()
df.bounds.hypox<-ReadBoundsHypoxicArea()
df.indicators<-ReadIndicatorType()
df.variances<-ReadVariances()
df.bathy<-ReadBathymetry()



# WBselect<-"SE570000-120701"
# type<-"4"
# indname<-"CoastOxygen"
# varname<-"O2"
# MonthInclude <- c(1,2,3,4,5,6,7,8,9,10,11,12)
# yr_from<-2010
# yr_to<-2015

WBselect<-"SE572135-120141"
type<-"1s"
indname<-"CoastSecchiEQR"
varname<-"secchi"
MonthInclude <- c(6,7,8)
yr_from<-2005
yr_to<-2009

varlist<- VarianceComponents(df.indicators,df.variances,type,indname)
RefCond_sali<-SalinityReferenceValues(df.bounds,"1s",indname,missing=1)


BoundariesHypoxicArea <<- df.bounds.hypox %>% filter(WB==WBselect) %>% select(Worst,P.B,M.P,G.M,H.G,RefCond) %>% as.list()
WB_bathymetry <<- df.bathy %>% filter(WB==WBselect) %>% select(area_pct,depth)
# ---------------------- if no bathymetry available, use alternative --------------
# if(!nrow(WB_bathymetry)>0){
#   area_pct<-seq(1,100,by=1) 
#   depth<-area_pct
#   WB_bathymetry<<-data.frame(area_pct,depth)
# }
if(!nrow(df.bounds.hypox %>% filter(WB==WBselect))>0){
  BoundariesHypoxicArea <<- df.bounds.hypox %>% filter(WB=="SE582000-115270") %>% select(Worst,P.B,M.P,G.M,H.G,RefCond) %>% as.list()
}
# -----------------------------------------
df <- dfall %>% filter(WB_ID==WBselect) %>% 
  mutate_("xvar"=varname) %>% filter(!is.na(xvar))
res<-CalculateIndicator(indname,df,RefCond_sali,varlist,MonthInclude,yr_from,yr_to,n_iter=1000)

Value<-res$indicator_sim
dfres<-as.data.frame(Value) %>% mutate(Type=type,Indicator=indname)

bounds <- df.bounds %>% filter(Indicator==indname,Type==type) %>%
  select(Indicator,Type,RefCond,H.G,G.M,M.P,P.B,Worst)

dfres<-dfres %>% left_join(bounds,by=c("Type","Indicator"))
dfres<-GetClass(dfres)

list<-c(bounds[1,4],bounds[1,5],bounds[1,6],bounds[1,7])
clist<-c('#00B0F0','#92D050','#FFFF00', '#FFC000')
#clist<-c('#FF0000','#FFC000','#FFFF00','#92D050','#00B0F0')

ggplot(dfres,aes(Value)) + geom_histogram(binwidth=0.02) + theme_classic() +
  geom_vline(xintercept=list,colour=clist)
ggplot(dfres,aes(ClassID)) + geom_bar() + theme_classic()

#dfresx<-dfres
