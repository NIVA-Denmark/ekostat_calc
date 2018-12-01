# Offline processing of indicators
# Do Monte Carlo simulations based on variance components
# Save results to be used by shiny app to a SQLite database 

# TeamViewer ID for niva\notebook-dk 753599521 MK0se4aw3
rm(list = ls())

library(RSQLite)
library(tidyverse)
library(lubridate)
library(prodlim)
library(matrixStats)
library(haven)

source("IndicatorFunctions.R")
source("CalculateIndicatorSupport.R")
source("Assessment.R")
source("ReadBounds.R")
source("ReadBathymetry.R")
source("ReadIndicatorType.R")
source("ReadVariances.R")


start_time <- Sys.time()
nSimMC <- 20#0  #number of Monte Carlo simulations

load("data/SASdata.Rda")
#df1<-df
#load("data/SASdata_v2.Rda")

#Read waterbody data
df_wb <- read.table("data/waterbodies.txt",fileEncoding = "UTF-8",
                    sep = "\t",stringsAsFactors = F,header = T)
df_wb <- df_wb %>% select(WaterbodyName, WaterbodyID, DistrictID, TypeID) %>%
  arrange(WaterbodyName)

#Fix observation data
df[df$WB_name=="Dragviksfjärden", "WB_name"] <- "Dragsviksfjärden"
df[df$WB_ID=="SE590020-114520", "WB_ID"] <- "SE0101010201-C" #Inre Idefjorden
df[df$WB_ID=="SE590670-111380", "WB_ID"] <- "SE0101010301-C" #Singlefjorden
df[df$WB_ID=="SE673283-158060", "WB_ID"] <- "SE604200-171765" #Yttre Fjärden

#Join waterbody district information to obs data
df <- df %>%
  select(type,WB_ID,WB_name,station,date,station_depth,time,DIN,DIP,TN,TP,
         sali,chla,biovol,secchi,institution,obspoint,depth,O2,
         O2_bot,HypoxicAreaPct,BQI,MSMDI,
         Year,Month,Period,Season) %>%
  left_join(df_wb,by = c("WB_ID"="WaterbodyID")) %>% 
  mutate(type=ifelse(type=="",TypeID,type))

df[df$WB_ID=="SE641840-211540", "DistrictID"] <- "SE1" #Inre Lövselefjärden
df[df$WB_ID=="SE634350-202000", "DistrictID"] <- "SE1" #Inre Österfjärden
df[df$WB_ID=="SE641720-211520", "DistrictID"] <- "SE1" #Yttre Lövselefjärden
df[df$WB_ID=="SE634110-201920", "DistrictID"] <- "SE1" #Yttre Österfjärden
df[df$WB_ID=="SE647050-213980", "DistrictID"] <- "SE1" #S m Bottenvikens kustvatten
df[df$WB_ID=="SE634210-202020", "DistrictID"] <- "SE1" #Holmsund*
df[df$WB_ID=="SE634210-202020", "type"] <- "22"        #Holmsund*
# *In VISS, I can only find vatttendrag with this name but it is SE1 and Luleå kommun

df[df$WB_ID=="SE575500-113750", "type"] <- "01n"        #Älgöfjorden
df[df$WB_ID=="SE583730-164501", "type"] <- "12n"        #Yttre Bråviken

#Fix records with missing type, using other records for the same waterbody
df <- df %>% 
  mutate(type=ifelse(substr(type,1,1)=="0",substr(type,2,4),type))  
  # %>% rename(typology=type)

type<-df %>% filter(!type=="") %>% group_by(WB_ID,type) %>% summarise() %>%
  rename(type2=type)

df <- df %>% left_join(type,by=c("WB_ID"="WB_ID")) %>% 
  mutate(type=ifelse(type=="",type2,type)) %>% 
  select(-type2) %>%
  mutate(station=ifelse(station=="",obspoint,station)) %>%
  mutate(CLR="Coast")

df <- df %>% select(CLR,DistrictID,typology=type,station,WB_name,WB_ID,institution,station_depth,
                    date,time,sali,depth,secchi,
                    DIN,DIP,TN,TP,chla,biovol,O2,
                    O2_bot,HypoxicAreaPct,BQI,MSMDI)

df$WB <- df$WB_ID
df$obspoint <- df$station


df <- df %>% mutate(year=year(date),month=month(date)) %>% 
  mutate(period=ifelse(year<2007,NA,ifelse(year<2013,"2007-2012","2013-2018")))
df <- df %>% filter(!is.na(period))
if(FALSE){
#Problem O2 data - to be analysed later. Removing these is just a quick fix!
df$O2<-ifelse(df$WB=="SE644730-210650" & 
                df$period=="2010-2015",NA,df$O2)
df$O2<-ifelse(df$WB=="SE633550-200700" & 
                df$period=="2004-2009",NA,df$O2)
df$O2<-ifelse(df$WB=="SE634110-201920" & 
                df$period=="2004-2009",NA,df$O2)
df$O2<-ifelse(df$WB=="SE634230-201605" & 
                df$period=="2010-2015",NA,df$O2)
df$O2<-ifelse(df$WB=="SE644150-211000" & 
                df$period=="2010-2015",NA,df$O2)
df$O2<-ifelse(df$WB=="SE634210-202020" & 
                df$period=="2004-2009",NA,df$O2)
df$O2<-ifelse(df$WB=="SE636570-203590" & 
                df$period=="2004-2009",NA,df$O2)
df$O2<-ifelse(df$WB %in% c("SE563000-123351","SE582705-163350","SE583000-165600"),
              NA,df$O2)
}


dfc <- df # df for coastal data
dfc <- dfc %>% mutate(typology_varcomp=typology)
#df <- df %>% filter(typology %in% c("10","11","12n","12s","13","14","15","24"))


wb1<-1

# ------------ read freshwater data  ------------------------
source("read_freshwater_data.R", encoding="utf-8")

# ------------ read boundary and indicator information ------------------------

df.bounds<-ReadBounds()
df.bounds$Worst<-as.numeric(df.bounds$Worst)
df.bounds.hypox<-ReadBoundsHypoxicArea()
df.bathy<-ReadBathymetry()
df.indicators<-ReadIndicatorType()
df.variances<-ReadVariances()



IndListAll<-c(#"CoastOxygen",    #1 Dissolved Oxygen (O2) 
              "CoastBottomOxygen",
              "CoastHypoxicArea",
              "CoastSecchi",       #2 Secchi Depth 
           "CoastSecchiEQR",    #3 Secchi Depth (EQR) 
           "CoastDINwinter",    #4 Winter DIN 
           "CoastDINwinterEQR", #5 Winter DIN (EQR)
           "CoastTNsummer",     #6 Summer TN
           "CoastTNsummerEQR",  #7 Summer TN (EQR)
           "CoastTNwinter",     #8 Winter TN
           "CoastTNwinterEQR",  #9 Winter TN (EQR)
           "CoastDIPwinter",    #10 Winter DIP 
           "CoastDIPwinterEQR", #11 Winter DIP (EQR)
           "CoastTPsummer",     #12 Summer TP
           "CoastTPsummerEQR",  #13 Summer TP (EQR)
           "CoastTPwinter",     #14 Winter TP
           "CoastTPwinterEQR",  #15 Winter TP (EQR)
           "CoastMSMDI",        #16 Multi Species Maximum Depth Index (MSMDI) 
           "CoastBQI",          #17 Benthic Quality Index (BQI) 
           "CoastChla",         #18 Chlorophyll a
           "CoastChlaEQR",      #19 Chlorophyll a (EQR),
           "CoastBiovol",       #20 Phytoplankton biovolume
           "CoastBiovolEQR",     #21 Phytoplankton biovolume (EQR)
           "LakeBiovol",       #22
           "LakeBiovolEQR",    #23
           "LakePropCyano",    #24
           "LakePropCyanoEQR", #25
           "LakeTPI",          #26
           "LakeTPIEQR",       #27
           "LakeNphytspec",    #28
           "LakeNphytspecEQR", #29
           "LakeChla",         #30
           "LakeChlaEQR",      #31
           "LakeTMIEQR",       #32
           "LakeASPTEQR",      #33
           "LakeBQIEQR",       #34
           "LakeMILAEQR",      #35
           "LakeEQR8",         #36
           "LakeAindexW5",     #37
           "LakeEindexW3",     #38
           "LakeTPsummerEQR",  #39
           "LakeSecchiDepthEQR",#40
           "RiverIPS",           #41
           "RiverIPSEQR",        #42
           "RiverPctPT",         #43
           "RiverTDI",           #44
           "RiverACID",          #45
           "RiverASPTEQR",       #46
           "RiverDJEQR",         #47
           "RiverMISAEQR",       #48
           "RiverVIX",           #49
           "RiverVIXh",          #50
           "RiverVIXsm",         #51
           "RiverTPEQR"          #52
)  

IndList<-IndListAll


  #df <- dfc
  #df <- dfl
  #df <- dfr
  outputdb<-"output/ekostat_coast1.db"
  #outputdb<-"output/ekostat_lake.db"
  #outputdb<-"output/ekostat_river.db"


df <- bind_rows(dfc,dfl,dfr)

#wbselect<-c("SE642489-151724"
#wbselect<-c("SE642489-151724","SE673813-153174")
wbselect<-c("SE583000-165600")
df <- df %>% filter(WB %in% wbselect)


wblist<-distinct(df,WB,typology,CLR)
wbcount<-nrow(wblist)

# Loop through distinct waterbodies and periods in the data
bOVR<-TRUE
bAPP<-FALSE
#bOVR<-FALSE
#bAPP<-TRUE

#--------------------------------------------------------------------------------------
#for(iWB in 53:wb1){
for(iWB in wbcount:wb1){
#for(iWB in 1:1){
  
  dfselect<-df %>% filter(WB == wblist$WB[iWB])
  cat(paste0(wblist$CLR[iWB]," WB: ",wblist$WB[iWB]," (",iWB," of ",wbcount ,")\n"))
  
  AssessmentResults <- Assessment(dfselect, nsim = nSimMC, IndList,df.bounds,df.bounds.hypox,df.bathy,df.indicators,df.variances)
  
  ETA <- Sys.time() + (Sys.time() - start_time)*(wbcount/(wbcount-iWB))
  cat(paste0("Time: ",Sys.time(),"  (elapsed: ",round(Sys.time() - start_time,4),") ETA=",ETA,"\n"))
  
  resAvg <- AssessmentResults[[1]]
  resMC <- AssessmentResults[[2]]
  resErr <- AssessmentResults[[3]]
  resYear <- AssessmentResults[[4]]
  db <- dbConnect(SQLite(), dbname=outputdb)
  
  if(!is.na(resAvg)){
    WB <- resAvg %>% group_by(WB,Type,Period,Region,Typename) %>% summarise()
    dbWriteTable(conn = db, name = "resMC", resMC, overwrite=bOVR,append=bAPP, row.names=FALSE)
    dbWriteTable(conn = db, name = "resYear", resYear, overwrite=bOVR,append=bAPP, row.names=FALSE)
    dbWriteTable(conn = db, name = "WB", WB, overwrite=bOVR,append=bAPP, row.names=FALSE)
    dbWriteTable(conn = db, name = "data", dfselect, overwrite=bOVR,append=bAPP, row.names=FALSE)
    dbWriteTable(conn = db, name = "resAvg", resAvg, overwrite=bOVR,append=bAPP, row.names=FALSE)
  }
  dbWriteTable(conn = db, name = "resErr", resErr, overwrite=bOVR,append=bAPP, row.names=FALSE)
  dbDisconnect(db)
  
  bOVR<-FALSE
  bAPP<-TRUE
  

}


