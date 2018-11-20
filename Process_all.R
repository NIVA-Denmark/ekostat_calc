# Offline processing of indicators
# Do Monte Carlo simulations based on variance components
# Save results to be used by shiny app to a SQLite database 

# TeamViewer ID for niva\notebook-dk 753599521 MK0se4aw3
rm(list = ls())

library(RSQLite)
library(tidyverse)
#library(dplyr)
library(lubridate)
library(prodlim)
library(matrixStats)

source("IndicatorFunctions.R")
source("CalculateIndicatorSupport.R")
source("Assessment.R")
source("ReadBounds.R")
source("ReadBathymetry.R")
source("ReadIndicatorType.R")
source("ReadVariances.R")


start_time <- Sys.time()
nSimMC <- 10#00 #200 #20 #1000  #number of Monte Carlo simulations

load("data/SASdata.Rda")

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
  left_join(df_wb,by = c("WB_name"="WaterbodyName","WB_ID"="WaterbodyID")) %>% 
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

df <- df %>% left_join(type,by=c("WB_ID"="WB_ID")) %>% mutate(type=ifelse(type=="",type2,type)) %>% select(-type2)

df <- df %>% select(DistrictID,typology=type,station,WB_name,WB_ID,institution,station_depth,
                    date,time,temp,sali,depth,secchi,
                    DIN,DIP,TN,TP,chla,biovol,O2,BQI,MSMDI)

#df$WB <- paste0(df$WB_ID, " ", df$WB_name)
df$WB <- df$WB_ID
df$obspoint <- df$station

#df<-df %>% filter(WB=="SE644730-210650")
#df<-df %>% filter(WB=="SE582705-163350")
#df<-df %>% filter(WB %in% c("SE581700-113000","SE582000-115270"))
#df<-df %>% filter(WB=="SE554500-125001")

df <- df %>% mutate(year=year(date),month=month(date)) %>% 
  mutate(period=ifelse(year<2004,NA,ifelse(year<2010,"2004-2009",ifelse(year<2016,"2010-2015","2016-2021"))))
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

wb1<-1


df.bounds<-ReadBounds()
df.bounds.hypox<-ReadBoundsHypoxicArea()
df.bathy<-ReadBathymetry()
df.indicators<-ReadIndicatorType()
df.variances<-ReadVariances()



IndListAll<-c("CoastOxygen",    #1 Dissolved Oxygen (O2) 
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
           "CoastBiovolEQR"     #21 Phytoplankton biovolume (EQR)
)  
#IndList<-IndListAll[2:15]
#IndList<-IndListAll[16:17]
#IndList<-IndListAll[18:21]
#IndList<-IndListAll[2:3]
IndList<-IndListAll
#IndList<-IndListAll[2:21] # without O2


#IndList<-c("CoastOxygen") 
#IndList<-c("CoastChlaEQR","CoastBiovolEQR","CoastDINwinter") 

wblist<-distinct(df,WB,typology)
wbcount<-nrow(wblist)

# Loop through distinct waterbodies and periods in the data
bOVR<-TRUE
bAPP<-FALSE
#bOVR<-FALSE
#bAPP<-TRUE

mtcars %>%
  split(.$cyl) %>% # from base R
  map(~ lm(mpg ~ wt, data = .)) %>%
  map(summary) %>%
  map_dbl("r.squared")

dflist <- df %>% split(.$WB)

dflist2 <- dflist %>% map(~ Assessment(.,nsim = 10, IndList,df.bounds,df.bounds.hypox,df.bathy,df.indicators,df.variances))

#--------------------------------------------------------------------------------------
for(iWB in wbcount:wb1){
#for(iWB in wb1:wbcount){
  cat(paste0("Time now: ",Sys.time(),"\n"))
  
  dfselect<-df %>% filter(WB == wblist$WB[iWB])
  cat(paste0("WB: ",wblist$WB[iWB]," (",iWB," of ",wbcount ,")\n"))
  
  AssessmentResults <- Assessment(dfselect, nsim = nSimMC, IndList,df.bounds,df.bounds.hypox,df.bathy,df.indicators,df.variances)
  
  cat(paste0("           time elapsed: ",Sys.time() - start_time,"\n"))
  
  resAvg <- AssessmentResults[[1]]
  resMC <- AssessmentResults[[2]]
  resErr <- AssessmentResults[[3]]
  resYear <- AssessmentResults[[4]]
  
  #cat("Saving results\n")
  #save(AssessmentResults,file="output/AssessmentResults.Rda")
  #save(df,file="output/AssessmentData.Rda")
  
  #cat("Saving to db\n")
  
  WB <- resAvg %>% group_by(WB,Type,Period,Region,Typename) %>% summarise()
  
  db <- dbConnect(SQLite(), dbname="output/ekostat_coast.db")
  dbWriteTable(conn = db, name = "resAvg", resAvg, overwrite=bOVR,append=bAPP, row.names=FALSE)
  dbWriteTable(conn = db, name = "resMC", resMC, overwrite=bOVR,append=bAPP, row.names=FALSE)
  dbWriteTable(conn = db, name = "resErr", resErr, overwrite=bOVR,append=bAPP, row.names=FALSE)
  dbWriteTable(conn = db, name = "resYear", resYear, overwrite=bOVR,append=bAPP, row.names=FALSE)
  dbWriteTable(conn = db, name = "WB", WB, overwrite=bOVR,append=bAPP, row.names=FALSE)
  dbWriteTable(conn = db, name = "data", dfselect, overwrite=bOVR,append=bAPP, row.names=FALSE)
  
  dbDisconnect(db)

  bOVR<-FALSE
  bAPP<-TRUE
  

}


