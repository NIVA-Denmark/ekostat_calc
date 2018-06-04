# Offline processing of indicators
# Do Monte Carlo simulations based on variance components
# Save results to be used by shiny app to a SQLite database 


rm(list = ls())

library(RSQLite)
library(dplyr)
library(tidyr)
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
nSimMC <- 100 #20 #1000  #number of Monte Carlo simulations

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
df <- df %>% mutate(year=year(date),month=month(date)) %>% 
  mutate(period=ifelse(year<2004,NA,ifelse(year<2010,"2004-2009",ifelse(year<2016,"2010-2015","2016-2021"))))
df <- df %>% filter(!is.na(period))

#Problem O2 data - to be analysed later. Removing these is just a quick fix!
df$O2<-ifelse(df$WB=="SE644730-210650" & 
                df$period=="2010-2015",NA,df$O2)


IndList<-c("CoastOxygen",       #Dissolved Oxygen (O2) 
           "CoastChla",         #Chlorophyll a
           "CoastChlaEQR",      #Chlorophyll a (EQR),
           "CoastBiovol",         #Phytoplankton biovolume
           "CoastBiovolEQR",      #Phytoplankton biovolume (EQR)
           "CoastMSMDI",        #Multi Species Maximum Depth Index (MSMDI) 
           "CoastBQI",          #Benthic Quality Index (BQI) 
           "CoastSecchi",       #Secchi Depth 
           "CoastSecchiEQR",    #Secchi Depth (EQR) 
           "CoastDINwinter",    #Winter DIN 
           "CoastTNsummer",     #Summer TN
           "CoastTNwinter",     #Winter TN
           "CoastDIPwinter",    #Winter DIP 
           "CoastTPsummer",     #Summer TP
           "CoastTPwinter"     #Winter TP
           )       

# IndList<-c("CoastChlaEQR","CoastBiovolEQR",
#            "CoastTNsummer","CoastTNsummerEQR",
#            "CoastTNwinter","CoastTNwinterEQR",
#            "CoastTPsummer","CoastTPsummerEQR",
#            "CoastTPwinter","CoastTPwinterEQR",
#            "CoastSecchi","CoastSecchiEQR",
#            "CoastBQI","CoastMSMDI","CoastOxygen") 

#IndList<-c("CoastOxygen") 

wblist<-distinct(df,WB,typology)
wbcount<-nrow(wblist)

# Loop through distinct waterbodies and periods in the data
bOVR<-TRUE
bAPP<-FALSE

for(iWB in 1:wbcount){
  
  dfselect<-df %>% filter(WB == wblist$WB[iWB])
  cat(paste0("WB: ",wblist$WB[iWB]," (",iWB," of ",wbcount ,")\n"))
  
  AssessmentResults <- Assessment(dfselect, nsim = nSimMC, IndList)
  
  cat(paste0("Time elapsed: ",Sys.time() - start_time,"\n"))
  
  resAvg <- AssessmentResults[[1]]
  resMC <- AssessmentResults[[2]]
  resErr <- AssessmentResults[[3]]
  resYear <- AssessmentResults[[4]]
  
  #cat("Saving results\n")
  #save(AssessmentResults,file="output/AssessmentResults.Rda")
  #save(df,file="output/AssessmentData.Rda")
  
  #cat("Saving to db\n")
  
  WB <- resAvg %>% group_by(WB,Type,Period,Region,Typename) %>% summarise()
  
  db <- dbConnect(SQLite(), dbname="output/ekostat.db")
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


