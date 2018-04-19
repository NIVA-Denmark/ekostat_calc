# Offline processing of indicators
# Do Monte Carlo simulations based on variance components
# Save results to be used by shiny app to a SQLite database 


rm(list = ls())

library(RSQLite)
library(tidyverse)
library(haven)
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
nSimMC <- 1000 #20 #1  #number of Monte Carlo simulations

df_water<-read_sas("data/coast_watersamples.sas7bdat") #19824
df_o2<-read_sas("data/coast_oxygen.sas7bdat")          #94747
df_bqi<-read_sas("data/coast_bqi.sas7bdat")            # 7142
df_msmdi<-read_sas("data/coast_msmdi.sas7bdat")        # 1089

df <- bind_rows(df_water,df_o2,df_bqi,df_msmdi)
df <- df %>% mutate(WB_ID=paste0("SE",WB_ID))

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

#Fix records with missing type, using other records for the same waterbody
type<-df %>% filter(!type=="") %>% group_by(WB_ID,type) %>% summarise() %>%
  rename(type2=type)

df <- df %>% left_join(type,by=c("WB_ID"="WB_ID")) %>% mutate(type=ifelse(type=="",type2,type)) %>% select(-type2)

df <- df %>% select(DistrictID,type,station,WB_name,WB_ID,institution,station_depth,
                    date,time,temp,sali,depth,secchi,
                    DIN,DIP,TN,TP,chla,biovol,O2,BQI,MSMDI)

df$WB <- paste0(df$WB_ID, " ", df$WB_name)
df$obspoint <- df$station

df <- df %>% 
  mutate(type=ifelse(substr(type,1,1)=="0",substr(type,2,4),type)) %>% 
  rename(typology=type)


df <- df %>% mutate(year=year(date),month=month(date)) %>% 
  mutate(period=ifelse(year<2001,NA,ifelse(year<2007,"2001-2006",ifelse(year<2013,"2007-2012","2013-2017"))))

IndList<-c("CoastChla",         #Chlorophyll a
           "CoastChlaEQR",      #Chlorophyll a (EQR)
           "CoastTNsummer",     #Summer TN
           "CoastTNsummerEQR",  #Summer TN (EQR)
           "CoastTNwinter",     #Winter TN
           "CoastTNwinterEQR",  #Winter TN (EQR)
           "CoastTPsummer",     #Summer TP
           "CoastTPsummerEQR",  #Summer TP (EQR)
           "CoastTPwinter",     #Winter TP
           "CoastTPwinterEQR",  #Winter TP (EQR)
           "CoastDINsummer",    #Summer DIN 
           "CoastDINsummerEQR", #Summer DIN (EQR) 
           "CoastDIPsummer",    #Summer DIP 
           "CoastDIPsummerEQR", #Summer DIP (EQR) 
           "CoastSecchi",       #Secchi Depth 
           "CoastSecchiEQR",    #Secchi Depth (EQR) 
           "CoastBQI",          #Benthic Quality Index (BQI) 
           "CoastMSMDI",        #Multi Species Maximum Depth Index (MSMDI) 
           "CoastOxygen")       #Dissolved Oxygen (O2) 

IndList<-c("CoastChla","CoastChlaEQR",
           "CoastTNsummer","CoastTNsummerEQR",
           "CoastTNwinter","CoastTNwinterEQR",
           "CoastTPsummer","CoastTPsummerEQR",
           "CoastTPwinter","CoastTPwinterEQR",
           "CoastSecchi","CoastSecchiEQR",
           "CoastBQI","CoastMSMDI","CoastOxygen") 

#IndList<-c("CoastOxygen") 

AssessmentResults <- Assessment(df, nsim = nSimMC, IndList)
cat(paste0("Time elapsed: ",Sys.time() - start_time))

resAvg <- AssessmentResults[[1]]
resMC <- AssessmentResults[[2]]
resErr <- AssessmentResults[[3]]

cat("Saving results\n")
save(AssessmentResults,file="output/AssessmentResults.Rda")
save(df,file="output/AssessmentData.Rda")

cat("Saving to db\n")

WB <- resAvg %>% group_by(WB,Type,Period,Region,Typename) %>% summarise()

db <- dbConnect(SQLite(), dbname="output/ekostat.db")
dbWriteTable(conn = db, name = "resAvg", resAvg, overwrite=T, row.names=FALSE)
dbWriteTable(conn = db, name = "resMC", resMC, overwrite=T, row.names=FALSE)
dbWriteTable(conn = db, name = "resErr", resErr, overwrite=T, row.names=FALSE)
dbWriteTable(conn = db, name = "WB", WB, overwrite=T, row.names=FALSE)
dbWriteTable(conn = db, name = "data", df, overwrite=T, row.names=FALSE)

dbDisconnect(db)



#Problem data - to be analysed later. Removing these is just a quick fix!
if(FALSE){
  df$O2<-ifelse(df$WB=="SE574450-165451 Västerviks kustvatten" & 
                  df$period=="2007-2012",NA,df$O2)
  df$O2<-ifelse(df$WB=="SE560205-143545 Sölvesborgsviken",NA,df$O2)
  df$O2<-ifelse(df$WB=="SE560790-145850 Tärnöfjärden sek namn",NA,df$O2)
  df$O2<-ifelse(df$WB=="SE560825-144215 Inre Pukaviksbukten",NA,df$O2)
  df$O2<-ifelse(df$WB=="SE561000-153320 Danmarksfjärden",NA,df$O2)
  df$O2<-ifelse(df$WB=="SE561005-150250 Tjäröfjärden",NA,df$O2)
  df$O2<-ifelse(df$WB=="SE562450-122751 Skäldervikens kustvatten",NA,df$O2)
  df$O2<-ifelse(df$WB=="SE563000-123351 Laholmsbuktens kustvatten",NA,df$O2)
  df$O2<-ifelse(df$WB=="SE571000-184001 Ö Gotlands s kustvatten",NA,df$O2)
  df$O2<-ifelse(df$WB=="SE591200-183600 Nämdöfjärden",NA,df$O2)
  df$O2<-ifelse(df$WB=="SE591300-182800 Ingaröfjärden",NA,df$O2)
  df$O2<-ifelse(df$WB=="SE591800-181360 Skurusundet",NA,df$O2)
  df$O2<-ifelse(df$WB=="SE592000-184700 Kanholmsfjärden",NA,df$O2)
  df$O2<-ifelse(df$WB=="SE592245-184400 Sollenkrokafjärden",NA,df$O2)
  df$O2<-ifelse(df$WB=="SE592420-182210 Södra Vaxholmsfjärden",NA,df$O2)
  df$O2<-ifelse(df$WB=="SE625710-183000 Omnefjärden",NA,df$O2)
  df$O2<-ifelse(df$WB=="SE631610-184500 Örnsköldsviksfjärden",NA,df$O2)
  df$O2<-ifelse(df$WB=="SE631840-191130 Husumbukten",NA,df$O2)
  df$O2<-ifelse(df$WB=="SE659024-162417 Edsviken",NA,df$O2)
  df$O2<-ifelse(df$WB=="SE554810-125240 Lundåkrabukten" & 
                  df$period=="2013-2017",NA,df$O2)
  df$O2<-ifelse(df$WB=="SE552500-124461 S Öresunds kustvatten",NA,df$O2)
  df$O2<-ifelse(df$WB=="SE554040-125750 Lommabukten",NA,df$O2)
}

# ---------- -----------------------------
#df<-df %>% filter(DistrictID=="SE5")

#df<-df %>% filter(!is.na(O2))


#df <- test %>% left_join(df,by=c("WB"="WB"))

#df<-df %>% filter(WB=="SE554800-142001 V Hanöbuktens kustvatten")
#df<-df %>% filter(WB=="SE575340-113000 Marstrandsfjorden")
#df<-df %>% filter(!is.na(O2))

test<-df %>% filter(WB=="SE591800-181360 Skurusundet" & !is.na(O2))
# Oxygen indicator - first version
RefCond_sali <- c(56,50,44,38,32,26,20,rep(17,29))
variance_list <- list(V_station=0.5,V_obspoint=0,
                      V_year=1.5,V_yearmonth=1.4,
                      V_stationyear=0.4,V_stationmonth=0.1,V_stationdate=1.0,
                      V_institution=0.0,V_replication=0)
MonthInclude <- c(1,2,3,4,5,6,7,8,9,10,11,12)
AssessmentResults <- CalculateIndicator("CoastOxygen",test,RefCond_sali,variance_list,MonthInclude,2007,2012,n_iter=10)


