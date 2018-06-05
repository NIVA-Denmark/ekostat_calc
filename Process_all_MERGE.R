rm(list = ls())

library(RSQLite)
library(dplyr)
library(tidyr)

dbs<-c("ekostat1.db","ekostat2.db","ekostat3.db","ekostatO2.db")
tabs<-c("resAvg","resMC","resErr","resYear","WB","data")
tabs2<-c("resAvg","resMC","resErr","resYear")
dbout<-"ekostat.db"


for(db in dbs){
  for(tab in tabs){
    cat(paste0(db," ",tab))
    dbconn <- dbConnect(SQLite(), dbname=paste0("output/",db))
    df <- dbGetQuery(dbconn, paste0("SELECT * FROM ",tab))
    dbDisconnect(dbconn)
    
    dbconn <- dbConnect(SQLite(), dbname=paste0("output/",dbout))
    dbWriteTable(conn=dbconn, name=tab, df, overwrite=F,append=T,row.names=F)
    dbDisconnect(dbconn)
    
    cat(paste0("  ...done \n"))
  }
  tabs<-tabs2
  
}


