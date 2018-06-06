rm(list = ls())

library(RSQLite)
library(dplyr)
library(tidyr)

db1<-"ekostat.db"
db2<-"ekostatSecchi.db"
dbout<-"ekostat_new.db"

tabs<-c("WB","data")
for(tab in tabs){
  sql<-paste0("SELECT * FROM ",tab)
  dbconn <- dbConnect(SQLite(), dbname=paste0("output/",db1))
  df <- dbGetQuery(dbconn, sql)             
  dbDisconnect(dbconn)
  
  dbconn <- dbConnect(SQLite(), dbname=paste0("output/",dbout))
  dbWriteTable(conn=dbconn, name=tab, df, overwrite=T,append=F,row.names=F)
  dbDisconnect(dbconn)
}  
  
tabs<-c("resAvg","resMC","resErr","resYear")
for(tab in tabs){
  sql<-paste0("SELECT * FROM ",tab," WHERE Indicator NOT IN ('CoastSecchi','CoastSecchiEQR')")
  dbconn <- dbConnect(SQLite(), dbname=paste0("output/",db1))
  df <- dbGetQuery(dbconn, sql)             
  dbDisconnect(dbconn)
  
  dbconn <- dbConnect(SQLite(), dbname=paste0("output/",dbout))
  dbWriteTable(conn=dbconn, name=tab, df, overwrite=T,append=F,row.names=F)
  dbDisconnect(dbconn)
  
  sql<-paste0("SELECT * FROM ",tab)
  dbconn <- dbConnect(SQLite(), dbname=paste0("output/",db2))
  df <- dbGetQuery(dbconn, sql)             
  dbDisconnect(dbconn)
  
  dbconn <- dbConnect(SQLite(), dbname=paste0("output/",dbout))
  dbWriteTable(conn=dbconn, name=tab, df, overwrite=F,append=T,row.names=F)
  dbDisconnect(dbconn)
}  

