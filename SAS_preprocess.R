# Read SAS files and save to R data file
# for use in process_all.R

rm(list = ls())

library(RSQLite)
library(dplyr)
library(tidyr)
library(haven)
library(lubridate)
library(prodlim)
library(matrixStats)


df_water<-read_sas("data/coast_watersamples.sas7bdat") #16975
df_o2<-read_sas("data/coast_oxygen.sas7bdat")          #94747
df_bqi<-read_sas("data/coast_bqi.sas7bdat")            # 6731
df_msmdi<-read_sas("data/coast_msmdi.sas7bdat")        # 1089
df_biovol<-read_sas("data/coast_biovol.sas7bdat")        # 1113

df_water <- df_water %>% mutate(time=hms::as.hms(ifelse(time=="","12:00:00",paste0(time,":00")))) # converting time values from character

df <- bind_rows(df_water,df_o2,df_bqi,df_msmdi,df_biovol)

df <- df %>% mutate(WB_ID=paste0("SE",WB_ID),
                    Year=ifelse(is.na(Year),lubridate::year(date),Year),
                    Month=ifelse(is.na(Month),lubridate::month(date),Month))

save(df,file="data/SASdata.Rda")
