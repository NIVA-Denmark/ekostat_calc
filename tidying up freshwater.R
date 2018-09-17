library(haven)
library(dplyr)

source("ReadMonitoringData.R")

# get list of waterbodies from the two lake data files
# tidy up
# there are some rows with same WB_ID where on has a name and the other doesn't
# if we can find a unique name for a WB_ID, then this should be used for all occurrences where the name is not given.

df1<-ReadMonitoringDataSMHI("data/lake_biolindex.sas7bdat")
df2<-ReadMonitoringDataSMHI("data/lake_WQdata.sas7bdat")

pars1<-names(df1)[15:35]
pars2<-names(df2)[10:17]

pars<-c(pars1,pars2)

pars


# --------------------------------------------------------
fillblanks<-function(df,param1,param){
  #param1<-"WB_ID"
  #param <- "WB_name"
  dots<-c(param1,param)
  df <- df %>% 
    group_by_(.dots = dots) %>%
    summarise() %>%
    ungroup()
  df <- df[!is.na(df[,param1]),]
  df <- df[df[,param1]!="",]
  df <- df[!is.na(df[,param]),]
  df <- df[df[,param]!="",]
  return(df)
}
# --------- WB1 -----------------------------------------------
wb1<-df1 %>% group_by(WB_ID,WB_name,typ,Distrikt,water_typee) %>% 
  summarise(n=n()) %>% 
  ungroup() %>%
  arrange(WB_ID,WB_name,typ,Distrikt,water_typee,desc(n))

wb1a<-fillblanks(wb1,"WB_ID","WB_name")  
wb1b<-fillblanks(wb1,"WB_ID","typ")  
wb1c<-fillblanks(wb1,"WB_ID","Distrikt")  
wb1d<-fillblanks(wb1,"WB_ID","water_typee")  

wb1 <- wb1 %>% 
  filter(WB_ID!="") %>%
  distinct(WB_ID) %>%
  left_join(wb1a,by="WB_ID") %>%
  left_join(wb1b,by="WB_ID") %>%
  left_join(wb1c,by="WB_ID") %>%
  left_join(wb1d,by="WB_ID") %>%
  mutate(X1=1) 
  
#wb1 <- wb1 %>% group_by(WB_ID) %>% summarise(n=n())

# --------- WB1 -----------------------------------------------
wb2<-df2 %>% group_by(WB_ID,water_type) %>% 
  summarise(n=n()) %>% 
  ungroup() %>%
  arrange(WB_ID,water_type,desc(n))

wb2a<-fillblanks(wb2,"WB_ID","water_type")

wb2 <- wb2 %>% 
  filter(WB_ID!="") %>%
  distinct(WB_ID) %>%
  left_join(wb2a,by="WB_ID") %>%
  mutate(X2=1)

#wb2 <- wb2 %>% group_by(WB_ID) %>% summarise(n=n())
# --------------------------------------------------------


wb1$WB_ID<-as.character(wb1$WB_ID)
wb2$WB_ID<-as.character(wb2$WB_ID)

wb1 <- wb1 %>%
  rename(water_type=water_typee)

wb21 <- wb2 %>%
  left_join(select(wb1,WB_ID,X1),by="WB_ID") %>%
  filter(is.na(X1)) %>%
  select(WB_ID,water_type)


wb1 <- wb1 %>% 
  select(-X1) %>%
  bind_rows(wb21) %>%
  rename(WaterbodyID=WB_ID,
         WaterbodyName=WB_name,
         TypeID=typ,
         WaterType=water_type,
         District=Distrikt)



