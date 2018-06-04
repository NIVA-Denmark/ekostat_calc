#Read waterbody bathymetry data 
#i.e. tables of cumulative %area vs depth for use in O2 indicator


ReadBathymetry<-function(){
  df<-read.table("data/hypsograph.txt", fileEncoding = "UTF-8", sep="\t", 
                 stringsAsFactors=F, header=T, comment.char="",na.string = "#N/A")
  dfwb<-read.table("data/waterbodies.txt", fileEncoding = "UTF-8", sep="\t", 
                   stringsAsFactors=F, header=T, comment.char="",na.string = "#N/A")
  
  df <- df %>% gather(key="area_pct",value="depth",2:102) %>%
    mutate(area_pct=as.numeric(substr(area_pct,2,4))) %>%
    arrange(WB_name,area_pct) %>% 
    left_join(select(dfwb,WaterbodyID,WaterbodyName),by=c("WB_name"="WaterbodyID")) %>%
    #mutate(WB=paste0(WB_name," ",ifelse(is.na(WaterbodyName),"",WaterbodyName))) %>%
    mutate(WB=WB_name) %>%
    select(WB,area_pct,depth)
  return(df)
}

ReadBathymetryDeprec<-function(){
  df<-read.table("data/WB_bathymetry.txt", fileEncoding = "UTF-8", sep="\t", 
                 stringsAsFactors=F, header=T, comment.char="",na.string = "#N/A")
  df$WB<-paste0(df$WaterbodyID," ",df$WB_name)
  return(df)
}

