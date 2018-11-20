ReadBounds<-function(){
  df<-read.table("data/boundaries.txt", fileEncoding = "UTF-8", sep="\t", 
                 stringsAsFactors=F, header=T, comment.char="",na.string = "#N/A")
  
  df$RefCond<-as.numeric(df$RefCond)
  df$H.G<-as.numeric(df$H.G)
  df$G.M<-as.numeric(df$G.M)
  df$M.P<-as.numeric(df$M.P)
  df$P.B<-as.numeric(df$P.B)
  df$Sali_0<-as.numeric(df$Sali_0)
  
  return(df)
}

ReadBoundsHypoxicArea<-function(){
  df<-read.table("data/BoundsHypoxicArea.txt", fileEncoding = "UTF-8", sep="\t", 
                 stringsAsFactors=F, header=T, comment.char="",na.string = "#N/A")
  #df$WB<-paste0(df$WaterbodyID," ",df$WB_name)
  df$WB<-df$WaterbodyID
  return(df)
}

