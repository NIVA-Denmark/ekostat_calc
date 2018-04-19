ReadMonitoringDataSMHI <- function(SASdataset) {
  
  res <- read_sas(SASdataset)
    
 
  return(res)
  
}



#df <- read_sas("data/Gullmarn_2001_2006.sas7bdat")
#df <- read_sas("data/Gullmarn_2007_2012.sas7bdat")
#df <- read_sas("data/Gullmarn_2013_2016.sas7bdat")
#df <- read_sas("data/Byfjorden_2007_2012.sas7bdat")
#df <- read_sas("data/Danafjord_2001_2006.sas7bdat")
#df <- read_sas("data/Koljoefjord_2013_2016.sas7bdat")