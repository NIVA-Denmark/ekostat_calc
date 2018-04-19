ReadParms_chla <- function() {
  
  covparams_CumCover <- haven::read_sas("data/covparms_chla_test.sas7bdat")
  parmest_CumCover <- haven::read_sas("data/parmest_chla_test.sas7bdat")

  res <- list(
    covparams_CumCover = covparams_CumCover,
    parmest_CumCover = parmest_CumCover
   )
  
  return(res)
  
}