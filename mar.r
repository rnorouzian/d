need <- "RBtest"
not.have <- need[!(need %in% installed.packages()[,"Package"])]
if(length(not.have)) install.packages(not.have)

library(RBtest)


mar_mcar <- function(data){
  
  res <- try(suppressWarnings(RBtest::RBtest.iter(data, 5)$type.final), silent = TRUE) 
  
  if(inherits(res, "try-error")) res <- suppressWarnings(RBtest::RBtest(data)$type)
  
  noquote(ifelse(res == 0, "MCAR", ifelse(res == 1, "MAR", NA)))
  
}
