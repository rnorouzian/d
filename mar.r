need <- "RBtest"
not.have <- need[!(need %in% installed.packages()[,"Package"])]
if(length(not.have)) install.packages(not.have)

library(RBtest)


mar_mcar <- function(data){
  
  res <- suppressWarnings(RBtest::RBtest.iter(data, 5)$type.final) 
  noquote(ifelse(res == 0, "MCAR", ifelse(res == 1, "MAR", "NA")))
  
}
