needxzc <- c("tidyverse", "survey", "sampling", "randomizr", "psych", "RBtest", "mice", "BiocManager", "DAKS")
not.have <- needxzc[!(needxzc %in% installed.packages()[,"Package"])]
if(length(not.have)) install.packages(not.have)


suppressWarnings(                                         
  suppressMessages({ 
    
    for(i in needxzc){
      library(i, character.only = TRUE)
    }
  })) 

source('https://raw.githubusercontent.com/rnorouzian/d/master/Design.R')

source('https://raw.githubusercontent.com/rnorouzian/d/master/cost_power.R')

source('https://raw.githubusercontent.com/rnorouzian/d/master/st.R')
