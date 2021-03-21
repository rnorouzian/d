need <- c("tidyverse", "survey", "sampling", "randomizr")
not.have <- need[!(need %in% installed.packages()[,"Package"])]
if(length(not.have)) install.packages(not.have)

suppressMessages({ 
library(tidyverse)
library(survey)
library(sampling)
})  

data(api)

prop_str_sample <- function(sample_frame, vars = NULL, n = 10, seed = NULL){
  
  if(!inherits(sample_frame, "data.frame")) stop("sample_frame must be a data.frame.", call. = FALSE)   
  vars2 <- names(sample_frame)[sapply(sample_frame, is.character)]
  if(is.null(vars)) vars <- vars2
  if(length(vars) == 0) stop("No variable name was found for stratification.", call. = FALSE)
  ok_names <- vars %in% vars2
  #if(!all(ok_names)) stop(paste(toString(vars[!ok_names]), "not found in the 'sample_frame'."), call. = FALSE)
  
  set.seed(seed)
  rpss <- function(stratum, n) {
    props <- table(stratum)/length(stratum)
    nstrat <- as.vector(round(n*props))
    nstrat[nstrat==0] <- 1
    names(nstrat) <- names(props)
    survey::stratsample(stratum, nstrat)
  }
  
  pick_index <- rpss(stratum = interaction(sample_frame[vars], drop = TRUE), n=n)
  DF <- sample_frame[pick_index, ] 
  
  f_freq <- table(sample_frame[vars])
  s_freq <- table(DF[vars])
  
  fpc <- s_freq/f_freq
  
  l1 <- length(dimnames(fpc))
  nm1 <- names(dimnames(fpc))
  if(l1 == 1 && nm1 == "") {
    names(dimnames(fpc)) <- vars
  }
  
  DF <- merge(DF, as.data.frame(fpc))
  names(DF)[names(DF) == "Freq"] <- "fpc"
  
  return(list(frame_strata_freq = addmargins(f_freq), frame_strata_prop = round(addmargins(prop.table(f_freq)),3), sample_strata_prop = round(addmargins(prop.table(s_freq)),3), sample_strata_freq=addmargins(s_freq), fpc = fpc, data = DF))
}

d <- read.csv('https://raw.githubusercontent.com/rnorouzian/d/master/su.csv')

#(out <- prop_str_sample(d, c("gender", "pre"), n=200))


npps <- function(sample_frame, vars, n) {
  
  stratum <- interaction(sample_frame[vars], drop = TRUE)
  props <- table(stratum)/length(stratum)
  nstrat <- as.vector(round(n*props))
  nstrat[nstrat==0] <- 1
  names(nstrat) <- names(props)
  nstrat
}
