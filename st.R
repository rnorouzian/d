need <- c("tidyverse", "survey", "sampling")
not.have <- need[!(need %in% installed.packages()[,"Package"])]
if(length(not.have)) install.packages(not.have)

library(tidyverse)
library(survey)
library(sampling)

data(api)


prop_str_sample <- function(sample_frame, vars = NULL, n = 10, seed = NULL){
  
  if(!inherits(sample_frame, c("data.frame", "tibble"))) stop("sample_frame must be a 'data.frame'.", call. = FALSE)   
  if(is.null(vars)) vars <- names(sample_frame)[sapply(sample_frame, is.character)]
  
  DATA1 <- sample_frame %>%
    group_by(across(all_of(vars))) %>%
    summarise(n = n(), .groups = 'drop') %>%
    mutate(fpc = n/sum(n)) %>% 
    right_join(sample_frame, by = vars)
  
  
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
  
  DF <- DF %>%
    group_by(across(all_of(vars))) %>%
    summarise(n = n(), .groups = 'drop') %>%
    mutate(fpc = n/sum(n)) %>% 
    right_join(DF, by = vars)
  
  DF <- DF %>%
    left_join(DATA1 %>% distinct(across(all_of(vars)), fpc), 
              by = vars) %>%
    mutate(fpc = coalesce(fpc.y, fpc.x)) %>%
    select(names(DF)) %>% as.data.frame()
  
  return(list(frame_strata_freq = addmargins(f_freq), frame_strata_prop = round(addmargins(prop.table(f_freq)),3), sample_strata_prop = round(addmargins(prop.table(s_freq)),3), sample_strata_freq=addmargins(s_freq), data = DF))
}

d <- read.csv('https://raw.githubusercontent.com/rnorouzian/d/master/su.csv')

#(out <- prop_str_sample(d, c("gender", "pre"), n=200))
