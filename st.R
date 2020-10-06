library(tidyverse)
library(survey)

prop_str_sample <- function(sample_frame, vars = NULL, n = 10, seed = NULL){
  
  if(!inherits(sample_frame, c("data.frame", "tibble", "matrix"))) stop("sample_frame must be a data.frame.", call. = FALSE)   
  if(is.null(vars)) vars <- names(sample_frame)
  
  
  DATA1 <- sample_frame %>%
    group_by(across(all_of(vars))) %>%
    summarise(n = n(), .groups = 'drop') %>%
    mutate(fpc = n/sum(n)) %>% 
    right_join(sample_frame)
  
  
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
    right_join(DF)
  
DF <- left_join(DATA1, DF, by = vars) %>%  
    mutate(fpc = coalesce(fpc.y, fpc.x)) %>% select(names(DF))  ## Error: Can't subset columns that don't exist. @@@@@@@@@@@@@@@@@@@@@
                                                                ## x Columns `n`, `fake.name`, `sector`, `pretest`, `state`, etc. don't exist. @@@@@@@
  
  return(list(frame_strata_freq = addmargins(f_freq), frame_strata_prop = round(addmargins(prop.table(f_freq)),3), sample_strata_prop = round(addmargins(prop.table(s_freq)),3), sample_strata_freq=addmargins(s_freq), data = DF))
}

d <- read.csv('https://raw.githubusercontent.com/rnorouzian/d/master/su.csv')

(out <- prop_str_sample(d, c("gender", "pre"), n=200))
