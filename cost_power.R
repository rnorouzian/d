cost_CRT <- function(
  
  cost_per_c_treat = 200 ,
  cost_per_s_treat = 15 ,
  cost_per_c_cont = 100 ,
  cost_per_s_cont = 10 ,
  iccmax = 0.15 ,
  mdes = .25,                      
  SDtreat_to_cont = 1.2 ,
  SDratiomin = 1/SDtreat_to_cont,
  power = 80,
  tail = 2 )
{
  
alpha <- 5
  
inv_d <- function(mdes) {
  c(mean_dif = 1, Vmax = 2/mdes^2)
}

  pars <- inv_d(mdes)
  mean_dif <- pars[[1]]
  Vmax <- pars[[2]]
  zbeta <- qnorm((power/100))
  zalpha <- qnorm(1-(alpha/(100*tail)))
  maxvarmean_difhat <- (mean_dif / (zbeta + zalpha))**2  
  
  
  ntreat <- sqrt((cost_per_c_treat/cost_per_s_treat)*((1-iccmax)/iccmax))
  ncont <- sqrt((cost_per_c_cont/cost_per_s_cont)*((1-iccmax)/iccmax))
  costpertreatcluster <- cost_per_c_treat + (cost_per_s_treat*ntreat)
  costperconcluster <- cost_per_c_cont + (cost_per_s_cont*ncont)
  gtreat <- (sqrt(cost_per_c_treat*iccmax) + sqrt(cost_per_s_treat*(1-iccmax)))**2
  gcon <- (sqrt(cost_per_c_cont*iccmax) + sqrt(cost_per_s_cont*(1-iccmax)))**2
  pratio <- sqrt(gtreat/gcon)
  
  
  budgetratio <- 99999
  budgetratio <- ifelse( ((pratio <= SDtreat_to_cont) & (pratio >= SDratiomin)), pratio**2, ifelse((pratio > SDtreat_to_cont), pratio*SDtreat_to_cont, pratio*SDratiomin))
  fraction <- budgetratio/(1 + budgetratio)
  
  mmvnumer <- 99999
  mmvnumer <- ifelse( ((pratio <= SDtreat_to_cont) & (pratio >= SDratiomin)),
                      gcon*Vmax*(1+(pratio**2)),                                #Varmax is used here as well
                      ifelse((pratio > SDtreat_to_cont),
                             gcon*Vmax*(((pratio*SDtreat_to_cont)+1)**2/((SDtreat_to_cont**2)+1)),
                             gcon*Vmax*(((pratio*SDratiomin)+1)**2/((SDratiomin**2) + 1))   ) )
  
  
  budget <- mmvnumer/maxvarmean_difhat
  treatbudget <- fraction*budget
  conbudget <- (1-fraction)*budget
  ktreat <- treatbudget/costpertreatcluster
  kcont <- conbudget/costperconcluster
  
  
  ktreatrup <- ceiling(ktreat)
  kcontrup <- ceiling(kcont)
  
  ktreatplus <- ifelse(alpha == 5, 
                       ifelse (pmin(ktreatrup,kcontrup) < 8, 
                               ktreatrup + 3, ktreatrup + 2), 
                       ifelse( (alpha == 1), ktreatrup + 4, 99999)) 
  
  kcontplus <- ifelse(alpha == 5, 
                     ifelse (pmin(ktreatrup,kcontrup) < 8, 
                             kcontrup + 3, kcontrup + 2) , 
                     ifelse(alpha == 1, kcontrup + 4,  99999)) 
  
  budgetplus <- (ktreatplus*costpertreatcluster) + 
    (kcontplus*costperconcluster)
  
  message("
    ncont: sample size per control cluster
   ntreat: sample size per treated cluster
   ktreat: # of treated clusters
    kcont: # of control clusters
   budget: total budget needed\n")
  
  data.frame(mdes = mdes, ncont = ncont <- ceiling(ncont), kcont = kcont <- kcontplus, 
             ntreat = ntreat <- ceiling(ntreat), ktreat = ktreat <- ktreatplus, budget = paste0("$", round(budgetplus, 2)), 
             power = paste0(power, "%"), total_N = ncont*kcont+ntreat*ktreat, row.names = "STUDY:")
}
