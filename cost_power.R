cost_crt <- function(
  
ctreat = 200 ,
streat = 10 ,
ccon = 100 ,
scon = 10 ,
iccmax = 0.1 ,
Vmax = 2,                      ## How can a user know what to input for 'Vmax' & 'mean_dif'?
SDratiomax = 1 ,
SDratiomin = 1/SDratiomax,
mean_dif = 0.5,               ## I changed beta_1 to 'mean_dif'
power = 80)
{

  alpha = 5  
  tail = 2 
  
ESmin <- mean_dif/sqrt(Vmax/2)
zbeta <- qnorm((power/100),mean = 0, sd =1)
zalpha <- qnorm(1-(alpha/(100*tail)),mean = 0, sd =1)
maxvarmean_difhat <- (mean_dif / (zbeta + zalpha))**2   # 'mean_dif' is also used here


ntreat <- sqrt((ctreat/streat)*((1-iccmax)/iccmax))
ncon <- sqrt((ccon/scon)*((1-iccmax)/iccmax))
costpertreatcluster <- ctreat + (streat*ntreat)
costperconcluster <- ccon + (scon*ncon)
gtreat <- (sqrt(ctreat*iccmax) + sqrt(streat*(1-iccmax)))**2
gcon <- (sqrt(ccon*iccmax) + sqrt(scon*(1-iccmax)))**2
pratio <- sqrt(gtreat/gcon)


budgetratio <- 99999
budgetratio <- ifelse( ((pratio <= SDratiomax) & (pratio >= SDratiomin)), pratio**2, ifelse((pratio > SDratiomax), pratio*SDratiomax, pratio*SDratiomin))
fraction <- budgetratio/(1 + budgetratio)

mmvnumer <- 99999
mmvnumer <- ifelse( ((pratio <= SDratiomax) & (pratio >= SDratiomin)),
                    gcon*Vmax*(1+(pratio**2)),                                #Varmax is used here as well
                    ifelse((pratio > SDratiomax),
                           gcon*Vmax*(((pratio*SDratiomax)+1)**2/((SDratiomax**2)+1)),
                           gcon*Vmax*(((pratio*SDratiomin)+1)**2/((SDratiomin**2) + 1))   ) )


budget <- mmvnumer/maxvarmean_difhat
treatbudget <- fraction*budget
conbudget <- (1-fraction)*budget
ktreat <- treatbudget/costpertreatcluster
kcon <- conbudget/costperconcluster


ktreatrup <- ceiling(ktreat)
kconrup <- ceiling(kcon)

ktreatplus <- ifelse(alpha == 5, 
                      ifelse (pmin(ktreatrup,kconrup) < 8, 
                              ktreatrup + 3, ktreatrup + 2), 
                      ifelse( (alpha == 1), ktreatrup + 4, 99999)) 

kconplus <- ifelse(alpha == 5, 
                    ifelse (pmin(ktreatrup,kconrup) < 8, 
                            kconrup + 3, kconrup + 2) , 
                    ifelse(alpha == 1, kconrup + 4,  99999)) 

budgetplus <- (ktreatplus*costpertreatcluster) + 
  (kconplus*costperconcluster)

message("
     ncon: sample size per control cluster
   ntreat: sample size per treated cluster
   ktreat: # of treated clusters
     kcon: # of control clusters
   budget: total budget needed\n")

data.frame(SE_effect = sqrt(maxvarmean_difhat),ncon, ntreat, ktreat = ktreatplus,kcon=kconplus,budget=paste0("$",round(budgetplus, 2)), row.names = "STUDY:")
}

# EXAMPLE OF USE:
#cost_crt(iccmax = .2, power = 80, Vmax = 3)
