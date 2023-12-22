needxzc <- c("tidyverse", "survey", "sampling", "randomizr", "psych", "RBtest", "mice", "BiocManager", "DAKS","RBtest","lattice")
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



penalty <- function(dayslate)
{
  halflife = 7
  expshape = 1 
  round(exp( log(.5)/halflife^expshape*(dayslate)^expshape ), 2)
}

#=================================================================================================================================

pen.plot <- function(dayslate = 50){
  
  curve(penalty(x), 0, dayslate, las = 1, tck = -0.03,
        xaxt = "n", xlab = "Days Late", ylab = "Penalty", lwd = 2, mgp = c(2, .4, 0), cex.axis = .9)
  axis(1, at = 0:max(dayslate), cex.axis = .6, mgp = c(2, .01, 0), tck = -0.03)
}

#=================================================================================================================================

my.penalty <- function(dayslate = 0, dayslate.span = 30){
  
  dayslate.span <- round(abs(dayslate.span))
  dayslate <- round(abs(dayslate))
  dayslate.span <- ifelse(dayslate > dayslate.span, dayslate, dayslate.span)
  pen.plot(dayslate.span)
  x <- dayslate
  y <- penalty(dayslate)
  points(x, y, type = "h", col = ifelse(dayslate != 0, "red", 1))
  points(x, y, bg = 'cyan', col = 'magenta', pch = 21, cex = 1.5)
  text(x, y, y, cex = .75, font = 2, pos = 3, xpd = NA, col = ifelse(dayslate != 0, "red", 1))
}     


#================================================================================================================================


mar_mcar <- function(data){
  
  res <- try(suppressWarnings(RBtest::RBtest.iter(data, 5)$type.final), silent = TRUE) 
  
  if(inherits(res, "try-error")) res <- suppressWarnings(RBtest::RBtest(data)$type)
  
  noquote(ifelse(res == 0, "MCAR", ifelse(res == 1, "MAR", NA)))
  
}
