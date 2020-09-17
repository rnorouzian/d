impute <- function(D, FUN = median){
  y <- sapply(D, is.numeric)
  D[y] <- lapply(D[y], function(x){x[is.na(x)] <- FUN(x, na.rm = TRUE); x})
  return(D)
}


mean_split_halves <- function(data){
  
rxx <- combn(seq_along(data), ncol(data)/2, function(x) cor(rowSums(data[,x]), rowSums(data[,-x])))

split_halves <- (2*rxx )/ (1+rxx)

return(c(alpha = mean(split_halves)))
}


cronbach <- function(data, impute = FALSE, na.rm = TRUE, impute_fun = median, level = .95, digits = 6){

if(isTRUE(impute)) data <- impute(data, FUN = impute_fun)    
if(isTRUE(na.rm)) data <- na.omit(data)  

N <- ncol(data)
Sn <- nrow(data)
var_cov <- cov(data)
v_bar <- mean(diag(var_cov))
c_bar <- mean(var_cov[lower.tri(var_cov)])

alpha <- (N * c_bar) / (v_bar + (N-1)*c_bar)
alpha_trans <- log(1-alpha)/2
se <- sqrt(N/(2*(N-1)*Sn))

lower <- (1-level)/2
a <- qnorm(c(1-lower,lower))
ci_alpha_trans <- alpha_trans + a * se
ci_alpha <- 1-exp(2*ci_alpha_trans)

round(data.frame(alpha = alpha, lower = ci_alpha[1], upper = ci_alpha[2]), digits = digits)
}
