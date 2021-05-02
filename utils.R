sim_cRCT <-function(m,sigma,mu1,mu2,k1,k2,rho,alpha){
  
  treat <- c(rep(0,(k1*m)),rep(1,(k2*m)))
  site <- c(rep(1:(k1+k2),each=m))
  
  
  var_b <-(rho*sigma^2)/(1-rho)
  
  err <- rnorm((k1+k2)*m,mean=0,sigma)
  a <- rnorm((k1+k2),mean=0,sqrt(var_b)) # site level random effect
  
  arep <- rep(a,each = m)
  
  b1 <- mu2-mu1
  y <- mu1 + b1*treat + arep + err
  #return(summary(lme4::lmer(y ~ treat + (1|site)))$coef[2,"t value"])
  res <- summary(lmerTest::lmer(y ~ treat + (1|site)))$coef[2,5]

  return(res  < alpha)
}

get_power <- function(m,sigma,mu1,mu2,k1,k2,rho,alpha,nreps){
  mean(replicate(nreps,
                 sim_cRCT(m,sigma,mu1,mu2,k1,k2,rho,alpha)
  ))
}

powersims <-function(sigma,
                     mu1,mu2,
                     k1,k2,
                     rho,
                     alpha, 
                     nreps, 
                     start, 
                     end, 
                     by){
  
  out <- data.frame("Sample Size" = seq(start,end,by),  "Power" = sapply(
    seq(start, end, by), 
    get_power, 
    sigma,mu1,mu2, k1,k2,rho,alpha,nreps
  ))
  return(out)
}
