library(lme4)
library(lmerTest)
sim_cRCT <-function(m,sigma,mu1,mu2,k1,k2,rho,alpha){
  
  treat <- c(rep(0,(k1*m)),rep(1,(k2*m)))
  site <- c(rep(1:(k1+k2),each=m))
  
  
  var_b <-(rho*sigma^2)/(1-rho)
  
  err <- rnorm((k1+k2)*m,mean=0,sigma)
  a <- rnorm((k1+k2),mean=0,sqrt(var_b)) # site level random effect
  
  arep <- rep(a,each = m)
  
  b1 <- mu2-mu1
  y <- mu1 + b1*treat + arep + err
  
  #tests<-anova(lmer(y ~ treat + (1|site)),type = 2, ddf = "Kenward-Roger")
  
  #return(tests$`Pr(>F)` < alpha)
  return(summary(lme4::lmer(y ~ treat + (1|site), control=lmerControl( optCtrl=list(maxfun=2e5))))$coef[2,3] > 1.96)
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


powersims(sigma=10,mu1=10,mu2=13,k1=10,k2=10,rho=.1,alpha=.05,nreps=500,start=30,end=40,by=5)


# what if the data is skewed??
library(fGarch)

simcRCTnonNorm<-function(k,m,mu1,mu2,sigma,rho,alpha,xi){
  
  treat <- c(rep(0,(k*m)),rep(1,(k*m)))
  site <- c(rep(1:(k*2),each=m))
  
  
  var_b <-(rho*sigma^2)/(1-rho)
  
  
  rsnorm(k*2*m,mean=0, sd = sigma, xi = 1.5)
  a <- rnorm(k*2,mean=0,sqrt(var_b)) # site level random effect
  
  arep <- rep(a,each = m)
  
  b1 <- mu2-mu1
  y <- mu1 + b1*treat + arep + err
  
  
  
  res <- lmer(y ~ treat + (1|site))
  summary(res)
  tests<-anova(res,type = 2, ddf = "Kenward-Roger")
  p<-tests$`Pr(>F)`
}

sims<-replicate(500,simcRCTnonNorm(k=10,
                                   m=3,
                                   rho=.05,
                                   sigma=10,
                                   mu1=10,
                                   mu2=13))

power<-mean(ifelse(sims<0.05,1,0))
power

