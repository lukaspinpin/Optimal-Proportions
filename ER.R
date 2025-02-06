library(BSDA)
library(rankFD)

source('Data_Generator.R')

sim_ER <-  function(N=200, dist="bern",  par1 = c(0,0), par2 = NULL, measure="sd", burnin=3, nsim=10^4, alpha=0.05, one.sided = FALSE){
  
  out = matrix(nrow = nsim, ncol = 8)
    for(i in 1:nsim){
      out[i,] = two_arm_ER(N=N, dist=dist,  par1 =par1, par2 = par2, measure=measure,burnin = burnin, Z_corrected=FALSE)
    }
  return(out)
}

two_arm_ER <- function(N=200, dist="bern",  par1 = c(0,0), par2 = NULL, measure="sd", burnin=3, Z_corrected=FALSE){
  # Generate Samples 
  n1 = min(max(rbinom(1, N, 0.5),burnin), N-burnin)
  n0 = N - n1
  x0 <- generate_data(N=n0, dist=dist, parV=c(par1[1],par2[1])) 
  x1 <- generate_data(N=n1, dist=dist, parV=c(par1[2],par2[2]))
  
  # Response
  s0 <- sum(x0)
  s1 <- sum(x1)
  response <- (s0+s1)/N
  
  #Imbalance Measure
  sup <- superior(dist=dist,par1 = par1, par2 = par2)
  if(sup==2){ #2 means that no arm is theoretically supirior 
    per.sup <- NA
  } else {
    if(sup==0){ 
      per.sup <- n0/N
    }
    if(sup==1){ 
      per.sup <- n1/N
    }
  }
  
  ##### Z - Test & BM-Test
  source('WaldTest.R')
  ##### Z - Test 
  if(dist=="bern"){
    Z_P <-wald.test.binary(c(x0),c(x1), measure = measure)
  } else {
    Z_P <- wald.test(x0,x1)
  }
    # One-Point-Sample-Correction
    sigx <- sd(x0)
    sigy <- sd(x1)
    if(sigx==0 && sigy==0){
      BM_P <- Z_P[1]
    } else {
      #BM <- lawstat::brunner.munzel.test(c(x0,0,1),c(x1,0,1))
      BM <- lawstat::brunner.munzel.test(c(x0),c(x1))
      if(BM$statistic == Inf){
        BM_P <- 0
      } else if(BM$statistic == -Inf) {
        BM_P <-  1
      } else {
        BM_P <- BM$p.value
      }
    }
    n <- c(n0,n1)/N
  return(c(response, Z_P, BM_P, per.sup, n))
}


#sim_ER(N=50, dist="bern", par1 = c(0.1,0.3),nsim=10^4, measure="sd")