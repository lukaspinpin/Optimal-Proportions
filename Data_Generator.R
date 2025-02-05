generate_data <- function(N=200, dist="bern", parV = 0.5){
  if(dist=="bern"){
    return(rbinom(N,1, parV))
  }
  if(dist=="norm"){
    return(rnorm(n=N,mean=parV[1],sd=parV[2]))
  }
  if(dist=="Lickert"){
    return(0)
  }
  if(dist=="Expon"){
    return(0)
  }
  if(dist=="Beta"){
    return(0)
  }
}

superior <- function(dist="bern",par1=c(0,0), par2=FALSE){
  if(dist=="bern"){
    if(par1[1]<par1[2]){
      return(1)
    }
    if(par1[1]>par1[2]){
      return(0)
    }
    if(par1[1]==par1[2]){
      return(2)
    }
  }
  if(dist=="norm"){
    if(par1[1]<par1[2]){
      return(1)
    }
    if(par1[1]>par1[2]){
      return(0)
    }
    if(par1[1]==par1[2]){
      return(2)
    }
  }
  if(dist=="Lickert" || dist=="Beta"){
    exp1 <- (par1[1]+par2[1])/par1[1]
    exp2 <- (par1[2]+par2[2])/par1[2]
    if(exp1[1]>exp1[2]){
      return(1)
    }
    if(exp1[1]<exp1[2]){
      return(0)
    }
    if(exp1[1]==exp1[2]){
      return(2)
    }
  }
  if(dist=="Expon"){
    if(par1[1]>par1[2]){
      return(1)
    }
    if(par1[1]<par1[2]){
      return(0)
    }
    if(par1[1]==par1[2]){
      return(2)
    }
  }
}