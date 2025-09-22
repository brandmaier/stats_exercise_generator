sample_corr <- function(n, r){ 
  MASS::mvrnorm(n=n, 
                mu=c(0,0), 
                Sigma=matrix(c(1,r,r,1),nrow=2))
  
}

generate_corr_twogroup <- function(n1, r1, n2, r2, alpha=0.05) {
  
  dat1 <- sample_corr(n1, r1)
  dat2 <- sample_corr(n2, r2)
  
  test1 = cor.test(dat1[,1],dat1[,2])
  test2 = cor.test(dat2[,1],dat2[,2])
  
  empr1 = round(test1$estimate,2)
  empr2 = round(test2$estimate,2)
  
  z1 = round(psych::fisherz(test1$estimate), 2)
  z2 = round(psych::fisherz(test2$estimate), 2)
  
  zdiff = (z1-z2) / sqrt(1/(n1-3)+1/(n2-3))
  
  zcrit = qnorm(1-alpha/2)
  
  return(list(n1=n1,r1=empr1,n2=n2,r2=empr2,dat1,dat2,test1,test2,z1,z2,zdiff=zdiff, zcrit=zcrit,z1=z1,z2=z2))
}

solution_corr_2grp <- function(x, round=2) {
  l1 <- paste0("$$Z=\\frac{",x$z1,"-",x$z2,"}{\\sqrt{","\\frac{1}{",x$n1,"-3}+\\frac{1}{",x$n2,"-3}","}} = ",round(x$zdiff,round),"$$\n")
  # l2 <- paste0("$$= ",round(x$zdiff,round),"$$\n")
  return(l1)
  # return(c(l1,l2))
}

fisher_z_solution <- function(r, round=2) {
  z = 0.5*log((1+r)/(1-r))
  cat("$$ \\frac{1}{2} \\log \\frac{1+",r,"}{1-",r,"} = ",round(z,round)," $$ ")
}
