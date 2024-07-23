sample_corr <- function(n, r){ 
  MASS::mvrnorm(n=n, 
                mu=c(0,0), 
                Sigma=matrix(c(1,r,r,1),nrow=2))
  
  }

generate_corr_twogroup <- function(n1, r1, n2, r2, alpha=0.05) {
  
  dat1 <- sample_corr(n1, r1)
  dat2 <- sample_corr(n2, r2)
  
  test1 = cor.test(d1[,1],d1[,2])
  test2 = cor.test(d2[,1],d2[,2])
  
  z1 = round(psych::fisherz(test1$estimate), 2)
  z2 = round(psych::fisherz(test2$estimate), 2)
  
  zdiff = (z1-z2) / sqrt(1/(n1-3)+1/(n2-3))
  
  zcrit = qnorm(1-alpha/2)
  
  return(list(n1=n1,r1=r2,n2=n2,r2=r2,dat1,dat2,test1,test2,z1,z2,zdiff=zdiff, zcrit=zcrit))
}

solution_corr_2grp <- function(x) {
  l1 <- paste0("$$Z=\\frac{",x$z1,"-",x$z2,"}{\\sqrt{","\\frac{1}{",x$n1,"-3}+\\frac{1}{",x$n2,"-3}","}}$$\n")
  l2 <- paste0("$$= ",x$zdiff,"$$\n")
  
  return(c(l1,l2))
}
#cat("Einsetzen in Formel 2.4 für den Test der Unterschiedlichkeit von Korrelationen in zwei unabhängigen Stichproben:\n")
