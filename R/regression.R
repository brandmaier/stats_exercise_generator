generate_regression <- function(n, ryx1, ryx2, rx12, xmu, ymu, sx1,sx2,sy, 
                                round=0, varnames=NULL) {
#  dat <- MASS::mvrnorm( n = n, mu, Sigma)
  
  b1 = round( (ryx1 - ryx2*rx12 ) / (1-rx12^2)* sy/sx1, 2)
  b2 = round( (ryx2 - ryx1*rx12) / (1-rx12^2) * sy/sx2, 2)
  
  
  
  list(n=n, xmu=xmu, ymu=ymu, sx1=sx1, sx2=sx2, sy=sy, b1=b1, b2=b2,
       ryx1=ryx1, ryx2=ryx2, rx12=rx12, varnames=varnames) 
}

solution_coeff <- function(x) {
  paste(
 paste0("b_1=\\frac{",x$ryx1,"-",x$ryx2,"\\cdot",x$rx12,"}{1-",x$rx12,"^2} \\cdot \\frac{",x$sy,"}{",x$sx1,"}","=",x$b0)  ,
"\n\n",
paste0("b_1=\\frac{",x$ryx1,"-",x$ryx2,"\\cdot",x$rx12,"}{1-",x$rx12,"^2} \\cdot \\frac{",x$sy,"}{",x$sx1,"}","=",x$b0)  
  )
}

solution_std_from_r2 <- function(x) {
  paste0("s_E=")
}

solution_adj_r2 <- function(x) {
  return(" 1-\\frac{(1-",x$r2,")\\cdot (",x$n,"-1)}{(",x$n,"-",x$k,"-1)}=",x$adjr2," ")
}

bivariate_table <- function(x) {
mat<-  matrix( c(1, "", "",
            x$ryx1, 1, "",
            x$ryx2, x$rx12, 1), ncol=3,
    byrow=TRUE
    )

mat <- data.frame(mat)
names(mat) <- x$varnames
rownames(mat) <- x$varnames
return(mat)
}


generate_multipleregression <- function(lm) {
  
  sm <- summary(lm)
  result <- list(lm=lm, r2 = sm$r.squared, adjr2 = sm$adj.r.squared,
                 n=length(sm$residuals), k = nrow(sm$coefficients)-1)
  #class(result) <- "multreg"
  return(result)
}

dat <- data.frame(y=rnorm(20),x1=rnorm(20),x2=rnorm(20))
sm <- summary(lm(y~x1+x2,dat))

library(kableExtra)

coeff_table <- function(x) {
  
  ncoeff <- x$k
  pres_data <- data.frame(Modell=c(1,rep("",x$k)),
                          RegressionskoeffizientB= round( coefficients(x$lm), 3),
                          StdFehler= round( sm$coefficients[,2], 3),
                          T= round( sm$coefficients[,3], 3),
                          Sig= round( sm$coefficients[,4],3))
  
  knitr::kable( pres_data
    
  ) %>%  kable_styling(latex_options = "striped")
}
