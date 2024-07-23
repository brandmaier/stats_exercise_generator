generate_simple_regression <- function(n, rxy, mx, my, sx, sy) {
  b1 = round(rxy * sy / sx,2)
  b0 = round(my - b1 * mx, 2)

  result <- list(b0 = b0, b1 = b1, n=n, rxy=rxy, mx = mx, my = my, sx=sx,sy=sy)
  return(result)
}

solution_simple_regression <- function(x) {
paste0(
  paste0("b_1 = ",x$rxy,"\\cdot \\frac{",x$sy, "}{",x$sx,"}=",x$b1), "\n\\\\",
  
  paste0("b_0 = ",x$my,"-",round(x$b1,2),"\\cdot",x$mx,"=",x$b0)
)
  
}

generate_regression <- function(n, ryx1, ryx2, rx12, x1mu, x2mu, ymu, sx1,sx2,sy, 
                                round=0, varnames=NULL) {
#  dat <- MASS::mvrnorm( n = n, mu, Sigma)
  
  b1 = round( (ryx1 - ryx2*rx12 ) / (1-rx12^2)* sy/sx1, 2)
  b2 = round( (ryx2 - ryx1*rx12) / (1-rx12^2) * sy/sx2, 2)
  b0 = round(ymu - b1* x1mu - b2*x2mu,2)
#  k <- 2
 # browser()
  
  cormat <- matrix(data=c(1,rx12, ryx1,
                          rx12, 1, ryx2,
                          ryx1, ryx2, 1),nrow=3,ncol=3,byrow=TRUE)
  
  covmat <- psych::cor2cov(cormat, c(sx1,sx2,sy))
  
  dat<- MASS::mvrnorm(n=n,mu=c(x1mu,x2mu,ymu), Sigma = covmat,empirical = FALSE)
  dat <- data.frame(dat)
  names(dat) <- c("x1","x2","y")
  
  reg <- generate_multipleregression(lm(y~.,dat)) 
  
  reg$ryx1 <- ryx1
  reg$ryx2 <- ryx2
  reg$rx12 <- rx12
  
  reg$b0 <- b0
  reg$b1 <- b1
  reg$b2 <- b2
  
  return(reg)
  
 # list(n=n, xmu=xmu, ymu=ymu, sx1=sx1, sx2=sx2, sy=sy, b1=b1, b2=b2,
#       ryx1=ryx1, ryx2=ryx2, rx12=rx12, varnames=varnames, k=k) 
}

solution_coeff <- function(x) {
  paste(
 paste0("b_1 = \\frac{",x$ryx1,"-",x$ryx2," \\cdot",x$rx12,"}{1-",x$rx12,"^2} \\cdot \\frac{",x$sy,"}{",x$sx1,"}","=",x$b0)  ,
"\n\n",
#paste0("b_1=\\frac{",x$ryx1,"-",x$ryx2,"\\cdot",x$rx12,"}{1-",x$rx12,"^2} \\cdot \\frac{",x$sy,"}{",x$sx1,"}","=",x$b0)
""
  )
}

solution_std_from_r2 <- function(x) {
  paste0("s_E=")
}

#solution_adj_r2 <- function(x) {
#  return(" 1-\\frac{(1-",x$r2,")\\cdot (",x$n,"-1)}{(",x$n,"-",x$k,"-1)}=",x$adjr2," ")
#}


solution_adjr2 <- function(model) {
  
  r2 <- round(model$r2,2)
  N <- model$n
  k <- model$k
   
  adjr2 <- round(1- (1-r2)*(N-1)/(N-k-1),2)
  
  paste0( " R^2_{adj}=",
          paste0("1-\\frac{(1-",r2,")(",N,"-1)}{(",N,"-",k,"-1)}",sep=""),
          "=",adjr2," ",sep=""
  )
  
  #paste0(" R^2 ")
  
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

coeff_table <- function(x, booktabs=TRUE) {
  
  ncoeff <- x$k
  pres_data <- data.frame(#Modell=c(1,rep("",x$k)),
                          Regressionskoeffizient= round( coefficients(x$lm), 3),
                          `Std. Fehler`= round( sm$coefficients[,2], 3),
                          T = round( sm$coefficients[,3], 3),
                          p = round( sm$coefficients[,4],3))
  
  rownames(pres_data)[1] <- "Konstante"
  
  knitr::kable( pres_data, booktabs=booktabs
    
  )# %>%  kable_styling(latex_options = "striped")
}
