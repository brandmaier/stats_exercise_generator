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