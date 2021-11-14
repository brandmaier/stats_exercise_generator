generate_regression <- function(n, ryx1, ryx2, rx12, xmu, ymu, sx1,sx2,sy, round=0) {
#  dat <- MASS::mvrnorm( n = n, mu, Sigma)
  
  b1 = round( (ryx1 - ryx2*rx12 ) / (1-rx12^2)* sy/sx1, 2)
  b2 = round( (ryx2 - ryx1*rx12) / (1-rx12^2) * sy/sx2, 2)
  
  list(n=n, r01=r01, xmu=xmu, ymu=ymu, sx=sx, sy=sy, b1=b1, b2=b2,
       ryx1=ryx1, ryx2=ryx2, rx12=rx12) 
}

solution_coeff <- function(x) {
  paste(
 paste0("b_1=\frac{",x$ryx1,"-",x$ryx2,"\cdot",x$rx12,"}{1-",x$rx12,"^2} \cdot \frac{",x$sy,"}{",x$sx1,"}")  ,

paste0("b_1=\frac{",x$ryx1,"-",x$ryx2,"\cdot",x$rx12,"}{1-",x$rx12,"^2} \cdot \frac{",x$sy,"}{",x$sx1,"}")  
  )
}

solution_std_from_r2 <- function(x) {
  
}