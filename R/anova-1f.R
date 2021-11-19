
generate_anova <- function(av.name = "",
                           factor.name = "", factor_level_names = NULL,
                           within = 3,
                           between = 4,
                           num.facs = 3,
                           num.obs.per.fac = 4,
                           overallmean = 5.5,
                           alpha = 0.05,
                           obs_round = 2,
                           obs_min = 0,
                           obs_max = 20
) {
  
  n <- num.facs*num.obs.per.fac
  
  if (any(is.null(factor_level_names))) {
    factor_level_names <- paste0("a",1:num.facs)
  }
  
  
  grp_means <- rnorm(n = num.facs,overallmean,between)
  dat <- round( rnorm(n=n, rep(grp_means, each=num.obs.per.fac), within), obs_round)
  dat <- pmax(dat, obs_min)
  dat <- pmin(dat, obs_max)
  dat <- data.frame(av=dat, grps=rep(1:num.facs,each=num.obs.per.fac),pids=rep(1:num.obs.per.fac,num.facs))
  
  
  
  wdat<-pivot_wider(dat,names_from = 2, values_from=1)  
  cmns <- round(colMeans(wdat[,-1]), 2)
  xm <- round(mean(dat$av),2)
  
  qs_tot <- round(
    sum(
      round((dat$av-xm)^2,2)
    )
    ,2)
  qs_btw <- num.obs.per.fac * round(
    sum(
      round((cmns-xm)^2,2)
    )
    ,2)
  qs_wth <- qs_tot - qs_btw
  
  
  
  
  ## run the actual ANOVA (1 factorial
  #print(anova(lm(av~factor(grps), data=dat)))
  
  J <- num.facs
  n <- num.obs.per.fac*num.facs
  
  df_btw <- J-1
  df_wth <- n-J
  df_tot <- df_btw + df_wth
  
  mqs_tot <- round( qs_tot / df_tot, 2)
  mqs_btw <- round( qs_btw / df_btw, 2)
  mqs_wth <- round( qs_wth / df_wth, 2)
  
  Fval <- round( mqs_btw / mqs_wth, 2)
  
  Fp <- round( df(Fval, df_btw, df_wth), 2)
  Fcrit <- round(qf(1-alpha, df_btw, df_wth),2)
  
  eta2 <- round(qs_btw/(qs_tot),2)
  
  factor_means=cmns
  nz=num.obs.per.fac
  grand_mean=xm
  result <- list(qs_tot=qs_tot, qs_btw=qs_btw, qs_wth=qs_wth, df_tot=df_tot,df_btw=df_btw, df_wth=df_wth, mqs_tot=mqs_tot, mqs_btw=mqs_btw, mqs_wth=mqs_wth, Fval=Fval, Fp=Fp,Fcrit=Fcrit,eta2=eta2, factor_means=factor_means, nz=nz, grand_mean=grand_mean, dat=dat, alpha=alpha,J=J,
                 factor_level_names=factor_level_names)
  return(result)
}

qs_solution <- function(x, id) {
  if (isFALSE(include_solution)) return(empty_str)
  strsol = paste0( "QS_{tot} = ",paste0("(",x$dat$av,"-",x$grand_mean,")^2",collapse=" + \\\\"),"=\\\\",
                   paste( round((x$dat$av-x$grand_mean)^2,2),collapse=" + " ),"= \\\\",
                   x$qs_tot
  )
  
  strsol2 = paste0("QS_{zw} = ",x$nz,"\\cdot[", paste0("(",x$factor_means,"-",x$grand_mean, ")^2",collapse=" + \\\\" ), "]=",
                   paste( round((x$factor_means-x$grand_mean)^2,2),collapse="+" ),"= \\\\",
                   x$qs_btw
  )
  #cat(strsol)
  
  #strsol3 = paste0("QS_{inn} = ",paste0( "(",x$dat$av,"-",x$grand_mean, ")^2",collapse=" +" ),
  #                 paste( round((x$factor_means-x$grand_mean)^2),collapse="+" ),"=",
  #                 round(sum(x$factor_means-x$grand_mean)^2,2)
  #                 )
  
  strsol3 = paste0("QS_{inn} = QS_{tot}-QS_{zw}=",x$qs_tot,"-",x$qs_btw,"=", x$qs_wth)
  
  #paste(strsol, strsol2, strsol3, collapse="\newline")
  if (id==1) return(strsol)
  else if (id==2) return (strsol2)
  else if (id==3) return (strsol3)
  else return("Unbekannte Quadratsumme angefordert!");
}

mqs_solution <- function(x) {
  if (isFALSE(include_solution)) return(empty_str)
  paste( "\\\\",
         paste0("MQS_{zw} = \\frac{ ", x$qs_btw, "}{",x$df_btw,"}=",x$mqs_btw),"\\\\",
         paste0("MQS_{inn} = \\frac{ ", x$qs_wth, "}{",x$df_wth,"}=",x$mqs_wth)
  )
}

f_solution <- function(x) {
  if (isFALSE(include_solution)) return(empty_str)
  paste0("F=\\frac{",x$mqs_btw,"}{",x$mqs_wth,"}=",x$Fval)
}

hypothesis_solution <- function(x) {
  if (isFALSE(include_solution)) return(empty_str)
  paste("H_0: ", paste(paste0("\\mu_",1:x$J), collapse="=" )  )
}


result_table <- function(x) {
  report_table <- data.frame(Quelle=c("Zwischen","Innerhalb","Total"),
                             QS=c(x$qs_tot, x$qs_btw, x$qs_wth), 
                             df=c(x$df_tot, x$df_btw, x$df_wth),
                             MQS=c(x$mqs_tot, x$mqs_btw, NA),
                             F = c(x$Fval,NA,NA),
                             p = c(x$Fp, NA, NA),
                             eta2=c(x$eta2,NA,NA))
  
  knitr::kable(report_table)
}

data_table <- function(x) {
  
  wdat<-pivot_wider(x$dat,names_from = 2, values_from=1)  
  knitr::kable(wdat[,-1],col.names = x$factor_level_names)
}


f_crit <- function(x) {
  if (isFALSE(include_solution)) return(empty_str)
  paste0("Der kritische Wert einer *F*-Verteilung mit ",x$df_btw, " Zählerfreiheitsgraden und ", x$df_wth, " Nennerfreiheitsgraden und einem Signifikanzniveau von ",x$alpha*100,"% ist ", x$Fcrit,".")
  
}

effect_solution <- function(x) {
  if (isFALSE(include_solution)) return(empty_str)
  paste0("\\hat{\\eta}^2=\\frac{",x$qs_btw,"}{",x$qs_tot,"}=",x$eta2)
  
}

group_means_solution <- function(x) {
  if (isFALSE(include_solution)) return(empty_str)
  wdat<-pivot_wider(x$dat,names_from = 2, values_from=1)  
  strlist <- ""
  for (j in 1:x$J) {
    strlist <-paste0(strlist, paste0("\\bar{x}_",j,"=\\frac{", paste0(simplify2array(wdat[,j+1]),collapse="+") ,"}{",x$nz,"}=", x$factor_means[j]  ),"\\\\")
  }
  
  strlist <- paste( strlist, "\\\\",
                    "\\bar{x}=",x$grand_mean)
  
  strlist
}

contrast <- function(x, ctr) {
  ln <- length(ctr)
  cntr <- paste0("\\Lambda=",paste0(ctr,"\\cdot \\mu_", 1:ln) )
  
  
}

orthogonal <- function(ctr1, ctr2) {
  result = round(sum(ctr1*ctr2),2)
  paste0( 
    paste0( ctr1,"\\cdot",ctr2,collapse="+"),"=",result
    )
}
