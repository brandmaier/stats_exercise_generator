#' @export
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
                           obs_max = 20,
                           rawdata = NULL
) {
  
  if (!is.null(factor_level_names)) {
    stopifnot(length(factor_level_names)==num.facs)
  }
  

  
  if (any(is.null(factor_level_names))) {
    factor_level_names <- paste0("a",1:num.facs)
  }


  if (!is.null(rawdata)) {
    wdat <- data.frame(pids=1:nrow(rawdata),rawdata)
    names(wdat)[2:(ncol(wdat))]<-factor_level_names
    num.facs <- ncol(wdat)-1
    num.obs.per.fac <- nrow(wdat)
    n <- prod(dim(wdat))
    dat <- pivot_longer(wdat,-pids)
    names(dat) <- c("pids","grps","av")
  
  } else {
    n <- num.facs*num.obs.per.fac
    if (length(between)==1) {
      grp_means <- rnorm(n = num.facs,overallmean,between)
    } else {
      grp_means <- between
    }
    dat <- round( rnorm(n=n, rep(grp_means, each=num.obs.per.fac), within), obs_round)
    dat <- pmax(dat, obs_min)
    dat <- pmin(dat, obs_max)
    dat <- data.frame(av=dat, 
                      grps=rep(1:num.facs,each=num.obs.per.fac),
                      pids=rep(1:num.obs.per.fac,num.facs))
    
    grp_means <- colMeans(dat)
    
    wdat<-pivot_wider(dat,names_from = 2, values_from=1)  

  }
  
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
  
  df1 <- J-1
  df2 <- n-J
  df_tot <- df1 + df2
  
  mqs_tot <- round( qs_tot / df_tot, 2)
  mqs_btw <- round( qs_btw / df1, 2)
  mqs_wth <- round( qs_wth / df2, 2)
  
  Fval <- round( mqs_btw / mqs_wth, 2)
  
  Fp <- round( 1-pf(Fval, df1, df2), 2)
  Fcrit <- round(qf(1-alpha, df1, df2),2)
  
  eta2 <- round(qs_btw/(qs_tot),2)
  
  omega2 <- round((qs_btw-(J-1)*mqs_wth)/(qs_tot+mqs_wth) , 2)
  
  factor_means=cmns
  nz=num.obs.per.fac
  grand_mean=xm
  result <- list(qs_tot=qs_tot, qs_btw=qs_btw, qs_wth=qs_wth, df_tot=df_tot,df1=df1, df2=df2, mqs_tot=mqs_tot, mqs_btw=mqs_btw, mqs_wth=mqs_wth, Fval=Fval, Fp=Fp,Fcrit=Fcrit,eta2=eta2, factor_means=factor_means, nz=nz, grand_mean=grand_mean, dat=dat, alpha=alpha,J=J,
                 factor_level_names=factor_level_names, eta2=eta2, omega2=omega2)
  return(result)
}

#' @export
qs_solution <- function(x, id) {

  strsol = paste0( "QS_{tot} = ",paste0g("(",x$dat$av,"-",x$grand_mean,")^2")
                                      ,"=\\\\",
                   "",paste0g( round((x$dat$av-x$grand_mean)^2,2) ),"= \\\\",
                   x$qs_tot
  )
  
  strsol2 = paste0("QS_{zw} = ",x$nz,"\\cdot[", paste0("(",x$factor_means,"-",x$grand_mean, ")^2",collapse=" + \\\\" ), "]=",
                   x$nz,"\\cdot[", paste( round((x$factor_means-x$grand_mean)^2,2),collapse="+" ),"]= \\\\",
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

  paste( "\\\\",
         paste0("MQS_{zw} = \\frac{ ", x$qs_btw, "}{",x$df1,"}=",x$mqs_btw),"\\\\",
         paste0("MQS_{inn} = \\frac{ ", x$qs_wth, "}{",x$df2,"}=",x$mqs_wth)
  )
}

f_solution <- function(x) {

  paste0("F=\\frac{",x$mqs_btw,"}{",x$mqs_wth,"}=",x$Fval)
}

hypothesis_solution <- function(x) {

  paste("H_0: ", paste(paste0("\\mu_",1:x$J), collapse="=" )  )
}

hypothesis_solution_h1 <- function(x) {
  
  paste("H_1: ", paste(paste0("\\mu_i \\ne \\mu_j für ein i \\in \\{1\\ldots",x$J,"\\}"), collapse="=" )  )
}


result_table <- function(x) {
  report_table <- data.frame(Quelle=c("Zwischen","Innerhalb","Total"),
                             QS=c(x$qs_tot, x$qs_btw, x$qs_wth), 
                             df=c(x$df_tot, x$df1, x$df2),
                             MQS=c(x$mqs_tot, x$mqs_btw, NA),
                             F = c(x$Fval,NA,NA),
                             p = c(x$Fp, NA, NA),
                             eta2=c(x$eta2,NA,NA))
  
  knitr::kable(report_table) 
}

data_table <- function(x, ...) {
  
  wdat<-pivot_wider(x$dat,names_from = 2, values_from=1)  
  knitr::kable(wdat[,-1],col.names = x$factor_level_names, ...)
}


f_crit <- function(x) {
  
  paste0("Der kritische Wert einer *F*-Verteilung mit ",x$df1, " Zählerfreiheitsgraden und ", x$df2, " Nennerfreiheitsgraden und einem Signifikanzniveau von ",x$alpha*100,"% ist ", x$Fcrit,".")
  
}

effect_solution <- function(x) {
 
  paste0("\\hat{\\eta}^2=\\frac{",x$qs_btw,"}{",x$qs_tot,"}=",x$eta2)
  
}

group_means_solution <- function(x) {

  wdat<-pivot_wider(x$dat,names_from = grps, values_from=av)  
  strlist <- ""
  for (j in 1:x$J) {
    strlist <-paste0(strlist, paste0("\\bar{x}_",j,"=\\frac{", paste0(simplify2array(wdat[,j+1]),collapse="+") ,"}{",x$nz,"}=", x$factor_means[j]  ),"\\\\")
  }
  
  strlist <- paste( strlist, "\\\\",
                    "\\bar{x}=",x$grand_mean)
  
  strlist
}

#' @export
contrast_qs <- function(x) {
  ctr <- x$ctr
  ln <- length(ctr)
  cntr <- paste0("$$\\Lambda=",paste0(ctr,"\\cdot \\mu_", 1:ln,collapse="+") , "$$")
  
 
  
  cntr2 <- paste0("$$L=",paste0(ctr,"\\cdot",x$aov$factor_means, collapse="+"), "=",x$L,"$$")
  
 
      
  cntr3 <- paste0("$$QS_{Kontrast}=\\frac{L^2}{\\frac{1}{n_z} \\cdot \\sum^J_{j=1}{K^2_j}}=\\frac{",
                  x$L2,"}{",x$ksum,"}=",x$qs_kontrast,"$$")
  
  paste0(cntr, cntr2, cntr3, collapse="\n")
  
}

#' @export
orthogonal <- function(ctr1, ctr2) {
  result = round(sum(ctr1$ctr*ctr2$ctr),2)
  paste0( "Test auf Orthogonalität:\n$$",
    paste0( ctr1$ctr,"\\cdot",ctr2$ctr,collapse="+"),"=",result,
    "$$\nDas Ergebnis ist ",result," => Die Kontraste sind ",ifelse(result==0,"","nicht "),"orthogonal"
    
    )
}

#' @export
contrast_hypotheses <- function(x, direction) {
  if (direction == 0) {
    cmpH0 <- "="
    cmpH1 <- "\\ne"
  } else if (direction>0) {
    cmpH1 <- ">"
    cmpH0 <- "\\le"
  } else {
    cmpH1 <- "<"
    cmpH0 <- "\\ge"
  }
  paste0("$$H_0: \\Lambda ", cmpH0, "0 \\mathrm{\\; und\\; } H_1: \\Lambda", cmpH1," 0$$")
}

#' @export
generate_contrast <- function(x, ctr, alpha=0.05, directed=FALSE) {

  if (directed) alpha = alpha*2
  
  L <- round( sum(ctr * x$factor_means),2 )
  L2 <- round(L*L,2)
  k2sum <- sum(ctr*ctr)
  ksum<- round( 1/x$nz*(k2sum),2 )
  
  qs_kontrast <- round(L2/ksum,2)
  
  Fest <- round(qs_kontrast/x$mqs_wth, 2)

  df1 <- 1
  df2 <- x$df2
  Fcrit <- round(qf(1-alpha, df1, df2),2)
  
  return(list(ctr=ctr,L=L,L2=L2,qs_kontrast=qs_kontrast,Fest=Fest, 
              aov=x,ksum=ksum,df1=df1,df2=df2,Fcrit=Fcrit,alpha=alpha))
}

#' @export
contrast_F <- function(x) {
  #
  paste0("$$F=\\frac{MQS_{Kontrast}}{MQS_{inn}}=\\frac{",x$qs_kontrast,"}{",x$aov$mqs_wth,"}=",x$Fest,"$$")
}