generate_anova_2f <- function(av.name = "",
                           factor.a.name = "",
                           factor.b.name = "",
                           factor.a.levels = c("a1","a2"),
                           factor.b.levels = c("b1","b2"),
                           cellmeans_by_row = c(1,2,3,4),
                           noise_sd = 3, nz = 20,
                           obs_min=0,
                           obs_max=20,
                           obs_round=2)
{
  
  
  qs_a <- 0
  qs_b <- 0
  qs_ab <- 0
  qs_inn <- 0
  qs_total <- 0
  
  factor_a_num_levels <- length(factor.a.levels)
  factor_b_num_levels <- length(factor.b.levels)
  
  av<-rep(cellmeans_by_row,each=nz)+rnorm(nz*length(cellmeans_by_row),0,noise_sd)
  av <- round(av, obs_round)
  av <- pmax(av, obs_min)
  av <- pmin(av, obs_max)
 
  
  dat <- data.frame(av, a = rep(factor.a.levels, each=nz), b=factor.b.levels)
  
  factor_a_means <- (dat %>% group_by(a) %>% summarise(avm=mean(av)))$avm
  factor_b_means <- (dat %>% group_by(b) %>% summarise(bvm=mean(av)))$bvm
  
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
  
  mqs_tot <- round( qs_tot / df_tot, 2)
  mqs_btw <- round( qs_btw / df_btw, 2)
  mqs_wth <- round( qs_wth / df_wth, 2)
  
  Fval <- round( mqs_btw / mqs_wth, 2)
  
  Fp <- round( df(Fval, df_btw, df_wth), 2)
  Fcrit <- round(qf(1-alpha, df_btw, df_wth),2)
  
  return(list(qs_tot=qs_tot, dat=dat))
}

interaction_diagram <- function(x) {
  
}

solution_anova2 <- function(x) {
  
  strsol2 = paste0("QS_{A} = ",x$nz,"\\cdot[", paste0("(",x$factor_means,"-",x$grand_mean, ")^2",collapse=" + \\\\" ), "]=",
                   paste( round((x$factor_means-x$grand_mean)^2,2),collapse="+" ),"= \\\\",
                   x$qs_btw
                   
}