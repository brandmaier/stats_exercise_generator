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
  
  p <- factor_a_num_levels <- length(factor.a.levels)
  q <- factor_b_num_levels <- length(factor.b.levels)
  
  av<-rep(cellmeans_by_row,each=nz)+rnorm(nz*length(cellmeans_by_row),0,noise_sd)
  av <- round(av, obs_round)
  av <- pmax(av, obs_min)
  av <- pmin(av, obs_max)
  
  xm = mean(av)
 
  
  dat <- data.frame(av, a = rep(factor.a.levels, each=nz*q), b=factor.b.levels)
  
  factor_a_means <- (dat %>% group_by(a) %>% summarise(avm=mean(av)))$avm
  factor_b_means <- (dat %>% group_by(b) %>% summarise(bvm=mean(av)))$bvm
  
  num.obs.per.fac = nz
  
  cmns = cellmeans_by_row
  
  qs_tot <- round(
    sum(
      round((dat$av-xm)^2,2)
    )
    ,2)
  qs_A <- q* nz * round(
    sum(
      round((factor_a_means-xm)^2,2)
    )
    ,2)
  qs_B <- p* nz * round(
    sum(
      round((factor_b_means-xm)^2,2)
    )
    ,2)
  
  cms <- rep(cellmeans_by_row,each=nz)
  ams <- rep(factor_a_means, each=nz*q)
  bms <- rep(factor_b_means, nz*p)
  
  qs_AxB <- round(sum( (cms+xm-ams-bms   )^2 ), 2)
  qs_inn <-  round( 
    sum((rep(cellmeans_by_row,each=nz)-av)^2) 
    , 2)
  
  df_tot <- p*q*nz - 1
  df_A <- p-1
  df_B <- q-1
  df_AxB <- (p-1)*(q-1)
  df_inn <- p*q*(nz-1)
  
  mqs_tot <- round( qs_tot / df_tot, 2)
  mqs_A <- round( qs_A / df_A, 2)
  mqs_B <- round( qs_B / df_B, 2)
  mqs_inn <- round( qs_inn / df_inn, 2)
  mqs_AxB = round( qs_AxB / df_AxB, 2)
  Fval_A <- round( mqs_A / mqs_inn, 2)
  Fval_B <- round( mqs_B / mqs_inn, 2)
  Fval_AxB <- round( mqs_AxB / mqs_inn, 2)
#  Fp <- round( df(Fval, df_btw, df_wth), 2)
#  Fcrit <- round(qf(1-alpha, df_btw, df_wth),2)
  
  return(list(qs_tot=qs_tot, dat=dat, df_tot=df_tot, df_A=df_A, 
              df_B=df_B, df_AxB=df_AxB, df_inn = df_inn,
              qs_A = qs_A,
              qs_B = qs_B,
              qs_AxB = qs_AxB,
              qs_tot = qs_tot,
              factor_a_means = factor_a_means,
              factor_b_means = factor_b_means,
              mqs_tot = mqs_tot,
              mqs_A = mqs_A,
              mqs_B = mqs_B,
              mqs_inn = mqs_inn,
              mqs_AxB=mqs_AxB,
              grand_mean=xm,
              p=p,
              q=q,
              Fval_A = Fval_A,
              Fval_B = Fval_B,
              Fval_AxB = Fval_AxB,
              av=av))
}

anova_in_R <- function(x) {
  anova(lm(av~a+b+a:b, x$dat))
}

data_plot <- function(x) {
#  ggplot(dat, aes(x=interaction(a,b),y=av))+geom_violin()+geom_boxplot(width=0.1)+geom_jitter(width=.01)+ggx::gg_("wrap labels on x-axis")
 gp<- ggplot(x$dat, aes(x=1,y=av))+geom_violin()+geom_boxplot(width=0.1)+geom_jitter(width=.01)+facet_wrap(~a+b)
 return(gp)
}

solution_anova2 <- function(x) {
  
  strsol = paste0("QS_{A} = ",x$nz,"\\cdot[", paste0("(",x$factor_a_means,"-",x$grand_mean, ")^2",collapse=" + \\\\" ), "]=",
                   paste( round((x$factor_a_means-x$grand_mean)^2,2),collapse="+" ),"= \\\\",
                   x$qs_A)

  strsol2 = paste0("QS_{B} = ",x$nz,"\\cdot[", paste0("(",x$factor_b_means,"-",x$grand_mean, ")^2",collapse=" + \\\\" ), "]=",
                   paste( round((x$factor_b_means-x$grand_mean)^2,2),collapse="+" ),"= \\\\",
                   x$qs_B)  
  
  strsol3 = paste0("QS_{inn} = ",x$nz,"\\cdot[", paste0("(",rep(x$factor_means,x$nz),"-",x$av, ")^2",collapse=" + \\\\" ), "]=",
                 #  paste( round((x$factor_a_means-x$grand_mean)^2,2),collapse="+" ),"= \\\\",
                   x$qs_inn)
  
  ret <- paste0(
    strsol,
    strsol2,
    strsol3,
    collapse="\\\\\n"
  )
  
  return(ret)
                   
}

f_crit <- function(x, type="A") {
  
  if (type=="A") {
    
  } else {
    df1 = NA
    df2 = NA
  }
  
  paste0("Der kritische Wert einer *F*-Verteilung mit ",df1, " Zählerfreiheitsgraden und ", df2, " Nennerfreiheitsgraden und einem Signifikanzniveau von ",x$alpha*100,"% ist ", x$Fcrit,".")
  
}

long_data_table <- function(x) {
  knitr::kable(x$dat)
}


table_of_means <- function(x) {
xxx<-x$dat %>% group_by(a,b) %>% summarise(m=mean(av)) %>% add_column(id=1:(x$p*x$q))
xxx<-xxx %>% pivot_wider(names_from=a, values_from=m,id_cols=b)

xxx<-xxx %>% mutate(rowMeans=rowMeans(select(.,2:3)))

rbind(xxx, 
      c(b=NA,xxx %>% select(where(is.numeric)) %>% colMeans()))

xxx
}

show_table_of_means <- function(x) {
  knitr::kable(table_of_means(x))
}

solution_df <- function(x) {
  paste0("df_A=",x$p,"-1=",x$df_A,"\\\\",
  "df_B=",x$q,"-1=",x$df_B,"\\\\",
  "df_{AxB}=(",x$p,"-1)\\cdot(",x$q,"-1)=",x$df_AxB,
  "df_{inn}=",x$n,"-(",x$p,"\\cdot",x$q,")=",x$df_inn)
}

solution_mqs <- function(x) {
  
  paste( "\\\\",
         paste0("MQS_{A} = \\frac{ ", x$qs_A, "}{",x$df_A,"}=",x$mqs_A),"\\\\",
         paste0("MQS_{B} = \\frac{ ", x$qs_B, "}{",x$df_B,"}=",x$mqs_B),"\\\\",
         paste0("MQS_{AxB} = \\frac{ ", x$qs_AxB, "}{",x$df_AxB,"}=",x$mqs_AxB)
  )
}

solution_f <- function(x) {
  paste0(
    
  paste0("F_A=\\frac{",x$mqs_A,"}{",x$mqs_inn,"}=",x$Fval_A),"\\\\",
  paste0("F_B=\\frac{",x$mqs_B,"}{",x$mqs_inn,"}=",x$Fval_B),"\\\\",
  paste0("F_{AxB}=\\frac{",x$mqs_AxB,"}{",x$mqs_inn,"}=",x$Fval_AxB),"\\\\"
  
  )
}

result_table <- function(x) {
  report_table <- data.frame(Quelle=c("A","B","AxB"),
                             QS=c(x$qs_A, x$qs_B, x$qs_AxB), 
                             df=c(x$df_A, x$df_B, x$df_AxB),
                             MQS=c(x$mqs_A, x$mqs_B, x$mqs_AxB))
                             #F = c(x$Fval,NA,NA),
                             #p = c(x$Fp, NA, NA),
                             #eta2=c(x$eta2,NA,NA))
  
  knitr::kable(report_table)
}

aov <- generate_anova_2f(av.name="Symptome",
                  factor.a.levels = c("Psychotherapie","Psychopharmaka"),
                  factor.a.name = "Therapie", factor.b.name = "Diagnose",
                  factor.b.levels = c("Depression","Angststörung"),
                  obs_min = 0, obs_max = 20, nz = 5, cellmeans_by_row = c(10,8,8,10))


