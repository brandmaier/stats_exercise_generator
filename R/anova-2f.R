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
  
  av<-rep(cellmeans,each=nz)+rnorm(nz*length(cellmeans_by_row),0,noise_sd)
  av <- round(av, obs_round)
  dat <- pmax(dat, obs_min)
  dat <- pmin(dat, obs_max)
  
  dat <- data.frame(av, a = rep)
  
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
  
}