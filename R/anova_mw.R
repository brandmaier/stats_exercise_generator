#' @export
#' 
generate_anova <- function(av.name = "",
                           factor.name = "",
                           factor_level_names = NULL,
                           between_person_variance = 1,
                           within = 2,
                           between = 4,
                           num.reps = 3,
                           num.facs = 3,
                           num.obs.per.fac = 4,
                           overallmean = 5.5,
                           alpha = 0.05,
                           obs_round = 2,
                           obs_min = 0,
                           obs_max = 20) {
  n <- num.facs * num.obs.per.fac
  
  
  if (length(between) == 1) {
    grp_means <- rnorm(n = num.facs, overallmean, between)
  } else {
    grp_means <- between
  }
  person_icepts <- rnorm(n = n, 0, between_person_variance)
  dat <- round(rnorm(n = n * num.reps, rep(grp_means, each = num.obs.per.fac), within) +
                 rep(person_icepts, each = num.reps),
               obs_round)
  dat <- pmax(dat, obs_min)
  dat <- pmin(dat, obs_max)
  dat <- data.frame(
    av = dat,
    # dependent variable
    grps = rep(1:num.facs, each = num.obs.per.fac * num.reps),
    pids = rep(1:num.obs.per.fac, num.facs, each = num.reps),
    # person ID
    trial = rep(1:num.reps, n)
  )
  
  grp_means <- colMeans(dat)
  
  wdat <- tidyr::pivot_wider(dat, names_from = 2, values_from = 1)
  
  ret <- list(grp_means = grp_means, dat = dat)
}

ao = generate_anova()

ao
