# specify cellmeans by row means, pick factor level a1, then list all
# factor levels of b
#' @export
generate_anova_2f <- function(av.name = "",
                              factor.a.name = "A",
                              factor.b.name = "B",
                              factor.a.levels = c("a1", "a2", "a3"),
                              factor.b.levels = c("b1", "b2"),
                              cellmeans_by_row = c(1, 2,
                                                   3, 4,
                                                   1, 7),
                              noise_sd = 3,
                              nz = 20,
                              obs_min = NULL,
                              obs_max = NULL,
                              obs_round = 2,
                              av = NULL,
                              alpha = 0.05,
                              extreme_rounding = FALSE,
                              ssq_round = 2)
{
  raw_data_given <- !is.null(av)
  
  qs_a <- 0
  qs_b <- 0
  qs_ab <- 0
  qs_inn <- 0
  qs_total <- 0
  
  p <- factor_a_num_levels <- length(factor.a.levels)
  q <- factor_b_num_levels <- length(factor.b.levels)
  
  n <- nz * p * q
  
  # if no raw data is given, generate data from given cell means
  if (!raw_data_given) {
    av <- rep(cellmeans_by_row, each = nz) + rnorm(nz * p * q, 0, noise_sd)
    av <- round(av, obs_round)
    if (!is.null(obs_min)) av <- pmax(av, obs_min)
    if (!is.null(obs_max)) av <- pmin(av, obs_max)
  }
  
  # create a data frame in long format
  # raw data is expected in this order:
  # A1/B1, A1/B2, A1/B3, ... A1/Bn, A2/B1, A2/B2, A2/B3, .... A2/Bn, ....., An/B1,...
  dat <-
    data.frame(
      av,
      a = rep(factor.a.levels, each = nz * q),
      b = rep(factor.b.levels, each = nz)
    )
  
  # cellmeans should be computed both if data are given
  # or simulated (because clipping can occur due to rounding)
  # so that given cell means actually do not represent empirical
  # means
#  if (raw_data_given) {
    cellmeans_by_row <- dat %>% group_by(a, b) %>% summarise(mean(av), .groups="keep")
    cellmeans_by_row <- round(cellmeans_by_row[, 3, drop = TRUE], 2)
#  }
  xm = round(mean(av), ssq_round)
  
  #browser()
  
  
  factor_a_means <-
    round((dat %>% group_by(a) %>% summarise(avm = mean(av)))$avm , 2)
  factor_b_means <-
    round((dat %>% group_by(b) %>% summarise(bvm = mean(av)))$bvm , 2)
  
  num.obs.per.fac = nz
  
  
  if (extreme_rounding) {
    ex_round <- round
  } else {
    ex_round <- function(x, ...) {
      x
    } # identity function
  }
  

  qs_A <- q * nz * round(sum(ex_round((
    factor_a_means - xm
  ) ^ 2, ssq_round))
  , ssq_round)
  qs_B <- p * nz * round(sum(ex_round((
    factor_b_means - xm
  ) ^ 2, ssq_round))
  , ssq_round)
  
  
  
  cms <- rep(cellmeans_by_row, each = nz)
  ams <- rep(factor_a_means, each = nz * q)
  bms <- rep(factor_b_means, each = nz, times = p)
  
  #  browser()
  
  # just as sanity check
  qs_tot <- round(sum(ex_round((dat$av - xm) ^ 2, ssq_round))
                  , ssq_round)
#  qs_tot <- round(sum((xm - av) ^ 2), ssq_round)
  
  
  #qs_AxB <- round(sum( (cms-ams-bms+xm   )^2 ), 2)
  qs_AxB <- round(nz * sum((
    cellmeans_by_row + xm -
      rep(factor_a_means, each = q) - rep(factor_b_means, p)
  ) ^ 2), ssq_round)
  
  qs_inn <-  round(sum((cms - av) ^ 2)
                   , ssq_round)
  
  
  
  df_tot <- p * q * nz - 1
  df_A <- p - 1
  df_B <- q - 1
  df_AxB <- (p - 1) * (q - 1)
  df_inn <- p * q * (nz - 1)
  
  mqs_tot <- round(qs_tot / df_tot, 2)
  mqs_A <- round(qs_A / df_A, 2)
  mqs_B <- round(qs_B / df_B, 2)
  mqs_inn <- round(qs_inn / df_inn, 2)
  mqs_AxB = round(qs_AxB / df_AxB, 2)
  Fval_A <- round(mqs_A / mqs_inn, 2)
  Fval_B <- round(mqs_B / mqs_inn, 2)
  Fval_AxB <- round(mqs_AxB / mqs_inn, 2)
  #  Fp <- round( df(Fval, df_btw, df_wth), 2)
  #  Fcrit <- round(qf(1-alpha, df_btw, df_wth),2)
  
  p_A <- 1 - pf(Fval_A, df_A, df_inn)
  p_B <- 1 - pf(Fval_B, df_B, df_inn)
  p_AxB <- 1 - pf(Fval_AxB, df_AxB, df_inn)
  
  return(
    list(
      qs_tot = qs_tot,
      dat = dat,
      df_tot = df_tot,
      df_A = df_A,
      df_B = df_B,
      df_AxB = df_AxB,
      df_inn = df_inn,
      qs_A = qs_A,
      qs_B = qs_B,
      qs_AxB = qs_AxB,
      qs_tot = qs_tot,
      qs_inn = qs_inn,
      factor_a_means = factor_a_means,
      factor_b_means = factor_b_means,
      mqs_tot = mqs_tot,
      mqs_A = mqs_A,
      mqs_B = mqs_B,
      mqs_inn = mqs_inn,
      mqs_AxB = mqs_AxB,
      grand_mean = xm,
      p = p,
      q = q,
      Fval_A = Fval_A,
      Fval_B = Fval_B,
      Fval_AxB = Fval_AxB,
      p_A = p_A,
      p_B = p_B,
      p_AxB = p_AxB,
      av = av,
      av.name = av.name,
      factor.a.name = factor.a.name,
      factor.b.name = factor.b.name,
      alpha = alpha,
      n = n
    )
  )
}

#' @export
anova_in_R <- function(x) {
  anova(lm(av ~ a + b + a:b, x$dat))
}

#' @export
data_plot <- function(x) {
  #  ggplot(dat, aes(x=interaction(a,b),y=av))+geom_violin()+geom_boxplot(width=0.1)+geom_jitter(width=.01)+ggx::gg_("wrap labels on x-axis")
  gp <- ggplot(x$dat, aes(x = 1, y = av)) +
    geom_boxplot(width = 0.1) + geom_jitter(width = .01) +
    facet_wrap( ~ a + b) +
    ylab(x$av.name) + xlab("") +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  return(gp)
}

#' @export
interaction_plot <- function(x, empty = FALSE, gray = FALSE) {
  alpha <- ifelse(empty, 0, 1)
  
  g1 <-
    x$dat %>% group_by(a, b) %>% summarise(m = mean(av)) %>% ggplot(aes(
      x = a,
      group = b,
      y = m,
      color = b
    )) +
    geom_line(lwd = 1.5, alpha = alpha) + 
    ylab(x$av.name) + 
    theme(legend.position = "bottom") +
    xlab(x$factor.a.name) +  
    guides(color = guide_legend(title = ""))+
    NULL
  
  g2 <-
    x$dat %>% group_by(a, b) %>% summarise(m = mean(av)) %>% ggplot(aes(
      x = b,
      group = a,
      y = m,
      color = a
    )) +
    geom_line(lwd = 1.5, alpha = alpha) + 
    ylab(x$av.name) + 
    theme(legend.position = "bottom") +
    xlab(x$factor.b.name) + 
    guides(color = guide_legend(title = "")) +
    NULL
  
  if (gray) {
    g1 <-
      x$dat %>% group_by(a, b) %>% summarise(m = mean(av)) %>% ggplot(aes(
        x = a,
        group = b,
        y = m,
        color = b
      )) +
      geom_line(lwd = 1.5, alpha = alpha) + ylab(x$av.name) + theme(legend.position = "bottom") +
      xlab(x$factor.a.name) +  
      guides(color = guide_legend(title = "")) +
      scale_color_grey()
    
    g2 <-
      x$dat %>% group_by(a, b) %>% summarise(m = mean(av)) %>% ggplot(aes(
        x = b,
        group = a,
        y = m,
        color = a
      )) +
      geom_line(lwd = 1.5, alpha = alpha) + ylab(x$av.name) + theme(legend.position = "bottom") +
      xlab(x$factor.b.name) + 
      guides(color = guide_legend(title = "")) +
      scale_color_grey()
  }
  
  library(patchwork)
  
  return(g1 + g2)
}

#' @export
solution_anova2 <- function(x) {
  strsol = paste0(
    "QS_{A} = ",
    x$nz,
    "\\cdot[",
    paste0("(", x$factor_a_means, "-", x$grand_mean, ")^2", collapse = " + \\\\"),
    "]=",
    paste(round((
      x$factor_a_means - x$grand_mean
    ) ^ 2, 2), collapse = "+"),
    "= \\\\",
    x$qs_A
  )
  
  strsol2 = paste0(
    "QS_{B} = ",
    x$nz,
    "\\cdot[",
    paste0("(", x$factor_b_means, "-", x$grand_mean, ")^2", collapse = " + \\\\"),
    "]=",
    paste(round((
      x$factor_b_means - x$grand_mean
    ) ^ 2, 2), collapse = "+"),
    "= \\\\",
    x$qs_B
  )
  
  strsol3 = paste0(
    "QS_{inn} = ",
    x$nz,
    "\\cdot[",
    paste0("(", rep(x$factor_means, x$nz), "-", x$av, ")^2", collapse = " + \\\\"),
    "]=",
    #  paste( round((x$factor_a_means-x$grand_mean)^2,2),collapse="+" ),"= \\\\",
    x$qs_inn
  )
  
  ret <- paste0(strsol,
                strsol2,
                strsol3,
                collapse = "\\\\\n")
  
  return(ret)
  
}

#' @export
f_crit <- function(x, type = "A") {
  alpha <- x$alpha
  
  if (type == "A") {
    df1 = x$df_A
    df2 = x$df_inn
    
  } else if (type == "B") {
    df1 = x$df_B
    df2 = x$df_inn
  } else if (type == "AxB") {
    df1 = x$df_AxB
    df2 = x$df_inn
  } else {
    df1 = NA
    df2 = NA
  }
  
  fc =  round(qf(1 - alpha, df1, df2), 2)
  
  paste0(
    "Der kritische Wert einer *F*-Verteilung mit ",
    df1,
    " Zählerfreiheitsgraden und ",
    df2,
    " Nennerfreiheitsgraden und einem Signifikanzniveau von ",
    x$alpha * 100,
    "% ist ",
    fc,
    "."
  )
  
}

#' @export
long_data_table <- function(x) {
  knitr::kable(x$dat)
}


#' @export
table_of_means <- function(x) {
  temporary_data <-
    x$dat %>% group_by(a, b) %>% summarise(m = mean(av)) %>% add_column(id =
                                                                          1:(x$p * x$q))
  temporary_data <- temporary_data %>% pivot_wider(names_from = a,
                                                   values_from = m,
                                                   id_cols = b)
  
  temporary_data <-
    temporary_data %>% mutate(rowMeans = round(rowMeans(select(., 2:3)), 2))
  
  temporary_data <- rbind(temporary_data,
                          c(b = "", round(
                            temporary_data %>% select(where(is.numeric)) %>% colMeans(), 2
                          )))
  
  colnames(temporary_data)[1] <- ""
  colnames(temporary_data)[ncol(temporary_data)] <- ""
  
  temporary_data
}

show_table_of_means <- function(x, booktabs=TRUE) {
  knitr::kable(table_of_means(x), booktabs=booktabs)
}

#' @export
solution_df <- function(x) {
  paste0(
    "df_A=",
    x$p,
    "-1=",
    x$df_A,
    "\\\\",
    "df_B=",
    x$q,
    "-1=",
    x$df_B,
    "\\\\",
    "df_{AxB}=(",
    x$p,
    "-1)\\cdot(",
    x$q,
    "-1)=",
    x$df_AxB,
    "\\\\",
    "df_{inn}=",
    x$n,
    "-(",
    x$p,
    "\\cdot",
    x$q,
    ")=",
    x$df_inn
  )
}

solution_mqs <- function(x) {
  paste(
    "\\\\",
    paste0("MQS_{A} = \\frac{ ", x$qs_A, "}{", x$df_A, "}=", x$mqs_A),
    "\\\\",
    paste0("MQS_{B} = \\frac{ ", x$qs_B, "}{", x$df_B, "}=", x$mqs_B),
    "\\\\",
    paste0(
      "MQS_{AxB} = \\frac{ ",
      x$qs_AxB,
      "}{",
      x$df_AxB,
      "}=",
      x$mqs_AxB
    )
  )
}

#' @export
solution_f <- function(x) {
  paste0(
    paste0("F_A=\\frac{", x$mqs_A, "}{", x$mqs_inn, "}=", x$Fval_A),
    "\\\\",
    paste0("F_B=\\frac{", x$mqs_B, "}{", x$mqs_inn, "}=", x$Fval_B),
    "\\\\",
    paste0(
      "F_{AxB}=\\frac{",
      x$mqs_AxB,
      "}{",
      x$mqs_inn,
      "}=",
      x$Fval_AxB
    ),
    "\\\\"
    
  )
}

#' @export
result_table <- function(x, booktabs = TRUE) {
  report_table <-
    data.frame(
      Quelle = c(
        x$factor.a.name ,
        x$factor.b.name,
        paste0(x$factor.a.name, " x ", x$factor.b.name),
        "Fehler"
      ),
      QS = c(x$qs_A, x$qs_B, x$qs_AxB, x$qs_inn),
      df = c(x$df_A, x$df_B, x$df_AxB, x$df_inn),
      MQS = c(x$mqs_A, x$mqs_B, x$mqs_AxB, x$mqs_inn),
      F = c(x$Fval_A, x$Fval_B, x$Fval_AxB, ""),
      p = c(wrap_p(x$p_A), wrap_p(x$p_B), wrap_p(x$p_AxB,tex_math=FALSE), "")
     #p = c(x$p_A, x$p_B, x$p_AxB, "")
    )
  #eta2=c(x$eta2,NA,NA))
  
  knitr::kable(report_table, booktabs=booktabs)
}

#' @export
pie_plot <- function(x) {
  qs_data <-
    data.frame(
      QS = c("QS_A", "QS_B", "QS_AxB" , "QS_inn"),
      value = c(x$qs_A, x$qs_B, x$qs_AxB, x$qs_inn)
    )
  qs_data %>% ggplot(aes(y = value, x = "", fill = QS)) + geom_col(color =
                                                                     "black") + coord_polar(theta = "y") +
    scale_fill_discrete() + theme_void() +
    # geom_text(aes(label = QS),
    #            position = position_stack(vjust = 0.5))
    NULL
}

#' @export
solution_partial_eta2 <- function(x) {
  eta2p_A <- round(x$qs_A / (x$qs_A + x$qs_inn), 2)
  eta2p_B <- round(x$qs_B / (x$qs_B + x$qs_inn), 2)
  eta2p_AxB <- round(x$qs_AxB / (x$qs_AxB + x$qs_inn), 2)
  
  ret <- paste0(
    paste0(
      "\\hat{\\eta}^2_{p,A}=\\frac{",
      x$qs_A,
      "}{",
      x$qs_A,
      "+",
      x$qs_inn,
      "}=",
      eta2p_A,
      ""
    ),
    "\\\\ \n",
    paste0(
      "\\hat{\\eta}^2_{p,B}=\\frac{",
      x$qs_B,
      "}{",
      x$qs_B,
      "+",
      x$qs_inn,
      "}=",
      eta2p_B,
      ""
    ),
    "\\\\ \n",
    paste0(
      "\\hat{\\eta}^2_{p,AxB}=\\frac{",
      x$qs_AxB,
      "}{",
      x$qs_AxB,
      "+",
      x$qs_inn,
      "}=",
      eta2p_AxB,
      ""
    ),
    sep = "",
    collapse = ""
  )
  
  return(ret)
}

#' @export
distractors_FvalA <- function(x) {
  d1 <- x$mqs_A / x$mqs_AxB
  d2 <- x$mqs_A / x$mqs_tot
  d3 <- x$df_AxB / x$mqs_inn
  return(c(x$Fval_A, d1, d2, d3))
}

distractors_partial_eta2_A <- function(x) {
  eta2p_A <- round(x$qs_A / (x$qs_A + x$qs_inn), 3)
  d1 <- round(x$qs_A / (x$qs_tot), 3)
  d2 <- round(x$qs_A / (x$qs_A + x$qs_AxB + x$qs_inn), 3)
  d3 <-  round(x$qs_AxB / (x$qs_AxB + x$qs_inn), 3)
  d4 <-  round(x$qs_A / (x$qs_inn), 3)
  
  return(c(eta2p_A, d1, d2, d3, d4))
}
