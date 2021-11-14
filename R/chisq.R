generate_chisq <- function(labels.a=c("Antidepressivum","Placebo"), labels.b=c("ja","nein"),
                           tbl = matrix(c(25,33,15,7),ncol=2), alpha=0.05 ) {
  
  df <- data.frame(tbl)
  rownames(df) <- labels.a
  colnames(df) <- labels.b
  
  nrows = nrow(df)
  ncols = ncol(df)
  
  df_raw <- data.frame(df)
  
  rsums <- rowSums(df)
  df$c <- rsums
  csums <- colSums(df)
  total <- csums[length(csums)]
  df <- rbind(df, csums)
  rownames(df)[nrow(df)] <- ""  
  colnames(df)[ncol(df)] <- ""
  
  df_sums <- data.frame(df)
  
  
  
  for (i in 1:nrows) {
    for (j in 1:ncols) {
      df[i,j] <- rsums[i]*csums[j]/total
    }
  }
  
  
  df_indep <- data.frame(df)
  
  list(labels.a=labels.a, labels.b=labels.b,tbl=tbl,df_raw=df_raw, df_sums=df_sums, df_indep=df_indep, nrows=nrows, ncols=ncols,alpha=alpha)
}

chisq_data_table <- function(x, with_sums=FALSE, expected=FALSE) {
  
  if (with_sums) {
    if (expected) {
      knitr::kable(x$df_indep)   
    } else {
      knitr::kable( x$df_raw )
    }
  } else {
    if (expected) {
      knitr::kable(x$df_indep)   
    } else {
      knitr::kable( x$df_sums )
    }
  }
  
}

chisq_solution <- function(x) {
  
  nij <- simplify(x$df_raw)
  eij <- simplify(x$df_indep[1:x$nrows,1:x$ncols])
  
  chisq <- round(sum((nij-eij)^2/eij),2)
  
  paste0("\\chi^2=",
         paste0(
           
           "\\frac{(",nij,"-",eij,")^2}{",eij,"}", collapse="+"
           
         ),"=",chisq
  )
  
}

chisq_crit <- function(x) {
  df <- (x$ncols-1)*(x$nrows-1)
  chisqcrt <- round(qchisq(1-x$alpha, df),2)
  paste0("Der kritische Wert einer $\\chi^2$-Verteilung mit ",df," Freiheitsgrad(en) ist ",chisqcrt)
}

# ---- # ----

generate_chisq_uni <- function(obs, exp, labels) {
  
  terms <- round( (obs-exp)^2/exp, 2)
  
  chi2 <- round(sum(terms),2)
  
  df <- length(obs)-1
  
  n <- sum(obs)
  
  list(obs=obs, exp=exp, terms=terms, chi2=chi2, df=df,n=n, labels=labels)
}

solution_chisq_uni <- function(x)
{
  paste0("\\chi^2=",
         paste0(
           
           "\\frac{(",x$obs,"-",x$exp,")^2}{",x$exp,"}", collapse="+"
           
         ),"=",x$chisq
  )
  
}

chisq_uni_data_table <- function(x)
{
  knitr::kable(x$obs)
}