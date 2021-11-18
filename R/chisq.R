generate_chisq <- function(labels.a=c("Antidepressivum","Placebo"), labels.b=c("ja","nein"),
                           tbl = matrix(c(25,33,15,7),ncol=2), alpha=0.05 ) {
  
  df <- data.frame(tbl)
  rownames(df) <- labels.a
  colnames(df) <- labels.b
  
  nrows = nrow(df)
  ncols = ncol(df)
  
  data_raw <- data.frame(df)
  
  rsums <- rowSums(df)
  df$c <- rsums
  csums <- colSums(df)
  total <- csums[length(csums)]
  df <- rbind(df, csums)
  rownames(df)[nrow(df)] <- ""  
  names(df)[ncol(df)] <- ""
  
  data_sums <- data.frame(df)
  
  n21<-data_raw[2,1] 
  n12<-data_raw[1,2]
  
  for (i in 1:nrows) {
    for (j in 1:ncols) {
      df[i,j] <- rsums[i]*csums[j]/total
    }
  }

  
  data_indep <- data.frame(df)
  
  nij <- simplify(data_raw)
  eij <- simplify(data_indep[1:nrows,1:ncols])
  
  chisq <- round(sum((nij-eij)^2/eij),2)
  
  df <- (ncols-1)*(nrows-1)
  
  list(labels.a=labels.a, labels.b=labels.b,tbl=tbl,data_raw=data_raw, 
       data_sums=data_sums, data_indep=data_indep, nrows=nrows, ncols=ncols,alpha=alpha,
       n12=n12,n21=n21, nij=nij, eij=eij, chisq=chisq, df=df)
}

chisq_data_table <- function(x, type="",caption="") {
  
  # if (with_sums) {
  #   if (expected) {
  #     knitr::kable(x$data_indep, caption=caption)   
  #   } else {
  #     knitr::kable( x$data_raw, caption=caption )
  #   }
  # } else {
  #   if (expected) {
  #     knitr::kable(x$data_indep, caption=caption)   
  #   } else {
  #     knitr::kable( x$df, caption=caption )
  #   }
  # }
  if (type=="") {
    if (caption=="") caption="Kontingenztabelle"
    knitr::kable( x$data_raw, caption=caption, col.names = x$labels.b )
  } else if (type=="indep") {
    if (caption=="") caption="Kontingenztabelle unter Unabhängigkeitsannahme"
    knitr::kable(x$data_indep, caption=caption)   
  } else if (type=="sums") {
    if (caption=="") caption="Kontingenztabelle mit Randsummen"
    knitr::kable( x$data_sums, caption=caption )
  } else {
    print("")
  }
  
}

mcnemar_solution <- function(x) {

 n12 <- x$n12
 n21 <- x$n21
 chisq = round( (n21-n12)^2/(n12+n21), 2)
 
 
 paste0("\\chi^2=",
        
          "\\frac{(",n12,"-",n21,")^2}{",n12,"+",n21,"}"
        ,
        "=",chisq,""
 )
}

mcnemar_effectsize <- function(x, type=1) {
  chance <- round(x$n12/x$n21,2)
  frac <- round((x$n12/(x$n12+x$n21)),2)*100

   if (type==1)
   rs<-paste0("\\frac{",x$n12,"}{",x$n21,"}=", chance,"")
  if (type==2)
   rs<-paste0("\\frac{",x$n12,"}{",(x$n12+x$n21),"}=",frac,"%")
  
  (rs)
}

chisq_solution <- function(x) {
  
  nij <- x$nij
  eij <- x$eij
  
  chisq <- x$chisq
  
  paste0("\\chi^2=",
         paste0(
           
           "\\frac{(",nij,"-",eij,")^2}{",eij,"}", collapse="+"
           
         ),"=",chisq
  )
  
}

chisq_crit <- function(x, undirected=TRUE) {
  df <- x$df
  if (!undirected) alpha <- x$alpha*2
  else alpha = x$alpha
  chisqcrt <- round(qchisq(1-alpha, df),2)
  cmp <- x$chisq >= chisqcrt
  
  paste0("Der kritische Wert einer $\\chi^2$-Verteilung mit ",df," Freiheitsgrad(en) ist ",
 # ifelse(undirected,"",paste0("(gerichtet) $\\chi^2_krit(",1-alpha,";",df,")$")), chisqcrt,".")
  ifelse(undirected,"","(gerichtet) "), chisqcrt,".")

}

chisq_decision <- function(x) {
  df <- (x$ncols-1)*(x$nrows-1)
  chisqcrt <- round(qchisq(1-x$alpha, df),2)
  cmp <- x$chisq >= chisqcrt
  
  paste0("Der empirische Wert der Prüfgröße ist ",
         ifelse(cmp,"größer/gleich","kleiner")
         ," als der kritische Wert. ", ifelse(cmp,"Wir lehnen die Nullhypothese ab.",
                                              "Wir können die Nullhypothese nicht ablehnen."))
}

# ---- # ----

generate_chisq_uni <- function(obs, exp_relative, labels, alpha=0.05) {
  
  n <- sum(obs)
  exp <- exp_relative*n/100
  terms <- round( (obs-exp)^2/exp, 2)
  
  chisq <- round(sum(terms),2)
  
  df <- length(obs)-1
  
 
  
  list(obs=obs, exp=exp, terms=terms, df=df,n=n, 
       labels=labels,exp_relative=exp_relative, chisq=chisq, alpha=alpha)
}

solution_chisq_uni <- function(x)
{
  part1 <- paste0(
    " e_{",x$label,"}=",x$exp_relative,"\\cdot",x$n,"=",x$exp,
    collapse="\\\\ "
  )
  
  part2 <- paste0("\\chi^2=",
         paste0(
           
           "\\frac{(",x$obs,"-",x$exp,")^2}{",x$exp,"}", collapse="+"
           
         ),"=",x$chisq
  )
  
 # paste( "\\begin{align}",part1,"\\end{align}","\\\\", part2, collapse=" \\n ")
  paste0(part1,"\\\\",part2)
}

chisq_effectsize <- function(x) {
  omega <- round( sqrt(x$chisq/x$n) , 2)
  
  paste0("\\omega=\\sqrt{\\frac{",x$chisq,"}{",x$n,"}}=",omega)
}

chisq_uni_data_table <- function(x)
{
  knitr::kable(t(x$obs),col.names = x$labels)
}