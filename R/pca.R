generate_pca <- function(item_names, data=NULL, loadings=NULL, nfactors=2) {
  
 if (!is.null(data)) {
   psych::fa(subs, nfactors=2,fm="pa")
 }
  
 x <- list(item_names = item_names, data = data, loadings=loadings, nfactors = nfactors)
 
 return(x)
}


solution_communality <- function(x) {
  ll <- x$loadings
  coms <- round(colSums(ll^2),2)
  
  solstr <- ""
  for (i in 1:nrow(ll)) {
    
    lrow <- ll[i,]
    for (j in 1:length(lrow)) {
      if (lrow[j]<0) lrow[j]=paste0("(",(lrow[j]),")")
    }
    solstr<-paste0(solstr, paste0(lrow,"^2",collapse="+"),"=",coms[i],"\n")
  }
  
  return(solstr)

}

solution_ev <- function(x) {
  ll <- x$loadings
  coms <- round(rowSums(ll^2),2)
  
  solstr <- ""
  for (i in 1:ncol(ll)) {
    
    lcol <- ll[,i]
    for (j in 1:length(lcol)) {
      if (lcol[j]<0) lcol[j]=paste0("(",(lcol[j]),")")
    }
    solstr<-paste0(solstr, paste0(lcol,"^2",collapse="+"),"=",coms[i],"\n")
  }
  
  return(solstr)
  
}

#communality <- round( rowSums(df^2), 2)
#ev <- round( colSums(df^2), 2)
#
#nvar <- ncol(subs)
#r2 <- round( ev/nvar*100,2)


communality_with_distractors <- function(x, i) {
  
  # correct
  ll <- x$loadings
  coms <- round(rowSums(ll^2)[i],2)
  
  distractor1 <- round(rowSums(ll)[i],2)
  distractor2 <- round(colSums(ll^2)[i],2)
  distractor3 <- round(colSums(ll)[i],2)
  
  if (length(unique(coms, distractor1,distractor2,distractor3) != 4)) {
    stop("Non-unique solutions!")
  }
  
  c(coms, distractor1, distractor2, distractor3)
}