generate_pca <- function(item_names, data=NULL, loadings=NULL, nfactors=2) {
  
 if (!is.null(data)) {
   psych::fa(subs, nfactors=2,fm="pa")
 }
  
 x <- list(item_names = item_names, data = data, loadings=loadings, nfactors = nfactors)
 
 return(x)
}



pca1 <- generate_pca( c("Ich war als Kind oft in Dänemark im Urlaub",
"Ich mag Knödel/Klöße als Beilage",
"Eine steife Brise im Gesicht macht mich glücklich",
"Der Anblick von Bergen verursacht bei mir Heimweh",
"Ich lese gern"
), loadings=matrix(c(0.62,	-0.02, 0.21,
                     -0.62,	-0.04,	0.13,
                     0.58,	0.11,	-0.13,
                     -0.66,	0.33,	-0.01,
                     0.33,	0.41,	0.07),nrow=3,byrow=TRUE))

solution_communality <- function(x) {
  ll <- x$loadings
  coms <- round(colSums(ll),2)
  
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
  coms <- round(rowSums(ll),2)
  
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

solution_communality(pca1)

solution_ev(pca1)