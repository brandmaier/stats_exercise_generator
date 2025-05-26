#' @export
generate_pca <-
  function(item_names,
           data = NULL,
           loadings = NULL,
           nfactors = 2) {
    if (!is.null(data)) {
#      fit <- psych::fa(data,
#                       nfactors = nfactors, fm = "pc", rotate="varimin")
      fit <- psych::principal(observed, nfactors=2, rotate="varimax", covar=FALSE)
      
      loadings <- loadings(fit)[,]
    }
    
    x <-
      list(
        item_names = item_names,
        data = data,
        loadings = loadings,
        nfactors = nfactors
      )
    
    class(x) <- "pca"
    
    return(x)
  }

communality <- function(x) {
  return(rowSums(x$loadings ^ 2))
}

eigenvectors <- function(x) {
  return(colSums(x$loadings ^ 2))
}

#' @export
solution_communality <- function(x) {
  
  stopifnot(inherits(x,"pca"))
  
  ll <- x$loadings
  coms <- round(communality(x), 2)
  
  solstr <- ""
  for (i in 1:nrow(ll)) {
    lrow <- ll[i, ]
    for (j in 1:length(lrow)) {
      if (lrow[j] < 0)
        lrow[j] = paste0("(", (lrow[j]), ")")
    }
    solstr <-
      paste0(solstr, paste0(lrow, "^2", collapse = "+"), "=", coms[i], "\n")
  }
  
  return(solstr)
  
}

#' @export
solution_ev <- function(x) {
  ll <- x$loadings
  coms <- round(rowSums(ll ^ 2), 2)
  
  solstr <- ""
  for (i in 1:ncol(ll)) {
    lcol <- ll[, i]
    for (j in 1:length(lcol)) {
      if (lcol[j] < 0)
        lcol[j] = paste0("(", (lcol[j]), ")")
    }
    solstr <-
      paste0(solstr, paste0(lcol, "^2", collapse = "+"), "=", coms[i], "\n")
  }
  
  return(solstr)
  
}

#communality <- round( rowSums(df^2), 2)
#ev <- round( colSums(df^2), 2)
#
#nvar <- ncol(subs)
#r2 <- round( ev/nvar*100,2)


#' @export
exam_communality_with_distractors <- function(x, i) {
  
  # correct
  ll <- x$loadings
  coms <- round(rowSums(ll ^ 2)[i], 2)
  
  distractor1 <- round(rowSums(ll)[i], 2)
  distractor2 <- round(colSums(ll ^ 2)[i], 2)
  distractor3 <- round(colSums(ll)[i], 2)
  
  if (any(duplicated(coms, distractor1, distractor2, distractor3))) {
    stop("Non-unique solutions in distractors! Choose different loading values. Solutions are: ",paste0(  
      c(coms, distractor1, distractor2, distractor3),collapse=", "))
  }
  
  c(coms, distractor1, distractor2, distractor3)
}

exam_explained_variance_with_distractors <- function(x, i)
{
  explained_variance <- colSums(x$loadings^2) / nrow(x$loadings)
  
  coms <- explained_variance[i]
  distractor1 <- colSums(x$loadings^2)[i]
  distractor2 <- colSums(x$loadings^2)[i]
  distractor3 <- colSums(x$loadings^2)  
  distractor3 <- rowSums(x$loadings^2) / nrow(x$loadings)  
  
  if (any(duplicated(coms, distractor1, distractor2, distractor3))) {
    stop("Non-unique solutions in distractors! Choose different loading values. Solutions are: ",paste0(  
      c(coms, distractor1, distractor2, distractor3),collapse=", "))
  }
  
  c(coms, distractor1, distractor2, distractor3)
}
