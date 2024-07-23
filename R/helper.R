
#' @export
wrap_math_inline <- function(x){cat(paste0("$",x,"$"))}
#' @export
wrap_math_block <- function(x){cat(paste0("$$",x,"$$"))}
#' @export
wrap_math_align <- function(x) {cat(paste0("\\begin{align*}\n",x,"\n\\end{align*}"))} 
#' @export
wrap_math_align_numbered <- function(x) {cat(paste0("\\begin{align}\n",x,"\n\\end{align}"))} 

#' @export
wrap_math_split <- function(x) { cat(paste0("\\begin{equation}\\begin{split}",x,"\\end{split}\\end{equation}"))}

#' @export
wrap_multline <- function(x) { cat(paste0("\\begin{multline}",x,"\\end{multline}"))}

#' @export
wrap_math_flalign <-  function(x) {cat(paste0("\\begin{flalign*}\n",x,"\\end{flalign*}"))} 

#' @export
wrap_test <- function(x){ cat("\\[ ",x,"\\]")}

solution_header <- function() {
  cat("_Lösung_:\n\n")
}

#' @export
paste0g <- function(..., collapse="+") {
  x <- paste0(...)
  stopifnot(collapse=="+")
  collapse = "+ \\\\"
  collapse_within = "+"
    grps <- split(x, ceiling(seq_along(x)/5))
  temp <- sapply(grps, function(x){paste0(x,collapse=collapse_within)})
  paste0(temp, collapse=collapse)
}

#' @export
wrap_p <- function(p, tex_math = TRUE) {
  if (is.na(p))return(NA)
  if (p<0.001){
    if (tex_math)
      return ("$<0.001$")
    else
      return("<0.001")
    }else{ return(round(p,3))}
}
