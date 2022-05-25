
wrap_math_inline <- function(x){cat(paste0("$",x,"$"))}
wrap_math_block <- function(x){cat(paste0("$$",x,"$$"))}
wrap_math_align <- function(x) {cat(paste0("\\begin{align*}\n",x,"\n\\end{align*}"))} 
wrap_math_align_numbered <- function(x) {cat(paste0("\\begin{align}\n",x,"\n\\end{align}"))} 

wrap_math_split <- function(x) { cat(paste0("\\begin{equation}\\begin{split}",x,"\\end{split}\\end{equation}"))}

wrap_multline <- function(x) { cat(paste0("\\begin{multline}",x,"\\end{multline}"))}

wrap_math_flalign <-  function(x) {cat(paste0("\\begin{flalign*}\n",x,"\\end{flalign*}"))} 

wrap_test <- function(x){ cat("\\[ ",x,"\\]")}

solution_header <- function() {
  cat("_Lösung_:\n\n")
}

paste0g <- function(...) {
  x <- paste0(...)
  collapse = "+ \\\\"
  collapse_within = "+"
    grps <- split(x, ceiling(seq_along(x)/5))
  temp <- sapply(grps, function(x){paste0(x,collapse=collapse_within)})
  paste0(temp, collapse=collapse)
}

wrap_p <- function(p) {
  if (p<0.001){ return ("$<0.001$") }else{ return(round(p,3))}
}
