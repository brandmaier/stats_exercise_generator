
wrap_math_inline <- function(x){cat(paste0("$",x,"$"))}
wrap_math_block <- function(x){cat(paste0("$$",x,"$$"))}
wrap_math_align <- function(x) {cat(paste0("\\begin{align}\n",x,"\n\\end{align}"))}


solution_header <- function() {
  cat("_Lösung_:\n\n")
}