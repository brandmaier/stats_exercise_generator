# placeholder
convert_to_latex <- function(x){x}
convert_latex <- function(x){x}

#' @export
generate_exams_question <- function(text,
                                    responses,
                                    correct_response=1,
                                    exname="no name",
                                    extype="schoice",
                                    round_digits = 3,
                                    include_none_response = TRUE) {
  


  
  responses <- base::round(responses, round_digits)
  
  smallest_abs_diff <- abs(min(diff(sort(responses))))
  
  tolerance <- smallest_abs_diff/2
  if (tolerance > 1) {
    tolerance <- 1
  } else {
    ee <- ceiling(log10(1/tolerance))
    tolerance <- 1/(10^ee)
  }
  
  
  rsp <- ""
  for (j in 1:length(responses)) {
    rsp <- paste0(rsp, "\\\\item ", responses[j], "\n")
  }
  
  if (include_none_response) {
    rsp <- paste0(rsp, "\\\\item Keine der übrigen Lösung ist korrekt (Toleranz ",tolerance,  ")\n")
  }
  
  exsolution <- rep(0, length(responses))
  exsolution[correct_response] <- 1
  if (include_none_response)
    responses <- c(responses, 0)
  exsolution <- paste0(exsolution, sep = "", collapse = "")

file_template <- "
\\begin{question}
ITEM_TEXT

\\begin{answerlist}
ANSWERS
\\end{answerlist}
\\end{question}

\\exname{EXNAME}
\\extype{EXTYPE}
\\exsolution{EXSOLUTION}
\\exshuffle{EXSHUFFLE}
"
cur_file <- file_template
cur_file <-
  stringr::str_replace(cur_file, "ITEM_TEXT", convert_latex(text))
cur_file <-
  stringr::str_replace(cur_file, "EXSOLUTION", exsolution)
cur_file <- stringr::str_replace(cur_file, "ANSWERS", rsp)
cur_file <-
  stringr::str_replace(cur_file, "EXSHUFFLE", as.character(length(responses)))
cur_file <- stringr::str_replace(cur_file, "EXNAME", exname)
cur_file <- stringr::str_replace(cur_file, "EXTYPE", extype)

return(cur_file)
}