# placeholder
convert_to_latex <- function(x){x}

generate_exams_question <- function(text, responses, correct_response=1, exname="no name", extype="schoice") {
  
  exsolution <- rep(0, length(responses))
  exsolution[correct_response] <- 1
  exsolution <- paste0(exsolution, sep = "", collapse = "")
  
  rsp <- ""
  for (j in 1:length(responses)) {
    rsp <- paste0(rsp, "\\\\item ", responses[j], "\n")
  }

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