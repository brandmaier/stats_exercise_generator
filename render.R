#
# render all work sheets
#

rmarkdown::render(input="AB2-Kontraste.Rmd", 
                  output_file="AB2-Kontraste.pdf", 
                  params=list(include_solution=FALSE))
rmarkdown::render(input="AB2-Kontraste.Rmd", 
                  output_file="AB2-Kontraste-mit-Lösung.pdf", 
                  params=list(include_solution=TRUE))

