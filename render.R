options(encoding = 'UTF-8')

#
# render all work sheets
#
rmarkdown::render(input="AB1-Varianzanalyse-einfaktoriell.Rmd", 
                  output_file="AB1-Varianzanalyse-einfaktoriell.pdf", 
                  params=list(include_solution=FALSE))
rmarkdown::render(input="AB1-Varianzanalyse-einfaktoriell.Rmd", 
                  output_file="AB1-Varianzanalyse-einfaktoriell-mit_Lösung.pdf", 
                  params=list(include_solution=TRUE))


rmarkdown::render(input="AB2-Kontraste.Rmd", 
                  output_file="AB2-Kontraste.pdf", 
                  params=list(include_solution=FALSE))
rmarkdown::render(input="AB2-Kontraste.Rmd", 
                  output_file="AB2-Kontraste-mit-Lösung.pdf", 
                  params=list(include_solution=TRUE))

rmarkdown::render(input="AB3-ChiQuadrat.Rmd", 
                  output_file="AB3-ChiQuadrat.pdf", 
                  params=list(include_solution=FALSE))
rmarkdown::render(input="AB3-ChiQuadrat.Rmd", 
                  output_file="AB3-ChiQuadrat-mit-Lösung.pdf", 
                  params=list(include_solution=TRUE))


rmarkdown::render(input="AB4-Hauptkomponentenanalyse.Rmd", 
                  output_file="AB4-Hauptkomponentenanalyse.pdf", 
                  params=list(include_solution=FALSE))
rmarkdown::render(input="AB4-Hauptkomponentenanalyse.Rmd", 
                  output_file="AB4-Hauptkomponentenanalyse-mit-Lösung.pdf", 
                  params=list(include_solution=TRUE))

rmarkdown::render(input="AB6-Korrelation.Rmd", 
                  output_file="AB6-Korrelation.pdf", 
                  params=list(include_solution=FALSE))
rmarkdown::render(input="AB6-Korrelation.Rmd", 
                  output_file="AB6-Korrelation-mit-Lösung.pdf", 
                  params=list(include_solution=TRUE))

