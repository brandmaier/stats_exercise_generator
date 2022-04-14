options(encoding = 'UTF-8')

output_path = "publish/"

#
# render all work sheets
#
rmarkdown::render(input="AB1-Varianzanalyse-einfaktoriell.Rmd", 
                  output_file=paste0(output_path,"AB1-Varianzanalyse-einfaktoriell.pdf"), 
                  params=list(include_solution=FALSE))
rmarkdown::render(input="AB1-Varianzanalyse-einfaktoriell.Rmd", 
                  output_file=paste0(output_path,"AB1-Varianzanalyse-einfaktoriell-mit_Lösung.pdf"), 
                  params=list(include_solution=TRUE))


rmarkdown::render(input="AB2-Kontraste.Rmd", 
                  output_file=paste0(output_path,"AB2-Kontraste.pdf"), 
                  params=list(include_solution=FALSE))
rmarkdown::render(input="AB2-Kontraste.Rmd", 
                  output_file=paste0(output_path,"AB2-Kontraste-mit-Lösung.pdf"), 
                  params=list(include_solution=TRUE))

rmarkdown::render(input="AB-ChiQuadrat.Rmd", 
                  output_file=paste0(output_path,"AB-ChiQuadrat.pdf"), 
                  params=list(include_solution=FALSE))
rmarkdown::render(input="AB-ChiQuadrat.Rmd", 
                  output_file=paste0(output_path,"AB-ChiQuadrat-mit-Lösung.pdf"), 
                  params=list(include_solution=TRUE))


rmarkdown::render(input="AB4-Hauptkomponentenanalyse.Rmd", 
                  output_file=paste0(output_path,"AB4-Hauptkomponentenanalyse.pdf"), 
                  params=list(include_solution=FALSE))
rmarkdown::render(input="AB4-Hauptkomponentenanalyse.Rmd", 
                  output_file=paste0(output_path,"AB4-Hauptkomponentenanalyse-mit-Lösung.pdf"), 
                  params=list(include_solution=TRUE))

rmarkdown::render(input="AB-Korrelation.Rmd", 
                  output_file=paste0(output_path,"AB-Korrelation.pdf"), 
                  params=list(include_solution=FALSE))
rmarkdown::render(input="AB-Korrelation.Rmd", 
                  output_file=paste0(output_path,"AB-Korrelation-mit-Lösung.pdf"), 
                  params=list(include_solution=TRUE))

