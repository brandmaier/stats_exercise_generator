options(encoding = 'UTF-8')

output_path = "publish/"

if (!dir.exists(output_path))
  dir.create(output_path)

#
# render all work sheets
#
rmarkdown::render(input="AB1-Varianzanalyse-einfaktoriell.Rmd", 
                  output_file=paste0(output_path,"AB-Varianzanalyse-einfaktoriell.pdf"), 
                  params=list(include_solution=FALSE))
rmarkdown::render(input="AB1-Varianzanalyse-einfaktoriell.Rmd", 
                  output_file=paste0(output_path,"AB-Varianzanalyse-einfaktoriell-mit_Lösung.pdf"), 
                  params=list(include_solution=TRUE))

rmarkdown::render(input="AB7-Varianzanalyse-zweifaktoriell.Rmd", 
                  output_file=paste0(output_path,"AB-Varianzanalyse-zweifaktoriell.pdf"), 
                  params=list(include_solution=FALSE))
rmarkdown::render(input="AB7-Varianzanalyse-zweifaktoriell.Rmd", 
                  output_file=paste0(output_path,"AB-Varianzanalyse-zweifaktoriell-mit_Lösung.pdf"), 
                  params=list(include_solution=TRUE))


rmarkdown::render(input="AB2-Kontraste.Rmd", 
                  output_file=paste0(output_path,"AB-Kontraste.pdf"), 
                  params=list(include_solution=FALSE))
rmarkdown::render(input="AB2-Kontraste.Rmd", 
                  output_file=paste0(output_path,"AB-Kontraste-mit-Lösung.pdf"), 
                  params=list(include_solution=TRUE))

rmarkdown::render(input="AB-ChiQuadrat.Rmd", 
                  output_file=paste0(output_path,"AB-ChiQuadrat.pdf"), 
                  params=list(include_solution=FALSE))
rmarkdown::render(input="AB-ChiQuadrat.Rmd", 
                  output_file=paste0(output_path,"AB-ChiQuadrat-mit-Lösung.pdf"), 
                  params=list(include_solution=TRUE))


rmarkdown::render(input="AB4-Hauptkomponentenanalyse.Rmd", 
                  output_file=paste0(output_path,"AB-Hauptkomponentenanalyse.pdf"), 
                  params=list(include_solution=FALSE))
rmarkdown::render(input="AB4-Hauptkomponentenanalyse.Rmd", 
                  output_file=paste0(output_path,"AB-Hauptkomponentenanalyse-mit-Lösung.pdf"), 
                  params=list(include_solution=TRUE))

rmarkdown::render(input="AB-Korrelation.Rmd", 
                  output_file=paste0(output_path,"AB-Korrelation.pdf"), 
                  params=list(include_solution=FALSE))
rmarkdown::render(input="AB-Korrelation.Rmd", 
                  output_file=paste0(output_path,"AB-Korrelation-mit-Lösung.pdf"), 
                  params=list(include_solution=TRUE))

