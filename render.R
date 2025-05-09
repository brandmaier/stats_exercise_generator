require("kableExtra")
require("datasauRus")

options(encoding = 'UTF-8')
#options(knitr.table.format = "latex")

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

rmarkdown::render(input="AB9-ALM.Rmd", 
                  output_file=paste0(output_path,"AB-ALM.pdf"), 
                  params=list(include_solution=FALSE))
rmarkdown::render(input="AB9-ALM.Rmd", 
                  output_file=paste0(output_path,"AB-ALM-mit-Lösung.pdf"), 
                  params=list(include_solution=TRUE))



rmarkdown::render(input="AB5-Regression.Rmd", 
                  output_file=paste0(output_path,"AB-Regression.pdf"), 
                  params=list(include_solution=FALSE))
rmarkdown::render(input="AB5-Regression.Rmd", 
                  output_file=paste0(output_path,"AB-Regression-mit-Lösung.pdf"), 
                  params=list(include_solution=TRUE))

rmarkdown::render(input="AB3-Teststaerke.Rmd", 
                  output_file=paste0(output_path,"AB-Teststärke.pdf"), 
                  params=list(include_solution=FALSE))
rmarkdown::render(input="AB3-Teststaerke.Rmd", 
                  output_file=paste0(output_path,"AB-Teststärke-mit-Lösung.pdf"), 
                  params=list(include_solution=TRUE))
