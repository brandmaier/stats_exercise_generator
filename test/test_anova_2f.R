library(tidyverse)
source("R/helper.R")
source("R/anova-2f.R")

myaov <- generate_anova_2f(av = rnorm(600),factor.a.levels = c("A1","A2"),factor.b.levels=c("B1","B2","B3"),nz=100)

show_table_of_means(myaov)

result_table(myaov)

anova_in_R(myaov)


myaov <- generate_anova_2f(av = rnorm(600),factor.a.levels = c("A1","A2"),factor.b.levels=c("B1","B2","B3"),nz=100)
