library(tidyverse)
source("R/helper.R")
source("R/anova-2f.R")

myaov <- generate_anova_2f(av = rnorm(600,c(rep(c(1,2,3),200)),1),factor.a.levels = c("A1","A2"),factor.b.levels=c("B1","B2","B3"),nz=100)

show_table_of_means(myaov)

result_table(myaov)

anova_in_R(myaov)

# 
# A1
#
#
myaov2 <- generate_anova_2f(noise_sd = 1,cellmeans_by_row = c(1,2,3,
                                                              40,5,6),factor.a.levels = c("A1","A2"),
                            factor.b.levels=c("B1","B2","B3"),nz=5,obs_max = 100)

result_table(myaov2)

anova_in_R(myaov2)

###
# myaov3:
#
# mean(A1) should be 2
# mean(A2) should be 5
# mean(B1) should be 2.5
# mean(B2) should be 3.5
# 4.5
#

myaov3 <- generate_anova_2f(noise_sd = 1.91,cellmeans_by_row = c(1,2,3,
                                                                 4,5,6),factor.a.levels = c("A1","A2"),
                            factor.b.levels=c("B1","B2","B3"),nz=3)

result_table(myaov3)

anova_in_R(myaov3)

####

#aov <- generate_anova_2f(av.name="Symptome",
#                  factor.a.levels = c("Psychotherapie","Psychopharmaka"),
#                  factor.a.name = "Therapie", factor.b.name = "Diagnose",
#                  factor.b.levels = c("Depression","Angststörung"),
#                  obs_min = 0, obs_max = 20, nz = 5, cellmeans_by_row = c(10,8,8,10))

# first row is a1, then list as columns all b1, b2,..
# second row is a2
aov <- generate_anova_2f( av = c(
  1,2,38,4, 7,6,7,16,
  5,6,7,8, 18,18,16,17
), factor.a.levels = c("a1","a2"), factor.b.levels=c("b1", "b2"), nz=4)

anova_in_R(aov)
result_table(aov)

aov2 <- generate_anova_2f(obs_round = 
                            1)
anova_in_R(aov2)
result_table(aov2)

rslt_lm <- anova_in_R(aov2)
testthat::expect_equal(rslt_lm$`Sum Sq`[1], aov2$qs_A)

## --
set.seed(234)
myaov <- generate_anova_2f(av = rnorm(24,c(rep(c(1,2,3),8)),1),
                           factor.a.levels = c("A1","A2"),
                           factor.b.levels=c("B1","B2","B3"),nz=4)

show_table_of_means(myaov)

result_table(myaov)

anova_in_R(myaov)
