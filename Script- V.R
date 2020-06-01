# Valentina's script 
# Dataset: ESS Cumulative (Rounds 1-7)

rm(list = ls())

#packages we need for data management 
library(foreign)
library(dplyr)

# 1. Importing cumulative datset 
ESSCumul <- read.dta("C:/Users/valen/Google Drive/Università Statale/1st year/Multivariate analysis/ESS Datasets/ESS1-7/ESS1-8e01.dta")

#Selecting needed variables
ESS0107 <- select(ESSCumul, essround, cntry, freehms, gndr, rlgdgr,
                  rlgatnd, pray, rlgblg, rlgdnm, agea, marsts, edulvlb, hinctnta,
                  domicil, impenv, chldhhe,imwbcnt, imueclt, iscoco, isco08, mnrgtjb)

class(ESS0107$cntry)
#cntry var: transformed as factor (categorical)
ESS0107$cntry<- as.factor(ESS0107$cntry)
summary(ESS0107$cntry)

#recoding variable age in six categories 
ESS0107$AGE6cat <- ifelse(ESS0107$agea < 25,
                          "15-24",
                          ifelse(ESS0107$agea < 35,
                                 "25-34",
                                 ifelse(ESS0107$agea < 45,
                                        "35-44",
                                        ifelse(ESS0107$agea < 55,
                                               "45-54",
                                               ifelse(ESS0107$agea < 65,
                                                      "55-64",
                                                      ifelse(ESS0107$agea < 75,
                                                             "65-74",
                                                             "75+"))))
                                 
                          )
)

ESS0107$AGE6cat <- factor(ESS0107$AGE6cat, ordered = TRUE)

summary(ESS0107$AGE6cat)


