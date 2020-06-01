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

#recode level of education
summary(ESS0107$edulvlb)

ESS0107 <- mutate(
  ESS0107,
  EDUCATION = recode(edulvlb,
                     "Not completed ISCED level 1" = "Low level",
                     "ISCED 1, completed primary education" = "Low level",
                     "Vocational ISCED 2C < 2 years, no access ISCED 3" = "Low level",
                     "General/pre-vocational ISCED 2A/2B, access ISCED 3 vocational" = "Low level",
                     "General ISCED 2A, access ISCED 3A general/all 3" = "Low level",
                     "Vocational ISCED 2C >= 2 years, no access ISCED 3" = "Low level",
                     "Vocational ISCED 2A/2B, access ISCED 3 vocational"  = "Low level",
                     "Vocational ISCED 2, access ISCED 3 general/all"= "Low level",
                     "Vocational ISCED 3C < 2 years, no access ISCED 5"= "Medium level",
                     "General ISCED 3 >=2 years, no access ISCED 5"= "Medium level",
                     "General ISCED 3A/3B, access ISCED 5B/lower tier 5A"= "Medium level",
                     "General ISCED 3A, access upper tier ISCED 5A/all 5"= "Medium level",
                     "Vocational ISCED 3C >= 2 years, no access ISCED 5"= "Medium level",
                     "Vocational ISCED 3A, access ISCED 5B/ lower tier 5A"= "Medium level",
                     "Vocational ISCED 3A, access upper tier ISCED 5A/all 5"= "Medium level",
                     "General ISCED 4A/4B, access ISCED 5B/lower tier 5A" = "Medium level",
                     "General ISCED 4A, access upper tier ISCED 5A/all 5" = "Medium level",
                     "ISCED 4 programmes without access ISCED 5" = "Medium level",
                     "Vocational ISCED 4A/4B, access ISCED 5B/lower tier 5A" = "Medium level",
                     "Vocational ISCED 4A, access upper tier ISCED 5A/all 5" = "Medium level",
                     "ISCED 5A short, intermediate/academic/general tertiary below bachelor" = "High level",
                     "ISCED 5B short, advanced vocational qualifications" = "High level",
                     "ISCED 5A medium, bachelor/equivalent from lower tier tertiary" = "High level",
                     "ISCED 5A medium, bachelor/equivalent from upper/single tier tertiary" = "High level",
                     "ISCED 5A long, master/equivalent from lower tier tertiary" = "High level",
                     "ISCED 5A long, master/equivalent from upper/single tier tertiary" = "High level",
                     "ISCED 6, doctoral degree" = "High level"))

summary(ESS0107$EDUCATION)

ESS0107$EDUCATION <- factor(ESS0107$EDUCATION, ordered = TRUE)

summary(ESS0107)
