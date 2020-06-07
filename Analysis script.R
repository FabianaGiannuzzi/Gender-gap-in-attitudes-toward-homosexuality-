# Analysis script 
# 1. Merging datasets 2, 4, 5 and 8 
library(rio)
rm(list = ls())


ESS02rec <- import("D:/R Projects/Multivariate Analysis Project/ESS02 Recoded.dta")
ESS04rec <- import("D:/R Projects/Multivariate Analysis Project/ESS04 Recoded.dta")
ESS05rec <- import("D:/R Projects/Multivariate Analysis Project/ESS05 Recoded.dta")

recoded_data <- rbind(ESS02rec, ESS04rec, ESS05rec)

export(recoded_data, "ESS02-05 Recoded.dta")