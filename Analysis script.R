# Analysis script 
# 1. Merging datasets 2, 4, 5 and 8 


ESS02rec <- import("D:/R Projects/Multivariate Analysis Project/ESS02 Recoded.dta")
ESS04rec <- import("D:/R Projects/Multivariate Analysis Project/ESS04 Recoded.dta")


ESS05rec <- import("D:/R Projects/Multivariate Analysis Project/ESS05 Recoded.dta")

recoded_data <- rbind(ESS02rec, ESS04rec, ESS05rec)
save(both_keywords_dataset, file = "both_keywords_fulldataset.RData")

summary(ESS02rec)
