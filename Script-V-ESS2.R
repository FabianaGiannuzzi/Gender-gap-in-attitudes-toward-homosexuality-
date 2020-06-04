# Valentina's script 
# Dataset: ESS Round 2

rm(list = ls())

#packages we need for data management 
library(foreign)
library(dplyr)

# 1. Importing cumulative datset 
ESS02 <- read.dta("D:/R Projects/ESS Datasets/ESS2/ESS2e03_6.dta")

View(ESS02$iscoco)
#Selecting needed variables
ESS02<- select(ESS02, essround, cntry, freehms, gndr, rlgdgr,
              rlgatnd, pray, rlgblg, rlgdnm, agea, marital, edulvla, hinctnt,
                  domicil, impenv, chldhhe,imwbcnt, imueclt, iscoco, mnrgtjb)

class(ESS02$cntry)
#cntry var: transformed as factor (categorical)
ESS02$cntry<- as.factor(ESS02$cntry)
summary(ESS02$cntry)

#selecting countries
ESS02 <- filter(
  ESS02,
  cntry %in% c("AT", "BE", "CH", "CZ", "DE", "EE", "FI", "FR", "GB", "HU", "IE", "IT", "NO", "PL", "NL", "SI")
)

summary(ESS02$cntry)


#recoding variable age in six categories 
ESS02$AGE6cat <- ifelse(ESS02$agea < 25,
                          "14-24",
                          ifelse(ESS02$agea < 35,
                                 "25-34",
                                 ifelse(ESS02$agea < 45,
                                        "35-44",
                                        ifelse(ESS02$agea < 55,
                                               "45-54",
                                               ifelse(ESS02$agea < 65,
                                                      "55-64",
                                                      ifelse(ESS02$agea < 75,
                                                             "65-74",
                                                             "75+"))))
                                 
                          )
)

ESS02$AGE6cat <- factor(ESS02$AGE6cat, ordered = TRUE)

summary(ESS02$AGE6cat)

#recode level of education
summary(ESS02$edulvla)

ESS02 <- mutate(
  ESS02,
  EDUCATION = recode(edulvla,
                     "Less than lower secondary education (ISCED 0-1)" = "Low level",
                     "Lower secondary education completed (ISCED 2)" = "Low level",
                     "Upper secondary education completed (ISCED 3)" = "Medium level",
                     "Post-secondary non-tertiary education completed (ISCED 4)" = "Medium level",
                     "Tertiary education completed (ISCED 5-6)" = "High level"))

summary(ESS02$EDUCATION)
ESS02$EDUCATION <- factor(ESS02$EDUCATION, ordered = TRUE)



ESS0107 <- import("ESS0107 Recoded.dta")

summary(ESS0107$isco08)
summary(ESS0107$iscoco)

#for occupation in the cumulative ESS we have iscoco for rounds 1-5 and isco08 for rounds 6-7
ESS02 <- mutate(
  ESS02,
  OCCUPATION = recode(iscoco,
                      "Managing directors and chief executives" = "Managers",
                      "Manufacturing managers" = "Managers",
                      "Retail and wholesale trade managers" = "Managers",
                      "Sales and marketing managers" = "Managers",
                      "Supply, distribution and related managers" = "Managers",
                      "Restaurant managers" = "Managers",
                      "Construction managers" = "Managers",
                      "Education managers" = "Managers",
                      "Finance and administration  managers" = "Managers",
                      "Professional services managers not elsewhere classified" = "Managers",
                      "Services managers not elsewhere classified" = "Managers",
                      "Secondary education teachers" = "Professionals",
                      "Nursing associate professionals" = "Professionals",
                      "Primary school teachers" = "Professionals",
                      "Nursing professionals" = "Professionals",
                      "Accountants" = "Professionals",
                      "Social work and counselling professionals" = "Professionals",
                      "Early childhood educators" = "Professionals",
                      "Teaching professionals" = "Professionals",
                      "Teaching professionals not elsewhere classified" = "Professionals",
                      "University and higher education teachers" = "Professionals",
                      "Management and organization analysts" = "Professionals",
                      "Software developers" = "Professionals",
                      "Policy administration professionals" = "Professionals",
                      "Vocational education teachers" = "Professionals",
                      "Technical and medical sales professionals (excluding ICT)" = "Professionals",
                      "Civil engineers" = "Professionals",
                      "Lawyers" = "Professionals",
                      "Advertising and marketing professionals" = "Professionals",
                      "Engineering professionals not elsewhere classified" = "Professionals",
                      "Mechanical engineers" = "Professionals",
                      "Financial and investment advisers" = "Professionals",
                      "Accounting associate professionals" = "Technicians and Associate Professionals",
                      "Social work associate professionals" = "Technicians and Associate Professionals",
                      "Administrative and executive secretaries" = "Technicians and Associate Professionals",
                      "Commercial sales representatives" = "Technicians and Associate Professionals",
                      "Manufacturing supervisors" = "Technicians and Associate Professionals",
                      "Office supervisors" = "Technicians and Associate Professionals",
                      "Insurance representatives" = "Technicians and Associate Professionals",
                      "Buyers" = "Technicians and Associate Professionals",
                      "Physical and engineering science technicians not elsewhere classified" = "Technicians and Associate Professionals",
                      "General office clerks " = "Clerical Support Workers",
                      "Accounting and bookkeeping clerks" = "Clerical Support Workers",
                      "Tellers and other counter clerk" = "Clerical Support Workers",
                      "Office clerks" = "Clerical Support Workers",
                      "Statistical and finance clerks"= "Clerical Support Workers",
                      "Secretaries (general)" = "Clerical Support Workers",
                      "Stock clerks" = "Clerical Support Workers",
                      "Mail carriers and sorting clerks" = "Clerical Support Workers",
                      "Bank tellers and related clerks" = "Clerical Support Workers",
                      "Receptionists (general)" = "Clerical Support Workers",
                      "Transport clerks" = "Clerical Support Workers",
                      "Shop sales assistants" = "Services and Sales Workers",
                      "Waiters" = "Services and Sales Workers",
                      "Cooks" = "Services and Sales Workers",
                      "Health care assistants" = "Services and Sales Workers",
                      "Child care workers" = "Services and Sales Workers",
                      "Home-based personal care workers" = "Services and Sales Workers",
                      "Hairdressers" = "Services and Sales Workers",
                      "Cashiers and ticket clerks" = "Services and Sales Workers",
                      "Shop keepers" = "Services and Sales Workers",
                      "Security guards" = "Services and Sales Workers",
                      "Shop supervisors" = "Services and Sales Workers",
                      "Teachers' aides" = "Services and Sales Workers",
                      "Beauticians and related workers" = "Services and Sales Workers",
                      "Police officers" = "Services and Sales Workers",
                      "Waiters, waitresses and bartenders" = "Services and Sales Workers",
                      "Bartenders" = "Services and Sales Workers",
                      "Mixed crop and animal producers" = "Skilled Agricultural, Forestry and Fishery Workers",
                      "Livestock and dairy producers" = "Skilled Agricultural, Forestry and Fishery Workers",
                      "Gardeners, horticultural and nursery growers" = "Skilled Agricultural, Forestry and Fishery Workers",
                      "Motor vehicle mechanics and repairers" = "Craft and Related Trades Workers",
                      "Carpenters and joiners" = "Craft and Related Trades Workers",
                      "Bricklayers and related workers" = "Craft and Related Trades Workers",
                      "Building and related electricians" = "Craft and Related Trades Workers",
                      "Metal working machine tool setters and operators" = "Craft and Related Trades Workers",
                      "Agricultural and industrial machinery mechanics and repairers" = "Craft and Related Trades Workers",
                      "Plumbers and pipe fitters" = "Craft and Related Trades Workers",
                      "Tailors, dressmakers, furriers and hatters" = "Craft and Related Trades Workers",
                      "Bakers, pastry-cooks and confectionery makers" = "Craft and Related Trades Workers",
                      "Structural-metal preparers and erectors" = "Craft and Related Trades Workers",
                      "Painters and related workers" = "Craft and Related Trades Workers",
                      "House builders" = "Craft and Related Trades Workers",
                      "Welders and flamecutters" = "Craft and Related Trades Workers",
                      "Electrical mechanics and fitters" = "Craft and Related Trades Workers",
                      "Cabinet-makers and related workers" = "Craft and Related Trades Workers",
                      "Product graders and testers (excluding foods and beverages)" = "Craft and Related Trades Workers",
                      "Heavy truck and lorry drivers" = "Plant and Machine Operators and Assemblers",
                      "Car, taxi and van drivers" = "Plant and Machine Operators and Assemblers",
                      "Mechanical machinery assemblers" = "Plant and Machine Operators and Assemblers",
                      "Bus and tram drivers" = "Plant and Machine Operators and Assemblers",
                      "Food and related products machine operators" = "Plant and Machine Operators and Assemblers",
                      "Lifting truck operators" = "Plant and Machine Operators and Assemblers",
                      "Cleaners and helpers in offices, hotels and other establishments" = "Elementary Occupations",
                      "Kitchen helpers" = "Elementary Occupations",
                      "Domestic cleaners and helpers" = "Elementary Occupations",
                      "Freight handlers" = "Elementary Occupations",
                      "Manufacturing labourers not elsewhere classified" = "Elementary Occupations",
                      "Building caretakers" = "Elementary Occupations",
                      "Shelf fillers" = "Elementary Occupations",
                      "Crop farm labourers" = "Elementary Occupations",
                      "Building construction labourers" = "Elementary Occupations",
                      "Hand packers" = "Elementary Occupations", 
                      "Shop,stall,market salespers, demonstrators" = "Services and Sales Workers",
                      "Helper,cleaner in office,hotel,other establ" = "Elementary Occupations"
  ))





summary(ESS02$OCCUPATION)








View(ESS02$OCCUPATION)


ESS2$OCCUPATION <- factor(ESS9_00$OCCUPATION, ordered = TRUE)














library(rio)
export(ESS02, "ESS0107 Recoded.dta")
