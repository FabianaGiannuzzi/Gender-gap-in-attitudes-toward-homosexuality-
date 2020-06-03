# Valentina's script 
# Dataset: ESS Cumulative (Rounds 1-7)

rm(list = ls())

#packages we need for data management 
library(foreign)
library(dplyr)

# 1. Importing cumulative datset 
ESSCumul <- read.dta("D:/R Projects/ESS Datasets/ESS Datasets/ESS1-7/ESS1-8e01.dta")

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


# Filtering countries that are present in all the versions of the ESS
ESS0107<- filter(
  ESS0107,
  cntry %in% c("AT", "BE", "CH", "CZ", "DE", "EE", "FI", "FR", "GB", "HU", "IE", "IT", "NO", "PL", "NL", "SI")
)

library(rio)
export(ESS0107, "ESS0107 Recoded.dta")

ESS0107 <- import("ESS0107 Recoded.dta")

summary(ESS0107$isco08)
summary(ESS0107$iscoco)

#for occupation in the cumulative ESS we have iscoco for rounds 1-5 and isco08 for rounds 6-7
ESS0107 <- mutate(
  ESS0107,
  OCCUPATION = recode(isco08,
                      "Managing directors and chief executives" = "Managers",
                      "Manufacturing managers" = "Managers",
                      "Retail and wholesale trade managers" = "Managers",
                      "Sales and marketing managers" = "Managers",
                      "Supply, distribution and related managers" = "Managers",
                      "Restaurant managers" = "Managers",
                      "Construction managers" = "Managers",
                      "Education managers" = "Managers",
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
                      "Hand packers" = "Elementary Occupations"
  ))

summary(ESS0107$OCCUPATION)
View(ESS0107$isco08 ESS0107$OCCUPATION)

View(ESS0107$isco08)

View(ESS0107$iscoco)
ESS9_00$OCCUPATION <- factor(ESS9_00$OCCUPATION, ordered = TRUE)



  ESS0107$OCCUPATION <- ifelse(ESS0107$isco08 < 15,
                                                "Managers",
                                                ifelse(ESS0107$isco08 < 27,
                                                       "Professionals",
                                                       ifelse(ESS0107$isco08 < 37,
                                                              "Technicians and Associate Professionals",
                                                              ifelse(ESS0107$isco08 < 47,
                                                                     "Clerical Support Workers",
                                                                     ifelse(ESS0107$isco08< 57,
                                                                            "Services and Sales Workers",
                                                                            ifelse(ESS0107$isco08 < 75,
                                                                                   "65-74",
                                                                                   "75+"))))
                                                       
                                                )
                      )
  
  
  
  
  
  
View(ESS3e03_7$iscoco)
View(ESS2e03_6$iscoco)
