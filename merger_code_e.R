# This file is developed to contruct Florida local pension data (2005 to 2017)
# Author:  Youngsung Kim, David Matkin
# Date:    3/5/2018 ~

rm(list = ls())

#### Load libraries
#install.packages("tidyverse")
library(tidyverse)
library(readxl)
library(lubridate)
library(purrr)

#setwd("C:/Users/David Matkin/Dropbox/Retirement_Research/Pension OPEB Data/FL_LocalPensionReports")
setwd(("C:/Users/dsm32/Dropbox/Retirement_Research/Pension OPEB Data/FL_LocalPensionReports"))

#### Part 1. Date importing and cleaning
### First, import excel files
### Second, ensure consistent variable names so they can be bound together
### Third, change date class variables and formats into "yyyy-mm-dd"
### Fourth, drop empty rows and columns

## 2005 data
blrs_e_2005 <- read_excel("2005 Report/2005-blrs-e.xls", na = c("N/A", "NA", "VR"))
blrs_e_2005 <- blrs_e_2005[!is.na(blrs_e_2005$CITY_OR_DISTRICT),]
blrs_e_2005$VAL_DATE <- ymd(blrs_e_2005$VAL_DATE)
blrs_e_2005$PLNYR_ENDED[blrs_e_2005$PLNYR_ENDED == "13-31-03"] <- 37986
blrs_e_2005$PLNYR_ENDED <- as.numeric(blrs_e_2005$PLNYR_ENDED)
blrs_e_2005 <- mutate(blrs_e_2005, PLNYR_ENDED=as.Date(PLNYR_ENDED, origin="1899-12-30")) 
blrs_e_2005$PR_GR_ASSMP[blrs_e_2005$PR_GR_ASSMP=="3.50F"] <- 3.50
blrs_e_2005$PR_GR_ASSMP[blrs_e_2005$PR_GR_ASSMP=="0.00%"] <- 0
blrs_e_2005$PR_GR_ASSMP <- as.numeric(blrs_e_2005$PR_GR_ASSMP)
blrs_e_2005$FUND_METHOD[blrs_e_2005$FUND_METHOD == "AGG/"] <- "AGG" 
blrs_e_2005$FUND_METHOD[blrs_e_2005$FUND_METHOD == "EAN/"] <- "EAN"
blrs_e_2005$FUND_METHOD[blrs_e_2005$FUND_METHOD == "ean/"] <- "EAN" 
blrs_e_2005$FUND_METHOD[blrs_e_2005$FUND_METHOD == "PUC/"] <- "PUC" 
blrs_e_2005$FUND_METHOD[blrs_e_2005$FUND_METHOD == "UC/"] <- "UC"
blrs_e_2005$FUND_METHOD[blrs_e_2005$FUND_METHOD == "FIL/"] <- "FIL" 
blrs_e_2005$REPORT_YR <- as.numeric("2005")
blrs_e_2005$TXT_MV_RETURN <- NA

## 2006 data
blrs_e_2006 <- read_excel("2006 Report/2006-07 E - Actuarial Data - 14e.xls", 
                          na = c("N/A", "NA", "VR"),
                          skip = 3,
                          col_names = c("CITY_OR_DISTRICT", "TYP_SYS", "VAL_DATE", "FUND_METHOD",
                                        "OLD_PLN", "PLNYR_ENDED", "SALRY_ASSMP", "SALRY_ACTUL", 
                                        "INT_ASSMP", "INT_ACTUL", "PR_GR_ASSMP", "RETIREMENT_AGE_ASSUMPTION_YR", 
                                        "RETIREMENT_AGE_ASSUMPTION_DESC", "COMMENTS"))
blrs_e_2006 <- blrs_e_2006[!is.na(blrs_e_2006$CITY_OR_DISTRICT),]
blrs_e_2006$VAL_DATE <- ymd(blrs_e_2006$VAL_DATE)
blrs_e_2006$PLNYR_ENDED <- ymd(blrs_e_2006$PLNYR_ENDED)
blrs_e_2006$REPORT_YR <- as.numeric("2006")
blrs_e_2006$FUND_METHOD[blrs_e_2006$FUND_METHOD == "AGG/"] <- "AGG" 
blrs_e_2006$FUND_METHOD[blrs_e_2006$FUND_METHOD == "EAN/"] <- "EAN"
blrs_e_2006$FUND_METHOD[blrs_e_2006$FUND_METHOD == "PUC/"] <- "PUC" 
blrs_e_2006$FUND_METHOD[blrs_e_2006$FUND_METHOD == "FIL/"] <- "FIL" 
blrs_e_2006$TXT_MV_RETURN <- NA

### 2007 data
blrs_e_2007 <- read_excel("2007 Report/Appendix_E.xls", na = c("N/A", "NA", "VR"))
blrs_e_2007 <- blrs_e_2007[!is.na(blrs_e_2007$CITY_OR_DISTRICT),]
blrs_e_2007$VAL_DATE <- ymd(blrs_e_2007$VAL_DATE)
blrs_e_2007$PLNYR_ENDED <- ymd(blrs_e_2007$PLNYR_ENDED)
blrs_e_2007$REPORT_YR <- as.numeric("2007")
blrs_e_2007$FUND_METHOD[blrs_e_2007$FUND_METHOD == "AGG/"] <- "AGG" 
blrs_e_2007$FUND_METHOD[blrs_e_2007$FUND_METHOD == "EAN/"] <- "EAN"
blrs_e_2007$FUND_METHOD[blrs_e_2007$FUND_METHOD == "PUC/"] <- "PUC" 
blrs_e_2007$FUND_METHOD[blrs_e_2007$FUND_METHOD == "FIL/"] <- "FIL" 
blrs_e_2007$TXT_MV_RETURN <- NA

### 2008 data
blrs_e_2008 <- read_excel("2008 Report/Appendix_E(1).xls", na = c("N/A", "NA", "VR"))
blrs_e_2008$INT_ASSMP[blrs_e_2008$INT_ASSMP=="8..00"] <- 8.0
blrs_e_2008$INT_ASSMP <- as.numeric(blrs_e_2008$INT_ASSMP)
blrs_e_2008$VAL_DATE <- ymd(blrs_e_2008$VAL_DATE)
blrs_e_2008$PLNYR_ENDED <- ymd(blrs_e_2008$PLNYR_ENDED)
blrs_e_2008$REPORT_YR <- as.numeric("2008")
blrs_e_2008$FUND_METHOD[blrs_e_2008$FUND_METHOD == "AGG/"] <- "AGG" 
blrs_e_2008$FUND_METHOD[blrs_e_2008$FUND_METHOD == "EAN/"] <- "EAN"
blrs_e_2008$FUND_METHOD[blrs_e_2008$FUND_METHOD == "PUC/"] <- "PUC" 
blrs_e_2008$FUND_METHOD[blrs_e_2008$FUND_METHOD == "FIL/"] <- "FIL" 
blrs_e_2008$FUND_METHOD[blrs_e_2008$FUND_METHOD == "UC/"] <- "UC" 
blrs_e_2008$FUND_METHOD[blrs_e_2008$FUND_METHOD == "AGG//"] <- "AGG" 
blrs_e_2008$FUND_METHOD[blrs_e_2008$FUND_METHOD == "EAB/"] <- "EAN" 
blrs_e_2008$TXT_MV_RETURN <- NA

### 2009 data
blrs_e_2009 <- read_excel("2009 Report/Appendix_E.xls", na = c("N/A", "NA", "VR", ""))
blrs_e_2009$SALRY_ASSMP <- as.numeric(blrs_e_2009$SALRY_ASSMP)
blrs_e_2009$RETIREMENT_AGE_ASSUMPTION_YR[blrs_e_2009$RETIREMENT_AGE_ASSUMPTION_YR=="AGE 55"] <- 55.0
blrs_e_2009$RETIREMENT_AGE_ASSUMPTION_YR <- as.numeric(blrs_e_2009$RETIREMENT_AGE_ASSUMPTION_YR)
blrs_e_2009$VAL_DATE <- ymd(blrs_e_2009$VAL_DATE)
blrs_e_2009$PLNYR_ENDED <- ymd(blrs_e_2009$PLNYR_ENDED)
blrs_e_2009$REPORT_YR <- as.numeric("2009")
blrs_e_2009$FUND_METHOD[blrs_e_2009$FUND_METHOD == "AGG/"] <- "AGG" 
blrs_e_2009$FUND_METHOD[blrs_e_2009$FUND_METHOD == "EAN/"] <- "EAN"
blrs_e_2009$FUND_METHOD[blrs_e_2009$FUND_METHOD == "PUC/"] <- "PUC" 
blrs_e_2009$FUND_METHOD[blrs_e_2009$FUND_METHOD == "FIL/"] <- "FIL" 
blrs_e_2009$FUND_METHOD[blrs_e_2009$FUND_METHOD == "agg/"] <- "AGG" 
blrs_e_2009$TXT_MV_RETURN <- NA

### 2010 data
blrs_e_2010 <- read_excel("2010 Report/Appendix_E.xls", na = c("N/A", "NA", "VR", ""))
blrs_e_2010$SALRY_ASSMP <- as.numeric(blrs_e_2010$SALRY_ASSMP)
blrs_e_2010$RETIREMENT_AGE_ASSUMPTION_YR <- as.numeric(blrs_e_2010$RETIREMENT_AGE_ASSUMPTION_YR)
blrs_e_2010$VAL_DATE <- ymd(blrs_e_2010$VAL_DATE)
blrs_e_2010$PLNYR_ENDED <- ymd(blrs_e_2010$PLNYR_ENDED)
blrs_e_2010$REPORT_YR <- as.numeric("2010")
blrs_e_2010$FUND_METHOD[blrs_e_2010$FUND_METHOD == "AGG/"] <- "AGG" 
blrs_e_2010$FUND_METHOD[blrs_e_2010$FUND_METHOD == "EAN/"] <- "EAN"
blrs_e_2010$FUND_METHOD[blrs_e_2010$FUND_METHOD == "PUC/"] <- "PUC" 
blrs_e_2010$FUND_METHOD[blrs_e_2010$FUND_METHOD == "FIL/"] <- "FIL" 
blrs_e_2010$FUND_METHOD[blrs_e_2010$FUND_METHOD == "agg/"] <- "AGG" 
blrs_e_2010$FUND_METHOD[blrs_e_2010$FUND_METHOD == "E"] <- "EAN"
blrs_e_2010$FUND_METHOD[blrs_e_2010$FUND_METHOD == "F"] <- "FIL" 
blrs_e_2010$TXT_MV_RETURN <- NA

### 2011 data (For this year, I resaved xlsx file to xls file)
blrs_e_2011 <- read_excel("2011 Report/Appendix_E.xls", 
                          na = c("N/A", "NA", "VR", ""),
                          skip = 1,
                          col_names = c("CITY_OR_DISTRICT", "TYP_SYS", "VAL_DATE", "FUND_METHOD",
                                        "OLD_PLN", "PLNYR_ENDED", "SALRY_ASSMP", "SALRY_ACTUL", 
                                        "INT_ASSMP", "INT_ACTUL", "PR_GR_ASSMP", "RETIREMENT_AGE_ASSUMPTION_YR", 
                                        "RETIREMENT_AGE_ASSUMPTION_DESC", "COMMENTS"))
blrs_e_2011$SALRY_ASSMP <- as.numeric(blrs_e_2011$SALRY_ASSMP)
blrs_e_2011$RETIREMENT_AGE_ASSUMPTION_YR <- as.numeric(blrs_e_2011$RETIREMENT_AGE_ASSUMPTION_YR)
blrs_e_2011$VAL_DATE <- ymd(blrs_e_2011$VAL_DATE)
blrs_e_2011$PLNYR_ENDED <- ymd(blrs_e_2011$PLNYR_ENDED)
blrs_e_2011$REPORT_YR <- as.numeric("2011")
blrs_e_2011$FUND_METHOD[blrs_e_2011$FUND_METHOD == "AGG/"] <- "AGG" 
blrs_e_2011$TXT_MV_RETURN <- NA

### 2012 data (For this year, xlsx file saved as xls file)
blrs_e_2012 <- read_excel("2012 Report/Appendix_E.xls", 
                          na = c("N/A", "NA", "VR", ""),
                          skip = 1,
                          col_names = c("CITY_OR_DISTRICT", "TYP_SYS", "VAL_DATE", "FUND_METHOD",
                                        "OLD_PLN", "PLNYR_ENDED", "SALRY_ASSMP", "SALRY_ACTUL", 
                                        "INT_ASSMP", "INT_ACTUL", "PR_GR_ASSMP", "RETIREMENT_AGE_ASSUMPTION_YR", 
                                        "RETIREMENT_AGE_ASSUMPTION_DESC", "COMMENTS"))
blrs_e_2012$SALRY_ASSMP[blrs_e_2012$SALRY_ASSMP == "VR W/SC"] <- NA
blrs_e_2012$SALRY_ASSMP[blrs_e_2012$SALRY_ASSMP == "Other"] <- NA
blrs_e_2012$SALRY_ASSMP <- as.numeric(blrs_e_2012$SALRY_ASSMP)
blrs_e_2012$RETIREMENT_AGE_ASSUMPTION_YR <- as.numeric(blrs_e_2012$RETIREMENT_AGE_ASSUMPTION_YR)
blrs_e_2012$VAL_DATE <- ymd(blrs_e_2012$VAL_DATE)
blrs_e_2012$PLNYR_ENDED <- ymd(blrs_e_2012$PLNYR_ENDED)
blrs_e_2012$REPORT_YR <- as.numeric("2012")
blrs_e_2012$TXT_MV_RETURN <- NA

### 2013 data (For this year, xlsx file saved as xls file)
blrs_e_2013 <- read_excel("2013 Report/2013-09 - Appendix E - Actuarial Data.xls", 
                          na = c("N/A", "NA", "VR", ""),
                          skip = 1,
                          col_names = c("CITY_OR_DISTRICT", "TYP_SYS", "VAL_DATE", "FUND_METHOD",
                                        "OLD_PLN", "PLNYR_ENDED", "SALRY_ASSMP", "SALRY_ACTUL", 
                                        "INT_ASSMP", "INT_ACTUL", "PR_GR_ASSMP", "RETIREMENT_AGE_ASSUMPTION_YR", 
                                        "RETIREMENT_AGE_ASSUMPTION_DESC", "COMMENTS"),
                          col_types = c("guess", "guess", "date", 
                                        "guess", "guess", "text", "guess",
                                        "guess", "guess", "guess",
                                        "guess", "guess", "guess", "guess"))
blrs_e_2013$SALRY_ASSMP <- as.numeric(blrs_e_2013$SALRY_ASSMP)
blrs_e_2013$VAL_DATE <- ymd(blrs_e_2013$VAL_DATE)
blrs_e_2013$PLNYR_ENDED <- as.numeric(blrs_e_2013$PLNYR_ENDED)
blrs_e_2013 <- mutate(blrs_e_2013, PLNYR_ENDED=as.Date(PLNYR_ENDED, origin="1899-12-30")) 
blrs_e_2013$PLNYR_ENDED <- ymd(blrs_e_2013$PLNYR_ENDED)
blrs_e_2013$REPORT_YR <- as.numeric("2013")
blrs_e_2013$TXT_MV_RETURN <- NA

### 2014 data
blrs_e_2014 <- read_excel("2014 Report/2014-09 - Appendix E - Actuarial Data.xls", 
                          na = c("N/A", "NA", "VR", ""),
                          skip = 1,
                          col_names = c("CITY_OR_DISTRICT", "TYP_SYS", "VAL_DATE", "FUND_METHOD",
                                        "PLNYR_ENDED", "SALRY_ASSMP", "SALRY_ACTUL", 
                                        "INT_ASSMP", "INT_ACTUL", "TXT_MV_RETURN", "PR_GR_ASSMP", 
                                        "RETIREMENT_AGE_ASSUMPTION_YR", "RETIREMENT_AGE_ASSUMPTION_DESC"))
blrs_e_2014$SALRY_ASSMP <- as.numeric(blrs_e_2014$SALRY_ASSMP)
blrs_e_2014$RETIREMENT_AGE_ASSUMPTION_YR <- as.numeric(blrs_e_2014$RETIREMENT_AGE_ASSUMPTION_YR)
blrs_e_2014$VAL_DATE <- ymd(blrs_e_2014$VAL_DATE)
blrs_e_2014$PLNYR_ENDED <- ymd(blrs_e_2014$PLNYR_ENDED)
blrs_e_2014$REPORT_YR <- as.numeric("2014")
blrs_e_2014$OLD_PLN <- NA
blrs_e_2014$COMMENTS <- NA

### 2015 data
blrs_e_2015 <- read_excel("2015 Report/Appendix_E.xls", 
                          na = c("N/A", "NA", "VR", ""),
                          skip = 1,
                          col_names = c("CITY_OR_DISTRICT", "TYP_SYS", "VAL_DATE", "FUND_METHOD",
                                        "PLNYR_ENDED", "SALRY_ASSMP", "SALRY_ACTUL", 
                                        "INT_ASSMP", "INT_ACTUL", "TXT_MV_RETURN", "PR_GR_ASSMP", 
                                        "RETIREMENT_AGE_ASSUMPTION_YR", "RETIREMENT_AGE_ASSUMPTION_DESC"))
blrs_e_2015$SALRY_ASSMP <- as.numeric(blrs_e_2015$SALRY_ASSMP)
blrs_e_2015$RETIREMENT_AGE_ASSUMPTION_YR <- as.numeric(blrs_e_2015$RETIREMENT_AGE_ASSUMPTION_YR)
blrs_e_2015$VAL_DATE <- ymd(blrs_e_2015$VAL_DATE)
blrs_e_2015$PLNYR_ENDED <- ymd(blrs_e_2015$PLNYR_ENDED)
blrs_e_2015$REPORT_YR <- as.numeric("2015")
blrs_e_2015$OLD_PLN <- NA
blrs_e_2015$COMMENTS <- NA

### 2016 data
blrs_e_2016 <- read_excel("2016 Report/Appendix_E.xls", 
                          na = c("N/A", "NA", "VR", ""),
                          skip = 1,
                          col_names = c("CITY_OR_DISTRICT", "TYP_SYS", "VAL_DATE", "FUND_METHOD",
                                        "PLNYR_ENDED", "SALRY_ASSMP", "SALRY_ACTUL", 
                                        "INT_ASSMP", "INT_ACTUL", "TXT_MV_RETURN", "PR_GR_ASSMP", 
                                        "RETIREMENT_AGE_ASSUMPTION_YR", "RETIREMENT_AGE_ASSUMPTION_DESC"))
blrs_e_2016$VAL_DATE <- ymd(blrs_e_2016$VAL_DATE)
blrs_e_2016$PLNYR_ENDED <- ymd(blrs_e_2016$PLNYR_ENDED)
blrs_e_2016$REPORT_YR <- as.numeric("2016")
blrs_e_2016$OLD_PLN <- NA
blrs_e_2016$COMMENTS <- NA

### 2017 data
blrs_e_2017 <- read_excel("2017 Report/Appendix_E.xls", 
                          na = c("N/A", "NA", "VR", ""),
                          skip = 1,
                          col_names = c("CITY_OR_DISTRICT", "TYP_SYS", "VAL_DATE", "FUND_METHOD",
                                        "PLNYR_ENDED", "SALRY_ASSMP", "SALRY_ACTUL", 
                                        "INT_ASSMP", "INT_ACTUL", "TXT_MV_RETURN", "PR_GR_ASSMP", 
                                        "RETIREMENT_AGE_ASSUMPTION_YR", "RETIREMENT_AGE_ASSUMPTION_DESC"))
blrs_e_2017$SALRY_ASSMP <- as.numeric(blrs_e_2017$SALRY_ASSMP)
blrs_e_2017$RETIREMENT_AGE_ASSUMPTION_YR <- as.numeric(blrs_e_2017$RETIREMENT_AGE_ASSUMPTION_YR)
blrs_e_2017$VAL_DATE <- ymd(blrs_e_2017$VAL_DATE)
blrs_e_2017$PLNYR_ENDED <- ymd(blrs_e_2017$PLNYR_ENDED)
blrs_e_2017$REPORT_YR <- as.numeric("2017")
blrs_e_2017$OLD_PLN <- NA
blrs_e_2017$COMMENTS <- NA


#### Part 2. Bind each year into a single table & relabel variables

blrs_e <- bind_rows(blrs_e_2005, blrs_e_2006, blrs_e_2007, blrs_e_2008, blrs_e_2009, blrs_e_2010, blrs_e_2011, blrs_e_2012, blrs_e_2013, blrs_e_2014, blrs_e_2015, blrs_e_2016, blrs_e_2017)

## reorder and rename columns

blrs_e <- blrs_e %>%
  select(plan_sponsor = CITY_OR_DISTRICT, 
         plan_type_e = TYP_SYS, 
         val_date_e = VAL_DATE, 
         report_year_e = REPORT_YR,
         funded_method_e = FUND_METHOD,
         old_plan_e = OLD_PLN,
         plan_year_ended_e = PLNYR_ENDED,
         salary_assumption_e = SALRY_ASSMP,
         salary_actual_e = SALRY_ACTUL,
         interest_assumption_e = INT_ASSMP,
         interest_av_actual_e = INT_ACTUL,
         interest_mv_actual_e = TXT_MV_RETURN,
         payroll_growth_assumption_e = PR_GR_ASSMP,
         retire_age_assumption_year_e = RETIREMENT_AGE_ASSUMPTION_YR,
         retire_age_assumption_description_e = RETIREMENT_AGE_ASSUMPTION_DESC,
         report_year_e = REPORT_YR,
         comments = COMMENTS)

blrs_e$plan_sponsor <- str_trim(blrs_e$plan_sponsor)
blrs_e$plan_sponsor <- str_squish(blrs_e$plan_sponsor)

blrs_e <- blrs_e[blrs_e$val_date_e>="2005-01-01",]
blrs_e$plan_type_e[blrs_e$plan_sponsor=="PALM BEACH LG-R"] <- "LG"



#### Part 4. Ensure clean identifiers

### Use this Excel file to identify sponsor names to change

sponsor_names <- table(blrs_e$plan_sponsor)
#print(sponsor_names)
sponsor_names <- as.data.frame(sponsor_names)
sponsor_names <- sponsor_names %>%
  rename(plan_sponsor = Var1) %>%
  mutate(plan_sponsor_clean = plan_sponsor) 
write_excel_csv(sponsor_names, "MatchingTables/appendix_e_to_match.csv")

### import comparison table
name_table <- read_excel("MatchingTables/appendix_e_matched.xls")
name_table <- name_table %>% 
  select(plan_sponsor, plan_sponsor_clean)

### consistent names for plan_type

blrs_e$plan_type_e[blrs_e$plan_type_e=="G&S"] <- "GS"

blrs_e <- right_join(name_table, blrs_e, by = "plan_sponsor")  

blrs_e <- arrange(blrs_e, plan_sponsor_clean, plan_type_e, val_date_e, report_year_e)

blrs_e <- blrs_e %>% rename(plan_sponsor_e = plan_sponsor)

blrs_e <- blrs_e %>%
  group_by(plan_sponsor_clean, plan_type_e, val_date_e) %>%
  filter(report_year_e == max(report_year_e))


save(blrs_e, file="blrs_e.RData")





