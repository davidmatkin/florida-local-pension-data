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
blrs_a_2005 <- read_excel("2005 Report/2005-blrs-a.xls", na = "N/A")
blrs_a_2005 <- blrs_a_2005[!is.na(blrs_a_2005$CITY_OR_DISTRICT),]
blrs_a_2005$VAL_DATE <- ymd(blrs_a_2005$VAL_DATE)
blrs_a_2005 <- blrs_a_2005 %>% #change date format
  mutate(PLAN_YEAR_BEGINNING=as.Date(PLAN_YEAR_BEGINNING, origin="1899-12-30"), PAYMENT_BEGINNING=as.Date(PAYMENT_BEGINNING, origin="1899-12-30")) 
blrs_a_2005$REPORT_YR <- as.numeric("2005")
blrs_a_2005 <- blrs_a_2005 %>% 
  mutate(TOTAL_ASSETS = TOTAL_ASSETS * 1000) %>%
  mutate(UAAL = UAAL * 1000) %>%
  mutate(VAL_PYROLL = VAL_PYROLL * 1000) %>%
  mutate(RETD_PYROLL = RETD_PYROLL * 1000) %>%
  mutate(NORMAL_COST = NORMAL_COST * 1000) %>%
  mutate(UAAL_PYMT = UAAL_PYMT * 1000) %>%
  mutate(REQD_CONT = REQD_CONT * 1000)

## 2006 data
blrs_a_2006 <- read_excel("2006 Report/2006-03 A - Financial Contribution Data - 14b.xls", 
                          na = "N/A",
                          skip = 3,
                          col_names = c("CITY_OR_DISTRICT", "TYP_SYS", "C_V", "VAL_DATE",
                                        "TOTAL_ASSETS", "UAAL", "VAL_PYROLL", "RETD_PYROLL", 
                                        "NORMAL_COST", "NORM_PERC", "X__1", "UAAL_PYMT", 
                                        "UAAL_PERC", "X__2", "REQD_CONT", "REQD_PERC", 
                                        "MEMB_PERC", "CITY_PERC", "OTHR_PERC", 
                                        "PLAN_YEAR_BEGINNING", "PAYMENT_BEGINNING"))
blrs_a_2006 <- blrs_a_2006 %>% 
  select(-X__1, -X__2)
blrs_a_2006 <- blrs_a_2006[!is.na(blrs_a_2006$CITY_OR_DISTRICT),]
blrs_a_2006$VAL_DATE <- ymd(blrs_a_2006$VAL_DATE)
blrs_a_2006$PLAN_YEAR_BEGINNING <- ymd(blrs_a_2006$PLAN_YEAR_BEGINNING)
blrs_a_2006$PAYMENT_BEGINNING <- ymd(blrs_a_2006$PAYMENT_BEGINNING)
blrs_a_2006$REPORT_YR <- as.numeric("2006")
blrs_a_2006 <- blrs_a_2006 %>% 
  mutate(TOTAL_ASSETS = TOTAL_ASSETS * 1000) %>%
  mutate(UAAL = UAAL * 1000) %>%
  mutate(VAL_PYROLL = VAL_PYROLL * 1000) %>%
  mutate(RETD_PYROLL = RETD_PYROLL * 1000) %>%
  mutate(NORMAL_COST = NORMAL_COST * 1000) %>%
  mutate(UAAL_PYMT = UAAL_PYMT * 1000) %>%
  mutate(REQD_CONT = REQD_CONT * 1000)

### 2007 data
blrs_a_2007 <- read_excel("2007 Report/Appendix_A.xls", na = "N/A")
blrs_a_2007 <- blrs_a_2007[!is.na(blrs_a_2007$CITY_OR_DISTRICT),]
blrs_a_2007 <- blrs_a_2007[!is.na(blrs_a_2007$CITY_OR_DISTRICT),]
blrs_a_2007$VAL_DATE <- ymd(blrs_a_2007$VAL_DATE)
blrs_a_2007 <- blrs_a_2007 %>% #change date format
  mutate(PLAN_YEAR_BEGINNING=as.Date(PLAN_YEAR_BEGINNING, origin="1899-12-30"), PAYMENT_BEGINNING=as.Date(PAYMENT_BEGINNING, origin="1899-12-30")) 
blrs_a_2007$REPORT_YR <- as.numeric("2007")
blrs_a_2007 <- blrs_a_2007 %>% 
  mutate(TOTAL_ASSETS = TOTAL_ASSETS * 1000) %>%
  mutate(UAAL = UAAL * 1000) %>%
  mutate(VAL_PYROLL = VAL_PYROLL * 1000) %>%
  mutate(RETD_PYROLL = RETD_PYROLL * 1000) %>%
  mutate(NORMAL_COST = NORMAL_COST * 1000) %>%
  mutate(UAAL_PYMT = UAAL_PYMT * 1000) %>%
  mutate(REQD_CONT = REQD_CONT * 1000)

### 2008 data
blrs_a_2008 <- read_excel("2008 Report/Appendix_A(1).xls", na = "N/A")
blrs_a_2008$UAAL_PERC[blrs_a_2008$UAAL_PERC=="2..9"] <- 2.9
blrs_a_2008$UAAL_PERC <- as.numeric(blrs_a_2008$UAAL_PERC)
blrs_a_2008 <- blrs_a_2008[!is.na(blrs_a_2008$CITY_OR_DISTRICT),]
blrs_a_2008$VAL_DATE <- ymd(blrs_a_2008$VAL_DATE)
blrs_a_2008$PAYMENT_BEGINNING <- ymd(blrs_a_2008$PAYMENT_BEGINNING)
blrs_a_2008$PLAN_YEAR_BEGINNING[blrs_a_2008$PLAN_YEAR_BEGINNING=="10.01-08"] <- 39722
blrs_a_2008$PLAN_YEAR_BEGINNING <- as.numeric(blrs_a_2008$PLAN_YEAR_BEGINNING)
blrs_a_2008 <- blrs_a_2008 %>% #change date format
  mutate(PLAN_YEAR_BEGINNING=as.Date(PLAN_YEAR_BEGINNING, origin="1899-12-30")) 
blrs_a_2008$PLAN_YEAR_BEGINNING[is.na(blrs_a_2008$PLAN_YEAR_BEGINNING)] <- mdy("10-01-08") #input for missing date
blrs_a_2008$REPORT_YR <- as.numeric("2008")
blrs_a_2008 <- blrs_a_2008 %>% 
  mutate(TOTAL_ASSETS = TOTAL_ASSETS * 1000) %>%
  mutate(UAAL = UAAL * 1000) %>%
  mutate(VAL_PYROLL = VAL_PYROLL * 1000) %>%
  mutate(RETD_PYROLL = RETD_PYROLL * 1000) %>%
  mutate(NORMAL_COST = NORMAL_COST * 1000) %>%
  mutate(UAAL_PYMT = UAAL_PYMT * 1000) %>%
  mutate(REQD_CONT = REQD_CONT * 1000)

### 2009 data
blrs_a_2009 <- read_excel("2009 Report/Appendix_A.xls")
blrs_a_2009 <- blrs_a_2009 %>%
  rename("CITY_OR_DISTRICT" = "CityName") %>%
  rename(TYP_SYS = PlanType)
blrs_a_2009$C_V <- NA
blrs_a_2009 <- blrs_a_2009[!is.na(blrs_a_2009$CITY_OR_DISTRICT),]
blrs_a_2009$VAL_DATE <- ymd(blrs_a_2009$VAL_DATE)
blrs_a_2009$PLAN_YEAR_BEGINNING <- ymd(blrs_a_2009$PLAN_YEAR_BEGINNING)
blrs_a_2009$PAYMENT_BEGINNING <- ymd(blrs_a_2009$PAYMENT_BEGINNING)
blrs_a_2009$REPORT_YR <- as.numeric("2009")


### 2010 data
blrs_a_2010 <- read_excel("2010 Report/Appendix_A.xls")
blrs_a_2010$C_V <- NA
blrs_a_2010 <- blrs_a_2010 %>%
  rename("CITY_OR_DISTRICT" = "CityName") %>%
  rename("TYP_SYS" = "PlanType")
blrs_a_2010 <- blrs_a_2010[!is.na(blrs_a_2010$CITY_OR_DISTRICT),] #remove "Grand Total" row
blrs_a_2010$VAL_DATE <- ymd(blrs_a_2010$VAL_DATE)
blrs_a_2010$PLAN_YEAR_BEGINNING <- ymd(blrs_a_2010$PLAN_YEAR_BEGINNING)
blrs_a_2010$PAYMENT_BEGINNING <- ymd(blrs_a_2010$PAYMENT_BEGINNING)
blrs_a_2010$REPORT_YR <- as.numeric("2010")


### 2011 data (For this year, I resaved xlsx file to xls file)
blrs_a_2011 <- read_excel("2011 Report/Appendix_A.xls", 
                          skip = 1,
                          col_names = c("CITY_OR_DISTRICT", "TYP_SYS", "VAL_DATE",
                                        "TOTAL_ASSETS", "UAAL", "VAL_PYROLL", "RETD_PYROLL", 
                                        "NORMAL_COST", "NORM_PERC", "UAAL_PYMT", 
                                        "UAAL_PERC", "REQD_CONT", "REQD_PERC", 
                                        "MEMB_PERC", "CITY_PERC", "OTHR_PERC", 
                                        "PLAN_YEAR_BEGINNING", "PAYMENT_BEGINNING"))
blrs_a_2011$C_V <- NA
blrs_a_2011 <- blrs_a_2011[!is.na(blrs_a_2011$CITY_OR_DISTRICT),]
blrs_a_2011$VAL_DATE <- ymd(blrs_a_2011$VAL_DATE)
blrs_a_2011$PLAN_YEAR_BEGINNING <- ymd(blrs_a_2011$PLAN_YEAR_BEGINNING)
blrs_a_2011$PAYMENT_BEGINNING <- ymd(blrs_a_2011$PAYMENT_BEGINNING)
blrs_a_2011$REPORT_YR <- as.numeric("2011")


### 2012 data (For this year, xlsx file saved as xls file)
blrs_a_2012 <- read_excel("2012 Report/Appendix_A.xls", 
                          skip = 1,
                          col_names = c("CITY_OR_DISTRICT", "TYP_SYS", "VAL_DATE",
                                        "TOTAL_ASSETS", "UAAL", "VAL_PYROLL", "RETD_PYROLL", 
                                        "NORMAL_COST", "NORM_PERC", "UAAL_PYMT", 
                                        "UAAL_PERC", "REQD_CONT", "REQD_PERC", 
                                        "MEMB_PERC", "CITY_PERC", "OTHR_PERC", 
                                        "PLAN_YEAR_BEGINNING", "PAYMENT_BEGINNING"))
blrs_a_2012$C_V <- NA
blrs_a_2012 <- blrs_a_2012[!is.na(blrs_a_2012$CITY_OR_DISTRICT),] #remove total row
blrs_a_2012$VAL_DATE <- ymd(blrs_a_2012$VAL_DATE)
blrs_a_2012$PLAN_YEAR_BEGINNING <- ymd(blrs_a_2012$PLAN_YEAR_BEGINNING)
blrs_a_2012$PAYMENT_BEGINNING <- ymd(blrs_a_2012$PAYMENT_BEGINNING)
blrs_a_2012$REPORT_YR <- as.numeric("2012")


### 2013 data (For this year, xlsx file saved as xls file)
blrs_a_2013 <- read_excel("2013 Report/2013-04 - Appendix A - Financial Contribution Data.xls", 
                          skip = 1,
                          col_names = c("CITY_OR_DISTRICT", "TYP_SYS", "VAL_DATE",
                                        "TOTAL_ASSETS", "UAAL", "VAL_PYROLL", "RETD_PYROLL", 
                                        "NORMAL_COST", "NORM_PERC", "UAAL_PYMT", 
                                        "UAAL_PERC", "REQD_CONT", "REQD_PERC", 
                                        "MEMB_PERC", "CITY_PERC", "OTHR_PERC", 
                                        "PLAN_YEAR_BEGINNING", "PAYMENT_BEGINNING"),
                          col_types = c("guess", "guess", "date", 
                                        "numeric", "numeric", "numeric", "numeric",
                                        "numeric", "numeric", "numeric",
                                        "numeric", "numeric", "numeric",
                                        "numeric", "numeric", "numeric",
                                        "text", "text"))
blrs_a_2013$C_V <- NA
blrs_a_2013 <- blrs_a_2013[!is.na(blrs_a_2013$CITY_OR_DISTRICT),] #remove total row
blrs_a_2013$VAL_DATE <- ymd(blrs_a_2013$VAL_DATE)
blrs_a_2013 <- mutate_at(blrs_a_2013, vars(PLAN_YEAR_BEGINNING, PAYMENT_BEGINNING), funs(as.numeric))
blrs_a_2013 <- blrs_a_2013 %>% #change date format
  mutate(PLAN_YEAR_BEGINNING=as.Date(PLAN_YEAR_BEGINNING, origin="1899-12-30"), PAYMENT_BEGINNING=as.Date(PAYMENT_BEGINNING, origin="1899-12-30")) 
blrs_a_2013$REPORT_YR <- as.numeric("2013")

### 2014 data
blrs_a_2014 <- read_excel("2014 Report/2014-04 - Appendix A - Financial Contribution Data.xls")
blrs_a_2014 <- blrs_a_2014 %>%
  rename("CITY_OR_DISTRICT" = "CityName") %>%
  rename("TYP_SYS" = "PlanType")
blrs_a_2014 <- blrs_a_2014[!is.na(blrs_a_2014$CITY_OR_DISTRICT),] #remove total row
blrs_a_2014$C_V <- NA
blrs_a_2014$VAL_DATE <- ymd(blrs_a_2014$VAL_DATE)
blrs_a_2014$PLAN_YEAR_BEGINNING <- ymd(blrs_a_2014$PLAN_YEAR_BEGINNING)
blrs_a_2014$PAYMENT_BEGINNING <- ymd(blrs_a_2014$PAYMENT_BEGINNING)
blrs_a_2014$REPORT_YR <- as.numeric("2014")

### 2015 data
blrs_a_2015 <- read_excel("2015 Report/Appendix_A.xls",
                          range = "A2:R495",
                          col_names = c("CITY_OR_DISTRICT", "TYP_SYS", "VAL_DATE",
                                        "TOTAL_ASSETS", "UAAL", "VAL_PYROLL", "RETD_PYROLL", 
                                        "NORMAL_COST", "NORM_PERC", "UAAL_PYMT", 
                                        "UAAL_PERC", "REQD_CONT", "REQD_PERC", 
                                        "MEMB_PERC", "CITY_PERC", "OTHR_PERC", 
                                        "PLAN_YEAR_BEGINNING", "PAYMENT_BEGINNING"))
blrs_a_2015 <- blrs_a_2015[!is.na(blrs_a_2015$CITY_OR_DISTRICT),] #remove total row and other missing rows
blrs_a_2015$VAL_DATE <- ymd(blrs_a_2015$VAL_DATE)
blrs_a_2015$C_V <- NA
blrs_a_2015$PLAN_YEAR_BEGINNING <- ymd(blrs_a_2015$PLAN_YEAR_BEGINNING)
blrs_a_2015$PAYMENT_BEGINNING <- ymd(blrs_a_2015$PAYMENT_BEGINNING)
blrs_a_2015$REPORT_YR <- as.numeric("2015")

### 2016 data
blrs_a_2016 <- read_excel("2016 Report/Appendix_A.xls",
                          skip = 1,
                          col_names = c("CITY_OR_DISTRICT", "TYP_SYS", "VAL_DATE",
                                        "TOTAL_ASSETS", "UAAL", "VAL_PYROLL", "RETD_PYROLL", 
                                        "NORMAL_COST", "NORM_PERC", "UAAL_PYMT", 
                                        "UAAL_PERC", "REQD_CONT", "REQD_PERC", 
                                        "MEMB_PERC", "CITY_PERC", "OTHR_PERC", 
                                        "PLAN_YEAR_BEGINNING", "PAYMENT_BEGINNING"))
blrs_a_2016 <- blrs_a_2016[!is.na(blrs_a_2016$CITY_OR_DISTRICT),] #remove total row
blrs_a_2016$C_V <- NA
blrs_a_2016$VAL_DATE <- ymd(blrs_a_2016$VAL_DATE)
blrs_a_2016$PLAN_YEAR_BEGINNING <- ymd(blrs_a_2016$PLAN_YEAR_BEGINNING)
blrs_a_2016$PAYMENT_BEGINNING <- ymd(blrs_a_2016$PAYMENT_BEGINNING)
blrs_a_2016$REPORT_YR <- as.numeric("2016")

### 2017 data
blrs_a_2017 <- read_excel("2017 Report/Appendix_A.xls",
                          skip = 1,
                          col_names = c("CITY_OR_DISTRICT", "TYP_SYS", "VAL_DATE",
                                        "TOTAL_ASSETS", "UAAL", "VAL_PYROLL", "RETD_PYROLL", 
                                        "NORMAL_COST", "NORM_PERC", "UAAL_PYMT", 
                                        "UAAL_PERC", "REQD_CONT", "REQD_PERC", 
                                        "MEMB_PERC", "CITY_PERC", "OTHR_PERC", 
                                        "PLAN_YEAR_BEGINNING", "PAYMENT_BEGINNING"))
blrs_a_2017 <- blrs_a_2017[!is.na(blrs_a_2017$TYP_SYS),] #remove total row
blrs_a_2017$C_V <- NA
blrs_a_2017$VAL_DATE <- ymd(blrs_a_2017$VAL_DATE)
blrs_a_2017$PLAN_YEAR_BEGINNING <- ymd(blrs_a_2017$PLAN_YEAR_BEGINNING)
blrs_a_2017$PAYMENT_BEGINNING <- ymd(blrs_a_2017$PAYMENT_BEGINNING)
blrs_a_2017$REPORT_YR <- as.numeric("2017")


#### Part 2. Bind each year into a single table & relabel variables

blrs_a <- bind_rows(blrs_a_2005, blrs_a_2006, blrs_a_2007, blrs_a_2008, blrs_a_2009, blrs_a_2010, blrs_a_2011, blrs_a_2012, blrs_a_2013, blrs_a_2014, blrs_a_2015, blrs_a_2016, blrs_a_2017)

## reorder and rename columns

blrs_a <- blrs_a %>%
  select(plan_sponsor = CITY_OR_DISTRICT, 
         plan_type_a = TYP_SYS, 
         val_date_a = VAL_DATE, 
         report_year_a = REPORT_YR,
         total_assets_a = TOTAL_ASSETS,
         uaal_a = UAAL,
         val_payroll_a = VAL_PYROLL,
         retired_payroll_a = RETD_PYROLL,
         normal_cost_a = NORMAL_COST,
         normal_percent_a = NORM_PERC,
         uaal_pymt_a = UAAL_PYMT,
         uaal_percent_a = UAAL_PERC,
         reqd_contribution_a = REQD_CONT,
         reqd_percent_a = REQD_PERC,
         member_percent_a = MEMB_PERC,
         city_percent_a = CITY_PERC,
         other_percent_a = OTHR_PERC,
         plan_year_begin_a = PLAN_YEAR_BEGINNING,
         pmt_begin_a = PAYMENT_BEGINNING,
         current_val_a = C_V)

blrs_a$plan_sponsor <- str_trim(blrs_a$plan_sponsor)
blrs_a$plan_sponsor <- str_squish(blrs_a$plan_sponsor)

blrs_a <- blrs_a[blrs_a$val_date_a>="2005-01-01",]
blrs_a$plan_type_a[blrs_a$plan_sponsor=="PALM BEACH LG-R"] <- "LG"

#### Part 4. Ensure clean identifiers

### Use this Excel file to identify sponsor names to change

sponsor_names <- table(blrs_a$plan_sponsor)
#print(sponsor_names)
sponsor_names <- as.data.frame(sponsor_names)
sponsor_names <- sponsor_names %>%
  rename(plan_sponsor = Var1) %>%
  mutate(plan_sponsor_clean = plan_sponsor) 
write_excel_csv(sponsor_names, "MatchingTables/appendix_a_to_match.csv")

### import comparison table
name_table <- read_excel("MatchingTables/appendix_a_matched.xls")
name_table <- name_table %>% 
  select(plan_sponsor, plan_sponsor_clean)

### consistent names for plan_type

blrs_a$plan_type_a[blrs_a$plan_type_a=="G&S"] <- "GS"

blrs_a <- right_join(name_table, blrs_a, by = "plan_sponsor")  

blrs_a <- arrange(blrs_a, plan_sponsor_clean, plan_type_a, val_date_a, report_year_a)

blrs_a <- blrs_a %>% rename(plan_sponsor_a = plan_sponsor)

blrs_a <- blrs_a %>%
  group_by(plan_sponsor_clean, plan_type_a, val_date_a) %>%
  filter(report_year_a == max(report_year_a))

    





