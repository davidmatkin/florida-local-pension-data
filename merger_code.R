# This file is developed to contruct Florida local pension data (2005 to 2017)
# Author:  Youngsung Kim
# Date:    3/5/2018 ~

### Load libraries
library(tidyverse) #includes "dplyr" library
library(stringr)
library(readxl)
library(lubridate)
library(purrr)
#install.packages("tidyverse")
#install.packages("xlsx")
#library(xlsx)
#install.packages("purrr")

setwd("C:/Users/blued/Google Drive/FL_LocalPensionReports") #You can change working directory here and the rest of codes will work in your setting.

### This code consists of three parts. 
### In the first part, I imported every table for every year (8 tables from 2005 to 2017)
### In the second, I rbinded every year for the same table
### In the third, I full_joined every rbinded tables.

#### Part 1. Date importing
### First, I imported execl files.
### Second, I changed data frame to tibble
### Third, I changed variable names for rbinding them later
### Fourth, I changed date types and formats into yyyy-mm-dd", which is R's default data format.
### Fianally, I dropped empty rows and columns.
### 2005 data
## Appendix A
blrs_a_2005 <- read_excel("2005 Report/2005-blrs-a.xls")
as_tibble(blrs_a_2005)
blrs_a_2005 <- blrs_a_2005[!is.na(blrs_a_2005$CITY_OR_DISTRICT),]
blrs_a_2005$VAL_DATE <- ymd(blrs_a_2005$VAL_DATE)
blrs_a_2005 <- blrs_a_2005 %>% #change date format
  mutate(PLAN_YEAR_BEGINNING=as.Date(PLAN_YEAR_BEGINNING, origin="1899-12-30"), PAYMENT_BEGINNING=as.Date(PAYMENT_BEGINNING, origin="1899-12-30")) 

## Appendix B1
blrs_b1_2005 <- read_excel("2005 Report/2005-blrs-b1.xls")
as_tibble(blrs_b1_2005)
blrs_b1_2005 <- blrs_b1_2005[!is.na(blrs_b1_2005$CITY_OR_DISTRICT),]
blrs_b1_2005$CITY_OR_DISTRICT <- str_replace(blrs_b1_2005$CITY_OR_DISTRICT, "EDGEW00D", "EDGEWOOD")

## Appendix B2
blrs_b2_2005 <- read_excel("2005 Report/2005-blrs-b2.xls")
as_tibble(blrs_b2_2005)
blrs_b2_2005 <- blrs_b2_2005[!is.na(blrs_b2_2005$CITY_OR_DISTRICT),]

## Appendix C
blrs_c_2005 <- read_excel("2005 Report/2005-blrs-c.xls")
as_tibble(blrs_c_2005)
names(blrs_c_2005)
blrs_c_2005 <- blrs_c_2005 %>% 
  select(-X__1)
blrs_c_2005$VAL_DATE <- ymd(blrs_c_2005$VAL_DATE)
blrs_c_2005 <- blrs_c_2005[!is.na(blrs_c_2005$TYP_SYS),]

## Appendix D
blrs_d_2005 <- read_excel("2005 Report/2005-blrs-d.xls")
as_tibble(blrs_d_2005)
names(blrs_d_2005)
blrs_d_2005$VAL_DATE <- ymd(blrs_d_2005$VAL_DATE)
blrs_d_2005 <- blrs_d_2005[!is.na(blrs_d_2005$TYP_SYS),]

## Appendix E
blrs_e_2005 <- read_excel("2005 Report/2005-blrs-e.xls")
as_tibble(blrs_e_2005)
names(blrs_e_2005)
blrs_e_2005$VAL_DATE <- ymd(blrs_e_2005$VAL_DATE)
blrs_e_2005 <- mutate_at(blrs_e_2005, vars(PLNYR_ENDED), funs(as.numeric))
blrs_e_2005 <- blrs_e_2005 %>% #change date format
  mutate(PLNYR_ENDED=as.Date(PLNYR_ENDED, origin="1899-12-30")) 
blrs_e_2005 <- blrs_e_2005[!is.na(blrs_e_2005$TYP_SYS),]
#sum(is.na(blrs_e_2005$PLNYR_ENDED)) #two missing PLNYR_ENDED: CORAL GABLES_F, QUINCY_G 
#blrs_e_2005 <- blrs_e_2005[is.na(blrs_e_2005$PLNYR_ENDED),]

## Appendix F
blrs_f_2005 <- read_excel("2005 Report/2005-blrs-f.xls")
as_tibble(blrs_f_2005)
names(blrs_f_2005)
blrs_f_2005 <- blrs_f_2005[!is.na(blrs_f_2005$CITY_OR_DISTRICT),]

## Appendix G
blrs_g_2005 <- read_excel("2005 Report/2005-blrs-g.xls")
as_tibble(blrs_g_2005)
names(blrs_g_2005)
blrs_g_2005$VAL_DATE <- ymd(blrs_g_2005$VAL_DATE)
blrs_g_2005 <- blrs_g_2005[!is.na(blrs_g_2005$CITY_OR_DISTRICT),]


### 2006 data
## Appendix A
blrs_a_2006 <- read_excel("2006 Report/2006-03 A - Financial Contribution Data - 14b.xls")
as_tibble(blrs_a_2006)
names(blrs_a_2006)
#rename for appending
blrs_a_2006 <- blrs_a_2006 %>%
  rename("CITY_OR_DISTRICT" = "CITY OR") %>%
  rename(TYP_SYS = TYP) %>%
  rename(C_V = X__1) %>%
  rename(VAL_DATE = VAL) %>%
  rename(TOTAL_ASSETS = TOTAL) %>%
  rename(UAAL_PYMT = UAAL) %>%
  rename(UAAL = X__2) %>%
  rename(VAL_PYROLL = VAL__1) %>%
  rename(RETD_PYROLL = RETIRED) %>%
  rename("NORMAL_COST" = "NORMAL COST") %>%
  rename(NORM_PERC = X__3) %>%
  rename(UAAL_PERC = X__5) %>%
  rename("REQD_CONT" = "REQ'D CONT") %>%
  rename(REQD_PERC = X__7) %>%
  rename(MEMB_PERC = MEMBER) %>%
  rename(CITY_PERC = CITY) %>%
  rename(OTHR_PERC = OTHER) %>%
  rename("PLAN_YEAR_BEGINNING" = "PLAN YR") %>%
  rename(PAYMENT_BEGINNING = PYMT) 
blrs_a_2006 <- blrs_a_2006[!is.na(blrs_a_2006$CITY_OR_DISTRICT),]
blrs_a_2006 <- blrs_a_2006[(blrs_a_2006$CITY_OR_DISTRICT!="DISTRICT"),]
blrs_a_2006 <- blrs_a_2006 %>% 
  select(-X__4, -X__6)
blrs_a_2006 <- mutate_at(blrs_a_2006, vars(VAL_DATE, PLAN_YEAR_BEGINNING, PAYMENT_BEGINNING), funs(as.numeric))
blrs_a_2006 <- blrs_a_2006 %>% #change date format
  mutate(VAL_DATE=as.Date(VAL_DATE, origin="1899-12-30"), PLAN_YEAR_BEGINNING=as.Date(PLAN_YEAR_BEGINNING, origin="1899-12-30"), PAYMENT_BEGINNING=as.Date(PAYMENT_BEGINNING, origin="1899-12-30")) 

## Appendix B1
blrs_b1_2006 <- read_excel("2006 Report/2006-04 B - Benefit Data - 14c.xls")
as_tibble(blrs_b1_2006)
names(blrs_b1_2006)
blrs_b1_2006 <- blrs_b1_2006 %>%
  rename("CITY_OR_DISTRICT" = "CITY OR") %>%
  rename("TYP_SYS" = "TYP") %>%
  rename("RETIREMENT_BENEFIT_RATE_PERC" = "RETIREMENT BENEFIT RATE") %>%
  rename("RETIREMENT_BENEFIT_RATE_DESC" = "X__1") %>%
  rename("AFC" = "X__2") %>%
  rename("YR" = "NORMAL RETIREMENT AGE") %>%
  rename("NORMAL_RETIREMENT_AGE_DESC" = "X__3") %>%
  rename("N" = "NORMAL FORM OF BENEFIT") %>%
  rename("NORMAL_FORM_OF_BENEFIT_DESC" = "X__5") %>%
  rename("VS" = "X__6") %>%
  rename("SS" = "X__7") %>%
  rename("COL" = "X__8") %>%
  rename("COMMENTS" = "X__9") 
blrs_b1_2006 <- blrs_b1_2006[!is.na(blrs_b1_2006$CITY_OR_DISTRICT),]
blrs_b1_2006 <- blrs_b1_2006[(blrs_b1_2006$CITY_OR_DISTRICT!="DISTRICT"),]
blrs_b1_2006 <- blrs_b1_2006 %>% 
  select(-X__4)
blrs_b1_2006$CITY_OR_DISTRICT <- str_replace(blrs_b1_2006$CITY_OR_DISTRICT, "EDGEW00D", "EDGEWOOD")

## Appendix B2 does not exist for 2006

## Appendix C
blrs_c_2006 <- read_excel("2006 Report/2006-05 C - Market Value of Assets - 14h.xls")
as_tibble(blrs_c_2006)
names(blrs_c_2006)
blrs_c_2006 <- blrs_c_2006 %>%
  rename("CITY_OR_DISTRICT" = "CITY OR") %>%
  rename("TYP_SYS" = "TYP") %>%
  rename("VAL_DATE" = "VAL") %>%
  rename("ASSETS_EQUITIES" = "ASSETS") %>%
  rename("ASSETS_CASH_EQUIV" = "X__1") %>%
  rename("ASSETS_INSURANCE" = "X__2") %>%
  rename("ASSETS_FIXED_INC" = "X__3") %>%
  rename("TOTAL_ASSETS" = "X__4") %>%
  rename("OTHER" = "X__5")
blrs_c_2006 <- blrs_c_2006[!is.na(blrs_c_2006$TYP_SYS),]
blrs_c_2006 <- blrs_c_2006[(blrs_c_2006$TYP_SYS!="SYS"),]
blrs_c_2006 <- mutate_at(blrs_c_2006, vars(VAL_DATE), funs(as.numeric))
blrs_c_2006 <- blrs_c_2006 %>% #change date format
  mutate(VAL_DATE=as.Date(VAL_DATE, origin="1899-12-30")) 

## Appendix D
blrs_d_2006 <- read_excel("2006 Report/2006-06 D - Funding Progress (FASB-35 basis) - 64h.xls")
as_tibble(blrs_d_2006)
names(blrs_d_2006)
blrs_d_2006 <- blrs_d_2006 %>%
  rename("CITY_OR_DISTRICT" = "CITY OR") %>%
  rename("TYP_SYS" = "TYP") %>%
  rename("VAL_DATE" = "VAL") %>%
  rename("C_V" = "X__1") %>%
  rename("TOTAL_ASSETS" = "TOTAL") %>%
  rename("LIABILITY_FOR_RET_BEN" = "LIABILITY FOR") %>%
  rename("LIABILITY_FOR_EE_CONTRIB" = "X__2") %>%
  rename("LIABILITY_FOR_ACTIVES" = "X__3") %>%
  rename("RATIO_ASSETS_LIABILITIES_INACT" = "RATIO OF ASSETS TO LIABILITIES FOR") %>%
  rename("RATIO_ASSETS_LIABILITIES_CONTR" = "X__5") %>%
  rename("RATIO_ASSETS_LIABILITIES_VESTD" = "X__6") %>%
  rename("RATIO_ASSETS_LIABILITIES_NVSTD" = "X__7") %>%
  rename("RATIO_ASSETS_LIABILITIES_ACTVS" = "X__8") %>%
  rename("COMMENTS" = "X__9")
blrs_d_2006 <- blrs_d_2006[!is.na(blrs_d_2006$TYP_SYS),]
blrs_d_2006 <- blrs_d_2006[(blrs_d_2006$TYP_SYS!="SYS"),]
blrs_d_2006 <- blrs_d_2006 %>% 
  select(-X__4)
blrs_d_2006 <- mutate_at(blrs_d_2006, vars(VAL_DATE), funs(as.numeric))
blrs_d_2006 <- blrs_d_2006 %>% #change date format
  mutate(VAL_DATE=as.Date(VAL_DATE, origin="1899-12-30")) 

## Appendix E
blrs_e_2006 <- read_excel("2006 Report/2006-07 E - Actuarial Data - 14e.xls")
as_tibble(blrs_e_2006)
names(blrs_e_2006)
blrs_e_2006 <- blrs_e_2006 %>%
  rename("CITY_OR_DISTRICT" = "CITY OR") %>%
  rename("TYP_SYS" = "TYP") %>%
  rename("VAL_DATE" = "VAL") %>%
  rename("FUND_METHOD" = "FUND") %>%
  rename("OLD_PLN" = "OLD") %>%
  rename("PLNYR_ENDED" = "PLAN YR") %>%
  rename("SALRY_ASSMP" = "SALARY") %>%
  rename("SALRY_ACTUL" = "SALARY__1") %>%
  rename("INT_ASSMP" = "INTEREST") %>%
  rename("INT_ACTUL" = "INTEREST__1") %>%
  rename("PR_GR_ASSMP" = "P/R GROWTH") %>%
  rename("RETIREMENT_AGE_ASSUMPTION_YR" = "RET. AGE") %>%
  rename("RETIREMENT_AGE_ASSUMPTION_DESC" = "RET. AGE__1") 
blrs_e_2006 <- blrs_e_2006[!is.na(blrs_e_2006$TYP_SYS),]
blrs_e_2006 <- blrs_e_2006[(blrs_e_2006$TYP_SYS!="SYS"),]
blrs_e_2006 <- mutate_at(blrs_e_2006, vars(VAL_DATE, PLNYR_ENDED), funs(as.numeric))
blrs_e_2006 <- blrs_e_2006 %>% #change date format
  mutate(VAL_DATE=as.Date(VAL_DATE, origin="1899-12-30"), PLNYR_ENDED=as.Date(PLNYR_ENDED, origin="1899-12-30")) 

## Appendix F
blrs_f_2006 <- read_excel("2006 Report/2006-08 F - Population Data - 14f.xls")
as_tibble(blrs_f_2006)
names(blrs_f_2006)
blrs_f_2006 <- blrs_f_2006 %>%
  rename("CITY_OR_DISTRICT" = "CITY OR") %>%
  rename("TYP_SYS" = "TYP") %>%
  rename("CITY_POPULATN" = "CITY") %>%
  rename("COUNTY_NAME" = "COUNTY") %>%
  rename("COUNTY_POPULATION" = "COUNTY__1") %>%
  rename("MEMBR_ACTIV" = "MEMBERS") %>%
  rename("MEMBR_RETRD" = "MEMBERS__1") %>%
  rename("MEMBR_TERMD" = "MEMBERS__2") %>%
  rename("COMMENTS" = "X__1")
blrs_f_2006 <- blrs_f_2006[!is.na(blrs_f_2006$CITY_OR_DISTRICT),]
blrs_f_2006 <- blrs_f_2006[(blrs_f_2006$CITY_OR_DISTRICT!="DISTRICT"),]
sum(is.na(blrs_f_2006$TYP_SYS))

## Appendix G
blrs_g_2006 <- read_excel("2006 Report/2006-09 G - Funding Progress (GASB-25 basis) - 64e.xls")
as_tibble(blrs_g_2006)
names(blrs_g_2006)
blrs_g_2006 <- blrs_g_2006 %>%
  rename("CITY_OR_DISTRICT" = "CITY OR") %>%
  rename("TYP_SYS" = "TYP") %>%
  rename("VAL_DATE" = "VAL") %>%
  rename("C_V" = "X__1") %>%
  rename("FUND_METHOD" = "FUNDING") %>%
  rename("TOTAL_ASSETS" = "TOTAL") %>%
  rename("AAL" = "X__2") %>%
  rename("UAAL" = "X__3") %>%
  rename("FUNDED_RATIO" = "FUNDED") %>%
  rename("COVERED_PAYROLL" = "COVERED") %>%
  rename("UAAL_AS_PERC_OF_COVD_PAY" = "UAAL AS % OF") %>%
  rename("COMMENTS" = "X__4")
blrs_g_2006 <- blrs_g_2006[!is.na(blrs_g_2006$CITY_OR_DISTRICT),]
blrs_g_2006 <- blrs_g_2006[(blrs_g_2006$CITY_OR_DISTRICT!="DISTRICT"),]
blrs_g_2006 <- mutate_at(blrs_g_2006, vars(VAL_DATE), funs(as.numeric))
blrs_g_2006 <- blrs_g_2006 %>% #change date format
  mutate(VAL_DATE=as.Date(VAL_DATE, origin="1899-12-30")) 



### 2007 data
## Appendix A
blrs_a_2007 <- read_excel("2007 Report/Appendix_A.xls")
as_tibble(blrs_a_2007)
blrs_a_2007 <- blrs_a_2007[!is.na(blrs_a_2007$CITY_OR_DISTRICT),]
blrs_a_2007$VAL_DATE <- ymd(blrs_a_2007$VAL_DATE)
blrs_a_2007 <- blrs_a_2007 %>% #change date format
  mutate(PLAN_YEAR_BEGINNING=as.Date(PLAN_YEAR_BEGINNING, origin="1899-12-30"), PAYMENT_BEGINNING=as.Date(PAYMENT_BEGINNING, origin="1899-12-30")) 

## Appendix B1
blrs_b1_2007 <- read_excel("2007 Report/Appendix_B.xls")
as_tibble(blrs_b1_2007)
names(blrs_b1_2007)
blrs_b1_2007 <- blrs_b1_2007[!is.na(blrs_b1_2007$CITY_OR_DISTRICT),]
blrs_b1_2007$CITY_OR_DISTRICT <- str_replace(blrs_b1_2007$CITY_OR_DISTRICT, "EDGEW00D", "EDGEWOOD")

## Appendix B2
blrs_b2_2007 <- read_excel("2007 Report/Appendix_B_Other.xls")
as_tibble(blrs_b2_2007)
names(blrs_b2_2007)
blrs_b2_2007 <- blrs_b2_2007[!is.na(blrs_b2_2007$CITY_OR_DISTRICT),]

## Appendix C
blrs_c_2007 <- read_excel("2007 Report/Appendix_C.xls")
as_tibble(blrs_c_2007)
names(blrs_c_2007)
blrs_c_2007$VAL_DATE <- ymd(blrs_c_2007$VAL_DATE)
blrs_c_2007 <- blrs_c_2007[!is.na(blrs_c_2007$TYP_SYS),]

## Appendix D
blrs_d_2007 <- read_excel("2007 Report/Appendix_D.xls")
as_tibble(blrs_d_2007)
names(blrs_d_2007)
blrs_d_2007$VAL_DATE <- ymd(blrs_d_2007$VAL_DATE)
blrs_d_2007 <- blrs_d_2007[!is.na(blrs_d_2007$TYP_SYS),]

## Appendix E
blrs_e_2007 <- read_excel("2007 Report/Appendix_E.xls")
as_tibble(blrs_e_2007)
names(blrs_e_2007)
blrs_e_2007$VAL_DATE <- ymd(blrs_e_2007$VAL_DATE)
blrs_e_2007$PLNYR_ENDED <- ymd(blrs_e_2007$PLNYR_ENDED)
blrs_e_2007 <- blrs_e_2007[!is.na(blrs_e_2007$TYP_SYS),]

## Appendix F
blrs_f_2007 <- read_excel("2007 Report/Appendix_F.xls")
as_tibble(blrs_f_2007)
names(blrs_f_2007)
blrs_f_2007 <- blrs_f_2007[!is.na(blrs_f_2007$CITY_OR_DISTRICT),]

## Appendix G
blrs_g_2007 <- read_excel("2007 Report/Appendix_G.xls")
as_tibble(blrs_g_2007)
names(blrs_g_2007)
blrs_g_2007 <- blrs_g_2007 %>% 
  select(-X__1, -X__2)
blrs_g_2007 <- blrs_g_2007[!is.na(blrs_g_2007$CITY_OR_DISTRICT),]
blrs_g_2007$VAL_DATE <- ymd(blrs_g_2007$VAL_DATE)



### 2008 data
## Appendix A
blrs_a_2008 <- read_excel("2008 Report/Appendix_A(1).xls")
as_tibble(blrs_a_2008)
blrs_a_2008 <- blrs_a_2008[!is.na(blrs_a_2008$CITY_OR_DISTRICT),]
blrs_a_2008$VAL_DATE <- ymd(blrs_a_2008$VAL_DATE)
blrs_a_2008$PAYMENT_BEGINNING <- ymd(blrs_a_2008$PAYMENT_BEGINNING)
blrs_a_2008 <- mutate_at(blrs_a_2008, vars(PLAN_YEAR_BEGINNING), funs(as.numeric))
blrs_a_2008 <- blrs_a_2008 %>% #change date format
  mutate(PLAN_YEAR_BEGINNING=as.Date(PLAN_YEAR_BEGINNING, origin="1899-12-30")) 
blrs_a_2008$PLAN_YEAR_BEGINNING[is.na(blrs_a_2008$PLAN_YEAR_BEGINNING)] <- mdy("10-01-08") #input for missing date

## Appendix B1
blrs_b1_2008 <- read_excel("2008 Report/Appendix_B1(1).xls")
as_tibble(blrs_b1_2008)
names(blrs_b1_2008)
blrs_b1_2008 <- blrs_b1_2008[!is.na(blrs_b1_2008$CITY_OR_DISTRICT),]

## Appendix B2
blrs_b2_2008 <- read_excel("2008 Report/Appendix_B2.xls") #I Just opened the original file and saved it again at the same file. After that, the file worked!
as_tibble(blrs_b2_2008)
names(blrs_b2_2008)
blrs_b2_2008 <- blrs_b2_2008[!is.na(blrs_b2_2008$CITY_OR_DISTRICT),]

## Appendix C
blrs_c_2008 <- read_excel("2008 Report/Appendix_C(1).xls")
as_tibble(blrs_c_2008)
names(blrs_c_2008)
blrs_c_2008$VAL_DATE <- ymd(blrs_c_2008$VAL_DATE)
blrs_c_2008 <- blrs_c_2008[!is.na(blrs_c_2008$TYP_SYS),]

## Appendix D
blrs_d_2008 <- read_excel("2008 Report/Appendix_D(1).xls")
as_tibble(blrs_d_2008)
names(blrs_d_2008)
blrs_d_2008$VAL_DATE <- ymd(blrs_d_2008$VAL_DATE)
blrs_d_2008 <- blrs_d_2008[!is.na(blrs_d_2008$TYP_SYS),]

## Appendix E
blrs_e_2008 <- read_excel("2008 Report/Appendix_E(1).xls")
as_tibble(blrs_e_2008)
names(blrs_e_2008)
blrs_e_2008$VAL_DATE <- ymd(blrs_e_2008$VAL_DATE)
blrs_e_2008$PLNYR_ENDED <- ymd(blrs_e_2008$PLNYR_ENDED)
blrs_e_2008 <- blrs_e_2008[!is.na(blrs_e_2008$TYP_SYS),]

## Appendix F
blrs_f_2008 <- read_excel("2008 Report/Appendix_F(1).xls")
as_tibble(blrs_f_2008)
names(blrs_f_2008)
blrs_f_2008 <- blrs_f_2008[!is.na(blrs_f_2008$CITY_OR_DISTRICT),]

## Appendix G
blrs_g_2008 <- read_excel("2008 Report/Appendix_G(1).xls")
as_tibble(blrs_g_2008)
names(blrs_g_2008)
blrs_g_2008 <- blrs_g_2008[!is.na(blrs_g_2008$CITY_OR_DISTRICT),]
blrs_g_2008$VAL_DATE <- ymd(blrs_g_2008$VAL_DATE)



### 2009 data
## Appendix A
blrs_a_2009 <- read_excel("2009 Report/Appendix_A.xls")
as_tibble(blrs_a_2009)
names(blrs_a_2009)
#rename for appending
blrs_a_2009 <- blrs_a_2009 %>%
  rename("CITY_OR_DISTRICT" = "CityName") %>%
  rename(TYP_SYS = PlanType)
blrs_a_2009$VAL_DATE <- ymd(blrs_a_2009$VAL_DATE)
blrs_a_2009$PLAN_YEAR_BEGINNING <- ymd(blrs_a_2009$PLAN_YEAR_BEGINNING)
blrs_a_2009$PAYMENT_BEGINNING <- ymd(blrs_a_2009$PAYMENT_BEGINNING)
blrs_a_2009 <- blrs_a_2009[!is.na(blrs_a_2009$CITY_OR_DISTRICT),]

## Appendix B1
blrs_b1_2009 <- read_excel("2009 Report/Appendix_B1.xls")
as_tibble(blrs_b1_2009)
names(blrs_b1_2009)
blrs_b1_2009 <- blrs_b1_2009[!is.na(blrs_b1_2009$CITY_OR_DISTRICT),]

## Appendix B2
blrs_b2_2009 <- read_excel("2009 Report/Appendix_B2.xls")
as_tibble(blrs_b2_2009)
names(blrs_b2_2009)
blrs_b2_2009 <- blrs_b2_2009[!is.na(blrs_b2_2009$CITY_OR_DISTRICT),]

## Appendix C
blrs_c_2009 <- read_excel("2009 Report/Appendix_C.xls")
as_tibble(blrs_c_2009)
names(blrs_c_2009)
blrs_c_2009$VAL_DATE <- ymd(blrs_c_2009$VAL_DATE)
blrs_c_2009 <- blrs_c_2009[!is.na(blrs_c_2009$TYP_SYS),]

## Appendix D
blrs_d_2009 <- read_excel("2009 Report/Appendix_D.xls")
as_tibble(blrs_d_2009)
names(blrs_d_2009)
blrs_d_2009$VAL_DATE <- ymd(blrs_d_2009$VAL_DATE)
blrs_d_2009 <- blrs_d_2009[!is.na(blrs_d_2009$TYP_SYS),]

## Appendix E
blrs_e_2009 <- read_excel("2009 Report/Appendix_E.xls")
as_tibble(blrs_e_2009)
names(blrs_e_2009)
blrs_e_2009$VAL_DATE <- ymd(blrs_e_2009$VAL_DATE)
blrs_e_2009$PLNYR_ENDED <- ymd(blrs_e_2009$PLNYR_ENDED)
blrs_e_2009 <- blrs_e_2009[!is.na(blrs_e_2009$TYP_SYS),]

## Appendix F
blrs_f_2009 <- read_excel("2009 Report/Appendix_F.xls")
as_tibble(blrs_f_2009)
names(blrs_f_2009)
blrs_f_2009 <- blrs_f_2009[!is.na(blrs_f_2009$CITY_OR_DISTRICT),]

## Appendix G
blrs_g_2009 <- read_excel("2009 Report/Appendix_G.xls")
as_tibble(blrs_g_2009)
names(blrs_g_2009)
blrs_g_2009 <- blrs_g_2009[!is.na(blrs_g_2009$CITY_OR_DISTRICT),]
blrs_g_2009$VAL_DATE <- ymd(blrs_g_2009$VAL_DATE)



### 2010 data
## Appendix A
blrs_a_2010 <- read_excel("2010 Report/Appendix_A.xls")
as_tibble(blrs_a_2010)
names(blrs_a_2010)
#rename for appending
blrs_a_2010 <- blrs_a_2010 %>%
  rename("CITY_OR_DISTRICT" = "CityName") %>%
  rename("TYP_SYS" = "PlanType")
blrs_a_2010 <- blrs_a_2010[!is.na(blrs_a_2010$CITY_OR_DISTRICT),] #remove total row
blrs_a_2010$VAL_DATE <- ymd(blrs_a_2010$VAL_DATE)
blrs_a_2010$PLAN_YEAR_BEGINNING <- ymd(blrs_a_2010$PLAN_YEAR_BEGINNING)
blrs_a_2010$PAYMENT_BEGINNING <- ymd(blrs_a_2010$PAYMENT_BEGINNING)

## Appendix B1
blrs_b1_2010 <- read_excel("2010 Report/Appendix_B1.xls")
as_tibble(blrs_b1_2010)
names(blrs_b1_2010)
blrs_b1_2010 <- blrs_b1_2010 %>%
  rename("RETIREMENT_BENEFIT_RATE_PERC" = "TxtRETIREMENT_BENEFIT_RATE_PERC") %>%
  rename("RETIREMENT_BENEFIT_RATE_DESC" = "TxtRETIREMENT_BENEFIT_RATE_DESC")
blrs_b1_2010 <- blrs_b1_2010[!is.na(blrs_b1_2010$CITY_OR_DISTRICT),]

## Appendix B2
blrs_b2_2010 <- read_excel("2010 Report/Appendix_B2.xls")
as_tibble(blrs_b2_2010)
names(blrs_b2_2010)
blrs_b2_2010 <- blrs_b2_2010 %>%
  rename("DISABILITY_BENEFITS_Y" = "TxtDISABILITY_BENEFITS_Y") %>%
  rename("EARLY_RETIREMENT_BENEFITS_Y" = "TxtEARLY_RETIREMENT_BENEFITS_Y")
blrs_b2_2010 <- blrs_b2_2010[!is.na(blrs_b2_2010$CITY_OR_DISTRICT),]

## Appendix C
blrs_c_2010 <- read_excel("2010 Report/Appendix_C.xls")
as_tibble(blrs_c_2010)
names(blrs_c_2010)
blrs_c_2010 <- blrs_c_2010 %>%
  rename("ASSETS_EQUITIES" = "TxtASSETS_EQUITIES") %>%
  rename("ASSETS_CASH_EQUIV" = "TxtASSETS_CASH_EQUIV") %>%
  rename("ASSETS_INSURANCE" = "TxtASSETS_INSURANCE") %>%
  rename("ASSETS_FIXED_INC" = "TxtASSETS_FIXED_INC") %>%
  rename("TOTAL_ASSETS" = "TxtTOTAL_ASSETS") %>%
  rename("drop_assets_amt" = "Txtdrop_assets_amt")
blrs_c_2010$VAL_DATE <- ymd(blrs_c_2010$VAL_DATE)
blrs_c_2010 <- blrs_c_2010[!is.na(blrs_c_2010$TYP_SYS),]

## Appendix D - I created xls file using pdf because no appendix D file existed.
blrs_d_2010 <- read_excel("2010 Report/Appendix_D_modified.xls")
as_tibble(blrs_d_2010)
names(blrs_d_2010)
blrs_d_2010 <- blrs_d_2010 %>%
  rename("CITY_OR_DISTRICT" = "FUNDING PROGRESS (FASB-35 BASIS)") %>%
  rename("TYP_SYS" = "X__1") %>%
  rename("VAL_DATE" = "X__2") %>%
  rename("TOTAL_ASSETS" = "X__5") %>%
  rename("LIABILITY_FOR_RET_BEN" = "X__6") %>%
  rename("LIABILITY_FOR_EE_CONTRIB" = "X__7") %>%
  rename("LIABILITY_FOR_ACTIVES" = "X__8") %>%
  rename("RATIO_ASSETS_LIABILITIES_INACT" = "X__9") %>%
  rename("RATIO_ASSETS_LIABILITIES_CONTR" = "X__10") %>%
  rename("RATIO_ASSETS_LIABILITIES_ACTVS" = "X__11") %>%
  rename("COMMENTS" = "X__12") %>%
  rename("RevisedIndicator" = "X__3") %>%
  rename("PlanChange" = "X__4")
blrs_d_2010 <- blrs_d_2010[!is.na(blrs_d_2010$TYP_SYS),]
blrs_d_2010 <- blrs_d_2010[(blrs_d_2010$TYP_SYS!="TYP"),]
blrs_d_2010 <- blrs_d_2010[(blrs_d_2010$TYP_SYS!="SYS"),]
blrs_d_2010 <- blrs_d_2010[(blrs_d_2010$TYP_SYS!="Page 87 of 87"),]
blrs_d_2010 <- blrs_d_2010 %>% 
  select(-X__13)
blrs_d_2010$VAL_DATE <- mdy(blrs_d_2010$VAL_DATE)
sum(is.na(blrs_d_2010$CITY_OR_DISTRICT))
sum(is.na(blrs_d_2010$TYP_SYS))

## Appendix E
blrs_e_2010 <- read_excel("2010 Report/Appendix_E.xls")
as_tibble(blrs_e_2010)
names(blrs_e_2010)
blrs_e_2010$VAL_DATE <- ymd(blrs_e_2010$VAL_DATE)
blrs_e_2010$PLNYR_ENDED <- ymd(blrs_e_2010$PLNYR_ENDED)
blrs_e_2010 <- blrs_e_2010[!is.na(blrs_e_2010$TYP_SYS),]

## Appendix F
blrs_f_2010 <- read_excel("2010 Report/Appendix_F.xls")
as_tibble(blrs_f_2010)
names(blrs_f_2010)
blrs_f_2010 <- blrs_f_2010 %>%
  rename("COUNTY_POPULATION" = "TxtCOUNTY_POPULATION") %>%
  rename("MEMBR_ACTIV" = "TxtMEMBR_ACTIV") %>%
  rename("MEMBR_RETRD" = "TxtMEMBR_RETRD") %>%
  rename("MEMBR_TERMD" = "TxtMEMBR_TERMD")
blrs_f_2010 <- blrs_f_2010[!is.na(blrs_f_2010$CITY_OR_DISTRICT),]

## Appendix G
blrs_g_2010 <- read_excel("2010 Report/Appendix_G.xls")
as_tibble(blrs_g_2010)
names(blrs_g_2010)
blrs_g_2010 <- blrs_g_2010[!is.na(blrs_g_2010$CITY_OR_DISTRICT),]
blrs_g_2010$VAL_DATE <- ymd(blrs_g_2010$VAL_DATE)



### 2011 data (For this year, I resaved xlsx file to xls file)
## Appendix A
blrs_a_2011 <- read_excel("2011 Report/Appendix_A.xls")
as_tibble(blrs_a_2011)
names(blrs_a_2011)
blrs_a_2011 <- blrs_a_2011 %>%
  rename("CITY_OR_DISTRICT" = "City or District") %>%
  rename("TYP_SYS" = "Type") %>%
  rename("VAL_DATE" = "Valuation\nDate") %>%
  rename("TOTAL_ASSETS" = "Total\nAssets") %>%
  rename("VAL_PYROLL" = "Valuation\nPayroll") %>%
  rename("RETD_PYROLL" = "Retired\nPayroll") %>%
  rename("NORMAL_COST" = "Normal\nCost") %>%
  rename("NORM_PERC" = "Normal\nPercent") %>%
  rename("UAAL_PYMT" = "UAAL\nPayment") %>%
  rename("UAAL_PERC" = "UAAL\nPercent") %>%
  rename("REQD_CONT" = "Required\nContribution") %>%
  rename("REQD_PERC" = "Required\nPercent") %>%
  rename("MEMB_PERC" = "Member\nPercent") %>%
  rename("CITY_PERC" = "City\nPercent") %>%
  rename("OTHR_PERC" = "Other\nPercent") %>%
  rename("PLAN_YEAR_BEGINNING" = "Plan Year\nBeginning") %>%
  rename("PAYMENT_BEGINNING" = "Payment\nBeginning")
blrs_a_2011 <- blrs_a_2011[!is.na(blrs_a_2011$CITY_OR_DISTRICT),] #remove total row
blrs_a_2011$VAL_DATE <- ymd(blrs_a_2011$VAL_DATE)
blrs_a_2011$PLAN_YEAR_BEGINNING <- ymd(blrs_a_2011$PLAN_YEAR_BEGINNING)
blrs_a_2011$PAYMENT_BEGINNING <- ymd(blrs_a_2011$PAYMENT_BEGINNING)

## Appendix B1
blrs_b1_2011 <- read_excel("2011 Report/Appendix_B1.xls")
as_tibble(blrs_b1_2011)
names(blrs_b1_2011)
blrs_b1_2011 <- blrs_b1_2011 %>%
  rename("CITY_OR_DISTRICT" = "City of District") %>%
  rename("TYP_SYS" = "Type") %>%
  rename("RETIREMENT_BENEFIT_RATE_PERC" = "Retirement Benefit\nRate") %>%
  rename("RETIREMENT_BENEFIT_RATE_DESC" = "Retirement Benefit\nDescription") %>%
  rename("YR" = "Normal Retirement\nYear") %>%
  rename("NORMAL_RETIREMENT_AGE_DESC" = "Normal Retirement\nDescription") %>%
  rename("N" = "Normal\nForm") %>%
  rename("NORMAL_FORM_OF_BENEFIT_DESC" = "Normal Form\nDescription") %>%
  rename("VS" = "Vesting\nSchedule") %>%
  rename("COL" = "Auto\nCOLA") %>%
  rename("COMMENTS" = "Comments")
blrs_b1_2011 <- blrs_b1_2011[!is.na(blrs_b1_2011$CITY_OR_DISTRICT),]
blrs_b1_2011 <- blrs_b1_2011[1:492,]

## Appendix B2
blrs_b2_2011 <- read_excel("2011 Report/Appendix_B2.xls")
as_tibble(blrs_b2_2011)
names(blrs_b2_2011)
blrs_b2_2011 <- blrs_b2_2011 %>%
  rename("CITY_OR_DISTRICT" = "City or District") %>%
  rename("TYP_SYS" = "Type") %>%
  rename("DISABILITY_BENEFITS_Y" = "Disability\nBenefits?") %>%
  rename("DISABILITY_BENEFITS_DESC" = "Disability Benefit\nDescription") %>%
  rename("DEATH_BENEFITS_Y" = "Death\nBenefits?") %>%
  rename("DEATH_BENEFITS_DESC" = "Death Benefits\nDescription") %>%
  rename("EARLY_RETIREMENT_BENEFITS_Y" = "Early Retirement\nBenefits?") %>%
  rename("EARLY_RETIREMENT_BENEFITS_DESC" = "Early Retirement Benefits\nDescription")
blrs_b2_2011 <- blrs_b2_2011[!is.na(blrs_b2_2011$TYP_SYS),]

## Appendix C
blrs_c_2011 <- read_excel("2011 Report/Appendix_C.xls")
as_tibble(blrs_c_2011)
names(blrs_c_2011)
blrs_c_2011 <- blrs_c_2011 %>%
  rename("CITY_OR_DISTRICT" = "City or District") %>%
  rename("TYP_SYS" = "Type") %>%
  rename("VAL_DATE" = "Valuation\nDate") %>%
  rename("ASSETS_EQUITIES" = "Assets\nEquities") %>%
  rename("ASSETS_CASH_EQUIV" = "Assets\nCash & Equiv") %>%
  rename("ASSETS_INSURANCE" = "Assets\nInsurance") %>%
  rename("ASSETS_FIXED_INC" = "Assets\nFixed Inc") %>%
  rename("TOTAL_ASSETS" = "Total\nAssets") %>%
  rename("COMMENTS" = "Comments") %>%
  rename("drop_assets_amt" = "DROP\nAmount")
blrs_c_2011$VAL_DATE <- ymd(blrs_c_2011$VAL_DATE)
blrs_c_2011 <- blrs_c_2011[!is.na(blrs_c_2011$TYP_SYS),]

## Appendix D 
blrs_d_2011 <- read_excel("2011 Report/Appendix_D.xls")
as_tibble(blrs_d_2011)
names(blrs_d_2011)
blrs_d_2011 <- blrs_d_2011 %>%
  rename("CITY_OR_DISTRICT" = "City or District") %>%
  rename("TYP_SYS" = "Type") %>%
  rename("VAL_DATE" = "Valuation\nDate") %>%
  rename("TOTAL_ASSETS" = "Market Value of\nAssets") %>%
  rename("LIABILITY_FOR_RET_BEN" = "Liability for\nRet & Ben") %>%
  rename("LIABILITY_FOR_EE_CONTRIB" = "Liability for\nEE Cont") %>%
  rename("LIABILITY_FOR_ACTIVES" = "Liability for\nActives") %>%
  rename("RATIO_ASSETS_LIABILITIES_INACT" = "Ratio\nAssets/Liabs\nInactive") %>%
  rename("RATIO_ASSETS_LIABILITIES_CONTR" = "Ratio\nAssets/Liabs\nContr") %>%
  rename("RATIO_ASSETS_LIABILITIES_ACTVS" = "Ratio\nAssets/Liabs\nActive") %>%
  rename("COMMENTS" = "Comments") %>%
  rename("RevisedIndicator" = "Revised") %>%
  rename("PlanChange" = "Plan\nChange") %>%
  rename("total_PAVB" = "Total PVAB") %>%
  rename("FASB35_ratio" = "FASB 35\nRatio")   
blrs_d_2011 <- blrs_d_2011[!is.na(blrs_d_2011$TYP_SYS),]
blrs_d_2011$VAL_DATE <- ymd(blrs_d_2011$VAL_DATE)

## Appendix E
blrs_e_2011 <- read_excel("2011 Report/Appendix_E.xls")
as_tibble(blrs_e_2011)
names(blrs_e_2011)
blrs_e_2011 <- blrs_e_2011 %>%
  rename("CITY_OR_DISTRICT" = "City or District") %>%
  rename("TYP_SYS" = "Type") %>%
  rename("VAL_DATE" = "Valuation\nDate") %>%
  rename("FUND_METHOD" = "Funding\nMethod") %>%
  rename("OLD_PLN" = "Old\nPlan") %>%
  rename("PLNYR_ENDED" = "Plan Year\nEnded") %>%
  rename("SALRY_ASSMP" = "Salary\nAssumption") %>%
  rename("SALRY_ACTUL" = "Salary\nActual") %>%
  rename("INT_ASSMP" = "Interest\nAssumption") %>%
  rename("INT_ACTUL" = "Interest\nActual") %>%
  rename("PR_GR_ASSMP" = "Payroll Growth\nAssumption") %>%
  rename("RETIREMENT_AGE_ASSUMPTION_YR" = "Retirement Age\nAssumption") %>%
  rename("RETIREMENT_AGE_ASSUMPTION_DESC" = "Retirement Age Assumption\nDescription") %>%
  rename("COMMENTS" = "Comments") 
blrs_e_2011$VAL_DATE <- ymd(blrs_e_2011$VAL_DATE)
blrs_e_2011$PLNYR_ENDED <- ymd(blrs_e_2011$PLNYR_ENDED)
blrs_e_2011 <- blrs_e_2011[!is.na(blrs_e_2011$TYP_SYS),]

## Appendix F
blrs_f_2011 <- read_excel("2011 Report/Appendix_F.xls")
as_tibble(blrs_f_2011)
names(blrs_f_2011)
blrs_f_2011 <- blrs_f_2011 %>%
  rename("CITY_OR_DISTRICT" = "City or District") %>%
  rename("TYP_SYS" = "Type") %>%
  rename("CITY_POPULATN" = "City\nPopulation") %>%
  rename("COUNTY_NAME" = "County\nName") %>%
  rename("COUNTY_POPULATION" = "County\nPopulation") %>%
  rename("MEMBR_ACTIV" = "Active\nMembers") %>%
  rename("MEMBR_RETRD" = "Retired\nMembers") %>%
  rename("MEMBR_TERMD" = "Terminated\nMembers") %>%
  rename("COMMENTS" = "Comments") 
blrs_f_2011 <- blrs_f_2011[!is.na(blrs_f_2011$CITY_OR_DISTRICT),]

## Appendix G
blrs_g_2011 <- read_excel("2011 Report/Appendix_G.xls")
as_tibble(blrs_g_2011)
names(blrs_g_2011)
blrs_g_2011 <- blrs_g_2011 %>%
  rename("CITY_OR_DISTRICT" = "City or District") %>%
  rename("TYP_SYS" = "Type") %>%
  rename("VAL_DATE" = "Valuation\nDate") %>%
  rename("FUND_METHOD" = "Funding\nMethod") %>%
  rename("TOTAL_ASSETS" = "Total\nAssets") %>%
  rename("FUNDED_RATIO" = "Funded\nRatio %") %>%
  rename("COVERED_PAYROLL" = "Covered\nPayroll") %>%
  rename("UAAL_AS_PERC_OF_COVD_PAY" = "UAAL as % of\nCovered Pay") %>%
  rename("COMMENTS" = "Comments") %>% 
  rename("RevisedIndicator" = "Revised?") %>% 
  rename("PlanChange" = "Plan\nChange") 
blrs_g_2011 <- blrs_g_2011[!is.na(blrs_g_2011$CITY_OR_DISTRICT),]
blrs_g_2011$VAL_DATE <- ymd(blrs_g_2011$VAL_DATE)



### 2012 data (For this year, I resaved xlsx file to xls file)
## Appendix A
blrs_a_2012 <- read_excel("2012 Report/Appendix_A.xls")
as_tibble(blrs_a_2012)
names(blrs_a_2012)
blrs_a_2012 <- blrs_a_2012 %>%
  rename("CITY_OR_DISTRICT" = "City or District") %>%
  rename("TYP_SYS" = "Type") %>%
  rename("VAL_DATE" = "Valuation\nDate") %>%
  rename("TOTAL_ASSETS" = "Total\nAssets") %>%
  rename("VAL_PYROLL" = "Valuation\nPayroll") %>%
  rename("RETD_PYROLL" = "Retired\nPayroll") %>%
  rename("NORMAL_COST" = "Normal\nCost") %>%
  rename("NORM_PERC" = "Normal\nPercent") %>%
  rename("UAAL_PYMT" = "UAAL\nPayment") %>%
  rename("UAAL_PERC" = "UAAL\nPercent") %>%
  rename("REQD_CONT" = "Required\nContribution") %>%
  rename("REQD_PERC" = "Required\nPercent") %>%
  rename("MEMB_PERC" = "Member\nPercent") %>%
  rename("CITY_PERC" = "City\nPercent") %>%
  rename("OTHR_PERC" = "Other\nPercent") %>%
  rename("PLAN_YEAR_BEGINNING" = "Plan Year\nBeginning") %>%
  rename("PAYMENT_BEGINNING" = "Payment\nBeginning")
blrs_a_2012 <- blrs_a_2012[!is.na(blrs_a_2012$CITY_OR_DISTRICT),] #remove total row
blrs_a_2012$VAL_DATE <- ymd(blrs_a_2012$VAL_DATE)
blrs_a_2012$PLAN_YEAR_BEGINNING <- ymd(blrs_a_2012$PLAN_YEAR_BEGINNING)
blrs_a_2012$PAYMENT_BEGINNING <- ymd(blrs_a_2012$PAYMENT_BEGINNING)


## Appendix B1
blrs_b1_2012 <- read_excel("2012 Report/Appendix_B1.xls")
as_tibble(blrs_b1_2012)
names(blrs_b1_2012)
blrs_b1_2012 <- blrs_b1_2012 %>%
  rename("CITY_OR_DISTRICT" = "City of District") %>%
  rename("TYP_SYS" = "Type") %>%
  rename("plan_status" = "Plan Status") %>%
  rename("RETIREMENT_BENEFIT_RATE_PERC" = "Retirement Benefit\nRate") %>%
  rename("RETIREMENT_BENEFIT_RATE_DESC" = "Retirement Benefit\nDescription") %>%
  rename("YR" = "Normal Retirement\nYear") %>%
  rename("NORMAL_RETIREMENT_AGE_DESC" = "Normal Retirement\nDescription") %>%
  rename("N" = "Normal\nForm") %>%
  rename("NORMAL_FORM_OF_BENEFIT_DESC" = "Normal Form\nDescription") %>%
  rename("VS" = "Vesting\nSchedule") %>%
  rename("COL" = "Auto\nCOLA") %>%
  rename("COMMENTS" = "Comments")
blrs_b1_2012 <- blrs_b1_2012[!is.na(blrs_b1_2012$CITY_OR_DISTRICT),]

## Appendix B2
blrs_b2_2012 <- read_excel("2012 Report/Appendix_B2.xls")
as_tibble(blrs_b2_2012)
names(blrs_b2_2012)
blrs_b2_2012 <- blrs_b2_2012 %>%
  rename("CITY_OR_DISTRICT" = "City or District") %>%
  rename("TYP_SYS" = "Type") %>%
  rename("DISABILITY_BENEFITS_Y" = "Disability\nBenefits?") %>%
  rename("DISABILITY_BENEFITS_DESC" = "Disability Benefit\nDescription") %>%
  rename("DEATH_BENEFITS_Y" = "Death\nBenefits?") %>%
  rename("DEATH_BENEFITS_DESC" = "Death Benefits\nDescription") %>%
  rename("EARLY_RETIREMENT_BENEFITS_Y" = "Early Retirement\nBenefits?") %>%
  rename("EARLY_RETIREMENT_BENEFITS_DESC" = "Early Retirement Benefits\nDescription") %>%
  rename("offer_drop" = "DROP\nBenefits?")
blrs_b2_2012 <- blrs_b2_2012[!is.na(blrs_b2_2012$TYP_SYS),]

## Appendix C
blrs_c_2012 <- read_excel("2012 Report/Appendix_C.xls")
as_tibble(blrs_c_2012)
names(blrs_c_2012)
blrs_c_2012 <- blrs_c_2012 %>%
  rename("CITY_OR_DISTRICT" = "City or District") %>%
  rename("TYP_SYS" = "Type") %>%
  rename("VAL_DATE" = "Valuation\nDate") %>%
  rename("ASSETS_EQUITIES" = "Assets\nEquities") %>%
  rename("ASSETS_CASH_EQUIV" = "Assets\nCash & Equiv") %>%
  rename("ASSETS_INSURANCE" = "Assets\nInsurance") %>%
  rename("ASSETS_FIXED_INC" = "Assets\nFixed Inc") %>%
  rename("TOTAL_ASSETS" = "Total\nAssets") %>%
  rename("COMMENTS" = "Comments") %>%
  rename("drop_assets_amt" = "DROP\nAmount")
blrs_c_2012$VAL_DATE <- ymd(blrs_c_2012$VAL_DATE)
blrs_c_2012 <- blrs_c_2012[!is.na(blrs_c_2012$TYP_SYS),]

## Appendix D 
blrs_d_2012 <- read_excel("2012 Report/Appendix_D.xls")
as_tibble(blrs_d_2012)
names(blrs_d_2012)
blrs_d_2012 <- blrs_d_2012 %>%
  rename("CITY_OR_DISTRICT" = "City or District") %>%
  rename("TYP_SYS" = "Type") %>%
  rename("VAL_DATE" = "Valuation\nDate") %>%
  rename("TOTAL_ASSETS" = "Market Value of\nAssets") %>%
  rename("LIABILITY_FOR_RET_BEN" = "Liability for\nRet & Ben") %>%
  rename("LIABILITY_FOR_EE_CONTRIB" = "Liability for\nEE Cont") %>%
  rename("LIABILITY_FOR_ACTIVES" = "Liability for\nActives") %>%
  rename("RATIO_ASSETS_LIABILITIES_INACT" = "Ratio\nAssets/Liabs\nInactive") %>%
  rename("RATIO_ASSETS_LIABILITIES_CONTR" = "Ratio\nAssets/Liabs\nContr") %>%
  rename("RATIO_ASSETS_LIABILITIES_ACTVS" = "Ratio\nAssets/Liabs\nActive") %>%
  rename("COMMENTS" = "Comments") %>%
  rename("RevisedIndicator" = "Revised") %>%
  rename("PlanChange" = "Plan\nChange") %>%
  rename("total_PAVB" = "Total PVAB") %>%
  rename("FASB35_ratio" = "FASB 35\nRatio")   
blrs_d_2012 <- blrs_d_2012[!is.na(blrs_d_2012$TYP_SYS),]
blrs_d_2012$VAL_DATE <- ymd(blrs_d_2012$VAL_DATE)

## Appendix E
blrs_e_2012 <- read_excel("2012 Report/Appendix_E.xls")
as_tibble(blrs_e_2012)
names(blrs_e_2012)
blrs_e_2012 <- blrs_e_2012 %>%
  rename("CITY_OR_DISTRICT" = "City or District") %>%
  rename("TYP_SYS" = "Type") %>%
  rename("VAL_DATE" = "Valuation\nDate") %>%
  rename("FUND_METHOD" = "Funding\nMethod") %>%
  rename("OLD_PLN" = "Old\nPlan") %>%
  rename("PLNYR_ENDED" = "Plan Year\nEnded") %>%
  rename("SALRY_ASSMP" = "Salary\nAssumption") %>%
  rename("SALRY_ACTUL" = "Salary\nActual") %>%
  rename("INT_ASSMP" = "Interest\nAssumption") %>%
  rename("INT_ACTUL" = "Interest\nActual") %>%
  rename("PR_GR_ASSMP" = "Payroll Growth\nAssumption") %>%
  rename("RETIREMENT_AGE_ASSUMPTION_YR" = "Retirement Age\nAssumption") %>%
  rename("RETIREMENT_AGE_ASSUMPTION_DESC" = "Retirement Age Assumption\nDescription") %>%
  rename("COMMENTS" = "Comments") 
blrs_e_2012$VAL_DATE <- ymd(blrs_e_2012$VAL_DATE)
blrs_e_2012$PLNYR_ENDED <- ymd(blrs_e_2012$PLNYR_ENDED)
blrs_e_2012 <- blrs_e_2012[!is.na(blrs_e_2012$TYP_SYS),]

## Appendix F
blrs_f_2012 <- read_excel("2012 Report/Appendix_F.xls")
as_tibble(blrs_f_2012)
names(blrs_f_2012)
blrs_f_2012 <- blrs_f_2012 %>%
  rename("CITY_OR_DISTRICT" = "City or District") %>%
  rename("TYP_SYS" = "Type") %>%
  rename("CITY_POPULATN" = "City\nPopulation") %>%
  rename("COUNTY_NAME" = "County\nName") %>%
  rename("COUNTY_POPULATION" = "County\nPopulation") %>%
  rename("MEMBR_ACTIV" = "Active\nMembers") %>%
  rename("MEMBR_RETRD" = "Retired\nMembers") %>%
  rename("MEMBR_TERMD" = "Terminated\nMembers") %>%
  rename("COMMENTS" = "Comments") %>% 
  rename("MEMBR_DROP" = "DROP\nMembers")
blrs_f_2012 <- blrs_f_2012[!is.na(blrs_f_2012$CITY_OR_DISTRICT),]

## Appendix G
blrs_g_2012 <- read_excel("2012 Report/Appendix_G.xls")
as_tibble(blrs_g_2012)
names(blrs_g_2012)
blrs_g_2012 <- blrs_g_2012 %>%
  rename("CITY_OR_DISTRICT" = "City or District") %>%
  rename("TYP_SYS" = "Type") %>%
  rename("VAL_DATE" = "Valuation\nDate") %>%
  rename("FUND_METHOD" = "Funding\nMethod") %>%
  rename("TOTAL_ASSETS" = "Total\nAssets") %>%
  rename("FUNDED_RATIO" = "Funded\nRatio %") %>%
  rename("COVERED_PAYROLL" = "Covered\nPayroll") %>%
  rename("UAAL_AS_PERC_OF_COVD_PAY" = "UAAL as % of\nCovered Pay") %>%
  rename("COMMENTS" = "Comments") %>% 
  rename("RevisedIndicator" = "Revised?") %>% 
  rename("PlanChange" = "Plan\nChange") 
blrs_g_2012 <- blrs_g_2012[!is.na(blrs_g_2012$CITY_OR_DISTRICT),]
blrs_g_2012$VAL_DATE <- ymd(blrs_g_2012$VAL_DATE)



### 2013 data (For this year, I resaved xlsx file to xls file)
## Appendix A
blrs_a_2013 <- read_excel("2013 Report/2013-04 - Appendix A - Financial Contribution Data.xls")
warnings()
as_tibble(blrs_a_2013)
names(blrs_a_2013)
blrs_a_2013 <- blrs_a_2013 %>%
  rename("CITY_OR_DISTRICT" = "City or District") %>%
  rename("TYP_SYS" = "Type") %>%
  rename("VAL_DATE" = "Valuation\nDate") %>%
  rename("TOTAL_ASSETS" = "Total\nAssets") %>%
  rename("VAL_PYROLL" = "Valuation\nPayroll") %>%
  rename("RETD_PYROLL" = "Retired\nPayroll") %>%
  rename("NORMAL_COST" = "Normal\nCost") %>%
  rename("NORM_PERC" = "Normal\nPercent") %>%
  rename("UAAL_PYMT" = "UAAL\nPayment") %>%
  rename("UAAL_PERC" = "UAAL\nPercent") %>%
  rename("REQD_CONT" = "Required\nContribution") %>%
  rename("REQD_PERC" = "Required\nPercent") %>%
  rename("MEMB_PERC" = "Member\nPercent") %>%
  rename("CITY_PERC" = "City\nPercent") %>%
  rename("OTHR_PERC" = "Other\nPercent") %>%
  rename("PLAN_YEAR_BEGINNING" = "Plan Year\nBeginning") %>%
  rename("PAYMENT_BEGINNING" = "Payment\nBeginning")
blrs_a_2013 <- blrs_a_2013[!is.na(blrs_a_2013$CITY_OR_DISTRICT),] #remove total row
blrs_a_2013$VAL_DATE <- ymd(blrs_a_2013$VAL_DATE)
blrs_a_2013 <- blrs_a_2013 %>% #change date format
  mutate(PLAN_YEAR_BEGINNING=as.Date(PLAN_YEAR_BEGINNING, origin="1899-12-30"), PAYMENT_BEGINNING=as.Date(PAYMENT_BEGINNING, origin="1899-12-30")) 

## Appendix B1
blrs_b1_2013 <- read_excel("2013 Report/2013-05 - Appendix B - Benefit Data.xls")
as_tibble(blrs_b1_2013)
names(blrs_b1_2013)
blrs_b1_2013 <- blrs_b1_2013 %>%
  rename("CITY_OR_DISTRICT" = "City of District") %>%
  rename("TYP_SYS" = "Type") %>%
  rename("plan_status" = "Plan Status") %>%
  rename("RETIREMENT_BENEFIT_RATE_PERC" = "Retirement Benefit\nRate") %>%
  rename("RETIREMENT_BENEFIT_RATE_DESC" = "Retirement Benefit\nDescription") %>%
  rename("YR" = "Normal Retirement\nYear") %>%
  rename("NORMAL_RETIREMENT_AGE_DESC" = "Normal Retirement\nDescription") %>%
  rename("N" = "Normal\nForm") %>%
  rename("NORMAL_FORM_OF_BENEFIT_DESC" = "Normal Form\nDescription") %>%
  rename("VS" = "Vesting\nSchedule") %>%
  rename("COL" = "Auto\nCOLA") %>%
  rename("year_full_vest" = "Years until fully vested") %>%
  rename("COMMENTS" = "Comments")
blrs_b1_2013 <- blrs_b1_2013[!is.na(blrs_b1_2013$CITY_OR_DISTRICT),]

## Appendix B2
blrs_b2_2013 <- read_excel("2013 Report/2013-06 - Appendix B2 - Added Benefit Data.xls")
as_tibble(blrs_b2_2013)
names(blrs_b2_2013)
blrs_b2_2013 <- blrs_b2_2013 %>%
  rename("CITY_OR_DISTRICT" = "City or District") %>%
  rename("TYP_SYS" = "Type") %>%
  rename("DISABILITY_BENEFITS_Y" = "Disability\nBenefits?") %>%
  rename("DISABILITY_BENEFITS_DESC" = "Disability Benefit\nDescription") %>%
  rename("DEATH_BENEFITS_Y" = "Death\nBenefits?") %>%
  rename("DEATH_BENEFITS_DESC" = "Death Benefits\nDescription") %>%
  rename("EARLY_RETIREMENT_BENEFITS_Y" = "Early Retirement\nBenefits?") %>%
  rename("EARLY_RETIREMENT_BENEFITS_DESC" = "Early Retirement Benefits\nDescription") %>%
  rename("offer_drop" = "DROP\nBenefits?")
blrs_b2_2013 <- blrs_b2_2013[!is.na(blrs_b2_2013$TYP_SYS),]

## Appendix C
blrs_c_2013 <- read_excel("2013 Report/2013-07 - Appendix C - Market Value of Assets.xls")
as_tibble(blrs_c_2013)
names(blrs_c_2013)
blrs_c_2013 <- blrs_c_2013 %>%
  rename("CITY_OR_DISTRICT" = "City or District") %>%
  rename("TYP_SYS" = "Type") %>%
  rename("VAL_DATE" = "Valuation\nDate") %>%
  rename("ASSETS_EQUITIES" = "Assets\nEquities") %>%
  rename("ASSETS_CASH_EQUIV" = "Assets\nCash & Equiv") %>%
  rename("ASSETS_INSURANCE" = "Assets\nInsurance") %>%
  rename("ASSETS_FIXED_INC" = "Assets\nFixed Inc") %>%
  rename("TOTAL_ASSETS" = "Total\nAssets") %>%
  rename("COMMENTS" = "Comments") %>%
  rename("drop_assets_amt" = "DROP\nAmount")
blrs_c_2013$VAL_DATE <- ymd(blrs_c_2013$VAL_DATE)
blrs_c_2013 <- blrs_c_2013[!is.na(blrs_c_2013$TYP_SYS),]

## Appendix D 
blrs_d_2013 <- read_excel("2013 Report/2013-08 - Appendix D - Funding Progress (FASB 35).xls")
as_tibble(blrs_d_2013)
names(blrs_d_2013)
blrs_d_2013 <- blrs_d_2013 %>%
  rename("CITY_OR_DISTRICT" = "City or District") %>%
  rename("TYP_SYS" = "Type") %>%
  rename("VAL_DATE" = "Valuation\nDate") %>%
  rename("TOTAL_ASSETS" = "Market Value of\nAssets") %>%
  rename("LIABILITY_FOR_RET_BEN" = "Liability for\nRet & Ben") %>%
  rename("LIABILITY_FOR_EE_CONTRIB" = "Liability for\nEE Cont") %>%
  rename("LIABILITY_FOR_ACTIVES" = "Liability for\nActives") %>%
  rename("RATIO_ASSETS_LIABILITIES_INACT" = "Ratio\nAssets/Liabs\nInactive") %>%
  rename("RATIO_ASSETS_LIABILITIES_CONTR" = "Ratio\nAssets/Liabs\nContr") %>%
  rename("RATIO_ASSETS_LIABILITIES_ACTVS" = "Ratio\nAssets/Liabs\nActive") %>%
  rename("COMMENTS" = "Comments") %>%
  rename("RevisedIndicator" = "Revised") %>%
  rename("PlanChange" = "Plan\nChange") %>%
  rename("total_PAVB" = "Total PVAB") %>%
  rename("FASB35_ratio" = "FASB 35\nRatio")   
blrs_d_2013 <- blrs_d_2013[!is.na(blrs_d_2013$TYP_SYS),]
blrs_d_2013$VAL_DATE <- ymd(blrs_d_2013$VAL_DATE)

## Appendix E
blrs_e_2013 <- read_excel("2013 Report/2013-09 - Appendix E - Actuarial Data.xls")
as_tibble(blrs_e_2013)
names(blrs_e_2013)
blrs_e_2013 <- blrs_e_2013 %>%
  rename("CITY_OR_DISTRICT" = "City or District") %>%
  rename("TYP_SYS" = "Type") %>%
  rename("VAL_DATE" = "Valuation\nDate") %>%
  rename("FUND_METHOD" = "Funding\nMethod") %>%
  rename("OLD_PLN" = "Old\nPlan") %>%
  rename("PLNYR_ENDED" = "Plan Year\nEnded") %>%
  rename("SALRY_ASSMP" = "Salary\nAssumption") %>%
  rename("SALRY_ACTUL" = "Salary\nActual") %>%
  rename("INT_ASSMP" = "Interest\nAssumption") %>%
  rename("INT_ACTUL" = "Interest\nActual") %>%
  rename("PR_GR_ASSMP" = "Payroll Growth\nAssumption") %>%
  rename("RETIREMENT_AGE_ASSUMPTION_YR" = "Retirement Age\nAssumption") %>%
  rename("RETIREMENT_AGE_ASSUMPTION_DESC" = "Retirement Age Assumption\nDescription") %>%
  rename("COMMENTS" = "Comments") %>%
  rename("Market_Value_Return_Actual" = "Market Value Return Actual")
blrs_e_2013$VAL_DATE <- ymd(blrs_e_2013$VAL_DATE)
blrs_e_2013 <- mutate_at(blrs_e_2013, vars(PLNYR_ENDED), funs(as.numeric))
blrs_e_2013 <- blrs_e_2013 %>% #change date format
  mutate(PLNYR_ENDED=as.Date(PLNYR_ENDED, origin="1899-12-30")) 
blrs_e_2013 <- blrs_e_2013[!is.na(blrs_e_2013$TYP_SYS),]

## Appendix F
blrs_f_2013 <- read_excel("2013 Report/2013-10 - Appendix F - Population Data.xls")
as_tibble(blrs_f_2013)
names(blrs_f_2013)
blrs_f_2013 <- blrs_f_2013 %>%
  rename("CITY_OR_DISTRICT" = "City or District") %>%
  rename("TYP_SYS" = "Type") %>%
  rename("CITY_POPULATN" = "City\nPopulation") %>%
  rename("COUNTY_NAME" = "County\nName") %>%
  rename("COUNTY_POPULATION" = "County\nPopulation") %>%
  rename("MEMBR_ACTIV" = "Active\nMembers") %>%
  rename("MEMBR_RETRD" = "Retired\nMembers") %>%
  rename("MEMBR_TERMD" = "Terminated\nMembers") %>%
  rename("COMMENTS" = "Comments") %>% 
  rename("MEMBR_DROP" = "DROP\nMembers")
blrs_f_2013 <- blrs_f_2013[!is.na(blrs_f_2013$CITY_OR_DISTRICT),]

## Appendix G
blrs_g_2013 <- read_excel("2013 Report/2013-11 - Appendix G - Funding Progress (GASB 25).xls")
as_tibble(blrs_g_2013)
names(blrs_g_2013)
blrs_g_2013 <- blrs_g_2013 %>%
  rename("CITY_OR_DISTRICT" = "City or District") %>%
  rename("TYP_SYS" = "Type") %>%
  rename("VAL_DATE" = "Valuation\nDate") %>%
  rename("FUND_METHOD" = "Funding\nMethod") %>%
  rename("TOTAL_ASSETS" = "Total\nAssets") %>%
  rename("FUNDED_RATIO" = "Funded\nRatio %") %>%
  rename("COVERED_PAYROLL" = "Covered\nPayroll") %>%
  rename("UAAL_AS_PERC_OF_COVD_PAY" = "UAAL as % of\nCovered Pay") %>%
  rename("COMMENTS" = "Comments") %>% 
  rename("RevisedIndicator" = "Revised?") %>% 
  rename("PlanChange" = "Plan\nChange") 
blrs_g_2013 <- blrs_g_2013[!is.na(blrs_g_2013$CITY_OR_DISTRICT),]
blrs_g_2013$VAL_DATE <- ymd(blrs_g_2013$VAL_DATE)



### 2014 data
## Appendix A
blrs_a_2014 <- read_excel("2014 Report/2014-04 - Appendix A - Financial Contribution Data.xls")
as_tibble(blrs_a_2014)
names(blrs_a_2014)
blrs_a_2014 <- blrs_a_2014 %>%
  rename("CITY_OR_DISTRICT" = "CityName") %>%
  rename("TYP_SYS" = "PlanType")
blrs_a_2014 <- blrs_a_2014[!is.na(blrs_a_2014$CITY_OR_DISTRICT),] #remove total row
blrs_a_2014$VAL_DATE <- ymd(blrs_a_2014$VAL_DATE)
blrs_a_2014$PLAN_YEAR_BEGINNING <- ymd(blrs_a_2014$PLAN_YEAR_BEGINNING)
blrs_a_2014$PAYMENT_BEGINNING <- ymd(blrs_a_2014$PAYMENT_BEGINNING)

## Appendix B1
blrs_b1_2014 <- read_excel("2014 Report/2014-05 - Appendix B - Benefit Data.xls")
as_tibble(blrs_b1_2014)
names(blrs_b1_2014)
blrs_b1_2014 <- blrs_b1_2014 %>%
  rename("RETIREMENT_BENEFIT_RATE_PERC" = "TxtRETIREMENT_BENEFIT_RATE_PERC") %>%
  rename("RETIREMENT_BENEFIT_RATE_DESC" = "TxtRETIREMENT_BENEFIT_RATE_DESC") %>%
  rename("year_full_vest" = "Text112")
blrs_b1_2014 <- blrs_b1_2014[!is.na(blrs_b1_2014$CITY_OR_DISTRICT),]

## Appendix B2
blrs_b2_2014 <- read_excel("2014 Report/2014-06 - Appendix B2 - Added Benefit Data.xls")
as_tibble(blrs_b2_2014)
names(blrs_b2_2014)
blrs_b2_2014 <- blrs_b2_2014 %>%
  rename("DISABILITY_BENEFITS_Y" = "TxtDISABILITY_BENEFITS_Y") %>%
  rename("EARLY_RETIREMENT_BENEFITS_Y" = "TxtEARLY_RETIREMENT_BENEFITS_Y") %>%
  rename("offer_drop" = "Text34")
blrs_b2_2014 <- blrs_b2_2014[!is.na(blrs_b2_2014$TYP_SYS),]

## Appendix C
blrs_c_2014 <- read_excel("2014 Report/2014-07 - Appendix C - Market Value of Assets.xls")
as_tibble(blrs_c_2014)
names(blrs_c_2014)
blrs_c_2014 <- blrs_c_2014 %>%
  rename("ASSETS_EQUITIES" = "TxtASSETS_EQUITIES") %>%
  rename("ASSETS_CASH_EQUIV" = "TxtASSETS_CASH_EQUIV") %>%
  rename("ASSETS_INSURANCE" = "TxtASSETS_INSURANCE") %>%
  rename("ASSETS_FIXED_INC" = "TxtASSETS_FIXED_INC") %>%
  rename("TOTAL_ASSETS" = "TxtTOTAL_ASSETS") %>%
  rename("drop_assets_amt" = "Txtdrop_assets_amt")
blrs_c_2014$VAL_DATE <- ymd(blrs_c_2014$VAL_DATE)
blrs_c_2014 <- blrs_c_2014[!is.na(blrs_c_2014$TYP_SYS),]

## Appendix D 
blrs_d_2014 <- read_excel("2014 Report/2014-08 - Appendix D - Funding Progress (FASB 35).xls")
as_tibble(blrs_d_2014)
names(blrs_d_2014)
blrs_d_2014 <- blrs_d_2014 %>%
  rename("TOTAL_ASSETS" = "TxtMVA") %>%
  rename("RATIO_ASSETS_LIABILITIES_INACT" = "TxtRATIO_ASSETS_LIABILITIES_INACT") %>%
  rename("RATIO_ASSETS_LIABILITIES_CONTR" = "TxtRATIO_ASSETS_LIABILITIES_CONTR") %>%
  rename("RATIO_ASSETS_LIABILITIES_ACTVS" = "TxtRATIO_ASSETS_LIABILITIES_ACTVS") %>%
  rename("total_PAVB" = "TxtTotalPVAB") %>%
  rename("FASB35_ratio" = "TxtFASB35Ratio")   
blrs_d_2014 <- blrs_d_2014[!is.na(blrs_d_2014$TYP_SYS),]
blrs_d_2014$VAL_DATE <- ymd(blrs_d_2014$VAL_DATE)

## Appendix E
blrs_e_2014 <- read_excel("2014 Report/2014-09 - Appendix E - Actuarial Data.xls")
as_tibble(blrs_e_2014)
names(blrs_e_2014)
blrs_e_2014 <- blrs_e_2014 %>%
  rename("Market_Value_Return_Actual" = "TxtMVReturn")
blrs_e_2014$VAL_DATE <- ymd(blrs_e_2014$VAL_DATE)
blrs_e_2014$PLNYR_ENDED <- ymd(blrs_e_2014$PLNYR_ENDED)
blrs_e_2014 <- blrs_e_2014[!is.na(blrs_e_2014$TYP_SYS),]

## Appendix F
blrs_f_2014 <- read_excel("2014 Report/2014-10 - Appendix F - Population Data.xls")
as_tibble(blrs_f_2014)
names(blrs_f_2014)
blrs_f_2014 <- blrs_f_2014 %>%
  rename("COUNTY_POPULATION" = "TxtCOUNTY_POPULATION") %>%
  rename("MEMBR_ACTIV" = "TxtMEMBR_ACTIV") %>%
  rename("MEMBR_RETRD" = "TxtMEMBR_RETRD") %>%
  rename("MEMBR_TERMD" = "TxtMEMBR_TERMD") %>%
  rename("MEMBR_DROP" = "Txtmembr_drop")
blrs_f_2014 <- blrs_f_2014[!is.na(blrs_f_2014$CITY_OR_DISTRICT),]

## Appendix G
blrs_g_2014 <- read_excel("2014 Report/2014-11 - Appendix G - Funding Progress (GASB 25).xls")
as_tibble(blrs_g_2014)
names(blrs_g_2014)
blrs_g_2014 <- blrs_g_2014[!is.na(blrs_g_2014$CITY_OR_DISTRICT),]
blrs_g_2014$VAL_DATE <- ymd(blrs_g_2014$VAL_DATE)



### 2015 data
## Appendix A
blrs_a_2015 <- read_excel("2015 Report/Appendix_A.xls")
as_tibble(blrs_a_2015)
names(blrs_a_2015)
blrs_a_2015 <- blrs_a_2015 %>%
  rename("CITY_OR_DISTRICT" = "City or District") %>%
  rename("TYP_SYS" = "Plan Type") %>%
  rename("VAL_DATE" = "Valuation Date") %>%
  rename("TOTAL_ASSETS" = "Total Market Assets") %>%
  rename("VAL_PYROLL" = "Valuation Payroll") %>%
  rename("RETD_PYROLL" = "Retired Payroll") %>%
  rename("NORMAL_COST" = "Normal Cost") %>%
  rename("NORM_PERC" = "Normal Percent") %>%
  rename("UAAL_PYMT" = "UAAL Payment") %>%
  rename("UAAL_PERC" = "UAAL Percent") %>%
  rename("REQD_CONT" = "Required Contributions") %>%
  rename("REQD_PERC" = "Required percent") %>%
  rename("MEMB_PERC" = "Member Percent") %>%
  rename("CITY_PERC" = "City Percent") %>%
  rename("OTHR_PERC" = "Other Percent") %>%
  rename("PLAN_YEAR_BEGINNING" = "Plan Year Beginning") %>%
  rename("PAYMENT_BEGINNING" = "Payment Beginning")
blrs_a_2015 <- blrs_a_2015[!is.na(blrs_a_2015$CITY_OR_DISTRICT),] #remove total row and other missing rows
blrs_a_2015$VAL_DATE <- ymd(blrs_a_2015$VAL_DATE)
blrs_a_2015$PLAN_YEAR_BEGINNING <- ymd(blrs_a_2015$PLAN_YEAR_BEGINNING)
blrs_a_2015$PAYMENT_BEGINNING <- ymd(blrs_a_2015$PAYMENT_BEGINNING)

## Appendix B1
blrs_b1_2015 <- read_excel("2015 Report/Appendix_B1.xls")
as_tibble(blrs_b1_2015)
names(blrs_b1_2015)
blrs_b1_2015 <- blrs_b1_2015 %>%
  rename("CITY_OR_DISTRICT" = "City or District") %>%
  rename("TYP_SYS" = "Type System") %>%
  rename("plan_status" = "Plan Status") %>%
  rename("RETIREMENT_BENEFIT_RATE_PERC" = "Retirement Benefit Rate: Percent") %>%
  rename("RETIREMENT_BENEFIT_RATE_DESC" = "Retirement Benefit Rate: Description") %>%
  rename("YR" = "Normal Retirement Age: Year") %>%
  rename("NORMAL_RETIREMENT_AGE_DESC" = "Normal Retirement Age: Description") %>%
  rename("N" = "Normal Form of Benefit: N (Type)") %>%
  rename("NORMAL_FORM_OF_BENEFIT_DESC" = "Normal Form of Benefit: Description") %>%
  rename("VS" = "Vesting: Schedule") %>%
  rename("COL" = "COLA") %>%
  rename("year_full_vest" = "Vesting: Years to Full")
blrs_b1_2015 <- blrs_b1_2015[!is.na(blrs_b1_2015$CITY_OR_DISTRICT),]

## Appendix B2
blrs_b2_2015 <- read_excel("2015 Report/Appendix_B2.xls")
as_tibble(blrs_b2_2015)
names(blrs_b2_2015)
blrs_b2_2015 <- blrs_b2_2015 %>%
  rename("CITY_OR_DISTRICT" = "City or District") %>%
  rename("TYP_SYS" = "Type of System") %>%
  rename("DISABILITY_BENEFITS_Y" = "Disability Benefits: Y/N") %>%
  rename("DISABILITY_BENEFITS_DESC" = "Disability Benefits: Description") %>%
  rename("DEATH_BENEFITS_Y" = "Death Benefits: Y/N") %>%
  rename("DEATH_BENEFITS_DESC" = "Death Benefits: Description") %>%
  rename("EARLY_RETIREMENT_BENEFITS_Y" = "Early Retirement Benefits: Y/N") %>%
  rename("EARLY_RETIREMENT_BENEFITS_DESC" = "Early Retirement Benefits: Description") %>%
  rename("offer_drop" = "Offer DROP?")
blrs_b2_2015 <- blrs_b2_2015[!is.na(blrs_b2_2015$TYP_SYS),]

## Appendix C
blrs_c_2015 <- read_excel("2015 Report/Appendix_C.xls")
as_tibble(blrs_c_2015)
names(blrs_c_2015)
blrs_c_2015 <- blrs_c_2015 %>%
  rename("CITY_OR_DISTRICT" = "City or District") %>%
  rename("TYP_SYS" = "Type of\nSystem") %>%
  rename("VAL_DATE" = "Valuation\nDate") %>%
  rename("ASSETS_CASH_EQUIV" = "Cash &\nEquivalents") %>%
  rename("TOTAL_ASSETS" = "Total Market\nAssets") %>%
  rename("receivable" = "Receivables") %>%
  rename("equity_intl" = "Equity:\nInternational") %>%
  rename("equity_domestic" = "Equities:\nDomestic") %>%
  rename("fix_inc_intl" = "Fixed Income:\nInternational") %>%
  rename("fix_inc_domestic" = "Fixed Income:\nDomestic") %>%
  rename("real_estate" = "Real\nEstate") %>%
  rename("alter_invest" = "Alternative") %>%
  rename("other_assets" = "Other") %>%
  rename("liabilities" = "Liabilities") %>%
  rename("drop_assets_amt" = "DROP\nAmount")  
blrs_c_2015$VAL_DATE <- ymd(blrs_c_2015$VAL_DATE)
blrs_c_2015 <- blrs_c_2015[!is.na(blrs_c_2015$TYP_SYS),]
blrs_c_2015[is.na(blrs_c_2015)] <- 0 #replaced missing values with zeros based on pdf report

## Appendix D 
blrs_d_2015 <- read_excel("2015 Report/Appendix_D.xls")
as_tibble(blrs_d_2015)
names(blrs_d_2015)
blrs_d_2015 <- blrs_d_2015 %>%
  rename("CITY_OR_DISTRICT" = "City or District") %>%
  rename("TYP_SYS" = "Type of System") %>%
  rename("VAL_DATE" = "Valuation Date") %>%
  rename("TOTAL_ASSETS" = "Total Market Assets") %>%
  rename("LIABILITY_FOR_RET_BEN" = "Liability for Retirees and Beneficiaries") %>%
  rename("LIABILITY_FOR_EE_CONTRIB" = "Liability for Employee Contributions") %>%
  rename("LIABILITY_FOR_ACTIVES" = "Liability for Actives") %>%
  rename("RATIO_ASSETS_LIABILITIES_INACT" = "Ratio: Assets to Liabilities for Inactives") %>%
  rename("RATIO_ASSETS_LIABILITIES_CONTR" = "Ratio: Assets to Liabilties for Contributions") %>%
  rename("RATIO_ASSETS_LIABILITIES_ACTVS" = "Ratio: Assets to Liabilities for Actives") %>%
  rename("RevisedIndicator" = "Revised?") %>%
  rename("PlanChange" = "Plan Change") %>%
  rename("total_PAVB" = "Total PVAB") %>%
  rename("FASB35_ratio" = "GASB 35 Ratio") #PDF report says that it's FASB 35 Ratio, so I changed variable name!  
blrs_d_2015 <- blrs_d_2015[!is.na(blrs_d_2015$TYP_SYS),]
blrs_d_2015$VAL_DATE <- ymd(blrs_d_2015$VAL_DATE)

## Appendix E
blrs_e_2015 <- read_excel("2015 Report/Appendix_E.xls")
as_tibble(blrs_e_2015)
names(blrs_e_2015)
blrs_e_2015 <- blrs_e_2015 %>%
  rename("CITY_OR_DISTRICT" = "City or District") %>%
  rename("TYP_SYS" = "Type of System") %>%
  rename("VAL_DATE" = "Valuation Date") %>%
  rename("FUND_METHOD" = "Funding Method") %>%
  rename("PLNYR_ENDED" = "Plan Year Ended") %>%
  rename("SALRY_ASSMP" = "Salary Assumption") %>%
  rename("SALRY_ACTUL" = "Salary Actual") %>%
  rename("INT_ASSMP" = "Interest Assumption") %>%
  rename("INT_ACTUL" = "Interest AV Actual") %>%
  rename("PR_GR_ASSMP" = "Payroll Growth Assumption") %>%
  rename("RETIREMENT_AGE_ASSUMPTION_YR" = "Retirement Age Assumption: Year") %>%
  rename("RETIREMENT_AGE_ASSUMPTION_DESC" = "Retirement Age Assumption: Description") %>%
  rename("Market_Value_Return_Actual" = "Interest MV Actual")
blrs_e_2015$VAL_DATE <- ymd(blrs_e_2015$VAL_DATE)
blrs_e_2015$PLNYR_ENDED <- ymd(blrs_e_2015$PLNYR_ENDED)
blrs_e_2015 <- blrs_e_2015[!is.na(blrs_e_2015$TYP_SYS),]

## Appendix F
blrs_f_2015 <- read_excel("2015 Report/Appendix_F.xls")
as_tibble(blrs_f_2015)
names(blrs_f_2015)
blrs_f_2015 <- blrs_f_2015 %>%
  rename("CITY_OR_DISTRICT" = "City or District") %>%
  rename("TYP_SYS" = "Type of System") %>%
  rename("CITY_POPULATN" = "City Population") %>%
  rename("COUNTY_NAME" = "County Name") %>%
  rename("COUNTY_POPULATION" = "County Population") %>%
  rename("MEMBR_ACTIV" = "Members: Active") %>%
  rename("MEMBR_RETRD" = "Members: Retired") %>%
  rename("MEMBR_TERMD" = "Members: Terminated") %>%
  rename("MEMBR_DROP" = "Members: DROP")
blrs_f_2015 <- blrs_f_2015[!is.na(blrs_f_2015$CITY_OR_DISTRICT),]

## Appendix G
blrs_g_2015 <- read_excel("2015 Report/Appendix_G.xls")
as_tibble(blrs_g_2015)
names(blrs_g_2015)
blrs_g_2015 <- blrs_g_2015 %>%
  rename("CITY_OR_DISTRICT" = "City or District") %>%
  rename("TYP_SYS" = "Type of System") %>%
  rename("VAL_DATE" = "Valuation Date") %>%
  rename("FUND_METHOD" = "Funding Method") %>%
  rename("TOTAL_ASSETS" = "Total Market Assets") %>%
  rename("FUNDED_RATIO" = "Funded Ratio") %>%
  rename("COVERED_PAYROLL" = "Covered Payroll") %>%
  rename("UAAL_AS_PERC_OF_COVD_PAY" = "UAAL as Percent of Payroll") %>%
  rename("RevisedIndicator" = "Revised?") %>% 
  rename("PlanChange" = "Plan Change") %>%  
  rename("Total_Actuarial_Assets" = "Total Actuarial Assets")
blrs_g_2015 <- blrs_g_2015[!is.na(blrs_g_2015$CITY_OR_DISTRICT),]
blrs_g_2015$VAL_DATE <- ymd(blrs_g_2015$VAL_DATE)



### 2016 data
## Appendix A
blrs_a_2016 <- read_excel("2016 Report/Appendix_A.xls")
as_tibble(blrs_a_2016)
names(blrs_a_2016)
blrs_a_2016 <- blrs_a_2016 %>%
  rename("CITY_OR_DISTRICT" = "City or District") %>%
  rename("TYP_SYS" = "Type") %>%
  rename("VAL_DATE" = "Valuation\nDate") %>%
  rename("TOTAL_ASSETS" = "Total Market\nAssets") %>%
  rename("VAL_PYROLL" = "Valuation\nPayroll") %>%
  rename("RETD_PYROLL" = "Retired\nPayroll") %>%
  rename("NORMAL_COST" = "Normal\nCost") %>%
  rename("NORM_PERC" = "Normal\nPercentage") %>%
  rename("UAAL_PYMT" = "UAAL\nPayment") %>%
  rename("UAAL_PERC" = "UAAL\nPercentage") %>%
  rename("REQD_CONT" = "Required\nContribution") %>%
  rename("REQD_PERC" = "Required\nPercentge") %>%
  rename("MEMB_PERC" = "Member\nPercentage") %>%
  rename("CITY_PERC" = "City\nPercentage") %>%
  rename("OTHR_PERC" = "Other\nPercentage") %>%
  rename("PLAN_YEAR_BEGINNING" = "Plan Year\nBeginning") %>%
  rename("PAYMENT_BEGINNING" = "Payment\nBeginning")
blrs_a_2016 <- blrs_a_2016[!is.na(blrs_a_2016$CITY_OR_DISTRICT),] #remove total row
blrs_a_2016$VAL_DATE <- ymd(blrs_a_2016$VAL_DATE)
blrs_a_2016$PLAN_YEAR_BEGINNING <- ymd(blrs_a_2016$PLAN_YEAR_BEGINNING)
blrs_a_2016$PAYMENT_BEGINNING <- ymd(blrs_a_2016$PAYMENT_BEGINNING)

## Appendix B1
blrs_b1_2016 <- read_excel("2016 Report/Appendix_B1.xls")
as_tibble(blrs_b1_2016)
names(blrs_b1_2016)
blrs_b1_2016 <- blrs_b1_2016 %>%
  rename("CITY_OR_DISTRICT" = "City or District") %>%
  rename("TYP_SYS" = "Type") %>%
  rename("plan_status" = "Plan\nStatus") %>%
  rename("RETIREMENT_BENEFIT_RATE_PERC" = "Retirement Benefit Rate\nPercentage") %>%
  rename("RETIREMENT_BENEFIT_RATE_DESC" = "Retirement Benefit Rate\nDescription") %>%
  rename("YR" = "Normal Retirement Age\nYear") %>%
  rename("NORMAL_RETIREMENT_AGE_DESC" = "Normal Retirement Age\nDescription") %>%
  rename("N" = "Normal Form of Benefit\nN") %>%
  rename("NORMAL_FORM_OF_BENEFIT_DESC" = "Normal Form of Benefit\nDescription") %>%
  rename("VS" = "Vesting\nSchedule") %>%
  rename("COL" = "COLA") %>%
  rename("year_full_vest" = "Years To\nFull Vesting") %>%
  rename("COMMENTS" = "Comments") 
blrs_b1_2016 <- blrs_b1_2016[!is.na(blrs_b1_2016$CITY_OR_DISTRICT),]

## Appendix B2
blrs_b2_2016 <- read_excel("2016 Report/Appendix_B2.xls")
as_tibble(blrs_b2_2016)
names(blrs_b2_2016)
blrs_b2_2016 <- blrs_b2_2016 %>%
  rename("CITY_OR_DISTRICT" = "City or District") %>%
  rename("TYP_SYS" = "Type") %>%
  rename("DISABILITY_BENEFITS_Y" = "Disability\nY/N?") %>%
  rename("DISABILITY_BENEFITS_DESC" = "Disability\nDescription") %>%
  rename("DEATH_BENEFITS_Y" = "Death Benefits\nY/N?") %>%
  rename("DEATH_BENEFITS_DESC" = "Death Benefits\nDescription") %>%
  rename("EARLY_RETIREMENT_BENEFITS_Y" = "Early Retirement\nY/N?") %>%
  rename("EARLY_RETIREMENT_BENEFITS_DESC" = "Early Retirement\nDescription") %>%
  rename("offer_drop" = "Offer\nDROP?")
blrs_b2_2016 <- blrs_b2_2016[!is.na(blrs_b2_2016$TYP_SYS),]

## Appendix C
blrs_c_2016 <- read_excel("2016 Report/Appendix_C.xls")
as_tibble(blrs_c_2016)
names(blrs_c_2016)
blrs_c_2016 <- blrs_c_2016 %>%
  rename("CITY_OR_DISTRICT" = "City or District") %>%
  rename("TYP_SYS" = "Type") %>%
  rename("VAL_DATE" = "Valuation\nDate") %>%
  rename("ASSETS_CASH_EQUIV" = "Cash & Equivalents") %>%
  rename("TOTAL_ASSETS" = "Total Market\nAssets") %>%
  rename("receivable" = "Receivables") %>%
  rename("equity_intl" = "Equity\nInternational") %>%
  rename("equity_domestic" = "Equity\nDomestic") %>%
  rename("fix_inc_intl" = "Fixed Income\nInternational") %>%
  rename("fix_inc_domestic" = "Fixed Income\nDomestic") %>%
  rename("real_estate" = "Real\nEstate") %>%
  rename("alter_invest" = "Alternative\nInvestments") %>%
  rename("other_assets" = "Other\nAssets") %>%
  rename("liabilities" = "Liabilities") %>%
  rename("drop_assets_amt" = "DROP\nAmount") 
blrs_c_2016$VAL_DATE <- ymd(blrs_c_2016$VAL_DATE)
blrs_c_2016 <- blrs_c_2016[!is.na(blrs_c_2016$TYP_SYS),]
blrs_c_2016[is.na(blrs_c_2016)] <- 0 #replaced missing values with zeros based on pdf report

## Appendix D 
blrs_d_2016 <- read_excel("2016 Report/Appendix_D.xls")
as_tibble(blrs_d_2016)
names(blrs_d_2016)
blrs_d_2016 <- blrs_d_2016 %>%
  rename("CITY_OR_DISTRICT" = "City or District") %>%
  rename("TYP_SYS" = "Type") %>%
  rename("VAL_DATE" = "Valuation\nDate") %>%
  rename("TOTAL_ASSETS" = "Market Value\nAssets") %>%
  rename("LIABILITY_FOR_RET_BEN" = "Liability for\nRetirees & Beneficiaries") %>%
  rename("LIABILITY_FOR_EE_CONTRIB" = "Liability for\nEmployee Contributions") %>%
  rename("LIABILITY_FOR_ACTIVES" = "Liability for\nActives") %>%
  rename("RATIO_ASSETS_LIABILITIES_INACT" = "Ratio of\nAssets / Liabilities Inactives") %>%
  rename("RATIO_ASSETS_LIABILITIES_CONTR" = "Ratio of\nAssets / Liabilities Contributions") %>%
  rename("RATIO_ASSETS_LIABILITIES_ACTVS" = "Ratio of\nAssets / Liabilites Actives") %>%
  rename("RevisedIndicator" = "Revised?") %>%
  rename("PlanChange" = "Plan\nChange") %>%
  rename("total_PAVB" = "Total\nPVAB") %>%
  rename("FASB35_ratio" = "FASB 35\nRatio")   
blrs_d_2016 <- blrs_d_2016[!is.na(blrs_d_2016$TYP_SYS),]
blrs_d_2016$VAL_DATE <- ymd(blrs_d_2016$VAL_DATE)

## Appendix E
blrs_e_2016 <- read_excel("2016 Report/Appendix_E.xls")
as_tibble(blrs_e_2016)
names(blrs_e_2016)
blrs_e_2016 <- blrs_e_2016 %>%
  rename("CITY_OR_DISTRICT" = "City or District") %>%
  rename("TYP_SYS" = "Type") %>%
  rename("VAL_DATE" = "Valuation\nDate") %>%
  rename("FUND_METHOD" = "Funding\nMethod") %>%
  rename("PLNYR_ENDED" = "Plan Year\nEnded") %>%
  rename("SALRY_ASSMP" = "Salary\nAssumption") %>%
  rename("SALRY_ACTUL" = "Salary\nActual") %>%
  rename("INT_ASSMP" = "Interest\nAssumption") %>%
  rename("INT_ACTUL" = "Interest (AV)\nActual") %>%
  rename("PR_GR_ASSMP" = "P/R Growth\nAssumption") %>%
  rename("RETIREMENT_AGE_ASSUMPTION_YR" = "Retirement Age Assumption\nYear") %>%
  rename("RETIREMENT_AGE_ASSUMPTION_DESC" = "Retirement Age Assumption\nDescription") %>%
  rename("Market_Value_Return_Actual" = "Interest (MV)\nActual")
blrs_e_2016$VAL_DATE <- ymd(blrs_e_2016$VAL_DATE)
blrs_e_2016$PLNYR_ENDED <- ymd(blrs_e_2016$PLNYR_ENDED)
blrs_e_2016 <- blrs_e_2016[!is.na(blrs_e_2016$TYP_SYS),]

## Appendix F
blrs_f_2016 <- read_excel("2016 Report/Appendix_F.xls")
as_tibble(blrs_f_2016)
names(blrs_f_2016)
blrs_f_2016 <- blrs_f_2016 %>%
  rename("CITY_OR_DISTRICT" = "City or District") %>%
  rename("TYP_SYS" = "Type") %>%
  rename("CITY_POPULATN" = "City\nPopulation") %>%
  rename("COUNTY_NAME" = "County\nName") %>%
  rename("COUNTY_POPULATION" = "County\nPopulation") %>%
  rename("MEMBR_ACTIV" = "Members\nActive") %>%
  rename("MEMBR_RETRD" = "Members\nRetired") %>%
  rename("MEMBR_TERMD" = "Members\nTerminated") %>%
  rename("MEMBR_DROP" = "Members\nDROP")
blrs_f_2016 <- blrs_f_2016[!is.na(blrs_f_2016$CITY_OR_DISTRICT),]

## Appendix G
blrs_g_2016 <- read_excel("2016 Report/Appendix_G.xls")
as_tibble(blrs_g_2016)
names(blrs_g_2016)
blrs_g_2016 <- blrs_g_2016 %>%
  rename("CITY_OR_DISTRICT" = "City or District") %>%
  rename("TYP_SYS" = "Type") %>%
  rename("VAL_DATE" = "Valuation\nDate") %>%
  rename("FUND_METHOD" = "Funding\nMethod") %>%
  rename("TOTAL_ASSETS" = "Total Market\nAssets") %>%
  rename("FUNDED_RATIO" = "Funded Ratio\nPercent") %>%
  rename("COVERED_PAYROLL" = "Covered\npayroll") %>%
  rename("UAAL_AS_PERC_OF_COVD_PAY" = "UAAL as a Percentage\nof Covered Payroll") %>%
  rename("RevisedIndicator" = "Revised?") %>% 
  rename("PlanChange" = "Plan\nChange") %>%  
  rename("Total_Actuarial_Assets" = "Total Actuarial\nAssets")
blrs_g_2016 <- blrs_g_2016[!is.na(blrs_g_2016$CITY_OR_DISTRICT),]
blrs_g_2016$VAL_DATE <- ymd(blrs_g_2016$VAL_DATE)



### 2017 data
## Appendix A
blrs_a_2017 <- read_excel("2017 Report/Appendix_A.xls")
  as_tibble(blrs_a_2017)
  names(blrs_a_2017)
  blrs_a_2017 <- blrs_a_2017 %>%
    rename("CITY_OR_DISTRICT" = "City or District") %>%
    rename("TYP_SYS" = "Type") %>%
    rename("VAL_DATE" = "Valuation\nDate") %>%
    rename("TOTAL_ASSETS" = "Total\nMarket Assets") %>%
    rename("VAL_PYROLL" = "Valuation\nPayroll") %>%
    rename("RETD_PYROLL" = "Retired\nPayroll") %>%
    rename("NORMAL_COST" = "Normal\nCost") %>%
    rename("NORM_PERC" = "Normal\nCost %") %>%
    rename("UAAL_PYMT" = "UAAL\nPayment") %>%
    rename("UAAL_PERC" = "UAAL\nPayment\n%") %>%
    rename("REQD_CONT" = "Required\nContributions") %>%
    rename("REQD_PERC" = "Required\nContributions\n%") %>%
    rename("MEMB_PERC" = "Member\n%") %>%
    rename("CITY_PERC" = "City\n%") %>%
    rename("OTHR_PERC" = "Other\n%") %>%
    rename("PLAN_YEAR_BEGINNING" = "Plan Year\nBeginning") %>%
    rename("PAYMENT_BEGINNING" = "Payment\nBeginning")
blrs_a_2017 <- blrs_a_2017[!is.na(blrs_a_2017$TYP_SYS),] #remove total row
blrs_a_2017$VAL_DATE <- ymd(blrs_a_2017$VAL_DATE)
blrs_a_2017$PLAN_YEAR_BEGINNING <- ymd(blrs_a_2017$PLAN_YEAR_BEGINNING)
blrs_a_2017$PAYMENT_BEGINNING <- ymd(blrs_a_2017$PAYMENT_BEGINNING)

## Appendix B1
blrs_b1_2017 <- read_excel("2017 Report/Appendix_B1.xls")
as_tibble(blrs_b1_2017)
names(blrs_b1_2017)
blrs_b1_2017 <- blrs_b1_2017 %>%
  rename("CITY_OR_DISTRICT" = "City or District") %>%
  rename("RETIREMENT_BENEFIT_RATE_PERC" = "TxtRETIREMENT_BENEFIT_RATE_PERC") %>%
  rename("RETIREMENT_BENEFIT_RATE_DESC" = "TxtRETIREMENT_BENEFIT_RATE_DESC") %>%
  rename("year_full_vest" = "TxtYrstofullvesting")
blrs_b1_2017 <- blrs_b1_2017[!is.na(blrs_b1_2017$CITY_OR_DISTRICT),]

## Appendix B2
blrs_b2_2017 <- read_excel("2017 Report/Appendix_B2.xls")
as_tibble(blrs_b2_2017)
names(blrs_b2_2017)
blrs_b2_2017 <- blrs_b2_2017 %>%
  rename("CITY_OR_DISTRICT" = "City or District") %>%
  rename("DISABILITY_BENEFITS_Y" = "TxtDISABILITY_BENEFITS_Y") %>%
  rename("EARLY_RETIREMENT_BENEFITS_Y" = "TxtEARLY_RETIREMENT_BENEFITS_Y") %>%
  rename("offer_drop" = "Text34")
blrs_b2_2017 <- blrs_b2_2017[!is.na(blrs_b2_2017$TYP_SYS),]

## Appendix C
blrs_c_2017 <- read_excel("2017 Report/Appendix_C.xls")
as_tibble(blrs_c_2017)
names(blrs_c_2017)
blrs_c_2017 <- blrs_c_2017 %>%
  rename("CITY_OR_DISTRICT" = "City or District") %>%
  rename("TYP_SYS" = "Type") %>%
  rename("VAL_DATE" = "Valuation\nDate") %>%
  rename("ASSETS_CASH_EQUIV" = "Cash & Equivalents") %>%
  rename("TOTAL_ASSETS" = "Total\nMarket Assets") %>%
  rename("receivable" = "Receivables") %>%
  rename("equity_intl" = "Equity: \nInternational") %>%
  rename("equity_domestic" = "Equity:\nDomestic") %>%
  rename("fix_inc_intl" = "Fixed Income: \nInternational") %>%
  rename("fix_inc_domestic" = "Fixed Income:\nDomestic") %>%
  rename("real_estate" = "Real\nEstate") %>%
  rename("alter_invest" = "Alternative\nInvestments") %>%
  rename("other_assets" = "Other\nAssets") %>%
  rename("liabilities" = "Liabilities") %>% 
  rename("drop_assets_amt" = "DROP\nAmount") 
blrs_c_2017$VAL_DATE <- ymd(blrs_c_2017$VAL_DATE)
blrs_c_2017 <- blrs_c_2017[!is.na(blrs_c_2017$TYP_SYS),]

## Appendix D 
blrs_d_2017 <- read_excel("2017 Report/Appendix_D.xls")
as_tibble(blrs_d_2017)
names(blrs_d_2017)
blrs_d_2017 <- blrs_d_2017 %>%
  rename("CITY_OR_DISTRICT" = "City or District") %>%
  rename("TOTAL_ASSETS" = "TxtMVA") %>%
  rename("RATIO_ASSETS_LIABILITIES_INACT" = "TxtRATIO_ASSETS_LIABILITIES_INACT") %>%
  rename("RATIO_ASSETS_LIABILITIES_CONTR" = "TxtRATIO_ASSETS_LIABILITIES_CONTR") %>%
  rename("RATIO_ASSETS_LIABILITIES_ACTVS" = "TxtRATIO_ASSETS_LIABILITIES_ACTVS") %>%
  rename("total_PAVB" = "TxtTotalPVAB") %>%
  rename("FASB35_ratio" = "TxtFASB35Ratio")   
blrs_d_2017 <- blrs_d_2017[!is.na(blrs_d_2017$TYP_SYS),]
blrs_d_2017$VAL_DATE <- ymd(blrs_d_2017$VAL_DATE)

## Appendix E
blrs_e_2017 <- read_excel("2017 Report/Appendix_E.xls")
as_tibble(blrs_e_2017)
names(blrs_e_2017)
blrs_e_2017 <- blrs_e_2017 %>%
  rename("CITY_OR_DISTRICT" = "City or District") %>%
  rename("Market_Value_Return_Actual" = "TxtMVReturn")
blrs_e_2017$VAL_DATE <- ymd(blrs_e_2017$VAL_DATE)
blrs_e_2017$PLNYR_ENDED <- ymd(blrs_e_2017$PLNYR_ENDED)
blrs_e_2017 <- blrs_e_2017[!is.na(blrs_e_2017$TYP_SYS),]

## Appendix F
blrs_f_2017 <- read_excel("2017 Report/Appendix_F.xls")
as_tibble(blrs_f_2017)
names(blrs_f_2017)
blrs_f_2017 <- blrs_f_2017 %>%
  rename("CITY_OR_DISTRICT" = "City or District") %>%
  rename("TYP_SYS" = "Type") %>%
  rename("CITY_POPULATN" = "City\nPopulation") %>%
  rename("COUNTY_NAME" = "County\nName") %>%
  rename("COUNTY_POPULATION" = "County \nPopulation") %>%
  rename("MEMBR_ACTIV" = "Active\nMembers") %>%
  rename("MEMBR_RETRD" = "Retired\nMembers") %>%
  rename("MEMBR_TERMD" = "Terminated\nMembers") %>%
  rename("MEMBR_DROP" = "DROP\nMembers")
blrs_f_2017 <- blrs_f_2017[!is.na(blrs_f_2017$CITY_OR_DISTRICT),]
blrs_f_2017 <- blrs_f_2017[(blrs_f_2017$CITY_OR_DISTRICT!="Grand Totals"),]
blrs_f_2017 <- blrs_f_2017[(blrs_f_2017$CITY_OR_DISTRICT!="Filtered Totals"),]

## Appendix G
blrs_g_2017 <- read_excel("2017 Report/Appendix_G.xls")
as_tibble(blrs_g_2017)
names(blrs_g_2017)
blrs_g_2017 <- blrs_g_2017 %>%
  rename("CITY_OR_DISTRICT" = "City or District") %>%
  rename("Total_Actuarial_Assets" = "Text32")
blrs_g_2017 <- blrs_g_2017[!is.na(blrs_g_2017$CITY_OR_DISTRICT),]
blrs_g_2017$VAL_DATE <- ymd(blrs_g_2017$VAL_DATE)





#### Part 2. Rbind for each table
### For this part, first I created additional columns if a certain year does not have that variable to match the number of columns for each year
### Second, I created year variable that incluedes the report year.
### Third, I rbinded every year for each table
### Finally, I renamed variables in a more meaningful way.
### Appendix A
blrs_a_2009$C_V <- NA
blrs_a_2010$C_V <- NA
blrs_a_2011$C_V <- NA
blrs_a_2012$C_V <- NA
blrs_a_2013$C_V <- NA
blrs_a_2014$C_V <- NA
blrs_a_2015$C_V <- NA
blrs_a_2016$C_V <- NA
blrs_a_2017$C_V <- NA

blrs_a_2005$year <- as.numeric("2005")
blrs_a_2006$year <- as.numeric("2006")
blrs_a_2007$year <- as.numeric("2007")
blrs_a_2008$year <- as.numeric("2008")
blrs_a_2009$year <- as.numeric("2009")
blrs_a_2010$year <- as.numeric("2010")
blrs_a_2011$year <- as.numeric("2011")
blrs_a_2012$year <- as.numeric("2012")
blrs_a_2013$year <- as.numeric("2013")
blrs_a_2014$year <- as.numeric("2014")
blrs_a_2015$year <- as.numeric("2015")
blrs_a_2016$year <- as.numeric("2016")
blrs_a_2017$year <- as.numeric("2017")

blrs_a <- rbind(blrs_a_2005, blrs_a_2006, blrs_a_2007, blrs_a_2008, blrs_a_2009, blrs_a_2010, blrs_a_2011, blrs_a_2012, blrs_a_2013, blrs_a_2014, blrs_a_2015, blrs_a_2016, blrs_a_2017)

#rename
blrs_a <- blrs_a %>%
  rename(plan_sponsor = CITY_OR_DISTRICT) %>%
  rename(plan_type = TYP_SYS) %>%
  rename(val_date_a = VAL_DATE) %>%
  rename(total_assets_a = TOTAL_ASSETS) %>%
  rename(uaal_a = UAAL) %>%
  rename(val_payroll_a = VAL_PYROLL) %>%
  rename(retired_payroll_a = RETD_PYROLL) %>%
  rename(normal_cost_a = NORMAL_COST) %>%
  rename(normal_percent_a = NORM_PERC) %>%
  rename(uaal_pymt_a = UAAL_PYMT) %>%
  rename(uaal_percent_a = UAAL_PERC) %>%
  rename(reqd_contribution_a = REQD_CONT) %>%
  rename(reqd_percent_a = REQD_PERC) %>%
  rename(member_percent_a = MEMB_PERC) %>%
  rename(city_percent_a = CITY_PERC) %>%
  rename(other_percent_a = OTHR_PERC) %>%
  rename(plan_year_begin_a = PLAN_YEAR_BEGINNING) %>%
  rename(pmt_begin_a = PAYMENT_BEGINNING) %>%
  rename(current_val_a = C_V) 

blrs_a$plan_sponsor <- str_trim(blrs_a$plan_sponsor)



### Appendix B1
blrs_b1_2005$plan_status <- NA
blrs_b1_2006$plan_status <- NA
blrs_b1_2007$plan_status <- NA
blrs_b1_2008$plan_status <- NA
blrs_b1_2009$plan_status <- NA
blrs_b1_2010$plan_status <- NA
blrs_b1_2011$plan_status <- NA

blrs_b1_2008$SS <- NA
blrs_b1_2009$SS <- NA
blrs_b1_2010$SS <- NA
blrs_b1_2011$SS <- NA
blrs_b1_2012$SS <- NA
blrs_b1_2013$SS <- NA
blrs_b1_2014$SS <- NA
blrs_b1_2015$SS <- NA
blrs_b1_2016$SS <- NA
blrs_b1_2017$SS <- NA

blrs_b1_2005$year_full_vest <- NA
blrs_b1_2006$year_full_vest <- NA
blrs_b1_2007$year_full_vest <- NA
blrs_b1_2008$year_full_vest <- NA
blrs_b1_2009$year_full_vest <- NA
blrs_b1_2010$year_full_vest <- NA
blrs_b1_2011$year_full_vest <- NA
blrs_b1_2012$year_full_vest <- NA


blrs_b1_2005$year <- as.numeric("2005")
blrs_b1_2006$year <- as.numeric("2006")
blrs_b1_2007$year <- as.numeric("2007")
blrs_b1_2008$year <- as.numeric("2008")
blrs_b1_2009$year <- as.numeric("2009")
blrs_b1_2010$year <- as.numeric("2010")
blrs_b1_2011$year <- as.numeric("2011")
blrs_b1_2012$year <- as.numeric("2012")
blrs_b1_2013$year <- as.numeric("2013")
blrs_b1_2014$year <- as.numeric("2014")
blrs_b1_2015$year <- as.numeric("2015")
blrs_b1_2016$year <- as.numeric("2016")
blrs_b1_2017$year <- as.numeric("2017")

blrs_b1 <- rbind(blrs_b1_2005, blrs_b1_2006, blrs_b1_2007, blrs_b1_2008, blrs_b1_2009, blrs_b1_2010, blrs_b1_2011, blrs_b1_2012, blrs_b1_2013, blrs_b1_2014, blrs_b1_2015, blrs_b1_2016, blrs_b1_2017)

#rename
blrs_b1 <- blrs_b1 %>%
  rename(plan_sponsor = CITY_OR_DISTRICT) %>%
  rename(plan_type = TYP_SYS) %>%
  rename(plan_status_b1 = plan_status) %>%
  rename(retire_benefit_rate_b1 = RETIREMENT_BENEFIT_RATE_PERC) %>%
  rename(retire_benefit_desc_b1 = RETIREMENT_BENEFIT_RATE_DESC) %>%
  rename(afc_b1 = AFC) %>%
  rename(normal_retire_age_b1 = YR) %>%
  rename(normal_retire_desc_b1 = NORMAL_RETIREMENT_AGE_DESC) %>%
  rename(benefit_type_b1 = N) %>%
  rename(benefit_type_desc_b1 = NORMAL_FORM_OF_BENEFIT_DESC) %>%
  rename(vesting_b1 = VS) %>%
  rename(social_security_b1 = SS) %>%
  rename(cola_b1 = COL) %>%
  rename(year_full_vest_b1 = year_full_vest)

blrs_b1$plan_sponsor <- str_trim(blrs_b1$plan_sponsor)



### Appendix B2 - year 2006 does not exist
blrs_b2_2005$offer_drop <- NA
blrs_b2_2007$offer_drop <- NA
blrs_b2_2008$offer_drop <- NA
blrs_b2_2009$offer_drop <- NA
blrs_b2_2010$offer_drop <- NA
blrs_b2_2011$offer_drop <- NA


blrs_b2_2005$year <- as.numeric("2005")
blrs_b2_2007$year <- as.numeric("2007")
blrs_b2_2008$year <- as.numeric("2008")
blrs_b2_2009$year <- as.numeric("2009")
blrs_b2_2010$year <- as.numeric("2010")
blrs_b2_2011$year <- as.numeric("2011")
blrs_b2_2012$year <- as.numeric("2012")
blrs_b2_2013$year <- as.numeric("2013")
blrs_b2_2014$year <- as.numeric("2014")
blrs_b2_2015$year <- as.numeric("2015")
blrs_b2_2016$year <- as.numeric("2016")
blrs_b2_2017$year <- as.numeric("2017")

blrs_b2 <- rbind(blrs_b2_2005, blrs_b2_2007, blrs_b2_2008, blrs_b2_2009, blrs_b2_2010, blrs_b2_2011, blrs_b2_2012, blrs_b2_2013, blrs_b2_2014, blrs_b2_2015, blrs_b2_2016, blrs_b2_2017)

#rename
blrs_b2 <- blrs_b2 %>%
  rename(plan_sponsor = CITY_OR_DISTRICT) %>%
  rename(plan_type = TYP_SYS) %>%
  rename(disability_benefit_b2 = DISABILITY_BENEFITS_Y) %>%
  rename(disability_benefit_desc_b2 = DISABILITY_BENEFITS_DESC) %>%
  rename(death_benefit_b2 = DEATH_BENEFITS_Y) %>%
  rename(death_benefit_desc_b2 = DEATH_BENEFITS_DESC) %>%
  rename(early_retire_benefit_b2 = EARLY_RETIREMENT_BENEFITS_Y) %>%
  rename(early_retire_benefit_desc_b2 = EARLY_RETIREMENT_BENEFITS_DESC) %>%
  rename(offer_drop_b2 = offer_drop)

blrs_b2$plan_sponsor <- str_trim(blrs_b2$plan_sponsor)



### Appendix C 
blrs_c_2015$ASSETS_EQUITIES <- NA
blrs_c_2016$ASSETS_EQUITIES <- NA
blrs_c_2017$ASSETS_EQUITIES <- NA

blrs_c_2015$ASSETS_INSURANCE <- NA
blrs_c_2016$ASSETS_INSURANCE <- NA
blrs_c_2017$ASSETS_INSURANCE <- NA

blrs_c_2015$ASSETS_FIXED_INC <- NA
blrs_c_2016$ASSETS_FIXED_INC <- NA
blrs_c_2017$ASSETS_FIXED_INC <- NA

blrs_c_2009$OTHER <- NA
blrs_c_2010$OTHER <- NA
blrs_c_2011$OTHER <- NA
blrs_c_2012$OTHER <- NA
blrs_c_2013$OTHER <- NA
blrs_c_2014$OTHER <- NA
blrs_c_2015$OTHER <- NA
blrs_c_2016$OTHER <- NA
blrs_c_2017$OTHER <- NA

blrs_c_2005$COMMENTS <- NA
blrs_c_2006$COMMENTS <- NA
blrs_c_2007$COMMENTS <- NA
blrs_c_2014$COMMENTS <- NA
blrs_c_2015$COMMENTS <- NA
blrs_c_2016$COMMENTS <- NA
blrs_c_2017$COMMENTS <- NA

blrs_c_2005$drop_assets_amt <- NA
blrs_c_2006$drop_assets_amt <- NA
blrs_c_2007$drop_assets_amt <- NA
blrs_c_2008$drop_assets_amt <- NA

blrs_c_2005$receivable <- NA
blrs_c_2006$receivable <- NA
blrs_c_2007$receivable <- NA
blrs_c_2008$receivable <- NA
blrs_c_2009$receivable <- NA
blrs_c_2010$receivable <- NA
blrs_c_2011$receivable <- NA
blrs_c_2012$receivable <- NA
blrs_c_2013$receivable <- NA
blrs_c_2014$receivable <- NA

blrs_c_2005$equity_intl <- NA
blrs_c_2006$equity_intl <- NA
blrs_c_2007$equity_intl <- NA
blrs_c_2008$equity_intl <- NA
blrs_c_2009$equity_intl <- NA
blrs_c_2010$equity_intl <- NA
blrs_c_2011$equity_intl <- NA
blrs_c_2012$equity_intl <- NA
blrs_c_2013$equity_intl <- NA
blrs_c_2014$equity_intl <- NA

blrs_c_2005$equity_domestic <- NA
blrs_c_2006$equity_domestic <- NA
blrs_c_2007$equity_domestic <- NA
blrs_c_2008$equity_domestic <- NA
blrs_c_2009$equity_domestic <- NA
blrs_c_2010$equity_domestic <- NA
blrs_c_2011$equity_domestic <- NA
blrs_c_2012$equity_domestic <- NA
blrs_c_2013$equity_domestic <- NA
blrs_c_2014$equity_domestic <- NA

blrs_c_2005$fix_inc_intl <- NA
blrs_c_2006$fix_inc_intl <- NA
blrs_c_2007$fix_inc_intl <- NA
blrs_c_2008$fix_inc_intl <- NA
blrs_c_2009$fix_inc_intl <- NA
blrs_c_2010$fix_inc_intl <- NA
blrs_c_2011$fix_inc_intl <- NA
blrs_c_2012$fix_inc_intl <- NA
blrs_c_2013$fix_inc_intl <- NA
blrs_c_2014$fix_inc_intl <- NA

blrs_c_2005$fix_inc_domestic <- NA
blrs_c_2006$fix_inc_domestic <- NA
blrs_c_2007$fix_inc_domestic <- NA
blrs_c_2008$fix_inc_domestic <- NA
blrs_c_2009$fix_inc_domestic <- NA
blrs_c_2010$fix_inc_domestic <- NA
blrs_c_2011$fix_inc_domestic <- NA
blrs_c_2012$fix_inc_domestic <- NA
blrs_c_2013$fix_inc_domestic <- NA
blrs_c_2014$fix_inc_domestic <- NA

blrs_c_2005$real_estate <- NA
blrs_c_2006$real_estate <- NA
blrs_c_2007$real_estate <- NA
blrs_c_2008$real_estate <- NA
blrs_c_2009$real_estate <- NA
blrs_c_2010$real_estate <- NA
blrs_c_2011$real_estate <- NA
blrs_c_2012$real_estate <- NA
blrs_c_2013$real_estate <- NA
blrs_c_2014$real_estate <- NA

blrs_c_2005$alter_invest <- NA
blrs_c_2006$alter_invest <- NA
blrs_c_2007$alter_invest <- NA
blrs_c_2008$alter_invest <- NA
blrs_c_2009$alter_invest <- NA
blrs_c_2010$alter_invest <- NA
blrs_c_2011$alter_invest <- NA
blrs_c_2012$alter_invest <- NA
blrs_c_2013$alter_invest <- NA
blrs_c_2014$alter_invest <- NA

blrs_c_2005$other_assets <- NA
blrs_c_2006$other_assets <- NA
blrs_c_2007$other_assets <- NA
blrs_c_2008$other_assets <- NA
blrs_c_2009$other_assets <- NA
blrs_c_2010$other_assets <- NA
blrs_c_2011$other_assets <- NA
blrs_c_2012$other_assets <- NA
blrs_c_2013$other_assets <- NA
blrs_c_2014$other_assets <- NA

blrs_c_2005$liabilities <- NA
blrs_c_2006$liabilities <- NA
blrs_c_2007$liabilities <- NA
blrs_c_2008$liabilities <- NA
blrs_c_2009$liabilities <- NA
blrs_c_2010$liabilities <- NA
blrs_c_2011$liabilities <- NA
blrs_c_2012$liabilities <- NA
blrs_c_2013$liabilities <- NA
blrs_c_2014$liabilities <- NA

blrs_c_2005$year <- as.numeric("2005")
blrs_c_2006$year <- as.numeric("2006")
blrs_c_2007$year <- as.numeric("2007")
blrs_c_2008$year <- as.numeric("2008")
blrs_c_2009$year <- as.numeric("2009")
blrs_c_2010$year <- as.numeric("2010")
blrs_c_2011$year <- as.numeric("2011")
blrs_c_2012$year <- as.numeric("2012")
blrs_c_2013$year <- as.numeric("2013")
blrs_c_2014$year <- as.numeric("2014")
blrs_c_2015$year <- as.numeric("2015")
blrs_c_2016$year <- as.numeric("2016")
blrs_c_2017$year <- as.numeric("2017")

blrs_c <- rbind(blrs_c_2005, blrs_c_2006, blrs_c_2007, blrs_c_2008, blrs_c_2009, blrs_c_2010, blrs_c_2011, blrs_c_2012, blrs_c_2013, blrs_c_2014, blrs_c_2015, blrs_c_2016, blrs_c_2017)

#rename
blrs_c <- blrs_c %>%
  rename(plan_sponsor = CITY_OR_DISTRICT) %>%
  rename(plan_type = TYP_SYS) %>%
  rename(val_date_c = VAL_DATE) %>%
  rename(equity_c = ASSETS_EQUITIES) %>%
  rename(cash_equ_c = ASSETS_CASH_EQUIV) %>%
  rename(insurance_c = ASSETS_INSURANCE) %>%
  rename(fix_inc_c = ASSETS_FIXED_INC) %>%
  rename(total_assets_c = TOTAL_ASSETS) %>%
  rename(current_val_c = OTHER) %>%
  rename(comments_c = COMMENTS) %>%
  rename(drop_assets_amt_c = drop_assets_amt) %>%
  rename(receivable_c = receivable) %>%
  rename(equity_intl_c = equity_intl) %>%
  rename(equity_domestic_c = equity_domestic) %>%
  rename(fix_inc_intl_c = fix_inc_intl) %>%
  rename(fix_inc_domestic_c = fix_inc_domestic) %>%
  rename(real_estate_c = real_estate) %>%
  rename(alter_invest_c = alter_invest) %>%
  rename(other_assets_c = other_assets) %>%
  rename(liabilities_c = liabilities)

blrs_c$plan_sponsor <- str_trim(blrs_c$plan_sponsor)



### Appendix D 
blrs_d_2009$C_V <- NA
blrs_d_2010$C_V <- NA
blrs_d_2011$C_V <- NA
blrs_d_2012$C_V <- NA
blrs_d_2013$C_V <- NA
blrs_d_2014$C_V <- NA
blrs_d_2015$C_V <- NA
blrs_d_2016$C_V <- NA
blrs_d_2017$C_V <- NA

blrs_d_2009$RATIO_ASSETS_LIABILITIES_VESTD <- NA
blrs_d_2010$RATIO_ASSETS_LIABILITIES_VESTD <- NA
blrs_d_2011$RATIO_ASSETS_LIABILITIES_VESTD <- NA
blrs_d_2012$RATIO_ASSETS_LIABILITIES_VESTD <- NA
blrs_d_2013$RATIO_ASSETS_LIABILITIES_VESTD <- NA
blrs_d_2014$RATIO_ASSETS_LIABILITIES_VESTD <- NA
blrs_d_2015$RATIO_ASSETS_LIABILITIES_VESTD <- NA
blrs_d_2016$RATIO_ASSETS_LIABILITIES_VESTD <- NA
blrs_d_2017$RATIO_ASSETS_LIABILITIES_VESTD <- NA

blrs_d_2009$RATIO_ASSETS_LIABILITIES_NVSTD <- NA
blrs_d_2010$RATIO_ASSETS_LIABILITIES_NVSTD <- NA
blrs_d_2011$RATIO_ASSETS_LIABILITIES_NVSTD <- NA
blrs_d_2012$RATIO_ASSETS_LIABILITIES_NVSTD <- NA
blrs_d_2013$RATIO_ASSETS_LIABILITIES_NVSTD <- NA
blrs_d_2014$RATIO_ASSETS_LIABILITIES_NVSTD <- NA
blrs_d_2015$RATIO_ASSETS_LIABILITIES_NVSTD <- NA
blrs_d_2016$RATIO_ASSETS_LIABILITIES_NVSTD <- NA
blrs_d_2017$RATIO_ASSETS_LIABILITIES_NVSTD <- NA

blrs_d_2015$COMMENTS <- NA
blrs_d_2016$COMMENTS <- NA
blrs_d_2017$COMMENTS <- NA

blrs_d_2005$RevisedIndicator <- NA
blrs_d_2006$RevisedIndicator <- NA
blrs_d_2007$RevisedIndicator <- NA
blrs_d_2008$RevisedIndicator <- NA

blrs_d_2005$PlanChange <- NA
blrs_d_2006$PlanChange <- NA
blrs_d_2007$PlanChange <- NA
blrs_d_2008$PlanChange <- NA

blrs_d_2005$total_PAVB <- NA
blrs_d_2006$total_PAVB <- NA
blrs_d_2007$total_PAVB <- NA
blrs_d_2008$total_PAVB <- NA
blrs_d_2009$total_PAVB <- NA
blrs_d_2010$total_PAVB <- NA

blrs_d_2005$FASB35_ratio <- NA
blrs_d_2006$FASB35_ratio <- NA
blrs_d_2007$FASB35_ratio <- NA
blrs_d_2008$FASB35_ratio <- NA
blrs_d_2009$FASB35_ratio <- NA
blrs_d_2010$FASB35_ratio <- NA

blrs_d_2005$year <- as.numeric("2005")
blrs_d_2006$year <- as.numeric("2006")
blrs_d_2007$year <- as.numeric("2007")
blrs_d_2008$year <- as.numeric("2008")
blrs_d_2009$year <- as.numeric("2009")
blrs_d_2010$year <- as.numeric("2010")
blrs_d_2011$year <- as.numeric("2011")
blrs_d_2012$year <- as.numeric("2012")
blrs_d_2013$year <- as.numeric("2013")
blrs_d_2014$year <- as.numeric("2014")
blrs_d_2015$year <- as.numeric("2015")
blrs_d_2016$year <- as.numeric("2016")
blrs_d_2017$year <- as.numeric("2017")

blrs_d <- rbind(blrs_d_2005, blrs_d_2006, blrs_d_2007, blrs_d_2008, blrs_d_2009, blrs_d_2010, blrs_d_2011, blrs_d_2012, blrs_d_2013, blrs_d_2014, blrs_d_2015, blrs_d_2016, blrs_d_2017)

#rename
blrs_d <- blrs_d %>%
  rename(plan_sponsor = CITY_OR_DISTRICT) %>%
  rename(plan_type = TYP_SYS) %>%
  rename(val_date_d = VAL_DATE) %>%
  rename(current_val_d = C_V) %>%
  rename(total_assets_d = TOTAL_ASSETS) %>%
  rename(liability_ret_ben_d = LIABILITY_FOR_RET_BEN) %>%
  rename(liability_ee_contrib_d = LIABILITY_FOR_EE_CONTRIB) %>%
  rename(liability_active_d = LIABILITY_FOR_ACTIVES) %>%
  rename(assetToliability_inact_d = RATIO_ASSETS_LIABILITIES_INACT) %>%
  rename(assetToliability_ee_d = RATIO_ASSETS_LIABILITIES_CONTR) %>%
  rename(assetToliability_vestd_d = RATIO_ASSETS_LIABILITIES_VESTD) %>%
  rename(assetToliability_nvstd_d = RATIO_ASSETS_LIABILITIES_NVSTD) %>%
  rename(assetToliability_actvs_d = RATIO_ASSETS_LIABILITIES_ACTVS) %>%
  rename(comments_d = COMMENTS) %>%
  rename(revised_indicator_d = RevisedIndicator) %>%
  rename(plan_change_d = PlanChange) %>%
  rename(total_pvab_d = total_PAVB) %>%
  rename(fasb35ratio_d = FASB35_ratio) 

blrs_d$plan_sponsor <- str_trim(blrs_d$plan_sponsor)



### Appendix E 
blrs_e_2014$OLD_PLN <- NA
blrs_e_2015$OLD_PLN <- NA
blrs_e_2016$OLD_PLN <- NA
blrs_e_2017$OLD_PLN <- NA

blrs_e_2014$COMMENTS <- NA
blrs_e_2015$COMMENTS <- NA
blrs_e_2016$COMMENTS <- NA
blrs_e_2017$COMMENTS <- NA

blrs_e_2005$Market_Value_Return_Actual <- NA
blrs_e_2006$Market_Value_Return_Actual <- NA
blrs_e_2007$Market_Value_Return_Actual <- NA
blrs_e_2008$Market_Value_Return_Actual <- NA
blrs_e_2009$Market_Value_Return_Actual <- NA
blrs_e_2010$Market_Value_Return_Actual <- NA
blrs_e_2011$Market_Value_Return_Actual <- NA
blrs_e_2012$Market_Value_Return_Actual <- NA

blrs_e_2005$year <- as.numeric("2005")
blrs_e_2006$year <- as.numeric("2006")
blrs_e_2007$year <- as.numeric("2007")
blrs_e_2008$year <- as.numeric("2008")
blrs_e_2009$year <- as.numeric("2009")
blrs_e_2010$year <- as.numeric("2010")
blrs_e_2011$year <- as.numeric("2011")
blrs_e_2012$year <- as.numeric("2012")
blrs_e_2013$year <- as.numeric("2013")
blrs_e_2014$year <- as.numeric("2014")
blrs_e_2015$year <- as.numeric("2015")
blrs_e_2016$year <- as.numeric("2016")
blrs_e_2017$year <- as.numeric("2017")

blrs_e <- rbind(blrs_e_2005, blrs_e_2006, blrs_e_2007, blrs_e_2008, blrs_e_2009, blrs_e_2010, blrs_e_2011, blrs_e_2012, blrs_e_2013, blrs_e_2014, blrs_e_2015, blrs_e_2016, blrs_e_2017)

#rename
blrs_e <- blrs_e %>%
  rename(plan_sponsor = CITY_OR_DISTRICT) %>%
  rename(plan_type = TYP_SYS) %>%
  rename(val_date_d = VAL_DATE) %>%
  rename(fund_method_e = FUND_METHOD) %>%
  rename(old_plan_e = OLD_PLN) %>%
  rename(planyr_ended_e = PLNYR_ENDED) %>%
  rename(salary_assmp_e = SALRY_ASSMP) %>%
  rename(actual_salary_e = SALRY_ACTUL) %>%
  rename(interest_assmp_e = INT_ASSMP) %>%
  rename(actual_return_e = INT_ACTUL) %>%
  rename(pay_gwth_assmp_e = PR_GR_ASSMP) %>%
  rename(assmp_retire_age_e = RETIREMENT_AGE_ASSUMPTION_YR) %>%
  rename(retire_age_desc_e = RETIREMENT_AGE_ASSUMPTION_DESC) %>%
  rename(current_val_e = COMMENTS) %>%
  rename(actual_mv_return_e = Market_Value_Return_Actual)

blrs_e$plan_sponsor <- str_trim(blrs_e$plan_sponsor)



### Appendix F 
blrs_f_2015$COMMENTS <- NA
blrs_f_2016$COMMENTS <- NA
blrs_f_2017$COMMENTS <- NA

blrs_f_2005$MEMBR_DROP <- NA
blrs_f_2006$MEMBR_DROP <- NA
blrs_f_2007$MEMBR_DROP <- NA
blrs_f_2008$MEMBR_DROP <- NA
blrs_f_2009$MEMBR_DROP <- NA
blrs_f_2010$MEMBR_DROP <- NA
blrs_f_2011$MEMBR_DROP <- NA

blrs_f_2005$year <- as.numeric("2005")
blrs_f_2006$year <- as.numeric("2006")
blrs_f_2007$year <- as.numeric("2007")
blrs_f_2008$year <- as.numeric("2008")
blrs_f_2009$year <- as.numeric("2009")
blrs_f_2010$year <- as.numeric("2010")
blrs_f_2011$year <- as.numeric("2011")
blrs_f_2012$year <- as.numeric("2012")
blrs_f_2013$year <- as.numeric("2013")
blrs_f_2014$year <- as.numeric("2014")
blrs_f_2015$year <- as.numeric("2015")
blrs_f_2016$year <- as.numeric("2016")
blrs_f_2017$year <- as.numeric("2017")

blrs_f <- rbind(blrs_f_2005, blrs_f_2006, blrs_f_2007, blrs_f_2008, blrs_f_2009, blrs_f_2010, blrs_f_2011, blrs_f_2012, blrs_f_2013, blrs_f_2014, blrs_f_2015, blrs_f_2016, blrs_f_2017)

#rename
blrs_f <- blrs_f %>%
  rename(plan_sponsor = CITY_OR_DISTRICT) %>%
  rename(plan_type = TYP_SYS) %>%
  rename(city_population_f = CITY_POPULATN) %>%
  rename(county_name_f = COUNTY_NAME) %>%
  rename(county_population_f = COUNTY_POPULATION) %>%
  rename(active_member_f = MEMBR_ACTIV) %>%
  rename(retire_member_f = MEMBR_RETRD) %>%
  rename(terminated_member_f = MEMBR_TERMD) %>%
  rename(comments_f = COMMENTS) %>%
  rename(drop_member_f = MEMBR_DROP) 

blrs_f$plan_sponsor <- str_trim(blrs_f$plan_sponsor)



### Appendix G 
blrs_g_2009$C_V <- NA
blrs_g_2010$C_V <- NA
blrs_g_2011$C_V <- NA
blrs_g_2012$C_V <- NA
blrs_g_2013$C_V <- NA
blrs_g_2014$C_V <- NA
blrs_g_2015$C_V <- NA
blrs_g_2016$C_V <- NA
blrs_g_2017$C_V <- NA

blrs_g_2015$COMMENTS <- NA
blrs_g_2016$COMMENTS <- NA
blrs_g_2017$COMMENTS <- NA

blrs_g_2005$RevisedIndicator <- NA
blrs_g_2006$RevisedIndicator <- NA
blrs_g_2007$RevisedIndicator <- NA
blrs_g_2008$RevisedIndicator <- NA

blrs_g_2005$PlanChange <- NA
blrs_g_2006$PlanChange <- NA
blrs_g_2007$PlanChange <- NA
blrs_g_2008$PlanChange <- NA

blrs_g_2005$Total_Actuarial_Assets <- NA
blrs_g_2006$Total_Actuarial_Assets <- NA
blrs_g_2007$Total_Actuarial_Assets <- NA
blrs_g_2008$Total_Actuarial_Assets <- NA
blrs_g_2009$Total_Actuarial_Assets <- NA
blrs_g_2010$Total_Actuarial_Assets <- NA
blrs_g_2011$Total_Actuarial_Assets <- NA
blrs_g_2012$Total_Actuarial_Assets <- NA
blrs_g_2013$Total_Actuarial_Assets <- NA
blrs_g_2014$Total_Actuarial_Assets <- NA

blrs_g_2005$year <- as.numeric("2005")
blrs_g_2006$year <- as.numeric("2006")
blrs_g_2007$year <- as.numeric("2007")
blrs_g_2008$year <- as.numeric("2008")
blrs_g_2009$year <- as.numeric("2009")
blrs_g_2010$year <- as.numeric("2010")
blrs_g_2011$year <- as.numeric("2011")
blrs_g_2012$year <- as.numeric("2012")
blrs_g_2013$year <- as.numeric("2013")
blrs_g_2014$year <- as.numeric("2014")
blrs_g_2015$year <- as.numeric("2015")
blrs_g_2016$year <- as.numeric("2016")
blrs_g_2017$year <- as.numeric("2017")

blrs_g <- rbind(blrs_g_2005, blrs_g_2006, blrs_g_2007, blrs_g_2008, blrs_g_2009, blrs_g_2010, blrs_g_2011, blrs_g_2012, blrs_g_2013, blrs_g_2014, blrs_g_2015, blrs_g_2016, blrs_g_2017)

#rename
blrs_g <- blrs_g %>%
  rename(plan_sponsor = CITY_OR_DISTRICT) %>%
  rename(plan_type = TYP_SYS) %>%
  rename(val_date_g = VAL_DATE) %>%
  rename(current_val_g = C_V) %>%
  rename(fund_method_g = FUND_METHOD) %>%
  rename(total_assets_g = TOTAL_ASSETS) %>%
  rename(aal_g = AAL) %>%
  rename(uaal_g = UAAL) %>%
  rename(funded_ratio_g = FUNDED_RATIO) %>%
  rename(covered_pay_g =COVERED_PAYROLL) %>% 
  rename(uaalTocoverPay_g = UAAL_AS_PERC_OF_COVD_PAY) %>% 
  rename(comments_g = COMMENTS) %>% 
  rename(revised_indicator_g = RevisedIndicator) %>% 
  rename(plan_change_g = PlanChange) %>% 
  rename(total_actuarial_assets_g = Total_Actuarial_Assets)  

blrs_g$plan_sponsor <- str_trim(blrs_g$plan_sponsor)





#### Part 3. Joinging the tables
### For this part, first I created a list that includes every rbinded tables
### Second, I used full_join to retain every plan_sponsor that have been used in the table
blrs_list <- list(blrs_a, blrs_b1, blrs_b2, blrs_c, blrs_d, blrs_e, blrs_f, blrs_g)
blrs_full <- reduce(blrs_list, full_join, by = c("plan_sponsor", "plan_type", "year"))
#write_csv(blrs_full, "YK work/blrs_full_trim.csv") #blrs_full_trim contains trimmed sponsor name (It doesn't seem to affect the number of names for plan_sponsor)




#### Part 4. Working on Appendices A, E, G
### Find out every possible name for plan_sponsor
fre <- table(blrs_full$plan_sponsor)
print(fre)
fre2 <- as.data.frame(fre)
fre2 <- fre2 %>%
  rename(plan_sponsor = Var1) %>%
  mutate(plan_sponsor_clean = plan_sponsor) 
write_csv(fre2, "YK work/sponsor_match_table.csv")

### import comparison table
name_table <- read.csv("YK work/sponsor_match_table_ver3.csv")
name_table <- name_table %>% 
  select(plan_sponsor, plan_sponsor_clean)
name_table <- mutate_at(name_table, vars(plan_sponsor, plan_sponsor_clean), funs(as.character))


###
## Appendix A
fre_blrs_a <- table(blrs_a$plan_type)
print(fre_blrs_a)
blrs_a <- blrs_a %>%
  mutate(plan_type_clean = plan_type)
blrs_a$plan_type_clean <- blrs_a$plan_type_clean %>%
  str_replace_all("G&S", "GS")

blrs_a <- left_join(blrs_a, name_table, by = "plan_sponsor")  
#sum(is.na(blrs_a$plan_type_clean))
refcols <- c("plan_sponsor", "plan_sponsor_clean", "plan_type", "plan_type_clean")
blrs_a <- blrs_a[, c(refcols, setdiff(names(blrs_a), refcols))]

blrs_a <- blrs_a %>%
  rename(plan_sponsor_a = plan_sponsor) %>%
  rename(plan_type_a = plan_type)


## Appendix E
fre_blrs_e <- table(blrs_e$plan_type)
print(fre_blrs_e)
blrs_e <- blrs_e %>%
  mutate(plan_type_clean = plan_type)
blrs_e$plan_type_clean <- blrs_e$plan_type_clean %>%
  str_replace_all("G F", "GF") %>% 
  str_replace_all("G&S", "GS") 

blrs_e <- left_join(blrs_e, name_table, by = "plan_sponsor")  
#sum(is.na(blrs_e$plan_type_clean))
refcols <- c("plan_sponsor", "plan_sponsor_clean", "plan_type", "plan_type_clean")
blrs_e <- blrs_e[, c(refcols, setdiff(names(blrs_e), refcols))]

blrs_e <- blrs_e %>%
  rename(plan_sponsor_e = plan_sponsor) %>%
  rename(plan_type_e = plan_type)


## Appendix G
fre_blrs_g <- table(blrs_g$plan_type)
print(fre_blrs_g)
blrs_g <- blrs_g %>%
  mutate(plan_type_clean = plan_type)
blrs_g$plan_type_clean <- blrs_g$plan_type_clean %>%
  str_replace_all("G&S", "GS") 

blrs_g <- left_join(blrs_g, name_table, by = "plan_sponsor")  
#sum(is.na(blrs_e$plan_type_clean))
refcols <- c("plan_sponsor", "plan_sponsor_clean", "plan_type", "plan_type_clean")
blrs_g <- blrs_g[, c(refcols, setdiff(names(blrs_g), refcols))]

blrs_g <- blrs_g %>%
  rename(plan_sponsor_g = plan_sponsor) %>%
  rename(plan_type_g = plan_type)


### creating the full table for Appendices A, E, & G
blrs_list_aeg <- list(blrs_a, blrs_e, blrs_g)
blrs_full_aeg <- reduce(blrs_list_aeg, full_join, by = c("plan_sponsor_clean", "plan_type_clean", "year"))


### Calculation
# 1. Funeded ratio, 2.Unfunded Liabilities as Percent of Covered Payroll, 3. Annual Required Contribution as Percent of Payroll
blrs_full_aeg <- blrs_full_aeg %>%
  mutate(total_assets_g_new = as.numeric(total_assets_g)) %>%
  mutate(aal_g_new = as.numeric(aal_g)) %>%
  mutate(covered_pay_g_new = as.numeric(covered_pay_g)) %>%
  mutate(reqd_contribution_a_new = as.numeric(reqd_contribution_a)) 
  mutate(funded_ratio_g_new = (total_assets_g_new/aal_g_new)*100) %>%
  mutate(uaalTocoverPay_g_new = ((aal_g_new - total_assets_g_new)/covered_pay_g_new)*100) %>%
  mutate(arcTocoverPay_a_new = (reqd_contribution_a_new/covered_pay_g_new)*100)
  
# 4. Assumed Return on Investments and Employee Contribution Levels  
blrs_full_aeg <- blrs_full_aeg %>%
  mutate(interest_assmp_e_new = as.numeric(interest_assmp_e)) %>%
  mutate(member_percent_a_new = as.numeric(member_percent_a))



write_csv(blrs_full_aeg, "YK work/blrs_full_aeg.csv") 








