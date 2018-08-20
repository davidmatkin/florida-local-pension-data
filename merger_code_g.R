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
blrs_g_2005 <- read_excel("2005 Report/2005-blrs-g.xls", 
                          na = c("N/A", "NA", "VR", "*"),
                          skip = 3,
                          col_names = c("CITY_OR_DISTRICT", "TYP_SYS", "VAL_DATE", 
                                        "C_V", "FUND_METHOD", "TOTAL_ASSETS", "AAL", 
                                        "UAAL", "FUNDED_RATIO", "COVERED_PAYROLL", 
                                        "UAAL_AS_PERC_OF_COVD_PAY", "COMMENTS"),
                          col_types = c("text", "text", "text",
                                        "text", "text", "text", "numeric",
                                        "numeric", "numeric", "text",
                                        "text", "text"))
blrs_g_2005$VAL_DATE <- as.Date(as.numeric(blrs_g_2005$VAL_DATE), origin = "1899-12-30")
blrs_g_2005$FUND_METHOD[blrs_g_2005$FUND_METHOD == "AGG/"] <- "AGG" 
blrs_g_2005$FUND_METHOD[blrs_g_2005$FUND_METHOD == "EAN/"] <- "EAN"
blrs_g_2005$FUND_METHOD[blrs_g_2005$FUND_METHOD == "FIL/"] <- "FIL"
blrs_g_2005$TOTAL_ASSETS[blrs_g_2005$TOTAL_ASSETS=="3+858"] <- NA
blrs_g_2005$TOTAL_ASSETS <- as.numeric(blrs_g_2005$TOTAL_ASSETS)
blrs_g_2005$COVERED_PAYROLL[blrs_g_2005$COVERED_PAYROLL == "9+6"] <- NA
blrs_g_2005$COVERED_PAYROLL <- as.numeric(blrs_g_2005$COVERED_PAYROLL)
blrs_g_2005$UAAL_AS_PERC_OF_COVD_PAY[blrs_g_2005$UAAL_AS_PERC_OF_COVD_PAY == "-64.35%"] <- -64.35
blrs_g_2005$UAAL_AS_PERC_OF_COVD_PAY[blrs_g_2005$UAAL_AS_PERC_OF_COVD_PAY == "-27.15%"] <- -27.15
blrs_g_2005$UAAL_AS_PERC_OF_COVD_PAY[blrs_g_2005$UAAL_AS_PERC_OF_COVD_PAY == "-10.30%"] <- -10.30
blrs_g_2005$UAAL_AS_PERC_OF_COVD_PAY[blrs_g_2005$UAAL_AS_PERC_OF_COVD_PAY == "0.0%"] <- 0
blrs_g_2005$UAAL_AS_PERC_OF_COVD_PAY[blrs_g_2005$UAAL_AS_PERC_OF_COVD_PAY == "55.8%"] <- 55.8
blrs_g_2005$UAAL_AS_PERC_OF_COVD_PAY[blrs_g_2005$UAAL_AS_PERC_OF_COVD_PAY == "========="] <- NA
blrs_g_2005$UAAL_AS_PERC_OF_COVD_PAY <- as.numeric(blrs_g_2005$UAAL_AS_PERC_OF_COVD_PAY)
blrs_g_2005$REPORT_YR <- as.numeric("2005")
blrs_g_2005$REVISED_INDICATOR <- NA
blrs_g_2005$PLAN_CHANGE <- NA
blrs_g_2005$TOTAL_MARKET_ASSETS <- NA

## 2006 data
blrs_g_2006 <- read_excel("2006 Report/2006-09 G - Funding Progress (GASB-25 basis) - 64e.xls", 
                          na = c("N/A", "NA", "VR", "*"),
                          skip = 3,
                          col_names = c("CITY_OR_DISTRICT", "TYP_SYS", "VAL_DATE", 
                                        "C_V", "FUND_METHOD", "TOTAL_ASSETS", "AAL", 
                                        "UAAL", "FUNDED_RATIO", "COVERED_PAYROLL", 
                                        "UAAL_AS_PERC_OF_COVD_PAY", "COMMENTS"),
                          col_types = c("text", "text", "text",
                                        "text", "text", "text", "numeric",
                                        "text", "numeric", "text",
                                        "text", "text"))
blrs_g_2006$VAL_DATE <- as.Date(as.numeric(blrs_g_2006$VAL_DATE), origin = "1899-12-30")
blrs_g_2006$FUND_METHOD[blrs_g_2006$FUND_METHOD == "AGG/"] <- "AGG" 
blrs_g_2006$FUND_METHOD[blrs_g_2006$FUND_METHOD == "EAN/"] <- "EAN"
blrs_g_2006$FUND_METHOD[blrs_g_2006$FUND_METHOD == "FIL/"] <- "FIL"
blrs_g_2006$TOTAL_ASSETS[blrs_g_2006$TOTAL_ASSETS=="3+858"] <- NA
blrs_g_2006$TOTAL_ASSETS <- as.numeric(blrs_g_2006$TOTAL_ASSETS)
blrs_g_2006$UAAL[blrs_g_2006$UAAL == "1892*"] <- 1892
blrs_g_2006$UAAL[blrs_g_2006$UAAL == "1991*"] <- 1991
blrs_g_2006$UAAL <- as.numeric(blrs_g_2006$UAAL)
blrs_g_2006$COVERED_PAYROLL[blrs_g_2006$COVERED_PAYROLL == "9+6"] <- NA
blrs_g_2006$COVERED_PAYROLL <- as.numeric(blrs_g_2006$COVERED_PAYROLL)
blrs_g_2006$UAAL_AS_PERC_OF_COVD_PAY[blrs_g_2006$UAAL_AS_PERC_OF_COVD_PAY == "-64.35%"] <- -64.35
blrs_g_2006$UAAL_AS_PERC_OF_COVD_PAY[blrs_g_2006$UAAL_AS_PERC_OF_COVD_PAY == "-27.15%"] <- -27.15
blrs_g_2006$UAAL_AS_PERC_OF_COVD_PAY[blrs_g_2006$UAAL_AS_PERC_OF_COVD_PAY == "-10.30%"] <- -10.30
blrs_g_2006$UAAL_AS_PERC_OF_COVD_PAY[blrs_g_2006$UAAL_AS_PERC_OF_COVD_PAY == "0.0%"] <- 0
blrs_g_2006$UAAL_AS_PERC_OF_COVD_PAY[blrs_g_2006$UAAL_AS_PERC_OF_COVD_PAY == "55.8%"] <- 55.8
blrs_g_2006$UAAL_AS_PERC_OF_COVD_PAY[blrs_g_2006$UAAL_AS_PERC_OF_COVD_PAY == "========="] <- NA
blrs_g_2006$UAAL_AS_PERC_OF_COVD_PAY <- as.numeric(blrs_g_2006$UAAL_AS_PERC_OF_COVD_PAY)
blrs_g_2006$REPORT_YR <- as.numeric("2006")
blrs_g_2006$REVISED_INDICATOR <- NA
blrs_g_2006$PLAN_CHANGE <- NA
blrs_g_2006$TOTAL_MARKET_ASSETS <- NA

### 2007 data
blrs_g_2007 <- read_excel("2007 Report/Appendix_G.xls", 
                          na = c("N/A", "NA", "VR", "*"),
                          skip = 3,
                          col_names = c("CITY_OR_DISTRICT", "X_1", "TYP_SYS","X_2", "VAL_DATE", 
                                        "C_V", "FUND_METHOD", "TOTAL_ASSETS", "AAL", 
                                        "UAAL", "FUNDED_RATIO", "COVERED_PAYROLL", 
                                        "UAAL_AS_PERC_OF_COVD_PAY", "COMMENTS"),
                          col_types = c("text", "text", "text", "text", "text",
                                        "text", "text", "text", "numeric",
                                        "text", "numeric", "text",
                                        "text", "text"))
blrs_g_2007 <- select(blrs_g_2007, -X_1, -X_2)
blrs_g_2007$VAL_DATE <- as.Date(as.numeric(blrs_g_2007$VAL_DATE), origin = "1899-12-30")
blrs_g_2007$FUND_METHOD[blrs_g_2007$FUND_METHOD == "AGG/"] <- "AGG" 
blrs_g_2007$FUND_METHOD[blrs_g_2007$FUND_METHOD == "EAN/"] <- "EAN"
blrs_g_2007$FUND_METHOD[blrs_g_2007$FUND_METHOD == "FIL/"] <- "FIL"
blrs_g_2007$TOTAL_ASSETS[blrs_g_2007$TOTAL_ASSETS=="3+858"] <- NA
blrs_g_2007$TOTAL_ASSETS <- as.numeric(blrs_g_2007$TOTAL_ASSETS)
blrs_g_2007$UAAL[blrs_g_2007$UAAL == "1842*"] <- 1842
blrs_g_2007$UAAL[blrs_g_2007$UAAL == "1892*"] <- 1892
blrs_g_2007$UAAL[blrs_g_2007$UAAL == "1991*"] <- 1991
blrs_g_2007$UAAL <- as.numeric(blrs_g_2007$UAAL)
blrs_g_2007$COVERED_PAYROLL[blrs_g_2007$COVERED_PAYROLL == "9+6"] <- NA
blrs_g_2007$COVERED_PAYROLL <- as.numeric(blrs_g_2007$COVERED_PAYROLL)
blrs_g_2007$UAAL_AS_PERC_OF_COVD_PAY[blrs_g_2007$UAAL_AS_PERC_OF_COVD_PAY == "-64.35%"] <- -64.35
blrs_g_2007$UAAL_AS_PERC_OF_COVD_PAY[blrs_g_2007$UAAL_AS_PERC_OF_COVD_PAY == "-27.15%"] <- -27.15
blrs_g_2007$UAAL_AS_PERC_OF_COVD_PAY[blrs_g_2007$UAAL_AS_PERC_OF_COVD_PAY == "-10.30%"] <- -10.30
blrs_g_2007$UAAL_AS_PERC_OF_COVD_PAY[blrs_g_2007$UAAL_AS_PERC_OF_COVD_PAY == "0.0%"] <- 0
blrs_g_2007$UAAL_AS_PERC_OF_COVD_PAY[blrs_g_2007$UAAL_AS_PERC_OF_COVD_PAY == "55.8%"] <- 55.8
blrs_g_2007$UAAL_AS_PERC_OF_COVD_PAY[blrs_g_2007$UAAL_AS_PERC_OF_COVD_PAY == "========="] <- NA
blrs_g_2007$UAAL_AS_PERC_OF_COVD_PAY <- as.numeric(blrs_g_2007$UAAL_AS_PERC_OF_COVD_PAY)
blrs_g_2007$REPORT_YR <- as.numeric("2007")
blrs_g_2007$REVISED_INDICATOR <- NA
blrs_g_2007$PLAN_CHANGE <- NA
blrs_g_2007$TOTAL_MARKET_ASSETS <- NA

### 2008 data
blrs_g_2008 <- read_excel("2008 Report/Appendix_G(1).xls", 
                          na = c("N/A", "NA", "VR", "*"),
                          skip = 3,
                          col_names = c("CITY_OR_DISTRICT", "TYP_SYS", "VAL_DATE", 
                                        "C_V", "FUND_METHOD", "TOTAL_ASSETS", "AAL", 
                                        "UAAL", "FUNDED_RATIO", "COVERED_PAYROLL", 
                                        "UAAL_AS_PERC_OF_COVD_PAY", "COMMENTS"),
                          col_types = c("text", "text", "text", 
                                        "text", "text", "text", "numeric",
                                        "text", "numeric", "text",
                                        "text", "text"))
blrs_g_2008$VAL_DATE <- as.Date(as.numeric(blrs_g_2008$VAL_DATE), origin = "1899-12-30")
blrs_g_2008$FUND_METHOD[blrs_g_2008$FUND_METHOD == "AGG/"] <- "AGG" 
blrs_g_2008$FUND_METHOD[blrs_g_2008$FUND_METHOD == "EAN/"] <- "EAN"
blrs_g_2008$FUND_METHOD[blrs_g_2008$FUND_METHOD == "FIL/"] <- "FIL"
blrs_g_2008$TOTAL_ASSETS[blrs_g_2008$TOTAL_ASSETS=="3+858"] <- NA
blrs_g_2008$TOTAL_ASSETS <- as.numeric(blrs_g_2008$TOTAL_ASSETS)
blrs_g_2008$UAAL[blrs_g_2008$UAAL == "1842*"] <- 1842
blrs_g_2008$UAAL[blrs_g_2008$UAAL == "1892*"] <- 1892
blrs_g_2008$UAAL[blrs_g_2008$UAAL == "1991*"] <- 1991
blrs_g_2008$UAAL[blrs_g_2008$UAAL == "3580*"] <- 3580
blrs_g_2008$UAAL <- as.numeric(blrs_g_2008$UAAL)
blrs_g_2008$COVERED_PAYROLL[blrs_g_2008$COVERED_PAYROLL == "9+6"] <- NA
blrs_g_2008$COVERED_PAYROLL <- as.numeric(blrs_g_2008$COVERED_PAYROLL)
blrs_g_2008$UAAL_AS_PERC_OF_COVD_PAY[blrs_g_2008$UAAL_AS_PERC_OF_COVD_PAY == "-64.35%"] <- -64.35
blrs_g_2008$UAAL_AS_PERC_OF_COVD_PAY[blrs_g_2008$UAAL_AS_PERC_OF_COVD_PAY == "-27.15%"] <- -27.15
blrs_g_2008$UAAL_AS_PERC_OF_COVD_PAY[blrs_g_2008$UAAL_AS_PERC_OF_COVD_PAY == "-10.30%"] <- -10.30
blrs_g_2008$UAAL_AS_PERC_OF_COVD_PAY[blrs_g_2008$UAAL_AS_PERC_OF_COVD_PAY == "0.0%"] <- 0
blrs_g_2008$UAAL_AS_PERC_OF_COVD_PAY[blrs_g_2008$UAAL_AS_PERC_OF_COVD_PAY == "55.8%"] <- 55.8
blrs_g_2008$UAAL_AS_PERC_OF_COVD_PAY[blrs_g_2008$UAAL_AS_PERC_OF_COVD_PAY == "========="] <- NA
blrs_g_2008$UAAL_AS_PERC_OF_COVD_PAY <- as.numeric(blrs_g_2008$UAAL_AS_PERC_OF_COVD_PAY)
blrs_g_2008$REPORT_YR <- as.numeric("2008")
blrs_g_2008$REVISED_INDICATOR <- NA
blrs_g_2008$PLAN_CHANGE <- NA
blrs_g_2008$TOTAL_MARKET_ASSETS <- NA

### 2009 data
blrs_g_2009 <- read_excel("2009 Report/Appendix_G.xls", 
                          na = c("N/A", "NA", "VR", "*"),
                          skip = 1,
                          col_names = c("CITY_OR_DISTRICT", "TYP_SYS", "VAL_DATE", 
                                        "REVISED_INDICATOR", "PLAN_CHANGE",
                                        "FUND_METHOD", "TOTAL_ASSETS", "AAL", 
                                        "UAAL", "FUNDED_RATIO", "COVERED_PAYROLL", 
                                        "UAAL_AS_PERC_OF_COVD_PAY", "COMMENTS"),
                          col_types = c("text", "text", "text", 
                                        "text", "text", 
                                        "text", "text", "numeric", 
                                        "text", "text", "text",
                                        "text", "text"))
blrs_g_2009$VAL_DATE <- as.Date(as.numeric(blrs_g_2009$VAL_DATE), origin = "1899-12-30")
blrs_g_2009$FUND_METHOD[blrs_g_2009$FUND_METHOD == "AGG/"] <- "AGG" 
blrs_g_2009$FUND_METHOD[blrs_g_2009$FUND_METHOD == "EAN/"] <- "EAN"
blrs_g_2009$FUND_METHOD[blrs_g_2009$FUND_METHOD == "FIL/"] <- "FIL"
blrs_g_2009$FUND_METHOD[blrs_g_2009$FUND_METHOD == "UC/"] <- "UC"
blrs_g_2009$FUND_METHOD[blrs_g_2009$FUND_METHOD == "agg/"] <- "AGG"
blrs_g_2009$FUND_METHOD[blrs_g_2009$FUND_METHOD == "EAB/"] <- "EAN"
blrs_g_2009$FUND_METHOD[blrs_g_2009$FUND_METHOD == "AGG."] <- "AGG"
blrs_g_2009$FUND_METHOD[blrs_g_2009$FUND_METHOD == "AGG//"] <- "AGG"
blrs_g_2009$FUND_METHOD[blrs_g_2009$FUND_METHOD == "PUC/"] <- "PUC"
blrs_g_2009$FUND_METHOD[blrs_g_2009$FUND_METHOD == "AAN/"] <- "AAN"
blrs_g_2009$FUND_METHOD[blrs_g_2009$FUND_METHOD == "ANN/"] <- "AAN"
blrs_g_2009$FUND_METHOD[blrs_g_2009$FUND_METHOD == "E"] <- "EAN"
blrs_g_2009$TOTAL_ASSETS <- as.numeric(blrs_g_2009$TOTAL_ASSETS)
blrs_g_2009$UAAL <- as.numeric(blrs_g_2009$UAAL)
blrs_g_2009$COVERED_PAYROLL <- as.numeric(blrs_g_2009$COVERED_PAYROLL)
blrs_g_2009$FUNDED_RATIO <- as.numeric(blrs_g_2009$FUNDED_RATIO)
blrs_g_2009$UAAL_AS_PERC_OF_COVD_PAY <- as.numeric(blrs_g_2009$UAAL_AS_PERC_OF_COVD_PAY)
blrs_g_2009$REPORT_YR <- as.numeric("2009")
blrs_g_2009$C_V <- NA
blrs_g_2009$TOTAL_MARKET_ASSETS <- NA

### 2010 data
blrs_g_2010 <- read_excel("2010 Report/Appendix_G.xls", 
                          na = c("N/A", "NA", "VR", "*"),
                          skip = 1,
                          col_names = c("CITY_OR_DISTRICT", "TYP_SYS", "VAL_DATE", 
                                        "REVISED_INDICATOR", "PLAN_CHANGE",
                                        "FUND_METHOD", "TOTAL_ASSETS", "AAL", 
                                        "UAAL", "FUNDED_RATIO", "COVERED_PAYROLL", 
                                        "UAAL_AS_PERC_OF_COVD_PAY", "COMMENTS"),
                          col_types = c("text", "text", "text", 
                                        "text", "text", 
                                        "text", "text", "numeric", 
                                        "text", "text", "text",
                                        "text", "text"))
blrs_g_2010$VAL_DATE <- as.Date(as.numeric(blrs_g_2010$VAL_DATE), origin = "1899-12-30")
blrs_g_2010$FUND_METHOD[blrs_g_2010$FUND_METHOD == "AGG/"] <- "AGG" 
blrs_g_2010$FUND_METHOD[blrs_g_2010$FUND_METHOD == "EAN/"] <- "EAN"
blrs_g_2010$FUND_METHOD[blrs_g_2010$FUND_METHOD == "FIL/"] <- "FIL"
blrs_g_2010$FUND_METHOD[blrs_g_2010$FUND_METHOD == "UC/"] <- "UC"
blrs_g_2010$FUND_METHOD[blrs_g_2010$FUND_METHOD == "agg/"] <- "AGG"
blrs_g_2010$FUND_METHOD[blrs_g_2010$FUND_METHOD == "EAB/"] <- "EAN"
blrs_g_2010$FUND_METHOD[blrs_g_2010$FUND_METHOD == "Ean"] <- "EAN"
blrs_g_2010$FUND_METHOD[blrs_g_2010$FUND_METHOD == "AGG."] <- "AGG"
blrs_g_2010$FUND_METHOD[blrs_g_2010$FUND_METHOD == "AGG//"] <- "AGG"
blrs_g_2010$FUND_METHOD[blrs_g_2010$FUND_METHOD == "AB"] <- "AGG"
blrs_g_2010$FUND_METHOD[blrs_g_2010$FUND_METHOD == "ACCR"] <- "AGG"
blrs_g_2010$FUND_METHOD[blrs_g_2010$FUND_METHOD == "PUC/"] <- "PUC"
blrs_g_2010$FUND_METHOD[blrs_g_2010$FUND_METHOD == "AAN/"] <- "AAN"
blrs_g_2010$FUND_METHOD[blrs_g_2010$FUND_METHOD == "ANN/"] <- "AAN"
blrs_g_2010$FUND_METHOD[blrs_g_2010$FUND_METHOD == "E"] <- "EAN"
blrs_g_2010$FUND_METHOD[blrs_g_2010$FUND_METHOD == "F"] <- "FIL"
blrs_g_2010$TOTAL_ASSETS <- as.numeric(blrs_g_2010$TOTAL_ASSETS)
blrs_g_2010$UAAL <- as.numeric(blrs_g_2010$UAAL)
blrs_g_2010$COVERED_PAYROLL <- as.numeric(blrs_g_2010$COVERED_PAYROLL)
blrs_g_2010$FUNDED_RATIO <- as.numeric(blrs_g_2010$FUNDED_RATIO)
blrs_g_2010$UAAL_AS_PERC_OF_COVD_PAY <- as.numeric(blrs_g_2010$UAAL_AS_PERC_OF_COVD_PAY)
blrs_g_2010$REPORT_YR <- as.numeric("2010")
blrs_g_2010$C_V <- NA
blrs_g_2010$TOTAL_MARKET_ASSETS <- NA

### 2011 data (For this year, xlsx file saved as xls file)
blrs_g_2011 <- read_excel("2011 Report/Appendix_G.xls", 
                          na = c("N/A", "NA", "VR", "*"),
                          skip = 1,
                          col_names = c("CITY_OR_DISTRICT", "TYP_SYS", "VAL_DATE", 
                                        "REVISED_INDICATOR", "PLAN_CHANGE",
                                        "FUND_METHOD", "TOTAL_ASSETS", "AAL", 
                                        "UAAL", "FUNDED_RATIO", "COVERED_PAYROLL", 
                                        "UAAL_AS_PERC_OF_COVD_PAY", "COMMENTS"),
                          col_types = c("text", "text", "text", 
                                        "text", "text", 
                                        "text", "text", "numeric", 
                                        "text", "text", "text",
                                        "text", "text"))
blrs_g_2011$VAL_DATE <- as.Date(as.numeric(blrs_g_2011$VAL_DATE), origin = "1899-12-30")
blrs_g_2011$FUND_METHOD[blrs_g_2011$FUND_METHOD == "AGG/"] <- "AGG" 
blrs_g_2011$FUND_METHOD[blrs_g_2011$FUND_METHOD == "EAN/"] <- "EAN"
blrs_g_2011$FUND_METHOD[blrs_g_2011$FUND_METHOD == "FIL/"] <- "FIL"
blrs_g_2011$FUND_METHOD[blrs_g_2011$FUND_METHOD == "UC/"] <- "UC"
blrs_g_2011$FUND_METHOD[blrs_g_2011$FUND_METHOD == "agg/"] <- "AGG"
blrs_g_2011$FUND_METHOD[blrs_g_2011$FUND_METHOD == "EAB/"] <- "EAN"
blrs_g_2011$FUND_METHOD[blrs_g_2011$FUND_METHOD == "Ean"] <- "EAN"
blrs_g_2011$FUND_METHOD[blrs_g_2011$FUND_METHOD == "AGG."] <- "AGG"
blrs_g_2011$FUND_METHOD[blrs_g_2011$FUND_METHOD == "AGG//"] <- "AGG"
blrs_g_2011$FUND_METHOD[blrs_g_2011$FUND_METHOD == "AB"] <- "AGG"
blrs_g_2011$FUND_METHOD[blrs_g_2011$FUND_METHOD == "ACCR"] <- "AGG"
blrs_g_2011$FUND_METHOD[blrs_g_2011$FUND_METHOD == "PUC/"] <- "PUC"
blrs_g_2011$FUND_METHOD[blrs_g_2011$FUND_METHOD == "AAN/"] <- "AAN"
blrs_g_2011$FUND_METHOD[blrs_g_2011$FUND_METHOD == "ANN/"] <- "AAN"
blrs_g_2011$FUND_METHOD[blrs_g_2011$FUND_METHOD == "E"] <- "EAN"
blrs_g_2011$FUND_METHOD[blrs_g_2011$FUND_METHOD == "F"] <- "FIL"
blrs_g_2011$TOTAL_ASSETS <- as.numeric(blrs_g_2011$TOTAL_ASSETS)
blrs_g_2011$UAAL <- as.numeric(blrs_g_2011$UAAL)
blrs_g_2011$COVERED_PAYROLL <- as.numeric(blrs_g_2011$COVERED_PAYROLL)
blrs_g_2011$FUNDED_RATIO <- as.numeric(gsub("\\%","",blrs_g_2011$FUNDED_RATIO))
blrs_g_2011$UAAL_AS_PERC_OF_COVD_PAY <- as.numeric(blrs_g_2011$UAAL_AS_PERC_OF_COVD_PAY)
blrs_g_2011$REPORT_YR <- as.numeric("2011")
blrs_g_2011$C_V <- NA
blrs_g_2011$TOTAL_MARKET_ASSETS <- NA

### 2012 data (For this year, xlsx file saved as xls file)
blrs_g_2012 <- read_excel("2012 Report/Appendix_G.xls", 
                          na = c("N/A", "NA", "VR", "*"),
                          skip = 1,
                          col_names = c("CITY_OR_DISTRICT", "TYP_SYS", "VAL_DATE", 
                                        "REVISED_INDICATOR", "PLAN_CHANGE",
                                        "FUND_METHOD", "TOTAL_ASSETS", "AAL", 
                                        "UAAL", "FUNDED_RATIO", "COVERED_PAYROLL", 
                                        "UAAL_AS_PERC_OF_COVD_PAY", "COMMENTS"),
                          col_types = c("text", "text", "text", 
                                        "text", "text", 
                                        "text", "text", "numeric", 
                                        "text", "text", "text",
                                        "text", "text"))
blrs_g_2012$VAL_DATE <- as.Date(as.numeric(blrs_g_2012$VAL_DATE), origin = "1899-12-30")
blrs_g_2012$FUND_METHOD[blrs_g_2012$FUND_METHOD == "AGG/"] <- "AGG" 
blrs_g_2012$FUND_METHOD[blrs_g_2012$FUND_METHOD == "EAN/"] <- "EAN"
blrs_g_2012$FUND_METHOD[blrs_g_2012$FUND_METHOD == "FIL/"] <- "FIL"
blrs_g_2012$FUND_METHOD[blrs_g_2012$FUND_METHOD == "UC/"] <- "UC"
blrs_g_2012$FUND_METHOD[blrs_g_2012$FUND_METHOD == "agg/"] <- "AGG"
blrs_g_2012$FUND_METHOD[blrs_g_2012$FUND_METHOD == "EAB/"] <- "EAN"
blrs_g_2012$FUND_METHOD[blrs_g_2012$FUND_METHOD == "Ean"] <- "EAN"
blrs_g_2012$FUND_METHOD[blrs_g_2012$FUND_METHOD == "AGG."] <- "AGG"
blrs_g_2012$FUND_METHOD[blrs_g_2012$FUND_METHOD == "AGG//"] <- "AGG"
blrs_g_2012$FUND_METHOD[blrs_g_2012$FUND_METHOD == "AB"] <- "AGG"
blrs_g_2012$FUND_METHOD[blrs_g_2012$FUND_METHOD == "ACCR"] <- "AGG"
blrs_g_2012$FUND_METHOD[blrs_g_2012$FUND_METHOD == "PUC/"] <- "PUC"
blrs_g_2012$FUND_METHOD[blrs_g_2012$FUND_METHOD == "AAN/"] <- "AAN"
blrs_g_2012$FUND_METHOD[blrs_g_2012$FUND_METHOD == "ANN/"] <- "AAN"
blrs_g_2012$FUND_METHOD[blrs_g_2012$FUND_METHOD == "E"] <- "EAN"
blrs_g_2012$FUND_METHOD[blrs_g_2012$FUND_METHOD == "F"] <- "FIL"
blrs_g_2012$TOTAL_ASSETS <- as.numeric(blrs_g_2012$TOTAL_ASSETS)
blrs_g_2012$UAAL <- as.numeric(blrs_g_2012$UAAL)
blrs_g_2012$COVERED_PAYROLL <- as.numeric(blrs_g_2012$COVERED_PAYROLL)
blrs_g_2012$FUNDED_RATIO <- as.numeric(gsub("\\%","",blrs_g_2012$FUNDED_RATIO))
blrs_g_2012$UAAL_AS_PERC_OF_COVD_PAY <- as.numeric(blrs_g_2012$UAAL_AS_PERC_OF_COVD_PAY)
blrs_g_2012$REPORT_YR <- as.numeric("2012")
blrs_g_2012$C_V <- NA
blrs_g_2012$TOTAL_MARKET_ASSETS <- NA

### 2013 data (For this year, xlsx file saved as xls file)
blrs_g_2013 <- read_excel("2013 Report/2013-11 - Appendix G - Funding Progress (GASB 25).xls", 
                          na = c("N/A", "NA", "VR", "*"),
                          skip = 1,
                          col_names = c("CITY_OR_DISTRICT", "TYP_SYS", "VAL_DATE", 
                                        "REVISED_INDICATOR", "PLAN_CHANGE",
                                        "FUND_METHOD", "TOTAL_ASSETS", "AAL", 
                                        "UAAL", "FUNDED_RATIO", "COVERED_PAYROLL", 
                                        "UAAL_AS_PERC_OF_COVD_PAY", "COMMENTS"),
                          col_types = c("text", "text", "text", 
                                        "text", "text", 
                                        "text", "text", "numeric", 
                                        "text", "text", "text",
                                        "text", "text"))
blrs_g_2013$VAL_DATE <- as.Date(as.numeric(blrs_g_2013$VAL_DATE), origin = "1899-12-30")
blrs_g_2013$FUND_METHOD[blrs_g_2013$FUND_METHOD == "AGG/"] <- "AGG" 
blrs_g_2013$FUND_METHOD[blrs_g_2013$FUND_METHOD == "EAN/"] <- "EAN"
blrs_g_2013$FUND_METHOD[blrs_g_2013$FUND_METHOD == "FIL/"] <- "FIL"
blrs_g_2013$FUND_METHOD[blrs_g_2013$FUND_METHOD == "UC/"] <- "UC"
blrs_g_2013$FUND_METHOD[blrs_g_2013$FUND_METHOD == "agg/"] <- "AGG"
blrs_g_2013$FUND_METHOD[blrs_g_2013$FUND_METHOD == "EAB/"] <- "EAN"
blrs_g_2013$FUND_METHOD[blrs_g_2013$FUND_METHOD == "Ean"] <- "EAN"
blrs_g_2013$FUND_METHOD[blrs_g_2013$FUND_METHOD == "AGG."] <- "AGG"
blrs_g_2013$FUND_METHOD[blrs_g_2013$FUND_METHOD == "AGG//"] <- "AGG"
blrs_g_2013$FUND_METHOD[blrs_g_2013$FUND_METHOD == "AB"] <- "AGG"
blrs_g_2013$FUND_METHOD[blrs_g_2013$FUND_METHOD == "ACCR"] <- "AGG"
blrs_g_2013$FUND_METHOD[blrs_g_2013$FUND_METHOD == "PUC/"] <- "PUC"
blrs_g_2013$FUND_METHOD[blrs_g_2013$FUND_METHOD == "AAN/"] <- "AAN"
blrs_g_2013$FUND_METHOD[blrs_g_2013$FUND_METHOD == "ANN/"] <- "AAN"
blrs_g_2013$FUND_METHOD[blrs_g_2013$FUND_METHOD == "E"] <- "EAN"
blrs_g_2013$FUND_METHOD[blrs_g_2013$FUND_METHOD == "F"] <- "FIL"
blrs_g_2013$TOTAL_ASSETS <- as.numeric(blrs_g_2013$TOTAL_ASSETS)
blrs_g_2013$UAAL <- as.numeric(blrs_g_2013$UAAL)
blrs_g_2013$COVERED_PAYROLL <- as.numeric(blrs_g_2013$COVERED_PAYROLL)
blrs_g_2013$FUNDED_RATIO <- as.numeric(gsub("\\%","",blrs_g_2013$FUNDED_RATIO))
blrs_g_2013$UAAL_AS_PERC_OF_COVD_PAY <- as.numeric(blrs_g_2013$UAAL_AS_PERC_OF_COVD_PAY)
blrs_g_2013$REPORT_YR <- as.numeric("2013")
blrs_g_2013$C_V <- NA
blrs_g_2013$TOTAL_MARKET_ASSETS <- NA

### 2014 data
blrs_g_2014 <- read_excel("2014 Report/2014-11 - Appendix G - Funding Progress (GASB 25).xls", 
                          na = c("N/A", "NA", "VR", "*"),
                          skip = 1,
                          col_names = c("CITY_OR_DISTRICT", "TYP_SYS", "VAL_DATE", 
                                        "REVISED_INDICATOR", "PLAN_CHANGE",
                                        "FUND_METHOD", "TOTAL_ASSETS", "AAL", 
                                        "UAAL", "FUNDED_RATIO", "COVERED_PAYROLL", 
                                        "UAAL_AS_PERC_OF_COVD_PAY", "COMMENTS"),
                          col_types = c("text", "text", "text", 
                                        "text", "text", 
                                        "text", "text", "numeric", 
                                        "text", "text", "text",
                                        "text", "text"))
blrs_g_2014$VAL_DATE <- as.Date(as.numeric(blrs_g_2014$VAL_DATE), origin = "1899-12-30")
blrs_g_2014$FUND_METHOD[blrs_g_2014$FUND_METHOD == "AGG/"] <- "AGG" 
blrs_g_2014$FUND_METHOD[blrs_g_2014$FUND_METHOD == "EAN/"] <- "EAN"
blrs_g_2014$FUND_METHOD[blrs_g_2014$FUND_METHOD == "FIL/"] <- "FIL"
blrs_g_2014$FUND_METHOD[blrs_g_2014$FUND_METHOD == "UC/"] <- "UC"
blrs_g_2014$FUND_METHOD[blrs_g_2014$FUND_METHOD == "agg/"] <- "AGG"
blrs_g_2014$FUND_METHOD[blrs_g_2014$FUND_METHOD == "EAB/"] <- "EAN"
blrs_g_2014$FUND_METHOD[blrs_g_2014$FUND_METHOD == "Ean"] <- "EAN"
blrs_g_2014$FUND_METHOD[blrs_g_2014$FUND_METHOD == "AGG."] <- "AGG"
blrs_g_2014$FUND_METHOD[blrs_g_2014$FUND_METHOD == "AGG//"] <- "AGG"
blrs_g_2014$FUND_METHOD[blrs_g_2014$FUND_METHOD == "AB"] <- "AGG"
blrs_g_2014$FUND_METHOD[blrs_g_2014$FUND_METHOD == "ACCR"] <- "AGG"
blrs_g_2014$FUND_METHOD[blrs_g_2014$FUND_METHOD == "PUC/"] <- "PUC"
blrs_g_2014$FUND_METHOD[blrs_g_2014$FUND_METHOD == "AAN/"] <- "AAN"
blrs_g_2014$FUND_METHOD[blrs_g_2014$FUND_METHOD == "ANN/"] <- "AAN"
blrs_g_2014$FUND_METHOD[blrs_g_2014$FUND_METHOD == "E"] <- "EAN"
blrs_g_2014$FUND_METHOD[blrs_g_2014$FUND_METHOD == "F"] <- "FIL"
blrs_g_2014$TOTAL_ASSETS <- as.numeric(blrs_g_2014$TOTAL_ASSETS)
blrs_g_2014$UAAL <- as.numeric(blrs_g_2014$UAAL)
blrs_g_2014$COVERED_PAYROLL <- as.numeric(blrs_g_2014$COVERED_PAYROLL)
blrs_g_2014$FUNDED_RATIO <- as.numeric(gsub("\\%","",blrs_g_2014$FUNDED_RATIO))
blrs_g_2014$UAAL_AS_PERC_OF_COVD_PAY <- as.numeric(blrs_g_2014$UAAL_AS_PERC_OF_COVD_PAY)
blrs_g_2014$REPORT_YR <- as.numeric("2014")
blrs_g_2014$C_V <- NA
blrs_g_2014$TOTAL_MARKET_ASSETS <- NA

### 2015 data
blrs_g_2015 <- read_excel("2015 Report/Appendix_G.xls", 
                          na = c("N/A", "NA", "VR", "*"),
                          skip = 1,
                          col_names = c("CITY_OR_DISTRICT", "TYP_SYS", "VAL_DATE", 
                                        "REVISED_INDICATOR", "PLAN_CHANGE",
                                        "FUND_METHOD", "TOTAL_MARKET_ASSETS", "AAL", 
                                        "UAAL", "FUNDED_RATIO", "COVERED_PAYROLL", 
                                        "UAAL_AS_PERC_OF_COVD_PAY", "TOTAL_ASSETS"),
                          col_types = c("text", "text", "text", 
                                        "text", "text", 
                                        "text", "text", "numeric", 
                                        "text", "text", "text",
                                        "text", "text"))
blrs_g_2015$VAL_DATE <- as.Date(as.numeric(blrs_g_2015$VAL_DATE), origin = "1899-12-30")
blrs_g_2015$FUND_METHOD[blrs_g_2015$FUND_METHOD == "AGG/"] <- "AGG" 
blrs_g_2015$FUND_METHOD[blrs_g_2015$FUND_METHOD == "EAN/"] <- "EAN"
blrs_g_2015$FUND_METHOD[blrs_g_2015$FUND_METHOD == "FIL/"] <- "FIL"
blrs_g_2015$FUND_METHOD[blrs_g_2015$FUND_METHOD == "UC/"] <- "UC"
blrs_g_2015$FUND_METHOD[blrs_g_2015$FUND_METHOD == "agg/"] <- "AGG"
blrs_g_2015$FUND_METHOD[blrs_g_2015$FUND_METHOD == "EAB/"] <- "EAN"
blrs_g_2015$FUND_METHOD[blrs_g_2015$FUND_METHOD == "Ean"] <- "EAN"
blrs_g_2015$FUND_METHOD[blrs_g_2015$FUND_METHOD == "AGG."] <- "AGG"
blrs_g_2015$FUND_METHOD[blrs_g_2015$FUND_METHOD == "AGG//"] <- "AGG"
blrs_g_2015$FUND_METHOD[blrs_g_2015$FUND_METHOD == "AB"] <- "AGG"
blrs_g_2015$FUND_METHOD[blrs_g_2015$FUND_METHOD == "ACCR"] <- "AGG"
blrs_g_2015$FUND_METHOD[blrs_g_2015$FUND_METHOD == "PUC/"] <- "PUC"
blrs_g_2015$FUND_METHOD[blrs_g_2015$FUND_METHOD == "AAN/"] <- "AAN"
blrs_g_2015$FUND_METHOD[blrs_g_2015$FUND_METHOD == "ANN/"] <- "AAN"
blrs_g_2015$FUND_METHOD[blrs_g_2015$FUND_METHOD == "E"] <- "EAN"
blrs_g_2015$FUND_METHOD[blrs_g_2015$FUND_METHOD == "F"] <- "FIL"
blrs_g_2015$TOTAL_MARKET_ASSETS <- as.numeric(blrs_g_2015$TOTAL_MARKET_ASSETS)
blrs_g_2015$TOTAL_ASSETS <- as.numeric(blrs_g_2015$TOTAL_ASSETS)
blrs_g_2015$UAAL <- as.numeric(blrs_g_2015$UAAL)
blrs_g_2015$COVERED_PAYROLL <- as.numeric(blrs_g_2015$COVERED_PAYROLL)
blrs_g_2015$FUNDED_RATIO <- as.numeric(gsub("\\%","",blrs_g_2015$FUNDED_RATIO))
blrs_g_2015$UAAL_AS_PERC_OF_COVD_PAY <- as.numeric(blrs_g_2015$UAAL_AS_PERC_OF_COVD_PAY)
blrs_g_2015$REPORT_YR <- as.numeric("2015")
blrs_g_2015$C_V <- NA
blrs_g_2015$COMMENTS <- NA

### 2016 data
blrs_g_2016 <- read_excel("2016 Report/Appendix_G.xls", 
                          na = c("N/A", "NA", "VR", "*"),
                          skip = 1,
                          col_names = c("CITY_OR_DISTRICT", "TYP_SYS", "VAL_DATE", 
                                        "REVISED_INDICATOR", "PLAN_CHANGE",
                                        "FUND_METHOD", "TOTAL_MARKET_ASSETS", "AAL", 
                                        "UAAL", "FUNDED_RATIO", "COVERED_PAYROLL", 
                                        "UAAL_AS_PERC_OF_COVD_PAY", "TOTAL_ASSETS"),
                          col_types = c("text", "text", "text", 
                                        "text", "text", 
                                        "text", "text", "numeric", 
                                        "text", "text", "text",
                                        "text", "text"))
blrs_g_2016$VAL_DATE <- as.Date(as.numeric(blrs_g_2016$VAL_DATE), origin = "1899-12-30")
blrs_g_2016$FUND_METHOD[blrs_g_2016$FUND_METHOD == "AGG/"] <- "AGG" 
blrs_g_2016$FUND_METHOD[blrs_g_2016$FUND_METHOD == "EAN/"] <- "EAN"
blrs_g_2016$FUND_METHOD[blrs_g_2016$FUND_METHOD == "FIL/"] <- "FIL"
blrs_g_2016$FUND_METHOD[blrs_g_2016$FUND_METHOD == "UC/"] <- "UC"
blrs_g_2016$FUND_METHOD[blrs_g_2016$FUND_METHOD == "agg/"] <- "AGG"
blrs_g_2016$FUND_METHOD[blrs_g_2016$FUND_METHOD == "EAB/"] <- "EAN"
blrs_g_2016$FUND_METHOD[blrs_g_2016$FUND_METHOD == "Ean"] <- "EAN"
blrs_g_2016$FUND_METHOD[blrs_g_2016$FUND_METHOD == "AGG."] <- "AGG"
blrs_g_2016$FUND_METHOD[blrs_g_2016$FUND_METHOD == "AGG//"] <- "AGG"
blrs_g_2016$FUND_METHOD[blrs_g_2016$FUND_METHOD == "AB"] <- "AGG"
blrs_g_2016$FUND_METHOD[blrs_g_2016$FUND_METHOD == "ACCR"] <- "AGG"
blrs_g_2016$FUND_METHOD[blrs_g_2016$FUND_METHOD == "PUC/"] <- "PUC"
blrs_g_2016$FUND_METHOD[blrs_g_2016$FUND_METHOD == "AAN/"] <- "AAN"
blrs_g_2016$FUND_METHOD[blrs_g_2016$FUND_METHOD == "ANN/"] <- "AAN"
blrs_g_2016$FUND_METHOD[blrs_g_2016$FUND_METHOD == "E"] <- "EAN"
blrs_g_2016$FUND_METHOD[blrs_g_2016$FUND_METHOD == "F"] <- "FIL"
blrs_g_2016$TOTAL_MARKET_ASSETS <- as.numeric(blrs_g_2016$TOTAL_MARKET_ASSETS)
blrs_g_2016$TOTAL_ASSETS <- as.numeric(blrs_g_2016$TOTAL_ASSETS)
blrs_g_2016$UAAL <- as.numeric(blrs_g_2016$UAAL)
blrs_g_2016$COVERED_PAYROLL <- as.numeric(blrs_g_2016$COVERED_PAYROLL)
blrs_g_2016$FUNDED_RATIO <- as.numeric(gsub("\\%","",blrs_g_2016$FUNDED_RATIO))*100
blrs_g_2016$UAAL_AS_PERC_OF_COVD_PAY <- as.numeric(blrs_g_2016$UAAL_AS_PERC_OF_COVD_PAY)
blrs_g_2016$REPORT_YR <- as.numeric("2016")
blrs_g_2016$C_V <- NA
blrs_g_2016$COMMENTS <- NA

### 2017 data
blrs_g_2017 <- read_excel("2017 Report/Appendix_G.xls", 
                          na = c("N/A", "NA", "VR", "*"),
                          skip = 1,
                          col_names = c("CITY_OR_DISTRICT", "TYP_SYS", "VAL_DATE", 
                                        "REVISED_INDICATOR", "PLAN_CHANGE",
                                        "FUND_METHOD", "TOTAL_MARKET_ASSETS", "AAL", 
                                        "UAAL", "FUNDED_RATIO", "COVERED_PAYROLL", 
                                        "UAAL_AS_PERC_OF_COVD_PAY", "TOTAL_ASSETS"),
                          col_types = c("text", "text", "text", 
                                        "text", "text", 
                                        "text", "text", "numeric", 
                                        "text", "text", "text",
                                        "text", "text"))
blrs_g_2017$VAL_DATE <- as.Date(as.numeric(blrs_g_2017$VAL_DATE), origin = "1899-12-30")
blrs_g_2017$FUND_METHOD[blrs_g_2017$FUND_METHOD == "AGG/"] <- "AGG" 
blrs_g_2017$FUND_METHOD[blrs_g_2017$FUND_METHOD == "EAN/"] <- "EAN"
blrs_g_2017$FUND_METHOD[blrs_g_2017$FUND_METHOD == "FIL/"] <- "FIL"
blrs_g_2017$FUND_METHOD[blrs_g_2017$FUND_METHOD == "UC/"] <- "UC"
blrs_g_2017$FUND_METHOD[blrs_g_2017$FUND_METHOD == "agg/"] <- "AGG"
blrs_g_2017$FUND_METHOD[blrs_g_2017$FUND_METHOD == "EAB/"] <- "EAN"
blrs_g_2017$FUND_METHOD[blrs_g_2017$FUND_METHOD == "Ean"] <- "EAN"
blrs_g_2017$FUND_METHOD[blrs_g_2017$FUND_METHOD == "AGG."] <- "AGG"
blrs_g_2017$FUND_METHOD[blrs_g_2017$FUND_METHOD == "AGG//"] <- "AGG"
blrs_g_2017$FUND_METHOD[blrs_g_2017$FUND_METHOD == "AB"] <- "AGG"
blrs_g_2017$FUND_METHOD[blrs_g_2017$FUND_METHOD == "ACCR"] <- "AGG"
blrs_g_2017$FUND_METHOD[blrs_g_2017$FUND_METHOD == "PUC/"] <- "PUC"
blrs_g_2017$FUND_METHOD[blrs_g_2017$FUND_METHOD == "AAN/"] <- "AAN"
blrs_g_2017$FUND_METHOD[blrs_g_2017$FUND_METHOD == "ANN/"] <- "AAN"
blrs_g_2017$FUND_METHOD[blrs_g_2017$FUND_METHOD == "E"] <- "EAN"
blrs_g_2017$FUND_METHOD[blrs_g_2017$FUND_METHOD == "F"] <- "FIL"
blrs_g_2017$TOTAL_MARKET_ASSETS <- as.numeric(blrs_g_2017$TOTAL_MARKET_ASSETS)
blrs_g_2017$TOTAL_ASSETS <- as.numeric(blrs_g_2017$TOTAL_ASSETS)
blrs_g_2017$UAAL <- as.numeric(blrs_g_2017$UAAL)
blrs_g_2017$COVERED_PAYROLL <- as.numeric(blrs_g_2017$COVERED_PAYROLL)
blrs_g_2017$FUNDED_RATIO <- as.numeric(gsub("\\%","",blrs_g_2017$FUNDED_RATIO))
blrs_g_2017$UAAL_AS_PERC_OF_COVD_PAY <- as.numeric(blrs_g_2017$UAAL_AS_PERC_OF_COVD_PAY)
blrs_g_2017$REPORT_YR <- as.numeric("2017")
blrs_g_2017$C_V <- NA
blrs_g_2017$COMMENTS <- NA

#### Part 2. Bind each year into a single table & relabel variables

blrs_g <- bind_rows(blrs_g_2005, blrs_g_2006, blrs_g_2007, blrs_g_2008, blrs_g_2009, blrs_g_2010, blrs_g_2011, blrs_g_2012, blrs_g_2013, blrs_g_2014, blrs_g_2015, blrs_g_2016, blrs_g_2017)

## reorder and rename columns

blrs_g <- blrs_g %>%
  select(plan_sponsor = CITY_OR_DISTRICT, 
         plan_type_g = TYP_SYS, 
         val_date_g = VAL_DATE, 
         report_year_g = REPORT_YR,
         funded_method_g = FUND_METHOD,
         current_version_g = C_V,
         total_assets_av_g = TOTAL_ASSETS,
         total_assets_mv_g = TOTAL_MARKET_ASSETS,
         aal_g = AAL,
         uaal_g = UAAL,
         funded_ratio_g = FUNDED_RATIO,
         covered_payroll_g = COVERED_PAYROLL,
         uaal_coveredpayroll_g = UAAL_AS_PERC_OF_COVD_PAY,
         revised_indicator_g = REVISED_INDICATOR,
         plan_change_g = PLAN_CHANGE,
         comments_g = COMMENTS)

blrs_g$plan_sponsor <- str_trim(blrs_g$plan_sponsor)
blrs_g$plan_sponsor <- str_squish(blrs_g$plan_sponsor)

blrs_g <- blrs_g[blrs_g$val_date_g>="2005-01-01",]
blrs_g$plan_type_g[blrs_g$plan_sponsor=="PALM BEACH LG-R"] <- "LG"


#### Part 4. Ensure clean identifiers

### Use this Excel file to identify sponsor names to change

sponsor_names <- table(blrs_g$plan_sponsor)
#print(sponsor_names)
sponsor_names <- as.data.frame(sponsor_names)
sponsor_names <- sponsor_names %>%
  rename(plan_sponsor = Var1) %>%
  mutate(plan_sponsor_clean = plan_sponsor) 
write_excel_csv(sponsor_names, "MatchingTables/appendix_g_to_match.csv")

### import comparison table
name_table <- read_excel("MatchingTables/appendix_g_matched.xls")
name_table <- name_table %>% 
  select(plan_sponsor, plan_sponsor_clean)

### consistent names for plan_type

blrs_g$plan_type_g[blrs_g$plan_type_g=="G&S"] <- "GS"

blrs_g <- right_join(name_table, blrs_g, by = "plan_sponsor")  

blrs_g <- arrange(blrs_g, plan_sponsor_clean, plan_type_g, val_date_g, report_year_g)

blrs_g <- blrs_g %>% rename(plan_sponsor_g = plan_sponsor)

blrs_g <- blrs_g %>%
  group_by(plan_sponsor_clean, plan_type_g, val_date_g) %>%
  filter(report_year_g == max(report_year_g))

save(blrs_g, file="blrs_g.RData")    





