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
blrs_f_2005 <- read_excel("2005 Report/2005-blrs-f.xls",
                          skip = 2,
                          col_names = c("CITY_OR_DISTRICT", "TYP_SYS", "CITY_POPULATION",
                                        "COUNTY_NAME", "COUNTY_POPULATION", "MEMBR_ACTIV",
                                        "MEMBR_RETRD", "MEMBR_TERMD", "COMMENTS"),
                          col_types = c("text", "text", "numeric", 
                                        "text", "numeric", "numeric",
                                        "numeric", "numeric", "text"))
blrs_f_2005$REPORT_YR <- as.numeric("2005")
blrs_f_2005$MEMBR_DROP <- as.numeric(NA)

## 2006 data
blrs_f_2006 <- read_excel("2006 Report/2006-08 F - Population Data - 14f.xls", 
                          skip = 4,
                          col_names = c("CITY_OR_DISTRICT", "TYP_SYS", "CITY_POPULATION",
                                        "COUNTY_NAME", "COUNTY_POPULATION", "MEMBR_ACTIV",
                                        "MEMBR_RETRD", "MEMBR_TERMD", "COMMENTS"),
                          col_types = c("text", "text", "numeric", 
                                        "text", "numeric", "numeric",
                                        "numeric", "numeric", "text"))
blrs_f_2006$REPORT_YR <- as.numeric("2006")
blrs_f_2006$MEMBR_DROP <- as.numeric(NA)


### 2007 data
blrs_f_2007 <- read_excel("2007 Report/Appendix_F.xls",
                          skip = 2,
                          col_names = c("CITY_OR_DISTRICT", "TYP_SYS", "CITY_POPULATION",
                                        "COUNTY_NAME", "COUNTY_POPULATION", "MEMBR_ACTIV",
                                        "MEMBR_RETRD", "MEMBR_TERMD", "COMMENTS"),
                          col_types = c("text", "text", "numeric", 
                                        "text", "numeric", "numeric",
                                        "numeric", "numeric", "text"))
blrs_f_2007$REPORT_YR <- as.numeric("2007")
blrs_f_2007$MEMBR_DROP <- as.numeric(NA)


### 2008 data
blrs_f_2008 <- read_excel("2008 Report/Appendix_F(1).xls", 
                          na = "-",
                          skip = 3,
                          col_names = c("CITY_OR_DISTRICT", "TYP_SYS", "CITY_POPULATION",
                                        "COUNTY_NAME", "COUNTY_POPULATION", "MEMBR_ACTIV",
                                        "MEMBR_RETRD", "MEMBR_TERMD", "COMMENTS"),
                          col_types = c("text", "text", "numeric", 
                                        "text", "numeric", "numeric",
                                        "numeric", "text", "text"))
blrs_f_2008$MEMBR_TERMD[blrs_f_2008$MEMBR_TERMD=="11*"] <- 11
blrs_f_2008$MEMBR_TERMD <- as.numeric(blrs_f_2008$MEMBR_TERMD)
blrs_f_2008$REPORT_YR <- as.numeric("2008")
blrs_f_2008$MEMBR_DROP <- as.numeric(NA)

### 2009 data
blrs_f_2009 <- read_excel("2009 Report/Appendix_F.xls",
                          na = "",
                          skip = 1,
                          col_names = c("CITY_OR_DISTRICT", "TYP_SYS", "CITY_POPULATION",
                                        "COUNTY_NAME", "COUNTY_POPULATION", "MEMBR_ACTIV",
                                        "MEMBR_RETRD", "MEMBR_TERMD", "COMMENTS"),
                          col_types = c("text", "text", "guess", 
                                        "text", "guess", "guess",
                                        "guess", "guess", "text"))
blrs_f_2009$CITY_POPULATION <- as.numeric(blrs_f_2009$CITY_POPULATION)
blrs_f_2009$COUNTY_POPULATION <- as.numeric(blrs_f_2009$COUNTY_POPULATION)
blrs_f_2009$MEMBR_ACTIV <- as.numeric(blrs_f_2009$MEMBR_ACTIV)
blrs_f_2009$MEMBR_RETRD <- as.numeric(blrs_f_2009$MEMBR_RETRD)
blrs_f_2009$MEMBR_TERMD <- as.numeric(blrs_f_2009$MEMBR_TERMD)
blrs_f_2009$REPORT_YR <- as.numeric("2009")
blrs_f_2009$MEMBR_DROP <- as.numeric(NA)

### 2010 data
blrs_f_2010 <- read_excel("2010 Report/Appendix_F.xls",
                          skip = 1,
                          col_names = c("CITY_OR_DISTRICT", "TYP_SYS", "CITY_POPULATION",
                                        "COUNTY_NAME", "COUNTY_POPULATION", "MEMBR_ACTIV",
                                        "MEMBR_RETRD", "MEMBR_TERMD", "COMMENTS"),
                          col_types = c("text", "text", "numeric", 
                                        "text", "numeric", "numeric",
                                        "numeric", "numeric", "text"))
blrs_f_2010$REPORT_YR <- as.numeric("2010")
blrs_f_2010$MEMBR_DROP <- as.numeric(NA)           


### 2011 data (For this year, I resaved xlsx file to xls file)
blrs_f_2011 <- read_excel("2011 Report/Appendix_F.xls", 
                          na = c("-",""),
                          range = "A2:I493",
                          col_names = c("CITY_OR_DISTRICT", "TYP_SYS", "CITY_POPULATION",
                                        "COUNTY_NAME", "COUNTY_POPULATION", "MEMBR_ACTIV",
                                        "MEMBR_RETRD", "MEMBR_TERMD", "COMMENTS"),
                          col_types = c("text", "text", "numeric", 
                                        "text", "numeric", "numeric",
                                        "numeric", "numeric", "text"))
blrs_f_2011$REPORT_YR <- as.numeric("2011")
blrs_f_2011$MEMBR_DROP <- as.numeric(NA)    

### 2012 data (For this year, xlsx file saved as xls file)
blrs_f_2012 <- read_excel("2012 Report/Appendix_F.xls", 
                          na = c("-",""),
                          range = "A2:J493",
                          col_names = c("CITY_OR_DISTRICT", "TYP_SYS", "CITY_POPULATION",
                                        "COUNTY_NAME", "COUNTY_POPULATION", "MEMBR_ACTIV",
                                        "MEMBR_RETRD", "MEMBR_DROP", "MEMBR_TERMD", "COMMENTS"),
                          col_types = c("text", "text", "text", 
                                        "text", "text", "text",
                                        "text", "text", "text", "text"))
blrs_f_2012$CITY_POPULATION <- as.numeric(gsub(",","",blrs_f_2012$CITY_POPULATION))
blrs_f_2012$COUNTY_POPULATION <- as.numeric(gsub(",","",blrs_f_2012$COUNTY_POPULATION))
blrs_f_2012$MEMBR_ACTIV <- as.numeric(blrs_f_2012$MEMBR_ACTIV)
blrs_f_2012$MEMBR_RETRD <- as.numeric(blrs_f_2012$MEMBR_RETRD)
blrs_f_2012$MEMBR_DROP <- as.numeric(blrs_f_2012$MEMBR_DROP)
blrs_f_2012$MEMBR_TERMD <- as.numeric(blrs_f_2012$MEMBR_TERMD)
blrs_f_2012$REPORT_YR <- as.numeric("2012")

### 2013 data (For this year, xlsx file saved as xls file)
blrs_f_2013 <- read_excel("2013 Report/2013-10 - Appendix F - Population Data.xls", 
                          na = c("-",""),
                          range = "A2:J492",
                          col_names = c("CITY_OR_DISTRICT", "TYP_SYS", "CITY_POPULATION",
                                        "COUNTY_NAME", "COUNTY_POPULATION", "MEMBR_ACTIV",
                                        "MEMBR_RETRD", "MEMBR_DROP", "MEMBR_TERMD", "COMMENTS"),
                          col_types = c("text", "text", "text", 
                                        "text", "text", "text",
                                        "text", "text", "text", "text"))
blrs_f_2013$CITY_POPULATION <- as.numeric(gsub(",","",blrs_f_2013$CITY_POPULATION))
blrs_f_2013$COUNTY_POPULATION <- as.numeric(gsub(",","",blrs_f_2013$COUNTY_POPULATION))
blrs_f_2013$MEMBR_ACTIV <- as.numeric(blrs_f_2013$MEMBR_ACTIV)
blrs_f_2013$MEMBR_RETRD <- as.numeric(blrs_f_2013$MEMBR_RETRD)
blrs_f_2013$MEMBR_DROP <- as.numeric(blrs_f_2013$MEMBR_DROP)
blrs_f_2013$MEMBR_TERMD <- as.numeric(blrs_f_2013$MEMBR_TERMD)
blrs_f_2013$REPORT_YR <- as.numeric("2013")

### 2014 data
blrs_f_2014 <- read_excel("2014 Report/2014-10 - Appendix F - Population Data.xls",
                          na = c("-",""),
                          range = "A2:J492",
                          col_names = c("CITY_OR_DISTRICT", "TYP_SYS", "CITY_POPULATION",
                                        "COUNTY_NAME", "COUNTY_POPULATION", "MEMBR_ACTIV",
                                        "MEMBR_RETRD", "MEMBR_DROP", "MEMBR_TERMD", "COMMENTS"),
                          col_types = c("text", "text", "text", 
                                        "text", "text", "text",
                                        "text", "text", "text", "text"))
blrs_f_2014$CITY_POPULATION <- as.numeric(gsub(",","",blrs_f_2014$CITY_POPULATION))
blrs_f_2014$COUNTY_POPULATION <- as.numeric(gsub(",","",blrs_f_2014$COUNTY_POPULATION))
blrs_f_2014$MEMBR_ACTIV <- as.numeric(blrs_f_2014$MEMBR_ACTIV)
blrs_f_2014$MEMBR_RETRD <- as.numeric(blrs_f_2014$MEMBR_RETRD)
blrs_f_2014$MEMBR_DROP <- as.numeric(blrs_f_2014$MEMBR_DROP)
blrs_f_2014$MEMBR_TERMD <- as.numeric(blrs_f_2014$MEMBR_TERMD)
blrs_f_2014$REPORT_YR <- as.numeric("2014")

### 2015 data
blrs_f_2015 <- read_excel("2015 Report/Appendix_F.xls",
                          range = "A2:I491",
                          na = c("-",""),
                           col_names = c("CITY_OR_DISTRICT", "TYP_SYS", "CITY_POPULATION",
                                        "COUNTY_NAME", "COUNTY_POPULATION", "MEMBR_ACTIV",
                                        "MEMBR_RETRD", "MEMBR_DROP", "MEMBR_TERMD"),
                          col_types = c("text", "text", "text", 
                                        "text", "text", "text",
                                        "text", "text", "text"))
blrs_f_2015$CITY_POPULATION <- as.numeric(gsub(",","",blrs_f_2015$CITY_POPULATION))
blrs_f_2015$COUNTY_POPULATION <- as.numeric(gsub(",","",blrs_f_2015$COUNTY_POPULATION))
blrs_f_2015$MEMBR_ACTIV <- as.numeric(blrs_f_2015$MEMBR_ACTIV)
blrs_f_2015$MEMBR_RETRD <- as.numeric(blrs_f_2015$MEMBR_RETRD)
blrs_f_2015$MEMBR_DROP <- as.numeric(blrs_f_2015$MEMBR_DROP)
blrs_f_2015$MEMBR_TERMD <- as.numeric(blrs_f_2015$MEMBR_TERMD)
blrs_f_2015$REPORT_YR <- as.numeric("2015")
blrs_f_2015$COMMENTS <- as.character(NA)

### 2016 data
blrs_f_2016 <- read_excel("2016 Report/Appendix_F.xls",
                          range = "A2:I490",
                          na = c("-",""),
                          col_names = c("CITY_OR_DISTRICT", "TYP_SYS", "CITY_POPULATION",
                                        "COUNTY_NAME", "COUNTY_POPULATION", "MEMBR_ACTIV",
                                        "MEMBR_RETRD", "MEMBR_DROP", "MEMBR_TERMD"),
                          col_types = c("text", "text", "text", 
                                        "text", "text", "text",
                                        "text", "text", "text"))
blrs_f_2016$CITY_POPULATION <- as.numeric(gsub(",","",blrs_f_2016$CITY_POPULATION))
blrs_f_2016$COUNTY_POPULATION <- as.numeric(gsub(",","",blrs_f_2016$COUNTY_POPULATION))
blrs_f_2016$MEMBR_ACTIV <- as.numeric(blrs_f_2016$MEMBR_ACTIV)
blrs_f_2016$MEMBR_RETRD <- as.numeric(blrs_f_2016$MEMBR_RETRD)
blrs_f_2016$MEMBR_DROP <- as.numeric(blrs_f_2016$MEMBR_DROP)
blrs_f_2016$MEMBR_TERMD <- as.numeric(blrs_f_2016$MEMBR_TERMD)
blrs_f_2016$REPORT_YR <- as.numeric("2016")
blrs_f_2016$COMMENTS <- as.character(NA)

### 2017 data
blrs_f_2017 <- read_excel("2017 Report/Appendix_F.xls",
                          range = "A2:I491",
                          na = c("-",""),
                          col_names = c("CITY_OR_DISTRICT", "TYP_SYS", "CITY_POPULATION",
                                        "COUNTY_NAME", "COUNTY_POPULATION", "MEMBR_ACTIV",
                                        "MEMBR_RETRD", "MEMBR_DROP", "MEMBR_TERMD"),
                          col_types = c("text", "text", "text", 
                                        "text", "text", "text",
                                        "text", "text", "text"))
blrs_f_2017$CITY_POPULATION <- as.numeric(gsub(",","",blrs_f_2017$CITY_POPULATION))
blrs_f_2017$COUNTY_POPULATION <- as.numeric(gsub(",","",blrs_f_2017$COUNTY_POPULATION))
blrs_f_2017$MEMBR_ACTIV <- as.numeric(blrs_f_2017$MEMBR_ACTIV)
blrs_f_2017$MEMBR_RETRD <- as.numeric(blrs_f_2017$MEMBR_RETRD)
blrs_f_2017$MEMBR_DROP <- as.numeric(blrs_f_2017$MEMBR_DROP)
blrs_f_2017$MEMBR_TERMD <- as.numeric(blrs_f_2017$MEMBR_TERMD)
blrs_f_2017$REPORT_YR <- as.numeric("2017")
blrs_f_2017$COMMENTS <- as.character(NA)


#### Part 2. Bind each year into a single table & relabel variables

blrs_f <- bind_rows(blrs_f_2005, blrs_f_2006, blrs_f_2007, blrs_f_2008, blrs_f_2009, blrs_f_2010, blrs_f_2011, blrs_f_2012, blrs_f_2013, blrs_f_2014, blrs_f_2015, blrs_f_2016, blrs_f_2017)

## reorder and rename columns

blrs_f <- blrs_f %>%
  select(plan_sponsor = CITY_OR_DISTRICT, 
         plan_type_f = TYP_SYS, 
         city_population_f = CITY_POPULATION, 
         report_year_f = REPORT_YR,
         county_name_f = COUNTY_NAME,
         county_population_f = COUNTY_POPULATION,
         active_members_f = MEMBR_ACTIV,
         retired_members_f = MEMBR_RETRD,
         terminated_members_f = MEMBR_TERMD,
         drop_members_f = MEMBR_DROP,
         comments_f = COMMENTS)

blrs_f$plan_sponsor <- str_trim(blrs_f$plan_sponsor)
blrs_f$plan_sponsor <- str_squish(blrs_f$plan_sponsor)

blrs_f$plan_type_f[blrs_f$plan_sponsor=="PALM BEACH LG-R"] <- "LG"

#### Part 4. Ensure clean identifiers

### Use this Excel file to identify sponsor names to change

sponsor_names <- table(blrs_f$plan_sponsor)
#print(sponsor_names)
sponsor_names <- as.data.frame(sponsor_names)
sponsor_names <- sponsor_names %>%
  rename(plan_sponsor = Var1) %>%
  mutate(plan_sponsor_clean = plan_sponsor) 
write_excel_csv(sponsor_names, "MatchingTables/appendix_f_to_match.csv")

### import comparison table
name_table <- read_excel("MatchingTables/appendix_f_matched.xls")
name_table <- name_table %>% 
  select(plan_sponsor, plan_sponsor_clean)

### consistent names for plan_type

blrs_f$plan_type_a[blrs_f$plan_type_a=="G&S"] <- "GS"

blrs_f <- right_join(name_table, blrs_f, by = "plan_sponsor")  

blrs_f <- arrange(blrs_f, plan_sponsor_clean, plan_type_f, report_year_f)

blrs_f <- blrs_f %>% rename(plan_sponsor_f = plan_sponsor)

save(blrs_f, file="blrs_f.RData")      





