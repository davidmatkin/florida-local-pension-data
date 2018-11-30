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
#setwd(("C:/Users/dsm32/Dropbox/Retirement_Research/Pension OPEB Data/FL_LocalPensionReports"))

##To get rid of all but EAN and PUC
# only keep EAN and PUC example
#blrs_g <- filter(blrs_g, funded_method_g == "EAN" | funded_method_g == "PUC")

load("blrs_a.RData")
load("blrs_e.RData")
load("blrs_f.RData")
load("blrs_g.RData")

## join together A, E & G -- by valuation dates

# set up key variables

blrs_a <- blrs_a %>%
  mutate(plan_type_clean = plan_type_a) %>%
  mutate(valuation_date_clean = val_date_a)

blrs_e <- blrs_e %>%
  mutate(plan_type_clean = plan_type_e) %>%
  mutate(valuation_date_clean = val_date_e)

blrs_g <- blrs_g %>%
  mutate(plan_type_clean = plan_type_g) %>%
  mutate(valuation_date_clean = val_date_g)

# creating the full table for Appendices A, E & G
blrs_list_aeg <- list(blrs_a, blrs_e, blrs_g)
blrs_full_aeg <- reduce(blrs_list_aeg, full_join, by = c("plan_sponsor_clean", "plan_type_clean", "valuation_date_clean"))

### join population data to blrs_full_aeg

blrs_full_aeg <- blrs_full_aeg %>%
  mutate(report_year_clean = report_year_a)

blrs_f <- blrs_f %>%
  mutate(report_year_clean = report_year_f) %>%
  mutate(plan_type_clean = plan_type_f)

#Left join originally, what information is lost when a left join is used instead of a full join?

blrs_full_aefg <- left_join(blrs_full_aeg, blrs_f, by = c("plan_sponsor_clean", "report_year_clean", "plan_type_clean"))


## this is all the merged data for appendix A, E, F & G. 

save(blrs_full_aefg, file = "blrs_full_aefg.RData")


## this is a process to select out the variables for analysis (simiplify dataframes)

## Adds two missing data groups plan_type_a, and val_date_a??  

blrs_select_aefg <- blrs_full_aefg %>%
  select(plan_sponsor_clean, plan_type_clean, valuation_date_clean, uaal_a, val_payroll_a, reqd_contribution_a, member_percent_a, 
         interest_assumption_e, interest_av_actual_e, city_population_f, active_members_f, funded_ratio_g, 
         funded_method_e, funded_method_g, report_year_a, report_year_e, report_year_f, report_year_g, report_year_clean)



## remove any duplicates

blrs_select_aefg <- distinct(blrs_select_aefg)

##Remove entries that do not have required data for grading system.

blrs_select_aefg <- blrs_select_aefg %>% 
  filter(!is.na(funded_ratio_g))
  filter(!is.na(uaal_a)) %>%
  filter(!is.na(val_payroll_a)) %>% 
  filter(!is.na(reqd_contribution_a)) %>%
  filter(!is.na(interest_assumption_e)) %>%
  filter(!is.na(member_percent_a))
  
## select only sponsors using EAN or PUC
  
blrs_select_aefg <- filter(blrs_select_aefg, funded_method_e %in% c("EAN", "PUC"))

## select out county and special district data
  
sponsors_remove <- c("ALACHUA COUNTY", "ALACHUA COUNTY LIBRARY", "ALACHUA COUNTY SCHOOL BOARD", 
                       "BONITA SPRINGS FCD", "COLLIER COUNTY HOUSING AUTHORITY", "DESTIN FCD", 
                       "EAST NAPLES FCD", "EAST NICEVILLE FCD", "ENGLEWOOD AREA FCD", 
                       "FORT PIERCE UTILITIES", "GREATER NAPLES FCD", "HILLSBOROUGH COUNTY",
                       "KEY WEST HOUSING", "KEY WEST UTILITIES", "KISSIMMEE UTILITY AUTHORITY",
                       "MARTIN COUNTY", "MIAMI-DADE COUNTY", "NORTH RIVER FCD",
                       "OCEAN CITY-WRIGHT FCD", "OKALOOSA ISLAND FCD", "ORANGE COUNTY LIBRARY",
                       "ORLANDO AVIATION", "ORLANDO UTILITY", "PALM HARBOR FCD", "PALM TRAN, INC. (ATU-1577)",
                       "PASCO COUNTY", "SAINT JOHNS RIVER PW PK", "SAINT LUCIE COUNTY FCD", 
                       "SARASOTA-MANATEE A", "SARASOTA MEMORIAL HEALTH CARE", "SOUTH BROWARD HOSPITAL (aka Memorial Healthcare)",
                     
                       "SOUTH WALTON FCD", "TRI-COUNTY TRANSIT", "WEST MANATEE FCD")
  
blrs_select_aefg <- blrs_select_aefg %>%
  filter(!plan_sponsor_clean %in% sponsors_remove) 

## measure 1

blrs_select_aefg$m1_fr <- 0
blrs_select_aefg$m1_fr[blrs_select_aefg$funded_ratio_g >= 60] <- 1
blrs_select_aefg$m1_fr[blrs_select_aefg$funded_ratio_g >= 80] <- 2
blrs_select_aefg$m1_fr[blrs_select_aefg$funded_ratio_g >= 100] <- 2.5

## measure 2

blrs_select_aefg$uaal_cv <- blrs_select_aefg$uaal_a/blrs_select_aefg$val_payroll_a * 100

blrs_select_aefg$m2_uaal <- 0
blrs_select_aefg$m2_uaal[blrs_select_aefg$uaal_cv <= 200 ] <- 0.5
blrs_select_aefg$m2_uaal[blrs_select_aefg$uaal_cv <= 100 ] <- 1.0

## measure 3

blrs_select_aefg$arc_perc <- blrs_select_aefg$reqd_contribution_a / blrs_select_aefg$val_payroll_a * 100  

blrs_select_aefg$m3_arc <- 0
blrs_select_aefg$m3_arc[blrs_select_aefg$arc_perc <= 30 ] <- 0.5
blrs_select_aefg$m3_arc[blrs_select_aefg$arc_perc <= 20 ] <- 1.0

## measure 4

blrs_select_aefg$m4_return <- 0
blrs_select_aefg$m4_return[blrs_select_aefg$interest_assumption_e <= 7.75 ] <- 0.5

## measure 5

blrs_select_aefg$m5_employee_con <- 0
blrs_select_aefg$m5_employee_con[blrs_select_aefg$member_percent_a >= 5 ] <- 0.5

## total score & grade

blrs_select_aefg <- blrs_select_aefg %>%
  mutate(grade_score = m1_fr + m2_uaal + m3_arc + m4_return + m5_employee_con) %>%
  mutate(grade_letter = "F")
blrs_select_aefg$grade_letter[blrs_select_aefg$grade_score >= 1 ] <- "D"
blrs_select_aefg$grade_letter[blrs_select_aefg$grade_score >= 2 ] <- "C"
blrs_select_aefg$grade_letter[blrs_select_aefg$grade_score >= 3 ] <- "B"
blrs_select_aefg$grade_letter[blrs_select_aefg$grade_score >= 4 ] <- "A"

#obtain year variable for figures
blrs_select_aefg$valuation_year <- year(blrs_select_aefg$valuation_date_clean)

#remove 2017 -- only two observations
blrs_select_aefg <- blrs_select_aefg[blrs_select_aefg$valuation_year <= 2016,]

### GRADES TABLE

grade_table_long <- blrs_select_aefg %>%
  ungroup() %>%
  select(plan_sponsor_clean, plan_type_clean, valuation_year, grade_letter, grade_score) %>%
  distinct() %>%
  group_by(plan_sponsor_clean, plan_type_clean, valuation_year) %>%
  filter(grade_score == max(grade_score)) %>%
  select(-grade_score)

write.csv(grade_table_long, file = "Report_Output/grades_table_long.csv", row.names = FALSE)

grade_table_wide <- spread(grade_table_long, valuation_year, grade_letter)

write.csv(grade_table_wide, file = "Report_Output/grades_table_wide.csv", row.names = FALSE)

## MEASURE 1

# figure 1
ggplot(blrs_select_aefg, aes(x = valuation_year,y = funded_ratio_g)) +
  geom_boxplot(aes(group = cut_width(valuation_year, 1.0)), outlier.shape = NA, fill = "green") +
  ylim(40, 120) +
  ylab("Funded Ratio in Percent") +
  xlab("Valuation Year") +
  scale_x_continuous(breaks = 2005:2016) +
  theme(legend.position = "none")

ggsave("Report_Output/figure_1.tiff")

# table 1
table_1 <- blrs_select_aefg %>%
  group_by(valuation_year, m1_fr) %>%
  summarise(Freq = n()) 

table_1_totals <- table_1 %>%
  group_by(valuation_year) %>%
  summarise(totals = sum(Freq))

table_1 <- spread(table_1, valuation_year, Freq)
table_1 <- arrange(table_1, desc(m1_fr))
table_1$Funded_Ratio <- "100% or Greater"
table_1$Funded_Ratio[table_1$m1_fr==2] <- "80% to 100%"
table_1$Funded_Ratio[table_1$m1_fr==1] <- "60% to 80%"
table_1$Funded_Ratio[table_1$m1_fr==0] <- "Less than 60%"

table_1[nrow(table_1)+1,] <- c(NA,table_1_totals$totals,"TOTALS")

table_1 <- table_1 %>%
  select(Funded_Ratio, Points = m1_fr, '2005':'2016')

## MEASURE 2

# figure 2
ggplot(blrs_select_aefg, aes(x = valuation_year,y = uaal_cv)) +
  geom_boxplot(aes(group = cut_width(valuation_year, 1.0)), outlier.shape = NA, fill = "green") +
  ylim(-200, 800) +
  ylab("Unfunded Liability as Percent of Payroll") +
  xlab("Valuation Year") +
  scale_x_continuous(breaks = 2005:2016) +
  theme(legend.position = "none")

ggsave("Report_Output/figure_2.tiff")

# table 2
table_2 <- blrs_select_aefg %>%
  group_by(valuation_year, m2_uaal) %>%
  summarise(Freq = n()) 

table_2_totals <- table_2 %>%
  group_by(valuation_year) %>%
  summarise(totals = sum(Freq))

table_2 <- spread(table_2, valuation_year, Freq)
table_2 <- arrange(table_2, desc(m2_uaal))
table_2$Unfunded_Liability_Payroll <- "Less than 100%"
table_2$Unfunded_Liability_Payroll[table_2$m2_uaal==.5] <- "100% to 200%"
table_2$Unfunded_Liability_Payroll[table_2$m2_uaal== 0] <- "More than 200%"

table_2[nrow(table_2)+1,] <- c(NA,table_2_totals$totals,"TOTALS")

table_2 <- table_2 %>%
  select(Unfunded_Liability_Payroll, Points = m2_uaal, '2005':'2016')

## MEASURE 3

# figure 3
ggplot(blrs_select_aefg, aes(x = valuation_year,y = arc_perc)) +
  geom_boxplot(aes(group = cut_width(valuation_year, 1.0)), outlier.shape = NA, fill = "green") +
  ylim(0, 100) +
  ylab("Annual Contribution as Percent of Payroll") +
  xlab("Valuation Year") +
  scale_x_continuous(breaks = 2005:2016) +
  theme(legend.position = "none")

ggsave("Report_Output/figure_3.tiff")

# table 3
table_3 <- blrs_select_aefg %>%
  group_by(valuation_year, m3_arc) %>%
  summarise(Freq = n()) 

table_3_totals <- table_3 %>%
  group_by(valuation_year) %>%
  summarise(totals = sum(Freq))

table_3 <- spread(table_3, valuation_year, Freq)
table_3 <- arrange(table_3, desc(m3_arc))
table_3$Annual_Contribution_Payroll <- "Less than 20%"
table_3$Annual_Contribution_Payroll[table_3$m3_arc==.5] <- "20% to 30%"
table_3$Annual_Contribution_Payroll[table_3$m3_arc== 0] <- "More than 30%"

table_3[nrow(table_3)+1,] <- c(NA,table_3_totals$totals,"TOTALS")

table_3 <- table_3 %>%
  select(Annual_Contribution_Payroll, Points = m3_arc, '2005':'2016')

## MEASURE 4

# figure 4
ggplot(blrs_select_aefg, aes(x = valuation_year,y = interest_assumption_e)) +
  geom_boxplot(aes(group = cut_width(valuation_year, 1.0)), fill = "green") +
  #ylim(0, 100) +
  ylab("Asset Return Assumptions") +
  xlab("Valuation Year") +
  scale_x_continuous(breaks = 2005:2016) +
  theme(legend.position = "none")

ggsave("Report_Output/figure_4.tiff")

# table 4
table_4 <- blrs_select_aefg %>%
  group_by(valuation_year, m4_return) %>%
  summarise(Freq = n()) 

table_4_totals <- table_4 %>%
  group_by(valuation_year) %>%
  summarise(totals = sum(Freq))

table_4 <- spread(table_4, valuation_year, Freq)
table_4 <- arrange(table_4, desc(m4_return))
table_4$RETURN_ASSUMPTION <- "More than 7.75%"
table_4$RETURN_ASSUMPTION[table_4$m4_return==.5] <- "7.75% or less"

table_4[nrow(table_4)+1,] <- c(NA,table_4_totals$totals,"TOTALS")

table_4 <- table_4 %>%
  select(RETURN_ASSUMPTION, Points = m4_return, '2005':'2016')


## MEASURE 5

# figure 5
ggplot(blrs_select_aefg, aes(x = valuation_year,y = member_percent_a)) +
  geom_boxplot(aes(group = cut_width(valuation_year, 1.0)), outlier.shape = NA, fill = "green") +
  ylim(0, 15) +
  ylab("Employee Contribution as Percent of Payroll") +
  xlab("Valuation Year") +
  scale_x_continuous(breaks = 2005:2016) +
  theme(legend.position = "none")

ggsave("Report_Output/figure_5.tiff")

# table 4
table_5 <- blrs_select_aefg %>%
  group_by(valuation_year, m5_employee_con) %>%
  summarise(Freq = n()) 

table_5_totals <- table_5 %>%
  group_by(valuation_year) %>%
  summarise(totals = sum(Freq))

table_5 <- spread(table_5, valuation_year, Freq)
table_5 <- arrange(table_5, desc(m5_employee_con))
table_5$EMPLOYEE_CONTRIBUTION <- "Less than 5%"
table_5$EMPLOYEE_CONTRIBUTION[table_5$m5_employee_con==.5] <- "5% or more"

table_5[nrow(table_5)+1,] <- c(NA,table_5_totals$totals,"TOTALS")

table_5 <- table_5 %>%
  select(EMPLOYEE_CONTRIBUTION, Points = m5_employee_con, '2005':'2016')

