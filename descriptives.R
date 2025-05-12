
library(tidyverse)
library(lubridate)
library(janitor)
library(haven)
library(tableone)
library(gtsummary)
library(sf)
library(ggmap)
library(ggpubr)
library(pROC)
library(caret)
library(rstatix)

# Create Table 1: Baseline characteristics by COVID-19 status
table1_vars <- c("age", "gender", "ethic", "educa", "occup", "htype", 
                 "bmi", "temp", "sblood", "dblood", "phase")

table1_cat_vars <- c("gender", "ethic", "educa", "occup", "htype", "phase")

table1 <- CreateTableOne(vars = table1_vars, 
                         factorVars = table1_cat_vars,
                         strata = "covid_status",
                         data = data,
                         addOverall = TRUE)

# Print table with percentages and p-values
print(table1, showAllLevels = TRUE, nonnormal = c("age", "bmi", "temp", "sblood", "dblood"))

# Create publication-ready table
tbl_summary(
  data,
  by = covid_status,
  include = c(age, gender, ethic, educa, occup, htype, bmi, temp, sblood, dblood, phase),
  type = list(c(age, bmi, temp, sblood, dblood) ~ "continuous"),
  statistic = list(all_continuous() ~ "{mean} ({sd})",
                   all_categorical() ~ "{n} ({p}%)"),
  digits = all_continuous() ~ 1,
  missing = "no"
) %>% 
  add_p() %>% 
  bold_labels() %>% 
  modify_caption("**Table 1. Baseline Characteristics by COVID-19 Status**")
