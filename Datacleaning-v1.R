########## Data processig and cleaning##############
# Load necessary libraries
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


data <- data %>% 
  clean_names()

# Data cleaning and preparation
clean_data <- data %>%
  # Convert dates
  mutate(across(contains("date"), ~dmy(.x))) %>%
  # Recode categorical variables
  mutate(
    gender = factor(genda, levels = c(1, 2), labels = c("Male", "Female")),
    covid_status = factor(pcr_results, levels = c(0, 1, 2), 
                          labels = c("Negative", "Positive", "Not done")),
    rdt_result = factor(rdt_results, levels = c(0, 1, 2),
                        labels = c("Negative", "Positive", "Not done")),
    phase = case_when(
      date >= ymd("2018-01-01") & date <= ymd("2020-02-29") ~ "Phase 1",
      date >= ymd("2020-03-01") & date <= ymd("2022-04-30") ~ "Phase 2",
      date >= ymd("2022-05-01") & date <= ymd("2022-12-31") ~ "Phase 3",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Phase 1", "Phase 2", "Phase 3"))
  ) %>%
  # Filter out invalid records
  filter(!is.na(phase), !is.na(pcr_results))

# Create analysis datasets
analysis_data <- clean_data %>%
  filter(pcr_results %in% c(0, 1))  # Only confirmed positive/negative by PCR

# Save cleaned data
write_csv(clean_data, "cleaned_covid_afi_data.csv")