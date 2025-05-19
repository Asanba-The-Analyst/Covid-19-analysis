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

# 1. DATA PREPARATION ------------------------------------------------------
# Load and clean the dataset
data <- data %>%
  mutate(
    # Convert dates
    date = mdy(date),
    dob = mdy(dob),
    
    # Calculate exact age
    age = as.numeric(floor((date - dob)/365.25)),
    
    # Create age groups
    age_group = case_when(
      age < 5 ~ "0-4",
      age >= 5 & age < 12 ~ "5-11",
      age >= 12 & age < 18 ~ "12-17",
      age >= 18 & age < 60 ~ "18-59",
      age >= 60 ~ "60+",
      TRUE ~ NA_character_
    ),
    
    # # Recode COVID-19 status
    # covid_status = case_when(
    #   sarscov2 == 1 ~ "Positive",
    #   sarscov2 == 0 ~ "Negative",
    #   sarscov2 == 2 ~ "Not Done",
    #   TRUE ~ NA_character_
    # ),
    # 
    # genda = case_when(
    #   genda == 1 ~ "Male",
    #   genda == 2 ~ "Female",
    #   TRUE ~ NA_character_
    # ),
    # 
    # #I HAVE EXCLUDED THE "Not done" WHICH IS 2 IN THE TEST RESULTS
    # # Recode RDT results
    # rdt_result = case_when(
    #   rdt_results == 1 ~ "Positive",
    #   rdt_results == 0 ~ "Negative",
    #   rdt_results == 2 ~ "Not Done",
    #   TRUE ~ NA_character_
    # ),
    # 
    # # Recode PCR results
    # pcr_result = case_when(
    #   pcr_results == 1 ~ "Positive",
    #   pcr_results == 0 ~ "Negative",
    #   pcr_results == 2 ~ "Not Done",
    #   TRUE ~ NA_character_
    # ),
    # 
    # Create COVID-19 severity categories
    covid_severity = case_when(
      covidhosp == 1 & icuadmision == 1 ~ "Severe",
      covidhosp == 1 & icuadmision == 0 ~ "Moderate",
      covidhosp == 0 ~ "Mild",
      TRUE ~ NA_character_
    ),
    
    # Create time periods for COVID-19 impact analysis
    time_period = case_when(
      date >= ymd("2018-01-01") & date <= ymd("2020-02-29") ~ "Pre-COVID",
      date >= ymd("2020-03-01") & date <= ymd("2022-04-30") ~ "COVID with Measures",
      date >= ymd("2022-05-01") & date <= ymd("2022-12-31") ~ "Post-Measures",
      TRUE ~ NA_character_
    ),
    
    # Create co-infection status
    coinfection = if_else((malaria == 1 | sepsisbact == 1 | uti == 1 | lrti == 1 | 
                             urti == 1 | enteric_fever == 1 | typhoid_fever == 1) & 
                            covid_status == "Positive", "Yes", "No")
  )

data <- data %>%
  mutate(
    educa = case_when(
      educa == 1 ~ "None",
      educa == 2 ~ "Primary",
      educa == 3 ~ "Middle school / JSS / JHS",
      educa == 4 ~ "SSS / SHS",
      educa == 5 ~ "Tertiary",
      educa == 6 ~ "Other (specify)",
      TRUE ~ NA_character_  # Handles cases where educa is not 1-6
    ),
    educa = as.factor(educa) # Convert to factor
  )

data <- data %>%
  mutate(
    occup = case_when(
      occup == 1 ~ "Farmer",
      occup == 2 ~ "Hair dressing",
      occup == 3 ~ "Trader",
      occup == 4 ~ "Carpentry, Masonry",
      occup == 5 ~ "Tailoring",
      occup == 6 ~ "Teacher",
      occup == 7 ~ "Health worker",
      occup == 8 ~ "Unemployed",
      occup == 9 ~ "Other specify",
      TRUE ~ NA_character_  # Handles cases where occup is not 1-9
    ),
    occup = as.factor(occup) # Convert to factor
  )




data <- data %>%
  mutate(
    ethic = case_when(
      ethic == 1 ~ "Akan (Bono, Ashanti etc)",
      ethic == 2 ~ "MO",
      ethic == 3 ~ "Kusasi",
      ethic == 4 ~ "Frafra",
      ethic == 5 ~ "Dagarti",
      ethic == 6 ~ "Chokosi",
      ethic == 7 ~ "Bimoba",
      ethic == 8 ~ "Fulani",
      ethic == 9 ~ "Basare",
      ethic == 10 ~ "Kokomba",
      ethic == 11 ~ "Wala",
      ethic == 12 ~ "Sisala",
      ethic == 13 ~ "Zambrama",
      ethic == 14 ~ "Ga Adangbe",
      ethic == 15 ~ "Mamprusi",
      ethic == 16 ~ "Dagomba",
      ethic == 17 ~ "Gonja",
      ethic == 18 ~ "Ewe",
      ethic == 19 ~ "Other specify",
      ethic == 20 ~ "Kasem",
      ethic == 21 ~ "Nankam",
      ethic == 22 ~ "Buli",
      TRUE ~ NA_character_  # Handles cases where ethnic is not 1-22
    ),
    ethic = as.factor(ethic) # Convert to factor
  )




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



# Create summary of AFI cases by phase
afi_summary <- clean_data %>%
  group_by(phase) %>%
  summarise(
    malaria = sum(malaria == 1, na.rm = TRUE),
    dengue = sum(dengue == 1, na.rm = TRUE),
    leptospirosis = sum(leptospirosis == 1, na.rm = TRUE),
    typhoid = sum(typhoid_fever == 1, na.rm = TRUE),
    scrub_typhus = sum(scrub_typhus == 1, na.rm = TRUE),
    pneumonia = sum(pneumonia == 1, na.rm = TRUE),
    total_cases = n()
  ) %>%
  pivot_longer(cols = -c(phase, total_cases), 
               names_to = "disease", 
               values_to = "cases") %>%
  mutate(proportion = cases / total_cases * 100)

# Plot trends
ggplot(afi_summary, aes(x = phase, y = cases, group = disease, color = disease)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Trends of Acute Febrile Illnesses Across COVID-19 Phases",
       x = "Phase",
       y = "Number of Cases",
       color = "Disease") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Kruskal-Wallis test for differences between phases
kruskal_results <- clean_data %>%
  select(phase, malaria, dengue, leptospirosis, typhoid_fever, scrub_typhus, pneumonia) %>%
  pivot_longer(cols = -phase, names_to = "disease", values_to = "value") %>%
  group_by(disease) %>%
  kruskal_test(value ~ phase) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance()

# Print results
print(kruskal_results)

# Post-hoc pairwise comparisons (Steel-Dwass)
posthoc_results <- clean_data %>%
  select(phase, malaria, dengue, leptospirosis, typhoid_fever, scrub_typhus, pneumonia) %>%
  pivot_longer(cols = -phase, names_to = "disease", values_to = "value") %>%
  group_by(disease) %>%
  wilcox_test(value ~ phase, p.adjust.method = "bonferroni")

# Print post-hoc results
print(posthoc_results)