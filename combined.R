
# COVID-19 DIFFERENTIAL DIAGNOSIS STUDY ANALYSIS
# KHRC Data Analysis Script By Alexander Atiah 

# Load required packages
# Install if not already
install.packages("caret")

# Load the package
library(caret)

library(tidyverse)
library(lubridate)
library(gtsummary)
library(pROC)
library(rstatix)
library(ggpubr)
library(ggcorrplot)
library(sf)
library(ggspatial)
library(patchwork)


# 1. DATA PREPARATION ------------------------------------------------------
library(dplyr)
library(lubridate)

data <- data %>%
  mutate(
    # Convert date columns
    date = mdy(date),
    dob = mdy(dob),
    
    # Calculate exact age
    age = as.numeric(floor((date - dob) / 365.25)),
    
    # Create age groups
    age_group = case_when(
      age < 5 ~ "0-4",
      age >= 5 & age < 12 ~ "5-11",
      age >= 12 & age < 18 ~ "12-17",
      age >= 18 & age < 60 ~ "18-59",
      age >= 60 ~ "60+",
      TRUE ~ NA_character_
    ),
    age_group = as.factor(age_group),
    
    # Recode SARS-CoV-2 test status
    sarscov2_status = case_when(
      sarscov2 == 1 ~ "Positive",
      sarscov2 == 0 ~ "Negative",
  
      TRUE ~ NA_character_
    ),
    sarscov2_status = as.factor(sarscov2_status),
    
    # Recode gender
    genda = case_when(
      genda == 1 ~ "Male",
      genda == 2 ~ "Female",
      TRUE ~ NA_character_
    ),
    genda = as.factor(genda),
    
    # Recode RDT result (Note: Not Done [2] is included)
    rdt_result = case_when(
      rdt_results == 1 ~ "Positive",
      rdt_results == 0 ~ "Negative",
      TRUE ~ NA_character_
    ),
    rdt_result = as.factor(rdt_result),
    
    # Recode PCR result (Note: Not Done [2] is included)
    covid_status = case_when(
      pcr_results == 1 ~ "Positive",
      pcr_results == 0 ~ "Negative",
      TRUE ~ NA_character_
    ),
    covid_status = as.factor(covid_status),
    
    # Define COVID-19 severity
    covid_severity = case_when(
      covidhosp == 1 & icuadmision == 1 ~ "Severe",
      covidhosp == 1 & icuadmision == 0 ~ "Moderate",
      covidhosp == 0 ~ "Mild",
      TRUE ~ NA_character_
    ),
    covid_severity = as.factor(covid_severity),
    
    # Define COVID-19 time periods
    time_period = case_when(
      date >= ymd("2018-01-01") & date <= ymd("2020-02-29") ~ "Pre-COVID",
      date >= ymd("2020-03-01") & date <= ymd("2022-04-30") ~ "COVID with Measures",
      date >= ymd("2022-05-01") & date <= ymd("2022-12-31") ~ "Post-Measures",
      TRUE ~ NA_character_
    ),
    time_period = as.factor(time_period)
  )

  

# data <- data %>%
#   mutate(
#     educa = case_when(
#       educa == 1 ~ "None",
#       educa == 2 ~ "Primary",
#       educa == 3 ~ "Middle school / JSS / JHS",
#       educa == 4 ~ "SSS / SHS",
#       educa == 5 ~ "Tertiary",
#       educa == 6 ~ "Other (specify)",
#       TRUE ~ NA_character_  # Handles cases where educa is not 1-6
#     ),
#     educa = as.factor(educa) # Convert to factor
#   )


data <- data %>%
  mutate(
    educa = case_when(
      educa == 1 ~ "No Education",
      educa %in% c(2, 3) ~ "Basic Education",
      educa %in% c(4, 5, 6) ~ "Secondary or Higher",
      TRUE ~ NA_character_
    ),
    educa = as.factor(educa)
  )

data <- data %>%
  mutate(
    occup = case_when(
      occup == 7 ~ "Health Worker",
      occup %in% c(2, 4, 5, 6) ~ "Skilled/Artisan",
      occup %in% c(1, 3, 8, 9) ~ "Informal/Other",
      TRUE ~ NA_character_
    ),
    occup = as.factor(occup)
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



# Create co-infection status
data <- data %>% mutate(coinfection = if_else((malaria == 1 | sepsisbact == 1 | uti == 1 | lrti == 1 | 
                         urti == 1 | enteric_fever == 1 | typhoid_fever == 1) & 
                           covid_status == "Positive", "Yes", "No"))



# Data cleaning and preparation
data <- data %>%
  # # Convert dates
  # mutate(across(contains("date"), ~mdy(.x))) %>%
  # Recode categorical variables
  mutate(
    # gender = factor(genda, levels = c(1, 2), labels = c("Male", "Female")),
    # covid_status = factor(pcr_results, levels = c(0, 1, 2), 
    #                       labels = c("Negative", "Positive", "Not done")),
    # rdt_result = factor(rdt_results, levels = c(0, 1, 2),
    #                     labels = c("Negative", "Positive", "Not done")),
    phase = case_when(
      date >= ymd("2018-01-01") & date <= ymd("2020-02-29") ~ "Phase 1",
      date >= ymd("2020-03-01") & date <= ymd("2022-04-30") ~ "Phase 2",
      date >= ymd("2022-05-01") & date <= ymd("2022-12-31") ~ "Phase 3",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Phase 1", "Phase 2", "Phase 3"))
  ) %>%
  # Filter out invalid records
  filter(!is.na(phase), !is.na(pcr_results))



# 2. DESCRIPTIVE ANALYSIS -------------------------------------------------
table(data$covid_status)
table(data$diabetes, data$covid_status)
table(data$hypertension, data$covid_status)
table(data$genda, data$covid_status)
table(data$occup, data$covid_status)

# 2.1 Demographic characteristics by COVID-19 status
demog_table <- data %>%
  select(age, age_group, genda, ethic, educa, occup, covid_status) %>%
  tbl_summary(
    by = covid_status,
    missing = "no",
    label = list(
      age ~ "Age (years)",
      age_group ~ "Age group",
      genda ~ "Gender",
      ethic ~ "Ethnicity",
      educa ~ "Education level",
      occup ~ "Occupation"
    )
  ) %>%
  add_p(test.args = everything() ~ list(simulate.p.value = TRUE)) %>%
  modify_header(label ~ "**Variable**") %>%
  bold_labels()

# 2.2 Clinical characteristics by COVID-19 severity
clinical_table <- data %>%
  filter(covid_status == "Positive") %>%
  select(fever, cough, hach, jpain, vomit, dirhea, covid_severity) %>%
  tbl_summary(
    by = covid_severity,
    missing = "no",
    label = list(
      fever ~ "Fever",
      cough ~ "Cough",
      hach ~ "Headache",
      jpain ~ "Joint pain",
      vomit ~ "Vomiting",
      dirhea ~ "Diarrhea"
    )
  ) %>%
  add_p(test.args = everything() ~ list(simulate.p.value = TRUE)) %>%
  modify_header(label ~ "**Variable**") %>%
  bold_labels()

# 2.3 Prevalence of co-infections
coinfection_prev <- data %>%
  filter(covid_status == "Positive") %>%
  select(malaria, sepsisbact, uti, lrti, urti, enteric_fever, typhoid_fever) %>%
  pivot_longer(everything(), names_to = "infection", values_to = "status") %>%
  group_by(infection) %>%
  summarise(
    n_cases = sum(status == 1, na.rm = TRUE),
    total = n(),
    prevalence = n_cases/total * 100
  )
print(coinfection_prev)
# 3. PERFORMANCE EVALUATION OF RDT ----------------------------------------

# Create confusion matrix


data <- data %>%
  filter(rdt_result %in% c("Positive", "Negative"),
         covid_status %in% c("Positive", "Negative"))

conf_matrix <- table(data$rdt_result, data$covid_status, dnn = c("RDT", "PCR"))

# Calculate performance metrics
rdt_performance <- data.frame(
  Sensitivity = sensitivity(conf_matrix),
  Specificity = specificity(conf_matrix),
  PPV = posPredValue(conf_matrix),
  NPV = negPredValue(conf_matrix),
  Accuracy = (conf_matrix[1,1] + conf_matrix[2,2])/sum(conf_matrix)
)

# ROC curve analysis
data <- data %>%
  mutate(rdt_score = ifelse(rdt_result == "Positive", 1, 0))

roc_obj <- roc(data$covid_status, as.numeric(data$rdt_score))
auc_val <- auc(roc_obj)

# Plot ROC curve
roc_plot <- ggroc(roc_obj, color = "#377eb8") +
  geom_abline(slope = 1, intercept = 1, linetype = "dashed") +
  annotate("text", x = 0.7, y = 0.3, 
           label = paste("AUC =", round(auc_val, 3))) +
  labs(title = "ROC Curve for COVID-19 RDT",
       subtitle = "Compared to PCR as gold standard") +
  theme_minimal()

print(roc_plot)

# 4. RISK FACTOR ANALYSIS -------------------------------------------------

# 4.1 Crude and adjusted prevalence ratios for COVID-19 risk factors
# Univariate analysis
univ_models <- list(
  age = glm(covid_status ~ age, family = binomial(link = "log"), data = data),
  gender = glm(covid_status ~ genda, family = binomial(link = "log"), data = data),
  occupation = glm(covid_status ~ occup, family = binomial(link = "log"), data = data),
  diabetes = glm(covid_status ~ diabetes, family = binomial(link = "log"), data = data),
  hypertension = glm(covid_status ~ hypertension, family = binomial(link = "log"), data = data)
)

univ_models <- list(
  age = glm(covid_status ~ age, family = binomial(link = "logit"), data = data),
  gender = glm(covid_status ~ genda, family = binomial(link = "logit"), data = data),
  occupation = glm(covid_status ~ occup, family = binomial(link = "logit"), data = data)
)

# Extract results
univ_results <- map_df(univ_models, ~{
  tidy(.x, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%
    select(term, estimate, conf.low, conf.high, p.value)
}, .id = "variable")

# Multivariate model
multiv_model <- glm(covid_status ~ age + genda + occup + diabetes + hypertension,
                    family = binomial(link = "log"), data = data)

multiv_model <- glm(covid_status ~ age + genda + occup + diabetes + hypertension,
                    family = binomial(link = "logit"), data = data)



library(sandwich)
library(lmtest)

multiv_model <- glm(covid_status ~ age + genda + occup + diabetes + hypertension,
                    family = poisson(link = "log"), data = data)

coeftest(multiv_model, vcov = sandwich)


# 4.2 Association between COVID-19 severity and co-infection
severity_model <- glm(as.factor(covid_severity) ~ coinfection + age + genda,
                      family = binomial(link = "logit"), 
                      data = filter(data, covid_status == "Positive"))

# 5. IMPACT OF COVID-19 ON AFI DIAGNOSIS ----------------------------------

# 5.1 Case counts by time period
afi_counts <- data %>%
  filter(!is.na(time_period)) %>%
  group_by(time_period) %>%
  summarise(
    malaria = sum(malrdt_results == 1, na.rm = TRUE),
    sepsis = sum(sepsisbact == 1, na.rm = TRUE),
    uti = sum(uti == 1, na.rm = TRUE),
    lrti = sum(lrti == 1, na.rm = TRUE),
    urti = sum(urti == 1, na.rm = TRUE),
    typhoid = sum(typhoid_fever == 1, na.rm = TRUE),
    covid = sum(covid_status == "Positive", na.rm = TRUE)
  ) %>%
  pivot_longer(-time_period, names_to = "infection", values_to = "cases")

# 5.2 Statistical comparison of AFI cases across time periods
# Kruskal-Wallis test for each infection
afi_tests <- afi_counts %>%
  group_by(infection) %>%
  kruskal_test(cases ~ time_period) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance()

# Pairwise comparisons for significant results
pairwise_tests <- afi_counts %>%
  group_by(infection) %>%
  wilcox_test(cases ~ time_period, p.adjust.method = "bonferroni") %>%
  filter(p.adj < 0.05)

# 5.3 Geographic heat maps (simplified example)
#

# Create sample geographic data (replace with real coordinates)
site_data <- data %>%
  group_by(comm) %>%
  summarise(
    malaria_cases = sum(malaria == 1, na.rm = TRUE),
    covid_cases = sum(sarscov2 == 1, na.rm = TRUE),
    n = n()
  ) %>%
  mutate(
    malaria_rate = malaria_cases/n * 1000,
    covid_rate = covid_cases/n * 1000
  )

# 6. VISUALIZATIONS -------------------------------------------------------

# 6.1 Demographic and clinical characteristics plots
age_plot <- ggplot(data %>% filter(!is.na(age_group)), aes(x = age_group, fill = covid_status)) +
  geom_bar(position = "dodge") +
  labs(x = "Age Group", y = "Count", fill = "COVID-19 Status") +
  theme_minimal()


age_plot <- ggplot(data %>% filter(!is.na(age_group)), aes(x = age_group, fill = covid_status)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = after_stat(count)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3) +
  labs(x = "Age Group", y = "Count", fill = "COVID-19 Status") +
  theme_minimal()


symptom_plot <- data %>%
  filter(covid_status == "Positive") %>%
  select(fever, cough, hach, jpain, vomit, dirhea) %>%
  pivot_longer(everything(), names_to = "symptom", values_to = "present") %>%
  group_by(symptom) %>%
  summarise(prevalence = mean(present == 1, na.rm = TRUE) * 100) %>%
  ggplot(aes(x = reorder(symptom, prevalence), y = prevalence)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(x = "Symptom", y = "Prevalence (%)") +
  theme_minimal()

# 6.2 Time trends of AFIs
trend_plot <- afi_counts %>%
  ggplot(aes(x = time_period, y = cases, fill = infection)) +
  geom_col(position = "dodge") +
  labs(x = "Time Period", y = "Case Count", fill = "Infection") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 7. REPORT GENERATION ----------------------------------------------------

# Save all results
output_dir <- "results"
dir.create(output_dir, showWarnings = FALSE)

# Save tables
write_csv(univ_results, file.path(output_dir, "univariate_results.csv"))
write_csv(multiv_results, file.path(output_dir, "multivariate_results.csv"))
write_csv(afi_tests, file.path(output_dir, "afi_tests.csv"))
write_csv(pairwise_tests, file.path(output_dir, "pairwise_tests.csv"))

# Save plots
ggsave(file.path(output_dir, "roc_curve.png"), roc_plot, width = 8, height = 6)
ggsave(file.path(output_dir, "age_distribution.png"), age_plot, width = 8, height = 6)
ggsave(file.path(output_dir, "symptom_prevalence.png"), symptom_plot, width = 8, height = 6)
ggsave(file.path(output_dir, "afi_trends.png"), trend_plot, width = 10, height = 6)

# Save HTML report of tables
demog_table %>%
  as_gt() %>%
  gt::gtsave(file.path(output_dir, "demographic_table.html"))

clinical_table %>%
  as_gt() %>%
  gt::gtsave(file.path(output_dir, "clinical_table.html"))

# 8. SESSION INFO ---------------------------------------------------------
sessionInfo()






