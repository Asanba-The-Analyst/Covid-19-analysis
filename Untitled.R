

# 2.2 Clinical characteristics by COVID-19 severity - FIXED VERSION

# Alternative robust version
create_clinical_table <- function(data) {
  # Identify which symptoms have variation across groups
  valid_symptoms <- clinical_data %>%
    select(-covid_severity) %>%
    map_lgl(~length(unique(.x)) > 1)
  
  # Only include symptoms with variation
  if(sum(valid_symptoms) > 0) {
    clinical_data %>%
      select(names(valid_symptoms)[valid_symptoms], covid_severity) %>%
      tbl_summary(
        by = covid_severity,
        missing = "no",
        statistic = list(all_categorical() ~ "{n} ({p}%)")
      ) %>%
      add_p(test = all_categorical() ~ "fisher.test") %>%
      modify_header(label ~ "**Variable**") %>%
      bold_labels()
  } else {
    tibble(Note = "No symptoms showed variation across severity groups") %>%
      gt::gt() %>%
      gt::tab_header(title = "Clinical Characteristics",
                     subtitle = "No analyzable symptom data available")
  }
}

# Use the function
clinical_table <- create_clinical_table(clinical_data)
print(clinical_table)
