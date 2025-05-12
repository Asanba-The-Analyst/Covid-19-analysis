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