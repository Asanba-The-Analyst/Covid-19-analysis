# Performance metrics for RDT compared to PCR (gold standard)
rdt_performance <- analysis_data %>%
  filter(!is.na(rdt_results), pcr_results %in% c(0, 1)) %>%
  mutate(
    true_pos = pcr_results == 1 & rdt_results == 1,
    true_neg = pcr_results == 0 & rdt_results == 0,
    false_pos = pcr_results == 0 & rdt_results == 1,
    false_neg = pcr_results == 1 & rdt_results == 0
  ) %>%
  summarise(
    sensitivity = sum(true_pos) / (sum(true_pos) + sum(false_neg)),
    specificity = sum(true_neg) / (sum(true_neg) + sum(false_pos)),
    ppv = sum(true_pos) / (sum(true_pos) + sum(false_pos)),
    npv = sum(true_neg) / (sum(true_neg) + sum(false_neg)),
    accuracy = (sum(true_pos) + sum(true_neg)) / n()
  )

# Print performance metrics
print(rdt_performance)

# ROC curve analysis
roc_obj <- roc(response = analysis_data$pcr_results, 
               predictor = analysis_data$rdt_results,
               levels = c(0, 1))

# Plot ROC curve
ggroc(roc_obj) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed") +
  labs(title = "ROC Curve for COVID-19 RDT",
       subtitle = paste0("AUC = ", round(auc(roc_obj), 3))) +
  theme_minimal()

# Confusion matrix
confusionMatrix(factor(analysis_data$rdt_results, levels = c(1, 0)),
                factor(analysis_data$pcr_results, levels = c(1, 0)),
                positive = "1")