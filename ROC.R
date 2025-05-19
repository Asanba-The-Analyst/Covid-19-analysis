library(dplyr)
library(pROC)

# 1. Filter to binary outcomes only
binary_data <- data %>%
  filter(
    rdt_result %in% c("Positive", "Negative"),
    covid_status %in% c("Positive", "Negative")
  ) %>%
  mutate(
    rdt_score = ifelse(rdt_result == "Positive", 1, 0),
    covid_status = factor(covid_status, levels = c("Negative", "Positive"))
  )

# 2. Generate ROC curve
roc_obj <- roc(binary_data$covid_status, binary_data$rdt_score)

# 3. Plot ROC curve with AUC
plot(roc_obj, main = "ROC Curve: RDT vs PCR", col = "blue", lwd = 2)
auc_value <- auc(roc_obj)
legend("bottomright", legend = paste("AUC =", round(auc_value, 3)), col = "blue", lwd = 2)

# 4. Find optimal threshold (Youden's index)
opt_coords <- coords(roc_obj, "best", best.method = "youden", 
                     ret = c("threshold", "sensitivity", "specificity"))

# Add point and label to plot
points(1 - opt_coords["specificity"], opt_coords["sensitivity"], col = "red", pch = 19)
text(1 - opt_coords["specificity"], opt_coords["sensitivity"] + 0.05, 
     labels = paste0("Threshold = ", round(opt_coords["threshold"], 2)), col = "red")

# 5. Export full sensitivity/specificity table
roc_stats <- coords(roc_obj, x = "all", ret = c("threshold", "sensitivity", "specificity"), 
                    transpose = FALSE)

# 6. Save to CSV
write.csv(roc_stats, "roc_rdt_vs_pcr_results.csv", row.names = FALSE)

# Optional: Print optimal threshold
print("Optimal Threshold (Youden's Index):")
print(opt_coords)
