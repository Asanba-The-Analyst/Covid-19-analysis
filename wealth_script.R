

# List of wealth variables (excluding specified variables)
wealth_vars_list <- c(
  "educa",
  "occup",
  "htype",
  "ownblockhse",
  "ownthatchse",
  "owncar",
  "ownradiotv",
  "ownbike",
  "ownfarm",
  "ownfridge",
  "ownstore",
  "veget",
  "waterb",
  "dwater",
  "bnet",
  "contani",
  "contanitype",
  "foodeaten"
)


# Select only the wealth variables from the dataset
wealth_data <- data[, wealth_vars_list, drop = FALSE]  # drop = FALSE prevents conversion to vector if only one variable is selected

# Remove any non-numeric columns that might have snuck in or were incorrectly included
wealth_data <- wealth_data[, sapply(wealth_data, is.numeric), drop = FALSE]

# Handle missing data (replace with median) - important for PCA
for (i in 1:ncol(wealth_data)) {
  wealth_data[is.na(wealth_data[,i]), i] <- median(wealth_data[,i], na.rm = TRUE)
}

# PCA approach
pca_result <- prcomp(wealth_data, scale. = TRUE) # Scale the variables

# Create wealth index using the first principal component
wealth_index_pca <- pca_result$x[, 1]

# Quintiles approach
wealth_quintiles <- cut(wealth_index_pca,
                        breaks = quantile(wealth_index_pca, probs = seq(0, 1, by = 0.2)),
                        labels = FALSE,
                        include.lowest = TRUE)

# Tuisiles (Tertiles)
wealth_tertiles <- cut(wealth_index_pca,
                       breaks = quantile(wealth_index_pca, probs = seq(0, 1, by = 0.333)),
                       labels = FALSE,
                       include.lowest = TRUE)

# Now you have:
# wealth_index_pca:  Wealth index from PCA (first principal component)
# wealth_quintiles:  Wealth quintiles based on PCA wealth index
# wealth_tertiles:  Wealth tertiles based on PCA wealth index

# You can add these back to your original dataset:
data$wealth_index_pca <- wealth_index_pca
data$wealth_quintiles <- wealth_quintiles
data$wealth_tertiles <- wealth_tertiles

# Print some summary statistics
print(summary(data$wealth_index_pca))
print(table(data$wealth_quintiles))
print(table(data$wealth_tertiles))

# Print PCA loadings to see variable contributions
print(pca_result$rotation[, 1]) # Loadings for the first principal component