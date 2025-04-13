# Load required packages
library(ggplot2)
library(scales)

# Assuming cancer_metadata is already loaded
# Step 1: Calculate missing percentages
missing_pct <- colSums(is.na(cancer_metadata)) / nrow(cancer_metadata)
missing_data <- data.frame(
  variable = names(missing_pct),
  missing_pct = missing_pct
)

# Step 2: Filter only variables with missing values
missing_data_filtered <- missing_data[missing_data$missing_pct > 0, ]

# Step 3: Bar chart
ggplot(missing_data_filtered, aes(x = reorder(variable, -missing_pct), y = missing_pct)) +
  geom_bar(stat = "identity", fill = "#FF6F61") +
  labs(
    title = "Missing Data Percentage per Variable",
    x = "Variable",
    y = "Percent Missing"
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 0.001)) +
  geom_text(aes(label = sprintf("%.4f%%", missing_pct * 100)), vjust = -0.5, size = 3.5) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
