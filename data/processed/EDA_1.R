# Load libraries
library(dplyr)
library(randomForest)
library(ggplot2)
library(reshape2)
library(patchwork)

# Define columns to drop
drop_columns <- c('isic_id', 'copyright_license', 'attribution', 'image_type',
                  'iddx_1', 'iddx_2', 'iddx_3', 'iddx_4', 'iddx_5', 'iddx_full',
                  'mel_mitotic_index', 'mel_thick_mm', 'tbp_tile_type',
                  'tbp_lv_dnn_lesion_confidence', 'lesion_id', 'tbp_lv_x', 'tbp_lv_y', 'tbp_lv_z')

df_new <- cancer_train[, !(names(cancer_train) %in% drop_columns)]

# Replace NA values with mode for each column
replace_na_with_mode <- function(column) {
  if (is.factor(column) || is.character(column)) {
    column[is.na(column)] <- names(sort(table(column), decreasing = TRUE))[1]
  } else if (is.numeric(column)) {
    column[is.na(column)] <- as.numeric(names(sort(table(column), decreasing = TRUE))[1])
  }
  return(column)
}

df_new <- df_new %>% mutate(across(everything(), replace_na_with_mode))

# Define categorical and continuous
cat_vars <- c('patient_id','target','age_approx','sex','anatom_site_general','tbp_lv_location','tbp_lv_location_simple')
con_vars <- setdiff(names(df_new), cat_vars)

# Convert to factor and encode as numeric
df_new[cat_vars] <- lapply(df_new[cat_vars], function(col) as.numeric(as.factor(col)))


set.seed(42)
# 75% train, 25% test
sample_index <- sample(nrow(df_new), 0.75 * nrow(df_new))
train_data <- df_new[sample_index, ]
test_data <- df_new[-sample_index, ]

X_train <- train_data %>% select(-target)
y_train <- train_data$target

rf_model <- randomForest(x = X_train, y = as.factor(y_train), 
                         ntree = 100, nodesize = 5, importance = TRUE)
print(rf_model)

# Get importance
importance_df <- data.frame(
  feature = rownames(importance(rf_model)),
  importance = importance(rf_model)[, "MeanDecreaseGini"]
)

# Plot
ggplot(importance_df, aes(x = reorder(feature, importance), y = importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Feature Importances", x = "Features", y = "Importance (Gini Decrease)") +
  theme_minimal(base_size = 14)

# # Compute correlation matrix (only numeric columns!)
# cor_matrix <- cor(X_train)
# 
# # Melt the correlation matrix for ggplot
# melted_cor <- melt(cor_matrix)
# 
# # Plot the heatmap
# ggplot(data = melted_cor, aes(x = Var1, y = Var2, fill = value)) +
#   geom_tile(color = "white") +
#   geom_text(aes(label = round(value, 2)), size = 3, color = "black") +
#   scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
#                        midpoint = 0, limit = c(-1,1), space = "Lab", 
#                        name="Correlation") +
#   theme_minimal(base_size = 12) +
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, 
#                                    size = 10, hjust = 1),
#         axis.text.y = element_text(size = 10)) +
#   coord_fixed() +
#   ggtitle("Correlation Heatmap")
# 
# 
# df_train <- cbind(X_train, target = y_train)
# cat_df <- c("patient_id", "age_approx", "sex", "anatom_site_general", "tbp_lv_location", "tbp_lv_location_simple")
# 
# plot_list <- list()
# 
# for (f in cat_df) {
#   p <- ggplot(df_train, aes(x = as.factor(target), y = .data[[f]])) +
#     geom_violin(trim = FALSE, fill = "#a0c4ff") +
#     geom_boxplot(width = 0.1, outlier.shape = NA, color = "black") +
#     labs(title = paste("Target vs", f), x = "Target", y = f) +
#     theme_minimal()
#   
#   plot_list[[f]] <- p
# }
# 
# # Arrange all plots 2 per row using patchwork
# library(patchwork)
# 
# wrap_plots(plot_list, ncol = 2)# 