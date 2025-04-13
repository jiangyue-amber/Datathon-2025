library(tidyverse)
library(caret)
library(pROC)

cancer_train <- read.csv("/Users/yvargas/isic-2024-challenge/train-metadata.csv", header=TRUE)
summary(cancer_train)
str(cancer_train)
table(cancer_train$iddx_full)

table(cancer_train$target)

# replace empty strings with NA values
cancer_train <- cancer_train %>%
  mutate(iddx_1 = na_if(iddx_1, "")) %>%
  mutate(iddx_2 = na_if(iddx_2, "")) %>%
  mutate(iddx_3 = na_if(iddx_3, "")) %>%
  mutate(iddx_4 = na_if(iddx_4, "")) %>%
  mutate(iddx_5 = na_if(iddx_5, "")) %>%
  mutate(mel_mitotic_index = na_if(mel_mitotic_index, "")) %>%
  mutate(sex = na_if(sex, ""))  %>%
  mutate(anatom_site_general = na_if(anatom_site_general, ""))

# calculate number of missing values
table(cancer_train$iddx_1, useNA = "ifany")                 # missing 0
table(cancer_train$iddx_2, useNA = "ifany")                 # missing 399991
table(cancer_train$iddx_3, useNA = "ifany")                 # missing 399994
table(cancer_train$iddx_4, useNA = "ifany")                 # missing 400508
table(cancer_train$iddx_5, useNA = "ifany")                 # missing 401006
table(cancer_train$mel_mitotic_index, useNA = "ifany")      # missing 401006
table(cancer_train$mel_thick_mm, useNA = "ifany")           # missing 400996
table(cancer_train$sex, useNA = "ifany")                    # missing 11517
table(cancer_train$anatom_site_general, useNA = "ifany")    # missing 5756
table(cancer_train$age_approx, useNA = "ifany")             # missing 2798

cancer_train[cancer_train == ""] <- NA


##########################################################################

# target vs sex
ggplot(cancer_train, aes(x=factor(target), fill=sex)) +
  geom_bar(position = "dodge") +
  scale_y_log10() +
  labs(x = "Target", y = "Log10(Count)", fill = "Sex")

# iddx_1 is directly related to target in which iddx_1 = benign, indeterminate
# is mapped as target = 0, and all iddx_1 = malignant is mapped as target = 1
ggplot(cancer_train, aes(x=factor(target), fill=iddx_1)) +
  scale_y_log10() +
  geom_bar(position = "dodge")

# target vs anatom_site_general
ggplot(cancer_train, aes(x=factor(target), fill=anatom_site_general)) +
  scale_y_log10() +
  geom_bar(position = "dodge")

# target vs tbp_lv_location_simple
ggplot(cancer_train, aes(x=factor(target), fill=tbp_lv_location_simple)) +
  scale_y_log10() +
  geom_bar(position = "dodge")

# target vs size of lesion
ggplot(cancer_train, aes(x=factor(target), y=clin_size_long_diam_mm)) +
  ylim(0, 25) +
  geom_boxplot()

# target vs tbp_lv_area_perim_ratio
ggplot(cancer_train, aes(x=factor(target), y=tbp_lv_area_perim_ratio)) +
  ylim(0, 50) +
  geom_boxplot()

# target vs tbp_lv_areaMM2
ggplot(cancer_train, aes(x=factor(target), y=tbp_lv_areaMM2)) +
  ylim(0, 50) +
  geom_boxplot()

# target vs tbp_lv_perimeterMM
ggplot(cancer_train, aes(x=factor(target), y=tbp_lv_perimeterMM)) +
  ylim(0, 50) +
  geom_boxplot()

# target vs tbp_lv_norm_color
ggplot(cancer_train, aes(x=factor(target), y=tbp_lv_norm_color)) +
  ylim(0, 11) +
  geom_boxplot()

# target vs tbp_lv_deltaLBnorm
ggplot(cancer_train, aes(x=factor(target), y=tbp_lv_deltaLBnorm)) +
  geom_boxplot()


###################################################################
# TESTING LOGISTIC REGRESSION ASSUMPTIONS

# Each observation (row) is independent (AKA no duplicate people)
# 1037 duplicate + 5 unique patient_ids = 1042
# DATA IS CORRELATED
cancer_train %>%
  group_by(patient_id) %>%
  filter(n() > 1)

###################################################################
# resampling to address imbalance of 400,000 benign vs 400 malignant
# Extract only unique rows with unique patient_ids to ensure INDEPENDENCE ASSUMPTION

# filter first occurrence of each unique patient_id
# filter out missing values, and keep only complete rows from the selected variable
# FIRST ATTEMPT
malignant_sample <- cancer_train %>% filter(target == 1) %>% distinct(patient_id, .keep_all = TRUE) # 393 -> 259 
benign_sample <- cancer_train %>% filter(target == 0) %>% distinct(patient_id, .keep_all = TRUE) # 400666 -> 1041
undersampled_data <- bind_rows(malignant_sample, benign_sample) %>%
  select(target, age_approx, clin_size_long_diam_mm, anatom_site_general) %>%
  na.omit()

# filter unique patient_id by confidence in order to improve performance of the logistic model
# SECOND ATTEMPT: USE IN MODELING
malignant_sample <- cancer_train %>%
  filter(target == 1) %>%
  group_by(patient_id) %>%
  slice_max(order_by = tbp_lv_dnn_lesion_confidence, n = 1, with_ties = FALSE) %>%
  ungroup()

benign_sample <- cancer_train %>%
  filter(target == 0) %>%
  group_by(patient_id) %>%
  slice_max(order_by = tbp_lv_dnn_lesion_confidence, n = 1, with_ties = FALSE) %>%
  ungroup()

undersampled_data <- bind_rows(malignant_sample, benign_sample) %>%
  select(target, age_approx, clin_size_long_diam_mm, anatom_site_general) %>%
  na.omit()


table(undersampled_data$target)

# FIRST ATTEMPT and SECOND: 1014 benign, 256 malignant


# Note: lower AIC is better
origlm1 <- glm(target ~ age_approx, data=cancer_train, family=binomial(link="logit"))                                                # AIC 6163.6
origlm2 <- glm(target ~ age_approx + anatom_site_general, data=cancer_train, family=binomial(link="logit"))                          # AIC 5988
origlm3 <- glm(target ~ age_approx + clin_size_long_diam_mm, data=cancer_train, family=binomial(link="logit"))                       # AIC 5943.1
origlm4 <- glm(target ~ age_approx + clin_size_long_diam_mm + anatom_site_general, data=cancer_train, family=binomial(link="logit")) # AIC 5780.4

summary(origlm1)
                                                                                                                                           # With 2:1 ratio.  With 5:1 ratio  With unique patients (1042 -> 1270)
underlm1 <- glm(target ~ age_approx, data=undersampled_data, family=binomial(link="logit"))                                                # AIC 1468.8         2086.1        1.   1227.1       2.   1227.7
underlm2 <- glm(target ~ age_approx + anatom_site_general, data=undersampled_data, family=binomial(link="logit"))                          # AIC 1397.7.        1991.1             1198.1            1148.7
underlm3 <- glm(target ~ age_approx + clin_size_long_diam_mm, data=undersampled_data, family=binomial(link="logit"))                       # AIC 1367.7         1928.9             1102.6            1021.5
underlm4 <- glm(target ~ age_approx + clin_size_long_diam_mm + anatom_site_general, data=undersampled_data, family=binomial(link="logit")) # AIC 1275.4         1819.2             1063.6            915.3

# interaction between size and location doesnt help the model much, use likelihood test to confirm
underlm5 <- glm(target ~ age_approx + clin_size_long_diam_mm + anatom_site_general + anatom_site_general*clin_size_long_diam_mm, data=undersampled_data, family=binomial(link="logit")) # AIC 1255.1     1793.7   | 2. 914.86

summary(underlm5)


# likelihood ratio test
# Is anatom_site_general significant to the model? Yes, pvalue is small
anova(underlm3, underlm4, test = "Chisq")

# likelihood ratio test (p-value = 0.003821)
# Is size:location interaction significant to the model? Yes, pvalue is small
# SECOND ATTEMPT: p-value is large (0.076) interaction term not needed
anova(underlm4, underlm5, test = "Chisq")


# underlm5 has ROC curve of 0.7537 (2:1 ratio)
# underlm5 has ROC curve of 0.7477 (5:1 ratio)
# underlm5 has ROC curve of 0.7746 (unique patients)
predicted_probs <- predict(underlm4, type = "response")
auc_result <- roc(undersampled_data$target, predicted_probs) 
print(auc_result)
plot(auc_result, col = "blue", main = "ROC Curve")


###################################################################
# RANDOM FOREST for feature selection for logistic regression predictors
undersampled_data_rf_vars <- bind_rows(malignant_sample, benign_sample) %>%
  select(target, tbp_lv_H, tbp_lv_Hext, tbp_lv_deltaB, tbp_lv_L,
         tbp_lv_deltaLBnorm, tbp_lv_perimeterMM, tbp_lv_deltaA,
         tbp_lv_B, tbp_lv_A, tbp_lv_Aext) %>%
  na.omit()

####################################################################
# Shreya's RF variables in a logistic regression

# ROC 0.9398
glm_from_rf <- glm(target ~ tbp_lv_H + tbp_lv_Hext + tbp_lv_deltaB + tbp_lv_L +
                   tbp_lv_deltaLBnorm + tbp_lv_perimeterMM + tbp_lv_deltaA +
                   tbp_lv_B + tbp_lv_A + tbp_lv_Aext, data=undersampled_data_rf_vars,
                   family=binomial(link="logit"))  

# remove tbp_lv_deltaB because p-value = 0.52
# ROC 0.9396
glm_from_rf <- glm(target ~ tbp_lv_H + tbp_lv_Hext + tbp_lv_L +
                     tbp_lv_deltaLBnorm + tbp_lv_perimeterMM + tbp_lv_deltaA +
                     tbp_lv_B + tbp_lv_A + tbp_lv_Aext, data=undersampled_data_rf_vars,
                   family=binomial(link="logit"))  

# tbp_lv_deltaLBnorm has lowest p-value
glm_from_rf <- glm(target ~ tbp_lv_deltaLBnorm + tbp_lv_perimeterMM + tbp_lv_Hext +
                   tbp_lv_L + tbp_lv_B, 
                   data=undersampled_data_rf_vars, family=binomial(link="logit"))

# 2: 0.797
# 3: 0.8653
# 4: 0.8948
# 5: 0.9069

entire_dataset <- cancer_train %>%
  select(target, tbp_lv_H, tbp_lv_Hext, tbp_lv_deltaB, tbp_lv_L,
         tbp_lv_deltaLBnorm, tbp_lv_perimeterMM, tbp_lv_deltaA,
         tbp_lv_B, tbp_lv_A, tbp_lv_Aext) %>%
  na.omit()

glm_entire_data <- glm(target ~ tbp_lv_deltaLBnorm + tbp_lv_perimeterMM + tbp_lv_Hext +
                         tbp_lv_L + tbp_lv_B, 
                       data=entire_dataset, family=binomial(link="logit"))


summary(glm_from_rf)

predicted_probs <- predict(glm_from_rf, type = "response")
auc_result <- roc(undersampled_data_rf_vars$target, predicted_probs) 
print(auc_result)
plot(auc_result, col = "blue", main = "ROC Curve")

summary(glm_entire_data)

predicted_probs <- predict(glm_entire_data, type = "response")
auc_result <- roc(entire_dataset$target, predicted_probs) 
print(auc_result)
plot(auc_result, col = "blue", main = "ROC Curve")

# with the glm trained on the undersampled data, test on entire dataset
# evaluate the logistic regression model (trained on undersampled data)
# by predicting on the full original dataset
pred <- predict(glm_from_rf, entire_dataset, type="response")
pred <- as.integer(pred>0.5)
confusionMatrix(as.factor(pred), as.factor(entire_dataset$target))

# Load pROC library (install it if you haven't already)
library(pROC)

predicted_probs <- predict(glm_from_rf, entire_dataset, type = "response")
roc_obj <- roc(entire_dataset$target, predicted_probs)
plot(roc_obj, col = "blue", main = "ROC Curve - Logistic Regression")
auc_value <- auc(roc_obj)
legend("bottomright", legend = paste("AUC =", round(auc_value, 3)), col = "blue", lwd = 1)

