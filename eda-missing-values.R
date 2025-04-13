library(tidyverse)
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
# resampling to address imbalance of 400,000 benign vs 400 malignant

# keep all malignant cases
# sample two times the number of malignant cases for benign cases
# filter out missing values, and keep only complete rows from the selected variable
malignant_sample <- cancer_train %>% filter(target == 1)
benign_sample <- cancer_train %>% filter(target == 0) %>% sample_n(nrow(malignant_sample) * 2)  # 1:2 ratio

undersampled_data <- bind_rows(malignant_sample, benign_sample) %>%
  select(target, age_approx, clin_size_long_diam_mm, anatom_site_general) %>%
  na.omit()

# Note: lower AIC is better
origlm1 <- glm(target ~ age_approx, data=cancer_train, family=binomial(link="logit"))                                                # AIC 6163.6
origlm2 <- glm(target ~ age_approx + anatom_site_general, data=cancer_train, family=binomial(link="logit"))                          # AIC 5988
origlm3 <- glm(target ~ age_approx + clin_size_long_diam_mm, data=cancer_train, family=binomial(link="logit"))                       # AIC 5943.1
origlm4 <- glm(target ~ age_approx + clin_size_long_diam_mm + anatom_site_general, data=cancer_train, family=binomial(link="logit")) # AIC 5780.4

summary(origlm1)

underlm1 <- glm(target ~ age_approx, data=undersampled_data, family=binomial(link="logit"))                                                # AIC 1468.8
underlm2 <- glm(target ~ age_approx + anatom_site_general, data=undersampled_data, family=binomial(link="logit"))                          # AIC 1397.7
underlm3 <- glm(target ~ age_approx + clin_size_long_diam_mm, data=undersampled_data, family=binomial(link="logit"))                       # AIC 1367.7
underlm4 <- glm(target ~ age_approx + clin_size_long_diam_mm + anatom_site_general, data=undersampled_data, family=binomial(link="logit")) # AIC 1275.4

# interaction between size and location doesnt help the model much
underlm5 <- glm(target ~ age_approx + clin_size_long_diam_mm + anatom_site_general + anatom_site_general*clin_size_long_diam_mm, data=undersampled_data, family=binomial(link="logit")) # AIC 1255.1

summary(underlm5)


# likelihood ratio test
# Is anatom_site_general significant to the model? Yes, pvalue is small
anova(underlm3, underlm4, test = "Chisq")

# likelihood ratio test
# Is size:location interaction significant to the model? Yes, pvalue is small
anova(underlm4, underlm5, test = "Chisq")



# underlm5 has ROC curve of 0.7537
predicted_probs <- predict(underlm5, type = "response")
auc_result <- roc(undersampled_data$target, predicted_probs) 
print(auc_result)
plot(auc_result, col = "blue", main = "ROC Curve")

