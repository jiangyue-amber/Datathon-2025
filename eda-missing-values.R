library(tidyverse)

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

ggplot(cancer_train, aes(x=factor(target), y=tbp_lv_deltaLBnorm)) +
  ylim(0, 11) +
  geom_boxplot()

