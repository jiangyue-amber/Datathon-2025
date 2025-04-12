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

# claculate number of missing values
table(cancer_train$iddx_1, useNA = "ifany")              # missing 0
table(cancer_train$iddx_2, useNA = "ifany")              # missing 399991
table(cancer_train$iddx_3, useNA = "ifany")              # missing 399994
table(cancer_train$iddx_4, useNA = "ifany")              # missing 400508
table(cancer_train$iddx_5, useNA = "ifany")              # missing 401006
table(cancer_train$mel_mitotic_index, useNA = "ifany")   # missing 401006
table(cancer_train$mel_thick_mm, useNA = "ifany")        # missing 400996
table(cancer_train$sex, useNA = "ifany")                 # missing 11517
table(cancer_train$anatom_site_general, useNA = "ifany") # missing 5756
table(cancer_train$clin_size_long_diam_mm, useNA = "ifany") # missing 5756
