library(tidyverse)
library(janitor)
install.packages("skimr")
library(skimr)
data_raw <- read_csv("vitamin_deficiency_disease_dataset_20260123.csv")
View(data_raw)
data <- data_raw
# View structure
str(data)
# Preview rows
head(data)
# Summary statistics
summary(data)
# Missing values overview
skim(data)
data <- data %>%
  clean_names()
data <- data %>%
  mutate(
    gender = as.factor(gender),
    diet_type = as.factor(diet_type),
    smoking_status = as.factor(smoking_status),
    alcohol_consumption = as.factor(alcohol_consumption),
    exercise_level = as.factor(exercise_level),
    sun_exposure = as.factor(sun_exposure),
    income_level = as.factor(income_level),
    latitude_region = as.factor(latitude_region),
    disease_diagnosis = as.factor(disease_diagnosis)
  )
colSums(is.na(data))
data <- data %>%
  mutate(
    alcohol_consumption = fct_explicit_na(alcohol_consumption, "Unknown"),
    smoking_status = fct_explicit_na(smoking_status, "Unknown")
  )
data <- data %>%
  mutate(
    alcohol_consumption = fct_na_value_to_level(alcohol_consumption, "Unknown"),
    smoking_status = fct_na_value_to_level(smoking_status, "Unknown")
  )
data <- data %>%
  mutate(
    hemoglobin = ifelse(is.na(hemoglobin),
                        median(hemoglobin, na.rm = TRUE),
                        hemoglobin)
  )
# Age should be positive
data <- data %>%
  filter(age > 0 & age < 120)
# Vitamin %RDA should be reasonable
data <- data %>%
  filter(
    vitamin_d_percent_rda >= 0,
    vitamin_b12_percent_rda >= 0,
    iron_percent_rda >= 0
  )
data <- data %>%
  mutate(
    hemoglobin_g_dl = ifelse(
      is.na(hemoglobin_g_dl),
      median(hemoglobin_g_dl, na.rm = TRUE),
      hemoglobin_g_dl
    )
  )
# Age should be positive
data <- data %>%
  filter(age > 0 & age < 120)

# Vitamin %RDA should be reasonable
data <- data %>%
  filter(
    vitamin_d_percent_rda >= 0,
    vitamin_b12_percent_rda >= 0,
    iron_percent_rda >= 0
  )
data <- data %>%
  mutate(
    vit_d_deficient = vitamin_d_percent_rda < 80,
    vit_b12_deficient = vitamin_b12_percent_rda < 80,
    iron_deficient = iron_percent_rda < 80
  )
data <- data %>%
  mutate(
    deficiency_count =
      vit_d_deficient +
      vit_b12_deficient +
      iron_deficient
  )
# Check factor levels
levels(data$diet_type)
# Cross-tab disease vs deficiencies
table(data$disease_diagnosis, data$vit_d_deficient)
# Final structure check
str(data)
# Save cleaned dataset
write_csv(data, "vitamin_deficiency_cleaned.csv")
# Age distribution
summary(data$age)
# Gender distribution
data %>%
  count(gender) %>%
  mutate(percent = n / sum(n) * 100)
data %>%
  summarise(
    mean_vit_d = mean(vitamin_d_percent_rda),
    mean_vit_b12 = mean(vitamin_b12_percent_rda),
    mean_iron = mean(iron_percent_rda)
  )
data %>%
  summarise(
    vit_d_def = mean(vit_d_deficient) * 100,
    vit_b12_def = mean(vit_b12_deficient) * 100,
    iron_def = mean(iron_deficient) * 100
  )
install.packages("ggplot2")
library(ggplot2)
ggplot(data, aes(age)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Age Distribution of Participants",
    x = "Age",
    y = "Count"
  )
ggplot(data, aes(vitamin_d_percent_rda)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Vitamin D %RDA Distribution",
    x = "Vitamin D (% RDA)",
    y = "Count"
  )
data %>%
  pivot_longer(
    cols = c(vit_d_deficient, vit_b12_deficient, iron_deficient),
    names_to = "nutrient",
    values_to = "deficient"
  ) %>%
  filter(deficient == TRUE) %>%
  count(nutrient) %>%
  ggplot(aes(nutrient, n)) +
  geom_col() +
  labs(
    title = "Number of Deficient Individuals by Nutrient",
    x = "Nutrient",
    y = "Count"
  )
ggplot(data, aes(sun_exposure, vitamin_d_percent_rda)) +
  geom_boxplot() +
  labs(
    title = "Vitamin D %RDA by Sun Exposure",
    x = "Sun Exposure Level",
    y = "Vitamin D (% RDA)"
  )
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)
library(tidyverse)
data %>%
  pivot_longer(
    cols = c(vit_d_deficient, vit_b12_deficient, iron_deficient),
    names_to = "nutrient",
    values_to = "deficient"
  ) %>%
  filter(deficient == TRUE) %>%
  count(nutrient) %>%
  ggplot(aes(nutrient, n)) +
  geom_col() +
  labs(
    title = "Number of Deficient Individuals by Nutrient",
    x = "Nutrient",
    y = "Count"
  )
data %>%
  select(vit_d_deficient, vit_b12_deficient, iron_deficient) %>%
  mutate(across(everything(), as.numeric)) %>%
  cor() %>%
  as.data.frame() %>%
  rownames_to_column("var1") %>%
  pivot_longer(-var1) %>%
  ggplot(aes(var1, name, fill = value)) +
  geom_tile() +
  labs(
    title = "Correlation Between Nutrient Deficiencies",
    x = "",
    y = ""
  )
install.packages("rsconnect")
library(rsconnect)


