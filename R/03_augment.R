# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
my_data_clean <- read_tsv(file = "data/02_nhgh_clean.tsv")


# Wrangle data ------------------------------------------------------------
# Load data on diagnostic and treatment
treatment_data <- read_tsv(file = "data/_raw/diagnose_and_treatment.tsv")

# Join data
clean_data_joined <- full_join(my_data_clean, 
                             treatment_data,
                             by = c("tx", "dx"))

# Mutate data for PCA visualization
clean_data_aug_2k <- clean_data_joined %>% 
  mutate(`BMI category` = case_when(
    bmi < 18.5 ~ "Underweight",
    bmi >= 18.5 & bmi < 25 ~ "Normal weight",
    bmi >= 25 & bmi < 30 ~ "Overweight",
    bmi >= 30 ~ "Obese"
  ))


# Mutate data to not include one out of k-coding
# on race
clean_data_aug_1k <- clean_data_aug_2k %>% 
  pivot_longer(c(starts_with("Non-"), 
                 starts_with("Other"),
                 starts_with("Mexican")),
               names_to = "re",
               values_to = "re_val") %>% 
  filter(re_val == 1) %>% 
  select(-re_val)


# Mutate data to not include one out of k-coding
# on gender
clean_data_aug <- clean_data_aug_1k %>% 
  pivot_longer(c("male", "female"),
               names_to = "gender",
               values_to = "gender_val") %>% 
  filter(gender_val == 1) %>% 
  select(-gender_val)


# Write data --------------------------------------------------------------
#The overall dataset with 1-out-of-k-coding 
#for both race and gender
write_tsv(x = clean_data_aug_2k,
          file = "data/03_nhgh_clean_aug_2k.tsv")

#The overall dataset with 1-out-of-k-coding 
#for gender
write_tsv(x = clean_data_aug_1k,
          file = "data/03_nhgh_clean_aug_1k.tsv")

#The overall dataset without 1-out-of-k-coding
write_tsv(x = clean_data_aug,
          file = "data/03_nhgh_clean_aug.tsv")
