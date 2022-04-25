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

my_data_clean_aug <- clean_data_joined # %>% ...



# Mutate data for PCA visualization
clean_data_pca <- clean_data_joined %>% 
  mutate(`BMI category` = case_when(
    bmi < 18.5 ~ "Underweight",
    bmi >= 18.5 & bmi < 25 ~ "Normal weight",
    bmi >= 25 & bmi < 30 ~ "Overweight",
    bmi >= 30 ~ "Obese"
  ))

# Write data --------------------------------------------------------------
#The overall dataset used as reference
write_tsv(x = my_data_clean_aug,
          file = "data/03_nhgh_clean_aug.tsv")

#Dataset used for PCA
write_tsv(x = clean_data_pca,
          file = "data/03_nhgh_clean_aug_PCA.tsv")
