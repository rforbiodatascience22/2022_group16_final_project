# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load main data ---------------------------------------------------------------
my_data_raw <- read_tsv(file = "data/_raw/nhgh.tsv")

# Load more data
treatment_data <- read_tsv(file = "data/_raw/diagnose_and_treatment.tsv")

# Wrangle data ------------------------------------------------------------

# Split data

# Join data
raw_data_joined <- full_join(my_data_raw, 
                  treatment_data,
                  by = c("tx", "dx"))

#my_data <- my_data_raw # %>% ...


# Write data --------------------------------------------------------------
write_tsv(x = raw_data_joined, # remember to change this when we have done wrangling
          file = "data/01_nhgh.tsv")
