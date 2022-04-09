# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
my_data_raw <- read_tsv(file = "data/_raw/nhgh.tsv")


# Wrangle data ------------------------------------------------------------

# Split data



# Join data


#my_data <- my_data_raw # %>% ...


# Write data --------------------------------------------------------------
write_tsv(x = my_data_raw, # remember to change this when we have done wrangling
          file = "data/01_nhgh.tsv")
