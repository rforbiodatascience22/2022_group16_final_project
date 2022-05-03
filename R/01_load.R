# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load main data ---------------------------------------------------------------
my_data_raw <- read_tsv(file = "data/_raw/nhgh.tsv")


# Write data --------------------------------------------------------------
write_tsv(x = my_data_raw, 
          file = "data/01_nhgh.tsv")

