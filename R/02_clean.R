# Load libraries ----------------------------------------------------------
library("tidyverse")
library("corrr")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
data <- read_tsv(file = "data/01_nhgh.tsv")

# Age as int --------------------------------------------------------------
data <- data %>% 
  mutate(age = round(age, 
                     digits = 0)) %>% 
  mutate_at(vars(age), 
            as.integer)

# sex and re as factor ----------------------------------------------------
data <- data %>% 
  mutate_at(vars(sex, re), 
            factor)


# NA treatment ------------------------------------------------------------

## Setting missing income data to avg of income ---------------------------

# substitute income ranges with avg of range
data <- data %>% 
  mutate(income = case_when(income == "[25000,35000)" ~ 30000,
                            income == "[45000,55000)" ~ 50000,
                            income == "[10000,15000)" ~ 12500,
                            income == "[35000,45000)" ~ 40000,
                            income == "[15000,20000)" ~ 17500,
                            income == "[75000,100000)" ~ 87500,
                            income == "[5000,10000)" ~ 7500,
                            income == "[0,5000)" ~ 2500,
                            income == "[20000,25000)" ~ 22500,
                            income == ">= 100000" ~ 100000,
                            income == "[65000,75000)" ~ 70000,
                            income == "> 20000" ~ 20000,
                            income == "[55000,65000)" ~ 60000,
                            income == "< 20000" ~ 10000))

# calculate avg income (without NA observations)
avg_income <- data %>%
  drop_na(income) %>% 
  summarise(avg_income = mean(income)) %>% 
  pull()

# substitute income NAs with avg income
data <- data %>% 
  replace_na(list(income = avg_income))

# Binarization of gender --------------------------------------------------
data %>% mutate(sex_binarized = case_when(sex == 'female' ~ -1, 
                                          sex == 'male' ~ 1)) 


# Imputation of NAs in variable "leg" -------------------------------------

# find correlated variables

data %>% 
  select(age, wt, ht, leg, arml, armc, waist) %>% 
  filter(leg > 0.5) %>% 
  correlate() 



# Write data --------------------------------------------------------------
write_tsv(x = data_clean,
          file = "data/02_nhgh_clean.tsv")