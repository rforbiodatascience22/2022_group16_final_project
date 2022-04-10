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
# (>= 100000 category set to 100000 as we have no other
# way of extrapolating values in this category)
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
                            income == "[55000,65000)" ~ 60000,
                            income == "< 20000" ~ 1, 
                            income == "> 20000" ~ 2))

# calculate replacement for <20000 category by
# distribution of the 0-5000, 5000-10000, 10000-15000
# and 15000-20000 income ranges
sub_20000 <- data %>% 
  group_by(income) %>% 
  filter(income == 2500 | 
           income == 7500 | 
           income == 12500 | 
           income == 17500 ) %>% 
  summarise(n_distinct(seqn)) %>% 
  mutate(incomeXn = income * `n_distinct(seqn)`) %>% 
  summarise(sum(incomeXn)/sum(`n_distinct(seqn)`)) %>% 
  pull()

# calculate replacement for >20000 category by same idea
over_20000 <- data %>% 
  group_by(income) %>% 
  filter(income == 22500 | 
           income == 30000 | 
           income == 40000 | 
           income == 50000 |
           income == 60000 |
           income == 70000 |
           income == 87500 |
           income == 100000) %>% 
  summarise(n_distinct(seqn)) %>% 
  mutate(incomeXn = income * `n_distinct(seqn)`) %>% 
  summarise(sum(incomeXn)/sum(`n_distinct(seqn)`)) %>% 
  pull()

# replace >20000 and <20000 categories with estimates
data <- data %>% 
  mutate(income = case_when(income == 1 ~ sub_20000,
                            income == 2 ~ over_20000,
                            TRUE ~ as.numeric(as.character(income))))


# calculate avg income (without NA observations)
avg_income <- data %>%
  drop_na(income) %>% 
  summarise(avg_income = mean(income)) %>% 
  pull()

# substitute income NAs with avg income
data <- data %>% 
  replace_na(list(income = avg_income))

# Binarization of gender --------------------------------------------------
#data %>% mutate(sex_binarized = case_when(sex == 'female' ~ -1, 
#                                          sex == 'male' ~ 1)) 


# Imputation of other NAs -------------------------------------

# Need to find a way to do correlation between factor (sex, re)
# and numeric value

# find correlated variables
#data %>% 
#  select(age,wt, ht, leg, arml, armc, waist, tri, sub, gh, albumin, bun, SCr) %>% 
#  filter(leg > 0.5) %>% 
#  corrr::correlate() 

# take top most correlated variables and use these to impute NAs

# Alternatively: Could use a KNN approach


# Write data --------------------------------------------------------------
write_tsv(x = data_clean,
          file = "data/02_nhgh_clean.tsv")