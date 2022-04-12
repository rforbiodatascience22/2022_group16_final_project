# Load libraries ----------------------------------------------------------
library("tidyverse")
library("corrr")
library("pdist")
library("broom")

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

# Binarization of sex/re --------------------------------------------------

data <- data %>% 
  mutate(bin = 1) %>% 
  pivot_wider(names_from = sex,
              values_from = bin,
              values_fill = 0)

data <- data %>% 
  mutate(bin = 1) %>% 
  pivot_wider(names_from = re,
              values_from = bin,
              values_fill = 0)

#data <- data %>% 
#  mutate(bin = 1) %>% 
#  pivot_wider(names_from = sex,                             # create a new variable for each factor in the sex variable (male/female)
#              values_from = bin,                            # gives value from bin (male: 1 when male, NA when not male)
#              values_fill = list(bin=0),                    # fills the above NA values with 0
#              values_fn = list(bin=function(x){head(x,1)})) # function returns the first value (head 1) of object x
                                                             # These two last lines are used when there are duplicate rows in the data - we do not have that, so we dont need it
#data <- data %>% 
#  mutate(bin = 1) %>% 
#  pivot_wider(names_from = re,
#              values_from = bin,
#              values_fill = list(bin=0),
#              values_fn = list(bin=function(x){head(x,1)}))

# Imputation of other NAs -------------------------------------

# find correlated variables
data %>% 
  #drop_na() %>% 
  select(-seqn, -income) %>% 
  filter(leg > 0.5) %>% 
  corrr::correlate() 
# take top most correlated variables and use these to impute NAs
# OR
# KNN approach (I will not do it with ML as the project FAQ says not to)
# 1) Normalize data
data_normalized <- data %>% 
  select(-seqn) %>% 
  mutate_all(normalize)

# 2) find rows which contain NAs
observations <- data %>% 
  filter_all(any_vars(is.na(.))) %>% 
  select(seqn) %>% 
  mutate_at(1, as.character)


NA_data <- data_normalized %>% 
  filter_all(any_vars(is.na(.)))
  

# 3) Make distance matrix with distances from NA-containing observations to all other observations
distances <- as_tibble((as.matrix(pdist(NA_data,                 # using pdist package to compute distances between rows of two matrices
                           data_normalized))))

# Renaming columns
distances <- distances %>% 
  rename_all(~data %>% 
               pull(seqn) %>% 
               as.character())

# Renaming rows
distances <- distances %>% 
  mutate(X = observations$seqn) 

distances <- distances %>% 
  remove_rownames %>% 
  column_to_rownames(var = "X")

# Distances between ALL observations
#distances <- tibble(as.matrix(dist(data_normalized))) # might have to see if there is a pure 'tidyverse' way of doing this

# 4) For each observation with missing values, select k nearest neighbours (tuning of k would be a ML task)
k = 100




# 5) Substitute NAs in each observation with median/avg (?) value from k nearest neighbours



# Write data --------------------------------------------------------------
write_tsv(x = data_clean,
          file = "data/02_nhgh_clean.tsv")
