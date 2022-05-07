# Load libraries ----------------------------------------------------------
library("tidyverse")
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
  mutate(income = 
           case_when(income == "[25000,35000)" ~ 30000,
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
  summarise(n = n_distinct(seqn)) %>% 
  mutate(incomeXn = income * n) %>% 
  summarise(sum(incomeXn)/sum(n)) %>% 
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
  summarise(n = n_distinct(seqn)) %>% 
  mutate(incomeXn = income * n) %>% 
  summarise(sum(incomeXn)/sum(n)) %>% 
  pull()

# replace >20000 and <20000 categories with estimates
data <- data %>% 
  mutate(income = 
           case_when(income == 1 ~ sub_20000,
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

# Imputation of other NAs via KNN -------------------------------------


# Drop rows with too many NAs to do reliable KNN
data <- data %>% 
  filter(rowSums(is.na(.)) < 3)

# get names of variables which contain NAs:
cols_with_NA <- data %>% 
  select_if(function(x) any(is.na(x))) %>% 
  colnames()


# normalize data
data_normalized <- data %>% 
  select(-seqn, 
         -tx, 
         -dx) %>% 
  mutate_all(normalize)


# Get only rows which contain NAs (we dont need distances for those that do not contain NA)
data_rows_containing_na <- data_normalized %>% 
  filter_all(any_vars(is.na(.)))


# We only want to get KNNs which have a value for the variable we are trying to impute,
# so we filter out those that do not have a value for those
data_rows_where_leg_is_not_na <- data_normalized %>% 
  filter(!is.na(leg))
data_rows_where_arml_is_not_na <- data_normalized %>% 
  filter(!is.na(arml))
data_rows_where_armc_is_not_na <- data_normalized %>% 
  filter(!is.na(armc))
data_rows_where_waist_is_not_na <- data_normalized %>% 
  filter(!is.na(waist))
data_rows_where_tri_is_not_na <- data_normalized %>% 
  filter(!is.na(tri))
data_rows_where_sub_is_not_na <- data_normalized %>% 
  filter(!is.na(sub))


# Calculate KNN for each observation which has NA in a given variable
# using knn function from 99_project_functions.
# NB: This takes ~15-25 seconds to run per variable on RStudio Cloud
armc_knn <- knn(data_rows_containing_na, 
                data_rows_where_armc_is_not_na, 
                var = "armc",
                k=5)
arml_knn <- knn(data_rows_containing_na, 
                data_rows_where_arml_is_not_na, 
                var = "arml",
                k=5)
leg_knn <- knn(data_rows_containing_na, 
               data_rows_where_leg_is_not_na, 
               var = "leg",
               k=5)
sub_knn <- knn(data_rows_containing_na, 
               data_rows_where_sub_is_not_na, 
               var = "sub",
               k=5)
tri_knn <- knn(data_rows_containing_na, 
               data_rows_where_tri_is_not_na, 
               var = "tri",
               k=5)
waist_knn <- knn(data_rows_containing_na, 
                 data_rows_where_waist_is_not_na, 
                 var = "waist",
                 k=5)


# Get the seqn number of observations which contain any number of NAs
seqns_with_NAs <- data %>% 
  filter_all(any_vars(is.na(.))) %>% 
  select(seqn) %>% 
  pull


# In the original data, when a row contains NAs in a given variable replace it with the mean value of that variable
# from the KNN list-column.
############## NB: This runs quite slow, 1-2 minutes per variable. Go get a coffee! #############
data <- data %>% 
  rowwise %>% 
  mutate(armc = case_when(seqn %in% seqns_with_NAs ~ 
                            indexing_function(cur_data(),
                                              armc_knn, 
                                              "armc"),
                          !seqn %in% seqns_with_NAs ~ armc))

data <- data %>% 
  rowwise %>% 
  mutate(arml = case_when(seqn %in% seqns_with_NAs ~ 
                            indexing_function(cur_data(),
                                              arml_knn, 
                                              "arml"),
                          !seqn %in% seqns_with_NAs ~ arml))

data <- data %>% 
  rowwise %>% 
  mutate(leg = case_when(seqn %in% seqns_with_NAs ~ 
                           indexing_function(cur_data(),
                                             leg_knn, 
                                             "leg"),
                         !seqn %in% seqns_with_NAs ~ leg))

data <- data %>% 
  rowwise %>% 
  mutate(sub = case_when(seqn %in% seqns_with_NAs ~ 
                           indexing_function(cur_data(),
                                             sub_knn, 
                                             "sub"),
                         !seqn %in% seqns_with_NAs ~ sub))

data <- data %>% 
  rowwise %>% 
  mutate(tri = case_when(seqn %in% seqns_with_NAs ~ 
                           indexing_function(cur_data(),
                                             tri_knn, 
                                             "tri"),
                         !seqn %in% seqns_with_NAs ~ tri))

data <- data %>% 
  rowwise %>% 
  mutate(waist = case_when(seqn %in% seqns_with_NAs ~ 
                             indexing_function(cur_data(),
                                               waist_knn, 
                                               "waist"),
                           !seqn %in% seqns_with_NAs ~ waist))

# Load data on diagnostic and treatment
treatment_data <- read_tsv(file = "data/_raw/diagnose_and_treatment.tsv")

# Join data
clean_data_joined <- full_join(data, 
                               treatment_data,
                               by = c("tx", 
                                      "dx"))

# Write data for outlier detection in 04_analysis_variable_exploration ----
write_tsv(x = clean_data_joined,
          file = "data/02_nhgh_no_NA.tsv")


# Removing outliers in SCr ------------------------------------------------

data_outliers_removed <- clean_data_joined %>% 
  filter(SCr < 5)

# Write data --------------------------------------------------------------
write_tsv(x = data_outliers_removed,
          file = "data/02_nhgh_clean.tsv")
