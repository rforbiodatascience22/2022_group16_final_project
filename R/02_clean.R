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

# Drop rows with too many NAs to impute
data <- data %>% 
  filter(rowSums(is.na(.)) < 3)


# find correlated variables
data %>% 
  #drop_na() %>% 
  select(-seqn, -income) %>% 
  filter(leg > 0.5) %>% 
  corrr::correlate() 
# take top most correlated variables and use these to impute NAs
# OR




# KNN approach (I will not do it with ML as the project FAQ says not to)
#### 1) Normalize data
data_normalized <- data %>% 
  select(-seqn, 
         -tx, 
         -dx) %>% 
  mutate_all(normalize)

#### 2) find rows which contain NAs
observations <- data %>% 
  filter_all(any_vars(is.na(.))) %>% 
  select(seqn) %>% 
  mutate_at(1, as.character)


NA_data <- data_normalized %>% 
  filter_all(any_vars(is.na(.)))
  

#### 3) Make distance matrix with distances from NA-containing observations to all other observations
distances <- as_tibble((as.matrix(pdist(NA_data,                 # using pdist package to compute distances between rows of two matrices
                                        data_normalized))))

# Renaming columns
distances <- distances %>% 
  rename_all(~data %>% 
               pull(seqn) %>% 
               as.character())




# Renaming rows (DEPRECATED)
#distances <- distances %>% 
#  mutate(X = observations$seqn) 

#distances <- distances %>% 
#  remove_rownames %>% 
#  column_to_rownames(var = "X")


# Distances between ALL observations
#distances <- tibble(as.matrix(dist(data_normalized))) # might have to see if there is a pure 'tidyverse' way of doing this



#### 4) For each observation with missing values, select k nearest neighbours (tuning of k would be a ML task)

#distances %>% 
 # rowwise() %>% 
 # map(somefunction)

#distances %>% 
#  rowwise(X) %>% 
#  transmute(min = min(c_across(cols = everything())))

#distances %>% 
#  rowwise() %>% 
#  transmute(min = min(c_across(where(is.numeric))))

############ TEST #############
#### TRANSPOSING TABLE
distances_plus <- distances %>% 
  mutate(seqn = observations %>% 
           select(seqn) %>% 
           unlist(., use.names = FALSE)) %>% 
  relocate(seqn)

distances_transposed <- distances_plus %>% 
  pivot_longer(cols =-1) %>% 
  pivot_wider(names_from = seqn, values_from = value) %>% 
  select(-name)
####

k = 5
distances_transposed %>% 
  map_df(sort) %>% 
  slice(2:(k+1))


# Problem: Knowing which indeces each sorted distance comes from.
# Solution: Turning each value into a list of two; the distance and the seqn and then sorting them by the distance

############ TEST OVER #############

#
knn_val_and_index <- distances %>% 
  rowwise() %>% 
  transmute(x = list(knn(c_across(where(is.numeric)), 
                             k = 5))) %>% 
  unnest(x)





# 5) Substitute NAs in each observation with avg value from k nearest neighbours

source(file = "R/99_project_functions.R")

seqn_rowid <- data %>% 
  rowid_to_column() %>% 
  filter_all(any_vars(is.na(.))) %>% 
  select(rowid, 
         seqn)

new <- seqn_rowid %>% 
  mutate(knn = knn_val_and_index %>% 
           select(6:10)) %>% 
  unnest(knn)


joined <- left_join(data, 
                    new %>% select(-rowid), 
                    by="seqn") 

joined <- joined %>% 
  pivot_longer(cols = c(V6,V7,V8,V9,V10), 
               names_to = "KNN", 
               values_to = "Value") %>% 
  group_by(seqn) %>% 
  summarize(KNN = list(Value))


joined2 <- left_join(data,
                     joined,
                     by = "seqn")

# How to get mean of the KNN
joined2 %>% 
  slice(joined2 %>% 
          slice(10) %>% 
          select(KNN) %>% 
          unlist(., use.names = FALSE)) %>% 
  summarise(mean = mean(sub, na.rm = TRUE))

# Backbone for doing NA value replacement
data %>% 
  mutate(sub = case_when(is.na(sub) ~ ,
                         !is.na(sub) ~ sub))


# Problem: Most similar vectors have NAs in sub as well







#new %>% 
#  mutate(sub = mean(data %>% slice()))


#observations %>% rowwise() %>% sapply(testfunction)
#observations  %>% 
#  tibble::rownames_to_column() %>% 
#  pivot_longer(-rowname) %>% 
#  pivot_wider(names_from = rowname, values_from = value) %>% 
#  lapply(testfunction)

#data %>% 
#  transmute(x = if_else(is.na(sub), testfunction(55), sub))

# Write data --------------------------------------------------------------
write_tsv(x = data_clean,
          file = "data/02_nhgh_clean.tsv")
