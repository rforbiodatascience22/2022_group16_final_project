# Load libraries ----------------------------------------------------------
library("tidyverse")
library("broom")
library("dplyr")
library("purrr")
library("tidyr")
library("ggplot2")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Normalize data
normalize<-function(x){
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}
# Load data ---------------------------------------------------------------
#all_data <- read_tsv(file = "data/01_nhgh.tsv")
clean_data <- read_tsv(file = "data/02_nhgh_clean.tsv")



my_test_data <-  select_if(clean_data, is.numeric) %>% select(-seqn) 

my_test_data <- my_test_data %>% select(-seqn)

my_test_data <- my_test_data %>%
  mutate_all(normalize)


# Model data-----------------------------------------------------

# Model data one k clusters------------------------------------------

# Make 3 clusters
kclust = kmeans(my_test_data, 3)
summary(kclust)
kclust


augmented = augment(kclust, my_test_data)

#Functions for plottingt the data
KMC_plot <- function(y_variable) {
  
  augmented.gathered <- augmented %>%
    gather(key = "variable", value = "value",
           -y_variable, -.cluster)
  
 p1 <- ggplot(augmented.gathered, aes_string(x = "value", y = y_variable)) +
    geom_point(aes(color = .cluster), alpha = 0.8) +
    facet_wrap(~variable)
 return(p1)
  
  #Save to pdf...
}




#Why do this loop not work??
for(i in colnames(my_test_data)){
  pl = KMC_plot(i)
  pl
}


age_pl = KMC_plot("age") # No clusters
KMC_plot("income") #
KMC_plot("tx")    #Nothing for factor
KMC_plot("dx")    #Nothing for factor
KMC_plot("wt")    #Linear correlations
KMC_plot("ht")    # cluster in tx, dx why?? income
KMC_plot("bmi")   #
KMC_plot("leg")
KMC_plot("arml")
KMC_plot("armc")
KMC_plot("waist")
KMC_plot("tri")
KMC_plot("sub")
KMC_plot("gh")
KMC_plot("albumin")
KMC_plot("bun")
KMC_plot("SCr")


# Data description

# Clusters  number of obs information of each point
# Centers, withinss and size  Information about each cluster
# totss, tot.withinss, betweens, iter   Information about the full clustering

augment(kclust, my_test_data) # point classification
tidy(kclust) #dummarizes per-cluster level
glance(kclust) #summarize in a single row




# Model data multiple k clusters----------------------------------------

res_kmeans = kmeans(my_test_data, 10)

kclusts <- 
  tibble(k = 1:9) %>%
  mutate(
    kclust = map(k, ~kmeans(my_test_data, .x)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, my_test_data)
  )


clusters <- 
  kclusts %>%
  unnest(cols = c(tidied))

assignments <- 
  kclusts %>% 
  unnest(cols = c(augmented))

clusterings <- 
  kclusts %>%
  unnest(cols = c(glanced))

# Visualise data ----------------------------------------------------------

p1 <- 
  ggplot(assignments, aes(x = age, y = bmi)) +
  geom_point(aes(color = .cluster), alpha = 0.8) + 
  facet_wrap(~ k)
p1

ggplot(clusterings, aes(k, tot.withinss)) +
  geom_line() +
  geom_point()

# Do something with the income variable
# Normalize the data
# Find a way to plot all against all variables
#What is the project function



# Wrangle data ------------------------------------------------------------
my_data_clean_aug %>% ...


# Model data
my_data_clean_aug %>% ...


# Visualise data ----------------------------------------------------------
my_data_clean_aug %>% ...


# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)