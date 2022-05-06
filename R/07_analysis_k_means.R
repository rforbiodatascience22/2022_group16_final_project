# Load libraries ----------------------------------------------------------
library("tidyverse")
library("broom")
library("dplyr")
library("purrr")
library("ggplot2")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------

clean_aug_data <- read_tsv(file = "data/03_nhgh_clean_aug.tsv")  

kmeans_data <- select_if(clean_aug_data, 
                         is.numeric) %>% 
  select(-seqn, -tx, -dx) 

kmeans_data <- kmeans_data %>%
  mutate_all(normalize)



# Model data-----------------------------------------------------

# Identify relevant number of clusters
res_kmeans <- kmeans(kmeans_data, 10)

kclusts <- 
  tibble(k = 1:9) %>%
  mutate(
    kclust = map(k, 
                 ~kmeans(kmeans_data,
                         .x)),
    tidied = map(kclust, 
                 tidy),
    glanced = map(kclust, 
                  glance),
    augmented = map(kclust, 
                    augment, 
                    kmeans_data)
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

# Visualize data ----------------------------------------------------------


ggplot(data = assignments, 
       mapping = aes(x = age, 
                     y = wt)) +
  geom_point(mapping = aes(color = .cluster), 
             alpha = 0.8) + 
  facet_wrap(~ k) +
  labs(x = "Age",
       y = "Weight")


ggplot(data = clusterings, 
       mapping = aes(x = k, 
                     y = tot.withinss)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(1,9,1)) +
  labs(x = "Number of clusters (k)")

# 3 clusters seem sufficient



# Model data with 3 clusters------------------------------------------

# Make 3 clusters

kclust = kmeans(x = kmeans_data, 
                centers = 3)

augmented = augment(x = kclust, 
                    kmeans_data) #Model object and dataset, assign the cluster to each obs in the original df


# Visualize data -----------------------------------------

# Plotting all against all clustering 
KMC_plot("age") # No clusters
KMC_plot("income")
KMC_plot("wt")    
KMC_plot("ht")    
KMC_plot("bmi")   
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