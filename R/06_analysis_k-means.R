# Load libraries ----------------------------------------------------------
library("tidyverse")
library("broom")
library("dplyr")
library("purrr")
library("tidyr")
library("ggplot2")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")



# Load data ---------------------------------------------------------------

clean_aug_data <- read_tsv(file = "data/03_nhgh_clean_aug.tsv")  

kmeans_data <-  select_if(clean_aug_data, is.numeric) %>% select(-seqn, -tx, -dx) 

kmeans_data <- kmeans_data %>%
  mutate_all(normalize)



# Model data-----------------------------------------------------

# Identify relevant number of clusters
res_kmeans = kmeans(kmeans_data, 10)

kclusts <- 
  tibble(k = 1:9) %>%
  mutate(
    kclust = map(k, ~kmeans(kmeans_data, .x)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, kmeans_data)
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
  ggplot(assignments, aes(x = age, y = wt)) +
  geom_point(aes(color = .cluster), alpha = 0.8) + 
  facet_wrap(~ k)
p1

ggplot(clusterings, aes(k, tot.withinss)) +
  geom_line() +
  geom_point()

# 3 clusters seem sufficient



# Model data with 3 clusters------------------------------------------

# Make 3 clusters
kclust = kmeans(kmeans_data, centers = 3)
summary(kclust)
kclust


augmented = augment(kclust, kmeans_data) #Model object and dataset, assign the cluster to each obs in the original df

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


# Clusters  number of obs information of each point
# Centers, withinss and size  Information about each cluster
# totss, tot.withinss, betweens, iter   Information about the full clustering


##Correlation matrix
#library(reshape2)
#cor_mat <- round(cor(clean_data),2)
#melted_cormat <- melt(cor_mat)
#ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
#  geom_tile() +
# theme(axis.text.x = element_text(angle = 45))

#correlation_data <- cor_mat %>%
# gather(key = "variable", value = "value",
#       -y_variable, -.cluster)   #key and value is names of the columns, y_variable is kept as a column, and the same for clusters. (from wide to long)





