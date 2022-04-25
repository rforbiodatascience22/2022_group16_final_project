# Load libraries ----------------------------------------------------------
library("tidyverse")
library("broom")
library("stringr")
library("patchwork")


###Spørgsmål: skal vi fjerne det "outlier" 
###punkt der er på 2. visualisering? (særligt høj SCr)

source(file = "R/99_project_functions.R")

# Load data
data <- read_tsv(file = "data/03_nhgh_clean_aug.tsv")

# Count number of observations in each "Treatment status" category
data %>% count(`Treatment status`)

# For data visualization purposes, arrange data
data <- data %>% arrange(Treatment_number)

#######THIS PIECE BELONGS TO 03_augment.r###############

data <- data %>% 
  mutate(`BMI category` = case_when(
    bmi < 18.5 ~ "Underweight",
    bmi >= 18.5 & bmi < 25 ~ "Normal weight",
    bmi >= 25 & bmi < 30 ~ "Overweight",
    bmi >= 30 ~ "Obese"
  ))

########################################################


#######THIS PIECE BELONGS TO 99_project_functions.r###############
#Make labels for axis, when presenting in PC directions
label_PCs <- function(pca_eigen_matrix, PC_number){
  PC1_var = round(pca_matrix_eigen$percent[PC_number]*100, 2)
  
  PC_label = str_c(cbind("Principal Component ", 
                         PC_number, 
                         " (", 
                         PC1_var, 
                         " %)"), 
                   collapse = "")
  return(PC_label)
}


##Find highest and lowest values in PC1 and PC2 of 
#rotation matrix for plot window design
rot_plot_axes <- function(rotation_matrix){
  x_max = max(rotation_matrix$PC1)+0.1
  x_min = min(rotation_matrix$PC1)-0.1
  y_max = max(rotation_matrix$PC2)+0.1
  y_min = min(rotation_matrix$PC2)-0.1
  axes_boundaries <- c(x_min, x_max, y_min, y_max)
  
  return(axes_boundaries)
}


select_data_subset <- function(data, exclude){
  pca_prep_data <- data %>% 
    select(where(is.numeric) &  #only keep numeric data
             !all_of(exclude))
  
  return(pca_prep_data)
}

########################################################
###Do PCA to predict/cluster diabetes status

#Clean dataset for pca; remove 
pca_prep_data = select_data_subset(
  data, 
  c("seqn", 
    "dx",
    "tx", 
    "Treatment_number"))

#Count number of variables that PCA is based on
vars = dim(pca_prep_data)[2]

#The below is created with inspiration from 
#https://clauswilke.com/blog/2020/09/07/pca-tidyverse-style/
#perform PCA on standardized variables
pca_ <- pca_prep_data %>% prcomp(scale = TRUE) # do PCA on scaled data

#Load in original dataset to principal components
data_merge_pca = pca_ %>% augment(data)


#Show distribution of explained variance by PCs using Skree plot
#Extract using tidy() from broom
pca_matrix_eigen = pca_ %>%
  tidy(matrix = "eigenvalues")

ggplot(data = pca_matrix_eigen,
       mapping = aes(PC, percent)) +
  geom_col(fill = "#3BB6FF", alpha = 0.8) +
  labs(x = "Principal component",
       y = "Variance explained [%]",
       title = "Variance explained within original dataset by each Principal Component") +
  scale_x_continuous(breaks = 1:vars) +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.01))
  )


#Extract variance explained by PC1 and PC2
PC1_label = label_PCs(pca_eigen_matrix = pca_eigen_matrix, PC_number = 1)
PC2_label = label_PCs(pca_eigen_matrix = pca_eigen_matrix, PC_number = 2)

# Plot visualization in the PC1,PC2-plane
# Color coordinates based on Treatment Status
pca_plot = ggplot(data = data_merge_pca,
       mapping = aes(x = .fittedPC1, 
                     y = .fittedPC2, 
                     color = `Treatment status`)) + 
  geom_point(size = 1.5, alpha = 0.8) +
  labs(x = PC1_label,
       y = PC2_label,
       title = "Visualization of data in Principal Component coordinates",
       subtitle = "Colored based on Treatment status"
       ) + 
  theme(legend.position = "right",
        text=element_text(size = 11,
                          family = "Avenir"))


#Plot rotation matrix

#Extract using tidy() from broom (could also be done by pca_$rotation)
rotation_matrix <- pca_ %>% 
  tidy(matrix = "rotation") %>% 
  pivot_wider(names_from = "PC", 
              names_prefix = "PC", 
              values_from = "value")

arrow_style <- arrow(angle = 20, 
                     ends = "first", 
                     type = "closed", 
                     length = grid::unit(8, "pt"))


##Find highest and lowest values in PC1 and PC2 of 
#rotation matrix for plot window design
rot_label_axes <- rot_plot_axes(rotation_matrix)


rot_plot = ggplot(data = rotation_matrix,
       mapping = aes(PC1, PC2)) +
  geom_segment(xend = 0, 
               yend = 0, 
               arrow = arrow_style) +
  geom_text(aes(label = column),
            hjust = 0, nudge_x = 0, 
            color = "#3F00FF") +
  labs(x = PC1_label,
       y = PC2_label,
       title = "Rotation Matrix") +
  xlim(rot_label_axes[1], rot_label_axes[2]) + 
  ylim(rot_label_axes[3], rot_label_axes[4]) +
  coord_fixed() + # fix aspect ratio to 1:1
  theme(text=element_text(size = 11,
                          family = "Avenir"))

#use patchwork library to display plots together
pca_plot + rot_plot




##Based on the results of the above analysis, we try to remove leg and arml
#Clean dataset for pca

##REMOVE WT, HT, SCr!!!
pca_prep_data = select_data_subset(
  data, 
  c("seqn", 
    "dx",
    "tx", 
    "Treatment_number",
    !"leg",
    !"arml"))

#Count number of variables that PCA is based on
vars = dim(pca_prep_data)[2]

#The below is created with inspiration from 
#https://clauswilke.com/blog/2020/09/07/pca-tidyverse-style/
#perform PCA on standardized variables
pca_ <- pca_prep_data %>% prcomp(scale = TRUE) # do PCA on scaled data

#Load in original dataset to principal components
data_merge_pca = pca_ %>% augment(data)


#Show distribution of explained variance by PCs using Skree plot
#Extract using tidy() from broom
pca_matrix_eigen = pca_ %>%
  tidy(matrix = "eigenvalues")

ggplot(data = pca_matrix_eigen,
       mapping = aes(PC, percent)) +
  geom_col(fill = "#3BB6FF", alpha = 0.8) +
  labs(x = "Principal component",
       y = "Variance explained [%]",
       title = "Variance explained within original dataset by each Principal Component") +
  scale_x_continuous(breaks = 1:vars) +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.01))
  )

#Extract variance explained by PC1 and PC2
PC1_label = label_PCs(pca_eigen_matrix = pca_eigen_matrix, PC_number = 1)
PC2_label = label_PCs(pca_eigen_matrix = pca_eigen_matrix, PC_number = 2)

# Plot visualization in the PC1,PC2-plane
# Color coordinates based on Treatment Status
pca_plot = ggplot(data = data_merge_pca,
       mapping = aes(x = .fittedPC1, 
                     y = .fittedPC2, 
                     color = `Treatment status`)) + 
  geom_point(size = 1.5, alpha = 0.8) +
  labs(x = PC1_label,
       y = PC2_label,
       title = "Visualization of data in Principal Component coordinates",
       subtitle = "Colored based on Treatment status"
  ) + 
  theme(legend.position = "right",
        text=element_text(size = 11,
                          family = "Avenir"))


#Plot rotation matrix

#Extract using tidy() from broom (could also be done by pca_$rotation)
rotation_matrix <- pca_ %>% 
  tidy(matrix = "rotation") %>% 
  pivot_wider(names_from = "PC", 
              names_prefix = "PC", 
              values_from = "value")

##Find highest and lowest values in PC1 and PC2 of 
#rotation matrix for plot window design
rot_label_axes <- rot_plot_axes(rotation_matrix)

arrow_style <- arrow(angle = 20, 
                     ends = "first", 
                     type = "closed", 
                     length = grid::unit(8, "pt"))

rot_plot = ggplot(data = rotation_matrix,
       mapping = aes(PC1, PC2)) +
  geom_segment(xend = 0, 
               yend = 0, 
               arrow = arrow_style) +
  geom_text(aes(label = column),
            hjust = 0, nudge_x = 0, 
            color = "#3F00FF") +
  labs(x = PC1_label,
       y = PC2_label,
       title = "Rotation Matrix") +
  xlim(rot_label_axes[1], rot_label_axes[2]) + 
  ylim(rot_label_axes[3], rot_label_axes[4]) +
  coord_fixed() + # fix aspect ratio to 1:1
  theme(text=element_text(size = 11,
                          family = "Avenir"))

pca_plot + rot_plot








###Do PCA to predict/cluster BMI category

#Clean dataset for pca
pca_prep_data = select_data_subset(
  data, 
  c("seqn", 
    "bmi", 
    "Treatment_number",
    "ht",
    "wt"))

#Count number of variables that PCA is based on
vars = dim(pca_prep_data)[2]

#The below is created with inspiration from 
#https://clauswilke.com/blog/2020/09/07/pca-tidyverse-style/
#perform PCA on standardized variables
pca_ <- pca_prep_data %>% prcomp(scale = TRUE) # do PCA on scaled data

#Load in original dataset to principal components
data_merge_pca = pca_ %>% augment(data)


#Show distribution of explained variance by PCs using Skree plot
#Extract using tidy() from broom
pca_matrix_eigen = pca_ %>%
  tidy(matrix = "eigenvalues")

ggplot(data = pca_matrix_eigen,
       mapping = aes(PC, percent)) +
  geom_col(fill = "#3BB6FF", alpha = 0.8) +
  labs(x = "Principal component",
       y = "Variance explained [%]",
       title = "Variance explained within original dataset by each Principal Component") +
  scale_x_continuous(breaks = 1:vars) +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.01))
  )

#Extract variance explained by PC1 and PC2
PC1_label = label_PCs(pca_eigen_matrix = pca_eigen_matrix, PC_number = 1)
PC2_label = label_PCs(pca_eigen_matrix = pca_eigen_matrix, PC_number = 2)

# Plot visualization in the PC1,PC2-plane
# Color coordinates based on Treatment Status
pca_plot = ggplot(data = data_merge_pca,
       mapping = aes(x = .fittedPC1, 
                     y = .fittedPC2, 
                     color = `BMI category`)) + 
  geom_point(size = 1.5, alpha = 0.8, shape = 17) +
  labs(x = PC1_label,
       y = PC2_label,
       title = "Visualization of data in Principal Component coordinates",
       subtitle = "Colored based on BMI category"
  ) + 
  theme(legend.position = "right",
        text=element_text(size = 11,
                          family = "Avenir")) 

#Plot rotation matrix

#Extract using tidy() from broom (could also be done by pca_$rotation)
rotation_matrix <- pca_ %>% 
  tidy(matrix = "rotation") %>% 
  pivot_wider(names_from = "PC", 
              names_prefix = "PC", 
              values_from = "value")

arrow_style <- arrow(angle = 20, 
                     ends = "first", 
                     type = "closed", 
                     length = grid::unit(8, "pt"))

##Find highest and lowest values in PC1 and PC2 of 
#rotation matrix for plot window design
rot_label_axes <- rot_plot_axes(rotation_matrix)


rot_plot = ggplot(data = rotation_matrix,
       mapping = aes(PC1, PC2)) +
  geom_segment(xend = 0, 
               yend = 0, 
               arrow = arrow_style) +
  geom_text(aes(label = column),
            hjust = 1, nudge_x = -0.02, 
            color = "#3F00FF") +
  labs(x = PC1_label,
       y = PC2_label,
       title = "Rotation Matrix") +
  xlim(rot_label_axes[1], rot_label_axes[2]) + 
  ylim(rot_label_axes[3], rot_label_axes[4]) +
  coord_fixed() + # fix aspect ratio to 1:1
  theme(text=element_text(size = 11,
                          family = "Avenir"))

pca_plot + rot_plot
