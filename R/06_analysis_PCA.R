# Load libraries ----------------------------------------------------------
library("tidyverse")
library("broom")
library("stringr")
library("patchwork")


source(file = "R/99_project_functions.R")

# Load data
data <- read_tsv(file = "data/03_nhgh_clean_aug_1k.tsv")

# Count number of observations in each "Treatment status" category
#(used for visualizations)
data %>% count(`Treatment status`)

#For data visualization purposes, arrange data 
#(the group with the most observations should be the "lowest layer")
data <- data %>% 
  arrange(Treatment_number)


###Do PCA to predict/cluster diabetes status

#Clean dataset for pca; remove 
pca_prep_data <- select_data_subset(
  data, 
  c("seqn", 
    "dx",
    "tx", 
    "Treatment_number"))

#Count number of variables that PCA is based on
vars <- pca_prep_data %>% ncol

#perform PCA on standardized variables
pca_ <- pca_prep_data %>% 
  prcomp(scale = TRUE) # do PCA on scaled data

#Augment principal components to original data
data_merge_pca <- pca_ %>% 
  augment(data)


#Show distribution of explained variance by PCs using Skree plot
#Extract using tidy() from broom
pca_matrix_eigen <- pca_ %>%
  tidy(matrix = "eigenvalues")

ggplot(data = pca_matrix_eigen,
       mapping = aes(x = PC,
                     y = percent)) +
  geom_col(fill = "#3BB6FF", 
           alpha = 0.8) +
  labs(x = "Principal component",
       y = "Variance explained [%]",
       title = "Variance explained within original dataset by each Principal Component",
       caption = "Inspiration from: https://clauswilke.com/blog/2020/09/07/pca-tidyverse-style/") +
  scale_x_continuous(breaks = 1:vars) +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.01)))

ggsave("./results/scree_dx_full.png")

#Extract variance explained by PC1 and PC2
PC1_label <- label_PCs(pca_eigen_matrix = pca_eigen_matrix, 
                       PC_number = 1)
PC2_label <- label_PCs(pca_eigen_matrix = pca_eigen_matrix, 
                       PC_number = 2)

# Plot visualization in the PC1,PC2-plane
# Color coordinates based on Treatment Status
pca_plot <- ggplot(data = data_merge_pca,
                   mapping = aes(x = .fittedPC1, 
                                 y = .fittedPC2, 
                                 color = `Treatment status`)) + 
  geom_point(size = 1.5, 
             alpha = 0.8) +
  labs(x = PC1_label,
       y = PC2_label,
       title = "Visualization of data in Principal Component coordinates",
       subtitle = "Colored based on diagnosis and treatment status",
       color = ""
  ) + 
  theme(legend.position = "bottom",
        text = element_text(size = 11,
                            family = "Avenir")) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))


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


rot_plot <- ggplot(data = rotation_matrix,
                   mapping = aes(x = PC1, 
                                 y = PC2)) +
  geom_segment(xend = 0, 
               yend = 0, 
               arrow = arrow_style) +
  geom_text(aes(label = column),
            hjust = 0, 
            nudge_x = 0, 
            color = "#3F00FF") +
  labs(x = PC1_label,
       y = PC2_label,
       title = "Rotation Matrix") +
  xlim(rot_label_axes[1], 
       rot_label_axes[2]) + 
  ylim(rot_label_axes[3], 
       rot_label_axes[4]) +
  coord_fixed() + # fix aspect ratio to 1:1
  theme(text = element_text(size = 11,
                            family = "Avenir"))

#use patchwork library to display plots together
(pca_plot + rot_plot) + 
  plot_annotation(caption = "Inspiration from: https://clauswilke.com/blog/2020/09/07/pca-tidyverse-style/")

ggsave("./results/PCA_dx_full.png")


##We try to remove arml, leg, wt and ht (wt and ht are directly correlated with bmi)
#Clean dataset for pca
pca_prep_data <- select_data_subset(
  data, 
  c("seqn", 
    "dx",
    "tx", 
    "Treatment_number",
    "leg",
    "arml",
    "wt",
    "ht"))

#Count number of variables that PCA is based on
vars <- pca_prep_data %>% ncol

#perform PCA on standardized variables
pca_ <- pca_prep_data %>% 
  prcomp(scale = TRUE) # do PCA on scaled data

#Load in original dataset to principal components
data_merge_pca <- pca_ %>% 
  augment(data)


#Show distribution of explained variance by PCs using Skree plot
#Extract using tidy() from broom
pca_matrix_eigen <- pca_ %>%
  tidy(matrix = "eigenvalues")

ggplot(data = pca_matrix_eigen,
       mapping = aes(x = PC, 
                     y = percent)) +
  geom_col(fill = "#3BB6FF", 
           alpha = 0.8) +
  labs(x = "Principal component",
       y = "Variance explained [%]",
       title = "Variance explained within original dataset by each Principal Component",
       caption = "Inspiration from: https://clauswilke.com/blog/2020/09/07/pca-tidyverse-style/") +
  scale_x_continuous(breaks = 1:vars) +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.01))
  )

ggsave("./results/scree_dx_filtered.png")

#Extract variance explained by PC1 and PC2
PC1_label <- label_PCs(pca_eigen_matrix = pca_eigen_matrix, 
                       PC_number = 1)
PC2_label <- label_PCs(pca_eigen_matrix = pca_eigen_matrix, 
                       PC_number = 2)

# Plot visualization in the PC1,PC2-plane
# Color coordinates based on Treatment Status
pca_plot <- ggplot(data = data_merge_pca,
                   mapping = aes(x = .fittedPC1, 
                                 y = .fittedPC2, 
                                 color = `Treatment status`)) + 
  geom_point(size = 1.5, 
             alpha = 0.8) +
  labs(x = PC1_label,
       y = PC2_label,
       title = "Visualization of data in Principal Component coordinates",
       subtitle = "Colored based on diagnosis and treatment status",
       color = ""
  ) + 
  theme(legend.position = "bottom",
        text = element_text(size = 11,
                            family = "Avenir")) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))


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

rot_plot <- ggplot(data = rotation_matrix,
                   mapping = aes(x = PC1, 
                                 y = PC2)) +
  geom_segment(xend = 0, 
               yend = 0, 
               arrow = arrow_style) +
  geom_text(aes(label = column),
            hjust = 0, 
            nudge_x = 0, 
            color = "#3F00FF") +
  labs(x = PC1_label,
       y = PC2_label,
       title = "Rotation Matrix") +
  xlim(rot_label_axes[1], 
       rot_label_axes[2]) + 
  ylim(rot_label_axes[3], 
       rot_label_axes[4]) +
  coord_fixed() + # fix aspect ratio to 1:1
  theme(text=element_text(size = 11,
                          family = "Avenir"))

(pca_plot + rot_plot) + plot_annotation(
  caption = "Inspiration from: https://clauswilke.com/blog/2020/09/07/pca-tidyverse-style/")

ggsave("./results/PCA_dx_filtered.png")



###Do PCA to predict/cluster BMI category

#Clean dataset for pca
pca_prep_data <- select_data_subset(
  data, 
  c("seqn", 
    "bmi", 
    "Treatment_number",
    "ht",
    "wt"))

#Count number of variables that PCA is based on
vars <- pca_prep_data %>% ncol

#perform PCA on standardized variables
pca_ <- pca_prep_data %>% 
  prcomp(scale = TRUE) # do PCA on scaled data

#Load in original dataset to principal components
data_merge_pca <- pca_ %>% 
  augment(data)


#Show distribution of explained variance by PCs using Skree plot
#Extract using tidy() from broom
pca_matrix_eigen <- pca_ %>%
  tidy(matrix = "eigenvalues")

ggplot(data = pca_matrix_eigen,
       mapping = aes(x = PC, 
                     y = percent)) +
  geom_col(fill = "#3BB6FF", 
           alpha = 0.8) +
  labs(x = "Principal component",
       y = "Variance explained [%]",
       title = "Variance explained within original dataset by each Principal Component",
       caption = "Inspiration from: https://clauswilke.com/blog/2020/09/07/pca-tidyverse-style/") +
  scale_x_continuous(breaks = 1:vars) +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.01))
  )

ggsave("./results/scree_bmi.png")

#Extract variance explained by PC1 and PC2
PC1_label <- label_PCs(pca_eigen_matrix = pca_eigen_matrix, 
                       PC_number = 1)
PC2_label <- label_PCs(pca_eigen_matrix = pca_eigen_matrix, 
                       PC_number = 2)

# Plot visualization in the PC1,PC2-plane
# Color coordinates based on Treatment Status
pca_plot <- ggplot(data = data_merge_pca,
                   mapping = aes(x = .fittedPC1, 
                                 y = .fittedPC2, 
                                 color = `BMI class`)) + 
  geom_point(size = 1.5, 
             alpha = 0.8, 
             shape = 17) +
  labs(x = PC1_label,
       y = PC2_label,
       title = "Visualization of data in Principal Component coordinates",
       subtitle = "Colored based on BMI class",
       color = "") + 
  theme(legend.position = "bottom",
        text = element_text(size = 11,
                            family = "Avenir")) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

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


rot_plot <- ggplot(data = rotation_matrix,
                   mapping = aes(x = PC1, 
                                 y = PC2)) +
  geom_segment(xend = 0, 
               yend = 0, 
               arrow = arrow_style) +
  geom_text(aes(label = column),
            hjust = 1, 
            nudge_x = -0.02, 
            color = "#3F00FF") +
  labs(x = PC1_label,
       y = PC2_label,
       title = "Rotation Matrix") +
  xlim(rot_label_axes[1],
       rot_label_axes[2]) + 
  ylim(rot_label_axes[3], 
       rot_label_axes[4]) +
  coord_fixed() + # fix aspect ratio to 1:1
  theme(text = element_text(size = 11,
                            family = "Avenir"))

(pca_plot + rot_plot) + plot_annotation(
  caption = "Inspiration from: https://clauswilke.com/blog/2020/09/07/pca-tidyverse-style/")

ggsave("./results/PCA_bmi.png")

