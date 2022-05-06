# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(patchwork)
library(scales)
library(ggridges)
library(corrplot)


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data with NAs imputed for outlier detection ------------------------

data <- read_tsv(file = "data/02_nhgh_no_NA.tsv")

# Normalize data to be able to show all variables in same plot
data <- data %>% 
  select(-(19:27),
         -dx,
         -tx) %>% 
  mutate_all(normalize)

# Boxplot showing outliers for all variables
data %>% 
  pivot_longer(cols = -seqn, 
               names_to = "ID", 
               values_to = "value") %>% 
  ggplot(mapping = aes(x = ID, 
                       y = value)) +
  geom_boxplot() +
  labs(x = "Variable",
       y = "Normalized value")
  


# Load data ---------------------------------------------------------------
data <- read_tsv(file = "data/03_nhgh_clean_aug.tsv")


##Linear correlations between each variable, presented
#using corrplot
corr_matrix <- data %>%
  select(-c(seqn, 
            gender, 
            re, 
            tx, 
            dx, 
            `BMI class`, 
            Treatment_number)) %>%
  select_if(is.numeric) %>% 
  cor(method = "pearson",
      use = "complete.obs")

<<<<<<< HEAD
corrplot(corr, type = "upper" , order = "hclust", 
         tl.col = "black", tl.srt = 45, tl.cex = 1) 
ggsave("./results/scree.png")
=======
corrplot(corr_matrix, 
         type = "upper", 
         order = "hclust", 
         tl.col = "black",
         tl.srt = 45,      
         tl.cex = 1)
>>>>>>> f2adc908edd96208ec32cd64781dc1d4bda27fdd


##Violin plot (and boxplots) of income, sorted on ethnicity 
data %>% 
  ggplot(mapping = aes(x = re, 
                       y = income)) + 
  geom_boxplot(outlier.color = "red", 
               outlier.shape = 1, 
               outlier.size = 2) + 
  geom_violin(alpha = 1/10, 
              color = "grey") +  
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_text(angle = 20, 
                                   size = 7)) + 
  labs(x = "", 
       y = "Annual income", 
       title = "Annual income by ethnicity", 
       caption = "Red dot = outlier", 
       size = 2)


#Distribution of individuals' BMI in the dataset 
bmi_cols <- data %>%  
  count(`BMI class`) %>% 
  drop_na(`BMI class`) %>% 
  mutate (Percentage = n / sum(n) * 100,
          `BMI class` = factor(`BMI class`, levels = 
                                 c("Underweight",
                                   "Normal weight",
                                   "Overweight",
                                   "Obese",
                                   "Severe obesity",
                                   "Morbid obesity",
                                   "Super obese"))) %>% 
  ggplot(mapping = aes(x = `BMI class`, 
                       y = Percentage,
                       fill = `BMI class`)) +
  geom_col() + 
  labs(x = "BMI class", 
       title = "Distribution of BMI classes") +
  theme(axis.text.x = element_text(angle = 15),
        legend.position = "none")


#Boxplots showing BMI class vs. age, stratified on diagnose status
bmi_boxplots <- data %>% 
  mutate(dx_class = case_when(dx == 0 ~ "Not diagnosed", 
                              dx == 1 ~ "Diagnosed")) %>% 
  mutate(`BMI class` = factor(`BMI class`, levels = 
                                c("Underweight",
                                  "Normal weight",
                                  "Overweight",
                                  "Obese",
                                  "Severe obesity",
                                  "Morbid obesity",
                                  "Super obese"))) %>% 
  ggplot(mapping = aes(x = `BMI class`, 
                       y = age, 
                       fill = dx_class)) + 
  geom_boxplot() + 
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 15)) +
  scale_fill_discrete(name = " ") + 
  labs( x = "BMI class", 
        y = "Age",
       title = "BMI class and age in relation to diagnosis")

#Use patchwork to combine the two plots
(bmi_cols / bmi_boxplots) + plot_annotation(
  caption = "BMI classes classified are as defined in the exercise of week 4.")



##Visualizations of body fat percentage dependent on ethnicity
data %>%  
  mutate(sex_class = case_when(gender == "female" ~ 0,
                               gender == "male" ~ 1),
         Gender = case_when(gender == "female" ~ "Female",
                            gender == "male" ~ "Male"),
         bfp = 1.39 * bmi + 0.16 * age - 10.34 * sex_class - 9) %>% 
  ggplot(mapping = aes(x = bfp,
                       y = stat(density),
                       color = Gender)) + 
  geom_freqpoly(binwidth = 2) + 
  facet_wrap(~ re) +
  theme(legend.position = "bottom") + 
  labs(x = " Body fat percentage", 
       y = "Density",
       title = "Gender-based distribution of body fat percentage",
       subtitle = "Divided based on ethnicity")

