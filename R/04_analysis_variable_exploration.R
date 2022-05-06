# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(patchwork)
library(scales)
library(ggridges)
library(psych)
library(corrplot)


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
data <- read_tsv(file = "data/03_nhgh_clean_aug.tsv")


##Linear correlations between each variable, presented
#using corrplot
corr <- data %>% 
  select(-c(seqn, gender, re, tx, dx, `BMI class`, Treatment_number)) %>%
  select_if(is.numeric) %>% 
  cor( method = "pearson", 
       use = "complete.obs" )

corrplot(corr, type = "upper" , order = "hclust", 
         tl.col = "black", tl.srt = 45, tl.cex = 1) 
ggsave("./results/scree.png")


##Violin plot (and boxplots) of income, filtered on ethnicity 
data %>% 
  ggplot(aes(x = re, 
             y = income)) + 
  geom_boxplot(outlier.color = "red", 
               outlier.shape = 1, 
               outlier.size = 2) + 
  geom_violin(alpha = 1/10, 
              color = "grey") +  
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_text(angle = 20, size = 7)) + 
  labs(x = " ", 
       y = "Annual income", 
       title = "Annual income by ethnicity", 
       caption = "red dot = outlier", size = 2)



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
             y = Percentage)) +
  geom_col() + 
  labs(x = "BMI class", 
       title = "Distribution of BMI classification") +
  theme(axis.text.x = element_text(angle = 15))


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
  ggplot(aes(x = `BMI class`, 
             y = age, 
             fill = dx_class)) + 
  geom_boxplot() + 
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 15)) +
  scale_fill_discrete(name = " ") + 
  labs( x = "BMI class", 
        y = "Age",
       title = "BMI class and age in relation to diagnosis")

bmi_cols / bmi_boxplots



##Visualizations of body fat percentage dependent on ethnicity
data %>%  
  mutate(sex_class = case_when(gender == "female" ~ 0,
                               gender == "male" ~ 1),
         bfp = 1.39 * bmi + 0.16 * age - 10.34 * sex_class - 9) %>% 
  ggplot(aes(x = bfp,
             y = stat(density),
             color = gender)) + 
  geom_freqpoly(binwidth = 2) + 
  facet_wrap(~ re) +
  theme(legend.position = "bottom") + 
  labs(x = " Body fat percentage", 
       title = "Sex-based distribution of body fat percentage")
