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


## Total diagnosed indviduals
dx_total <- data %>% 
  filter(dx == 1) %>% 
  select(-seqn)


###Plot diagnosis-status for age groups###
## Premenopausal aged women and men with equivalent age
count(data,sex)

dx_below50 <- data %>% 
  filter(age <= 50) %>% 
  filter(dx == 1)

p1 <- dx_below50 %>% 
  ggplot(aes(x = gender)) + 
  geom_bar(aes(y = 100*(..count..)/data %>% filter(age <= 50) %>% nrow())) + 
  #scale_y_continuous(breaks = seq(0,40,10)) + 
  theme_classic() +  
  labs(x = "", 
       y = "Percentage of total diagnosed individuals", 
       subtitle = "Age group below 50", 
       caption = "The average age for menopause in USA is 50")


dx_above50 <- data %>% 
  filter(age > 50) %>% 
  filter(dx == 1)

p2 <- dx_above50 %>% 
  ggplot(aes(x = gender)) + 
  geom_bar(aes(y = 100*(..count..)/data %>% filter(age >50) %>% nrow())) + 
  #scale_y_continuous(breaks = seq(0,40,10)) + 
  theme_classic() + 
  labs(x = "", 
       y = " ", 
       subtitle = "Age group above 50")

(p1 + p2) + 
  plot_annotation(title = "Pre- and postmenopausal prevalence of diabetes mellitus")




##Boxplots showing relations between treatment, 
#ethnicity and income
dx_total %>% 
  mutate(tx_class = case_when(tx == 0 ~ "Not treated", 
                              tx == 1 ~ "Treated")) %>% 
  ggplot(aes(x = re, 
             y = income)) + 
  geom_boxplot() + 
  facet_wrap(~ tx_class) + 
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_text(angle = 25,size = 7)) + 
  labs(x = " ", 
       y = "Annual income [USD]", 
       title = "Annual income and ethnicity in relation to treatment status")



###Plot distribution of diabetes mellitus patients 
#dependent on treatment status
ggplot(data = dx_total, 
       mapping = aes(x = `Treatment status`, 
                     fill = `Treatment status`)) + 
  geom_bar() +
  labs(title = "Distribution of diabetes mellitus patients",
       subtitle = "Based on treatment or no treatment",
       y = "Count of patients") +
  theme(legend.position = "none")



#Group income levels for visualisations
dx_group_income <- data %>% 
  filter(dx == 1) %>% 
  mutate(`Treatment` = case_when(
    tx == 0 ~ "Not Treated",
    tx == 1 ~ "Treated"),
    Income_level = case_when(
      income <= 10000 ~ "Income: <10.000",
      income > 10000 & income <= 25000 ~ "Income: ]10.000;25.000]",
      income > 25000 & income <= 50000 ~ "Income: ]25.000;50.000]",
      income > 50000 & income <= 75000 ~ "Income: ]50.000;75.000]",
      income > 75000 ~ "Income: >75.000"),
    Income_level = factor(Income_level,
                          levels = c("Income: <10.000",
                                     "Income: ]10.000;25.000]",
                                     "Income: ]25.000;50.000]",
                                     "Income: ]50.000;75.000]",
                                     "Income: >75.000")))


ggplot(data = dx_group_income, 
       mapping = aes(x = age,
                     y = Income_level,
                     fill = `Treatment`)) +
  geom_density_ridges(alpha = 0.5, scale = 0.95) + 
  theme_minimal(base_family = "Avenir") +
  geom_vline(xintercept = 52, linetype = "dotted") + 
  geom_vline(xintercept = 63, linetype = "dotted") + 
  theme(plot.title = element_text(hjust = 1, vjust=0),
        legend.position = "bottom") +
  labs(title = "Distribution of people diagnosed with Diabetes Mellitus",
       x = "Age",
       y = "Income group") 



ggplot(data = dx_group_income, 
       mapping = aes(x = age,
                     y = re,
                     fill = `Treatment`)) +
  geom_density_ridges(alpha = 0.5, scale = 0.95) + 
  theme_minimal(base_family = "Avenir") +
  geom_vline(xintercept = 52, linetype = "dotted") + 
  geom_vline(xintercept = 63, linetype = "dotted") + 
  theme(plot.title = element_text(hjust = 1, vjust=0),
        legend.position = "bottom") +
  labs(title = "Distribution of people diagnosed with Diabetes Mellitus",
       x = "Age",
       y = "Ethnicity") 



data = data %>% 
  mutate(diagnosed = case_when(
    dx == 0 ~ "Not diagnosed",
    dx == 1 ~ "Diagnosed"),
    setup_diagnose = 1,
    `BMI class` = factor(`BMI class`, levels = 
                                  c("Underweight",
                                    "Normal weight",
                                    "Overweight",
                                    "Obese",
                                    "Severe obesity",
                                    "Morbid obesity",
                                    "Super obese")))

ggplot(data = data,
       mapping = aes(x = `BMI class`,
                     fill = diagnosed)) +
  geom_bar(position="fill") + 
  theme(axis.text.x = element_text(angle = 15))
  



##Gender distribution in relation to diagnosis
#Barplot of distribution of diagnosis in genders
ggplot(data = data,
       mapping = aes(x = gender,
                     fill = diagnosed)) + 
  geom_bar(alpha = 0.8) +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  labs(title = "Distribution of people diagnosed with diabetes mellitus",
       subtitle = "Divided into gender",
       x = "Gender",
       y = "Number of individuals")

#Due to the relation between diagnosis, gender and albumin levels (see below),
#we test boxplots over BMI distributions for genders
ggplot(data = data,
       mapping = aes(x = gender,
                     fill = `BMI class`)) +
  geom_bar(position = "dodge") +
  labs(x = "Gender",
       y = "Count of people")



###Analyses involving Albumin levels

data_albumin_vis <- data %>% 
  mutate(`BMI class` = factor(`BMI class`, levels = 
                                c("Underweight",
                                  "Normal weight",
                                  "Overweight",
                                  "Obese",
                                  "Severe obesity",
                                  "Morbid obesity",
                                  "Super obese")),
         diagnosed = case_when(dx == 0 ~ "Not diagnosed",
                               dx == 1 ~ "Diagnosed"))

ggplot(data = data_albumin_vis,
       mapping = aes(x = albumin, 
                     y = `BMI class`,
                     fill = `BMI class`)) +
  geom_boxplot() +
  labs(title = "Distributions of Albumin levels based on BMI class",
       x = "Albumin (g/dL)")


#Density plot of albumin based on diagnosis
ggplot(data = data_albumin_vis,
       mapping = aes(x = albumin,
                     color = diagnosed)) + 
  geom_density() + 
  labs(x = "Albumin (g/dL)",
       y = "Density",
       title = "Distribution of Albumin levels",
       subtitle = "Divided into diagnosed and not diagnosed with diabetes mellitus") +
  theme(legend.position = "bottom",
        legend.title = element_blank())


#Density plot of albumin based on gender
ggplot(data = data_albumin_vis,
       mapping = aes(x = albumin,
                     color = gender)) + 
  geom_density() + 
  labs(x = "Albumin (g/dL)",
       y = "Density",
       title = "Distribution of Albumin levels",
       subtitle = "Divided by gender") +
  theme(legend.position = "bottom",
        legend.title = element_blank())

