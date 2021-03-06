# Load libraries ----------------------------------------------------------
library("tidyverse")
library("ggplot2")
library("patchwork")
library("scales")
library("ggridges")
library("corrplot")
library("broom")
library("stringr")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
data <- read_tsv(file = "data/03_nhgh_clean_aug.tsv")


## Extract total diagnosed indivduals
dx_total <- data %>% 
  filter(dx == 1) %>% 
  select(-seqn)


###Plot diagnosis-status for age groups###
## Premenopausal aged women and men with equivalent age
data %>% count(gender)

dx_below50 <- data %>% 
  filter(age <= 50,
         dx == 1)

diag_below50 <- dx_below50 %>% 
  ggplot(mapping = aes(x = gender)) + 
  geom_bar(aes(y = 100*(..count..)/data %>% 
                 filter(age <= 50) %>% 
                 nrow())) + 
  theme_classic() +  
  labs(x = "", 
       y = "Diagnosed individuals [%]", 
       subtitle = "Age group below 50")


dx_above50 <- data %>% 
  filter(age > 50,
         dx == 1)

diag_above50 <- dx_above50 %>% 
  ggplot(mapping = aes(x = gender)) + 
  geom_bar(aes(y = 100*(..count..)/data %>% 
                 filter(age >50) %>% 
                 nrow())) + 
  theme_classic() + 
  labs(x = "", 
       y = "", 
       subtitle = "Age group above 50")

(diag_below50 + diag_above50) + 
  plot_annotation(title = "Prevalence of diabetes mellitus",
                  subtitle = "Divided on age and gender")
ggsave("./results/Dx_age50.png")



##Boxplots showing relations between treatment, 
#ethnicity and income
dx_total %>% 
  mutate(tx_class = 
           case_when(tx == 0 ~ "Not treated", 
                     tx == 1 ~ "Treated")) %>% 
  ggplot(mapping = aes(x = re, 
                       y = income)) + 
  geom_boxplot() + 
  facet_wrap(~ tx_class) + 
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_text(angle = 25,
                                   size = 7)) + 
  labs(x = " ", 
       y = "Annual income [USD]", 
       title = "Annual income and ethnicity in relation to treatment status",
       subtitle = "In diagnosed individuals")
ggsave("./results/tx_Ethnicity_income.png")



###Plot distribution of diabetes mellitus patients 
#dependent on treatment status
ggplot(data = dx_total, 
       mapping = aes(x = `Treatment status`, 
                     fill = `Treatment status`)) + 
  geom_bar() +
  labs(title = "Distribution of diabetes mellitus patients",
       subtitle = "Based on treatment status",
       y = "Count of individuals") +
  theme(legend.position = "none")
ggsave("./results/tx.png")



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
  theme(legend.position = "bottom",
        axis.text.y = element_text(angle = 25,
                                   size = 9)) +
  labs(title = "Distribution of people diagnosed with Diabetes Mellitus",
       x = "Age",
       y = "Income group") 
ggsave("./results/Age_income_dx.png")



ggplot(data = dx_group_income, 
       mapping = aes(x = age,
                     y = re,
                     fill = `Treatment`)) +
  geom_density_ridges(alpha = 0.5, 
                      scale = 0.95) + 
  theme_minimal(base_family = "Avenir") +
  geom_vline(xintercept = 52, 
             linetype = "dotted") + 
  geom_vline(xintercept = 63, 
             linetype = "dotted") + 
  theme(legend.position = "bottom",
        axis.text.y = element_text(angle = 25,
                                   size = 9)) +
  labs(title = "Distribution of people diagnosed with Diabetes Mellitus",
       x = "Age",
       y = "Ethnicity") 
ggsave("./results/Age_ethnicity_tx.png")



data = data %>% 
  mutate(`Diagnose status` = case_when(
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
                     fill = `Diagnose status`)) +
  geom_bar(position="fill") + 
  theme(axis.text.x = element_text(angle = 15)) +
  labs(y = "Fraction",
       title = "Distribution of diagnose status",
       subtitle = "Based on BMI class")
ggsave("./results/BMI_dx.png")




##Gender distribution in relation to diagnosis
#Barplot of distribution of diagnosis in genders
ggplot(data = data,
       mapping = aes(x = gender,
                     fill = `Diagnose status`)) + 
  geom_bar(alpha = 0.8) +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  labs(title = "Distribution of people diagnosed with diabetes mellitus",
       subtitle = "Divided into gender",
       x = "Gender",
       y = "Number of individuals")
ggsave("./results/Gender_dx.png")

#Due to the relation between diagnosis, gender and albumin levels (see below),
#we test boxplots over BMI distributions for genders
data %>% 
  mutate(Gender = 
           case_when(gender == "female" ~ "Female",
                     gender == "male" ~ "Male")) %>% 
  ggplot(mapping = aes(x = Gender,
                       fill = `BMI class`)) +
  geom_bar(position = "dodge") +
  labs(x = "Gender",
       y = "Count of people", 
       title = "Distribution of BMI class", 
       subtitle = "Divided by gender")
ggsave("./results/BMI_gender.png")



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
ggsave("./results/Albumin_BMI.png")


#Density plot of albumin based on diagnosis
t_test <- t.test(data_albumin_vis %>%
                   filter(diagnosed == "Diagnosed") %>% 
                   select(albumin), 
                 data_albumin_vis %>% 
                   filter(diagnosed == "Not diagnosed") %>% 
                   select(albumin), 
                 alternative = "less") %>% 
  tidy()


ggplot(data = data_albumin_vis,
       mapping = aes(x = albumin,
                     color = diagnosed)) + 
  geom_density() + 
  labs(x = "Albumin (g/dL)",
       y = "Density",
       title = "Distribution of Albumin levels",
       subtitle = "Divided into diagnosed and not diagnosed with diabetes mellitus",
       caption = str_c("P-value: ", 
                       t_test %>% 
                         select(p.value))) +
  theme(legend.position = "bottom",
        legend.title = element_blank())
ggsave("./results/Albumin_dx.png")




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
ggsave("./results/Albumin_gender.png")
