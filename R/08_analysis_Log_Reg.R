# Load libraries ----------------------------------------------------------
library("tidyverse")
library("ggplot2")
library("dplyr")
library("purrr")
library("glmnet")
library("tidymodels")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
my_data_clean_aug <- read_tsv(file = "data/03_nhgh_clean_aug_2k.tsv")


# Code to convert my information
my_data_clean_aug <- my_data_clean_aug %>%
  rename_all(list(~make.names(.))) 
#mutate(age,wt,income,ht,bmi,leg,sub,gh,albumin,bun,SCr,normalize(my_data_clean_aug))#Creating a co-relation matrix
corr_matrix <- my_data_clean_aug %>% 
  select(-seqn,-Treatment.status,-BMI.category) %>% 
  
  correlate()

#Creating a normalized data set to see if our code runs better 


# Single Factor Logistic Regression


data2_logit <-  glm(dx ~ bmi,
                    data = my_data_clean_aug,
                    family = binomial(link = "logit"))
data3_logit <- glm (dx ~ income,
                    data = my_data_clean_aug,
                    family = binomial(link = "logit"))
data4_logit <- glm (dx ~ gh,
                    data = my_data_clean_aug,
                    family = binomial(link = "logit"))

summary(data2_logit)
summary(data3_logit)
summary(data4_logit)


# Multi-factor Model -------------------------------------------------------

data1 <- glm(dx ~ age +bmi + male+female,
             data = my_data_clean_aug,
             family = binomial(link = "logit"))
summary(data1)


data2 <- glm(dx ~ age + bmi +male+ female + Non.Hispanic.White +	Non.Hispanic.Black +	Mexican.American +	Other.Hispanic +	Other.Race.Including.Multi.Racial+albumin,
             data = my_data_clean_aug,
             family = binomial(link = "logit"))
data3 <- glm(dx ~ age +bmi +gh+albumin,
             data = my_data_clean_aug,
             family = binomial(link = "logit"))
#improved algorithm after checking for co-linearity and singular value estimation
data4 <- glm(dx ~ age +bmi +gh+albumin+SCr+bun,
             data = my_data_clean_aug,
             family = binomial(link = "logit"))

summary(data4)
summary(data2)
summary(data3)


### code for making a new variable that is prediction on whether a person is diagnosed with diabetes or not
### the fitted values from data_two are responsible for this 


my_data_clean_aug <- my_data_clean_aug %>%
  
  mutate(mult_model2_fitted = data2$fitted.values, model1_fited = data1$fitted.values,
         mult_model3_fitted= data3$fitted.values,single_model1_fitted = data2_logit$fitted.values,
         single_model2_fitted = data3_logit$fitted.values, single_model3_fitted = data4_logit$fitted.values,
         mult_model4_fitted = data4$fitted.values)


#data_3_sort <- my_data_clean_aug %>% 
# arrange(my_data_clean_aug, desc(mult_model3_fitted))

data_3srt <- my_data_clean_aug %>% 
  arrange(desc(mult_model3_fitted)) %>% 
  mutate(rank_of_mult3 = 1:nrow(my_data_clean_aug))

data4_srt <- my_data_clean_aug %>% 
  arrange(desc(mult_model4_fitted)) %>% 
  mutate(rank_of_mult4 = 1:nrow(my_data_clean_aug))

data4_srt <- my_data_clean_aug %>% 
  arrange(desc(mult_model4_fitted)) %>% 
  mutate(rank_of_mult4 = 1:nrow(my_data_clean_aug))

data_gh_single <- my_data_clean_aug %>% 
  arrange(desc(single_model3_fitted)) %>% 
  mutate(rank_of_single = 1:nrow(my_data_clean_aug))






#Casper's

data_3_test <- my_data_clean_aug %>% 
  select(-tx, -`Treatment.status`, -`BMI.category`, -tx, -seqn, -female, -Treatment_number, -ht, -wt, -armc, -waist) %>% 
  mutate_all(normalize) %>% 
  pivot_longer(cols = -dx,
               names_to = "id",
               values_to = "value") %>% 
  group_by(id) %>% 
  nest %>% 
  ungroup %>% 
  mutate(mdl = map(data, ~glm(dx ~ value,
                              data = .x,
                              family = binomial(link = "logit")))) %>% 
  mutate(mdl_tidy = map(mdl, ~tidy(.x, conf.int = TRUE))) %>% 
  unnest(mdl_tidy) %>% 
  filter(str_detect(term, "value")) %>% 
  mutate("adjusted_p" = p.adjust(p.value, method="fdr")) %>%               # adjusting p-values for multiple comparisons
  mutate(identified_as = case_when(adjusted_p < 0.05 ~ "Significant",
                                   TRUE ~ "Non-significant"),
         label_x = case_when(identified_as == "Significant" ~ id,
                             identified_as == "Non-significant" ~ "")) %>% 
  mutate(neg_log10_p = -log10(adjusted_p))

### Manhattan Plot  

data_3_test %>% 
  ggplot(mapping = aes(x = id, 
                       y = neg_log10_p,
                       color = identified_as,
                       label = label_x)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_hline(yintercept = -log10(0.05),
             linetype = 'dashed') +
  theme_classic(base_family = "Avenir",
                base_size = 8) + 
  theme(axis.text.x = element_blank(), 
        legend.position = "bottom") + 
  labs(x = "var",
       y = "Minus log10(p)")




### trying to plot probability of getting diagnosed with diabetes

ggplot(data=data_3srt , aes(x=rank_of_mult3, y=mult_model3_fitted)) +
  geom_point(aes(color=dx), alpha=1, shape=4,stroke=2)

ggplot(data=data4_srt , aes(x=rank_of_mult4, y=mult_model4_fitted)) +
  geom_point(aes(color=dx), alpha=1, shape=4,stroke=2)

ggplot(data=data_gh_single , aes(x=rank_of_single, y=single_model3_fitted)) +
  geom_point(aes(color=dx), alpha=1, shape=4,stroke=2)

data_3srt <- data_3srt %>% 
  mutate(predicted_mult3= case_when(mult_model3_fitted > 0.35 ~ 1,
                                    mult_model3_fitted <= 0.35 ~ 0))

data4_srt <- data4_srt %>% 
  mutate(predicted_mult4= case_when(mult_model4_fitted > 0.35 ~ 1,
                                    mult_model4_fitted <= 0.35 ~ 0))
data_gh_single <- data_gh_single %>% 
  mutate(predicted_single= case_when(single_model3_fitted > 0.35 ~ 1,
                                     single_model3_fitted <= 0.35 ~ 0))



xtabs(~ dx + predicted_mult3, data=data_3srt)
xtabs (~ dx + predicted_mult4, data=data4_srt)
xtabs (~ dx + predicted_single, data =data_gh_single)