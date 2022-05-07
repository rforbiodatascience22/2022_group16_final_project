# Load libraries ----------------------------------------------------------
# Load libraries ----------------------------------------------------------
library("tidyverse")
library("ggplot2")
library("glmnet")
library("corrr")
library("broom")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
my_data_clean_aug <- read_tsv(file = "data/03_nhgh_clean_aug_2k.tsv")


# Code to convert remove the spaces in the id names and perform normalization

my_data_clean_aug <- my_data_clean_aug %>%
  rename_all(list(~make.names(.))) %>% 
  select_if(is.numeric)




#Creating a co-relation matrix We use this to try and identify which variables 
# to avoid due to co-linearity

corr_matrix <- my_data_clean_aug %>% 
  select(-seqn) %>% 
  correlate()

# Multi-factor Model -------------------------------------------------------
## DATA 1 factors are randomly chosen

data1 <- glm(dx ~ age +
               bmi +
               male + 
               female,
             data = my_data_clean_aug,
             family = binomial(link = "logit"))
summary(data1)

### Data mlm with race 
multi_with_race <- glm(dx ~ age +
                         bmi +
                         male + 
                         female +
                         Non.Hispanic.White +
                         Non.Hispanic.Black +
                         Mexican.American + 
                         Other.Hispanic +
                         Other.Race.Including.Multi.Racial +
                         albumin,
                       data = my_data_clean_aug,
                       family = binomial(link = "logit")) 


#Code to check for data confidence and single parameter impact estimation
# the following code, serves as a method to evaluate a single logistic regression on all variables
# the result of which we will use to identify which variables have an impact on
# the diabetes value and whether that impact evaluation is statistically 
#significant or not


data_visual <- my_data_clean_aug %>% 
  select(-tx,
         -seqn,
         -female, 
         -Treatment_number, 
         -ht,
         -wt, 
         -armc,
         -waist) %>% 
  mutate_all(normalize) %>% 
  pivot_longer(cols = -dx,
               names_to = "id",
               values_to = "value") %>% 
  group_by(id) %>% 
  nest %>% 
  ungroup %>% 
  mutate(mdl = map(data,
                   ~glm(dx ~ value,
                        data = .x,
                        family = binomial(link = "logit")))) %>% 
  mutate(mdl_tidy = map(mdl,
                        ~ tidy(.x,
                               conf.int = TRUE))) %>% 
  unnest(mdl_tidy) %>% 
  filter(str_detect(term, 
                    "value")) %>% 
  mutate("adjusted_p" = p.adjust(p.value,
                                 method="fdr")) %>% # adjusting p-values for multiple comparisons
  mutate(identified_as = case_when(adjusted_p < 0.05 ~ "Significant",
                                   TRUE ~ "Non-significant"),
         label_x = case_when(identified_as == "Significant" ~ id,
                             identified_as == "Non-significant" ~ "")) %>% 
  mutate(neg_log10_p = -log10(adjusted_p))


###  Variable impact estimation with confidence interval 

data_visual %>% 
  filter(id != "single_model1_fitted",
         id != "mult_race_fitted", 
         id != "model1_fited", 
         id != "mult_model4_fitted", 
         id != "mlm_v1",
         id != "single_gh_fitted") %>% 
  ggplot(aes(x = estimate,
             y = fct_reorder(id,
                             desc(estimate)),
             colour = identified_as,
             label = label_x)) +
  geom_vline(xintercept = 0,
             linetype = "dashed") +
  geom_point(alpha = 0.5) +
  geom_errorbarh(aes(xmin = conf.low,
                     xmax = conf.high,
                     height = 0.2)) +
  geom_text(aes(x = conf.high),
            size = 2,
            colour = "black",
            nudge_x = 0.5) +
  theme_classic(base_family = "Avenir",
                base_size = 8) +
  theme(axis.text.y = element_blank(),
        legend.position = "bottom") +
  labs(color = "Identified As", 
       y = "Parameter type ",
       x = "Estimated Impact on Diagnosis") 

### Manhattan Plot for identifying variables that have significant P Values   

data_visual %>%
  filter(id != "single_model1_fitted",
         id != "mult_race_fitted",
         id != "model1_fited",
         id != "mult_model4_fitted",
         id != "mlm_v1",
         id != "single_gh_fitted") %>% 
  ggplot(mapping = aes(x = id, 
                       y = neg_log10_p,
                       color = identified_as,
                       label = label_x)) +
  geom_point(alpha = 0.5, 
             size = 2) +
  geom_hline(yintercept = -log10(0.05), 
             linetype = 'dashed') +
  geom_text(aes(x = id),
            size = 2,
            colour = "black",
            nudge_x = 0.5)+
  theme_classic(base_family = "Avenir",
                base_size = 8) + 
  theme(axis.text.x = element_blank(), 
        legend.position = "bottom") + 
  labs(caption = "------ Negative log value is 0.05" , colour = "Identified As",
       x = "The Different Parameters of the Dataset",
       y = "Minus log10(p)")

# Single Factor Logistic Regression


single_bmi <-  glm(dx ~ bmi,
                   data = my_data_clean_aug,
                   family = binomial(link = "logit"))

single_gh <- glm (dx ~ gh,
                  data = my_data_clean_aug,
                  family = binomial(link = "logit"))

summary(single_bmi)

summary(single_gh)




mult_improved1 <- glm(dx ~ age +bmi +gh+albumin,
                      data = my_data_clean_aug,
                      family = binomial(link = "logit"))

#improved algorithm after checking for co-linearity and singular value estimation
mlm_after_colinearity_check <- glm(dx ~ age +bmi +gh+albumin+SCr+bun,
                                   data = my_data_clean_aug,
                                   family = binomial(link = "logit"))

summary(mlm_after_colinearity_check)
summary(multi_with_race)
summary(mult_improved1)


### code for making a new variable that is a prediction on whether a person is 
### diagnosed with diabetes or not


#Adding the fitted values from our different values to the main dataset
my_data_clean_aug <- my_data_clean_aug %>%
  
  mutate(mult_race_fitted = multi_with_race$fitted.values, model1_fited = data1$fitted.values,
         mlm_v1= mult_improved1$fitted.values,single_model1_fitted =single_bmi$fitted.values,
         single_gh_fitted = single_gh$fitted.values,
         mult_model4_fitted = mlm_after_colinearity_check$fitted.values)


### Sorting the dataset by 

mlm_prediction1_vs_dx <- my_data_clean_aug %>% 
  arrange(desc(mlm_v1)) %>% 
  mutate(rank_of_mlm_v1 = 1:nrow(my_data_clean_aug))

mlm_after_colinearity_check_srt <- my_data_clean_aug %>% 
  arrange(desc(mult_model4_fitted)) %>% 
  mutate(rank_of_mult4 = 1:nrow(my_data_clean_aug))


data_gh_single <- my_data_clean_aug %>% 
  arrange(desc(single_gh_fitted)) %>% 
  mutate(rank_of_single = 1:nrow(my_data_clean_aug))

data_bmi_single <- my_data_clean_aug %>%
  arrange(desc(single_model1_fitted)) %>% 
  mutate(rank_of_bmi_pred = 1:nrow(my_data_clean_aug))


#### Converting fitted values into a prediction value, and comparing the data
### of predicted vs actual presence of diabetes

data_bmi_single <- data_bmi_single %>% 
  mutate(predicted_single_bmi = case_when(single_model1_fitted > 0.35 ~1,
                                          single_model1_fitted <= 0.35 ~ 0))

mlm_prediction1_vs_dx <- mlm_prediction1_vs_dx %>% 
  mutate(predicted_mult3= case_when(mlm_v1 > 0.35 ~ 1,
                                    mlm_v1 <= 0.35 ~ 0))

mlm_after_colinearity_check_srt <- mlm_after_colinearity_check_srt %>% 
  mutate(predicted_mult4= case_when(mult_model4_fitted > 0.35 ~ 1,
                                    mult_model4_fitted <= 0.35 ~ 0))
data_gh_single <- data_gh_single %>% 
  mutate(predicted_single= case_when(single_gh_fitted > 0.35 ~ 1,
                                     single_gh_fitted <= 0.35 ~ 0))




### trying to plot probability of getting diagnosed with diabetes v/s 
## actually having diabetes

ggplot(data=mlm_prediction1_vs_dx , mapping =  aes(x=rank_of_mlm_v1, y=mlm_v1)) +
  geom_point( mapping = aes(color=as.factor(dx)), alpha=1, shape=4,stroke=2)+
  labs(y = " Prediction based on only colinearity check ")

ggplot(data=mlm_after_colinearity_check_srt , mapping =  aes(x=rank_of_mult4, y=mult_model4_fitted)) +
  geom_point( mapping = aes(color=as.factor(dx)), alpha=1, shape=4,stroke=2) +
  labs( y = "Prediction based on model after Colinearity check and p-values")

ggplot(data=data_gh_single ,  mapping = aes(x=rank_of_single, y=single_gh_fitted)) +
  geom_point( mapping = aes(color=as.factor(dx)), alpha=1, shape=4,stroke=2)+
  labs( y = " Prediction based only GH value")

ggplot(data=data_bmi_single ,  mapping = aes(x=rank_of_bmi_pred, y=single_model1_fitted)) +
  geom_point( aes(color=as.factor(dx)), alpha=1, shape=4,stroke=2)+
  labs( y = "Prediction based on BMI Value")





xtabs(~ dx + predicted_mult3, data=mlm_prediction1_vs_dx)
xtabs (~ dx + predicted_mult4, data=mlm_after_colinearity_check_srt)
xtabs (~ dx + predicted_single, data =data_gh_single)
xtabs(~ dx + predicted_single_bmi, data = data_bmi_single )
