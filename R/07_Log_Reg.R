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

#Creating a co-relation matrix
corr_matrix <- my_data_clean_aug %>% 
  select(-seqn,-Treatment.status,-BMI.category) %>% 
  
  correlate()

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


#Not sure why this summary function isn't working  
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
data3 <- glm(dx ~ age +bmi +gh+income+albumin,
             data = my_data_clean_aug,
             family = binomial(link = "logit"))

summary(data2)
summary(data3)


### code for making a new variable that is prediction on whether a person is diagnosed with diabetes or not
### the fitted values from data_two are responsible for this 
predicted_values = data2$fitted.values %>% 
  as_tibble()
sequence_id = my_data_clean_aug$seqn %>% 
  as_tibble()
predicted_values %>% 
  mutate(seqn=my_data_clean_aug %>% select(seqn))


view(my_data_clean_aug %>% 
  mutate(fitted = data2$fitted.values))

my_data_clean_aug <- left_join(my_data_clean_aug,predicted_values  %>%
           select(value),
          by = "seqn")

view(data2 %>% 
  augment(my_data_clean_aug))




### ordering the new variables by prediction probability 

predicted_data <- predicted.data[
  order(predicted.data1$probability_of_dx, decreasing=False),] %>%
  predicted.data1$rank <- 1:nrow(predicted.data)

### trying to plot probability of getting diagnosed with diabetes

ggplot(data=predicted_data , aes(x=rank, y=probability_of_dx))%>%
  geom_point(aes(color=hd), alpha=1, shape=4)



