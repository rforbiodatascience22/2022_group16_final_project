library(tidyverse)
library(ggplot2)
library(gridExtra)
library(patchwork)
library(scales)
library(ggridges)

data <- read_tsv(file = "data/02_nhgh_clean.tsv")

`03_nhgh_clean_aug` %>% 
ggplot(aes(x = income)) + 
  geom_bar(aes(fill = re),
           position = "dodge") +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        axis.text.x = element_text(angle = 45,size = 7)) +
  labs(
    x = "Annual income [USD]",
    y = "Number of individuals",
    title = "Annual income by ethnicity"
  ) +
  coord_fixed(ratio = 100) + 
  scale_y_continuous(breaks = seq(0, 550, 100)) + 
  scale_x_continuous(breaks = seq(0, 100000, 5000))



data %>% 
  ggplot(aes(y = income)) + 
  geom_histogram(aes(fill = re),
                 position = "dodge") + 
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        axis.text.x = element_text(angle = 25,size = 7)) + 
  scale_y_log10() + 
  scale_x_continuous(breaks = seq(0, 100000, 20000)) + 
  labs(
    y = "Annual income [USD]",
    title = "Annual income by ethnicity"
  )


# Diagnosis and treatment:

Dx <- data %>% 
  ggplot(aes(x = dx)) + 
  geom_bar() + 
  labs(x = "Diagnosis", 
       y = "Number of individuals") + 
  annotate("text", x = 0, y = 5300, label = "Not diagnosed") +
  annotate("text", x = 1, y = 900, label = "Diagnosed") + 
  scale_x_continuous(breaks = seq(0, 1)) + 
  scale_y_continuous(breaks = seq(0, 5500, 1000))
    
Tx <- data %>% 
  ggplot(aes(x = tx)) + 
  geom_bar() + 
  labs(x = "Treatment", 
       y = "Number of individuals") + 
  annotate("text", x = 0, y = 5500, label = "Not treated") +
  annotate("text", x = 1, y = 700, label = "Treated") + 
  scale_x_continuous(breaks = seq(0, 1)) + 
  scale_y_continuous(breaks = seq(0, 5500, 1000))
  
grid.arrange(Dx,Tx)


count(data,sex)
## total: 5596
## female: 2735
## male: 2861

################################################################

# Histogram of numeric variables: 

## Weight
#### The two dots with count are a visual indicator highlighting that variable is not present in the original data, but has been computed by the statistic
data %>% 
       ggplot(aes(x = wt)) + 
  geom_histogram(aes(y = 100*(..count..)/sum(..count..))) +
  facet_wrap(sex ~ .) + 
  labs(x = "Weight [kg]", 
       y = "Percentage",
       title = "Weight")

## Height 
data %>% 
  ggplot(aes(x = ht)) + 
  geom_histogram(aes(y = 100*(..count..)/sum(..count..))) + 
  facet_wrap(sex ~ .) + 
  labs(x = "Height [cm]", 
       y = "Percentage",
       title = "Height") 

## BMI
data %>% 
  ggplot(aes(x = bmi)) + 
  geom_histogram(aes(y = 100*(..count..)/sum(..count..))) + 
  facet_wrap(sex ~ .) +
  labs(x = "BMI [kg/m^2]",
       y = "Percentage",
       title = "BMI")


## Glycohemoglobin
data %>% 
  ggplot(aes(x = gh)) + 
  geom_histogram(aes(y = 100*(..count..)/sum(..count..))) + 
  facet_wrap(sex ~ .) + 
  labs(x = "Glycohemoglobin [%]",
       y = "Percentage",
       title = "Glycohemoglobin")


## Albumin 
data %>% 
  ggplot(aes(x = albumin)) + 
  geom_histogram(aes(y = 100*(..count..)/sum(..count..))) + 
  facet_wrap(sex ~ .) +
  labs(x = "Albumin [g/dL]", 
       y = "Percentage", 
       title = "Albumin")


## Blood urea nitrogen
data %>% 
  ggplot(aes(x = bun)) + 
  geom_histogram(aes(y = 100*(..count..)/sum(..count..))) + 
  facet_wrap(sex ~ .) +
  labs(x = "Blood urea nitrogen [mg/dL]", 
       y = "Percentage", 
       title = "Blood urea nitrogen")


## Creatinine 
data %>% 
  ggplot(aes(x = SCr)) + 
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), 
                 bins = 30) + 
  facet_wrap(sex ~ .) +
  labs(x = "Creatinine [mg/dL]",
       y = "Percentage",
       title = "Creatinine" ) + 
  scale_x_continuous(breaks = seq(0,20,1)) + 
  scale_y_log10()

############################################################

## Income by ethnicity, geom_histogram and geom_boxplot 
data %>% 
  ggplot(aes(x = income)) + 
  geom_histogram(aes(y = 100*(..count..)/sum(..count..))) + 
  facet_wrap(sex ~ .) + 
  labs(x = "Annual income [USD]",
       y = "Percentage",
       title = "Annual income") + 
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        axis.text.x = element_text(angle = 45, size = 5)) + 
  scale_x_continuous(breaks = seq(0,100000,5000)) 

## The boxplot compactly displays the distribution of a continuous variable. It visualises five summary statistics (the median, two hinges and two whiskers), and all "outlying" points individually.
## The lower and upper hinges correspond to the first and third quartiles (the 25th and 75th percentiles)
## The upper whisker extends from the hinge to the largest value no further than 1.5 * IQR from the hinge (where IQR is the inter-quartile range, or distance between the first and third quartiles). The lower whisker extends from the hinge to the smallest value at most 1.5 * IQR of the hinge. Data beyond the end of the whiskers are called "outlying" points and are plotted individually.

## Geom_violin describes the distribution of the data for each group

normalize <-function(x){
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}



data %>% 
  mutate(income_nor = normalize(data$income)) %>% 
  ggplot(aes(x = re, 
             y = income_nor)) + 
  geom_boxplot(outlier.color = "red", 
               outlier.shape = 1, 
               outlier.size = 2) + 
  geom_violin(alpha = 1/10, 
              color = "grey") +  
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_text(angle = 20, size = 7)) + 
  labs(x = " ", 
       y = "Normalized annual income", 
       title = "Annual income by ethnicity", 
       caption = "red dot = outlier", size = 2)

## With the outliers for the mexican group, should we do some measurements of spread? e.g. The interquartile range IQR(x) and median absolute deviation mad(x)



###############################################################

## Diagnosis by sex
diagnosis <- c(`0` = "Not diagnosed",
               `1` = "Diagnosed with DM or Pre-DM")

## Sex-based prevalence of diabetes mellitus 

dx_total <- data %>% 
  filter(dx == 1) %>% 
  select(-seqn) %>% 
  as_tibble()

dx_total %>% 
  ggplot(aes(x = dx)) + 
  geom_bar(aes(fill = sex), 
           position = "dodge")

## Premenopausal aged women and men with equivalent age
count(data,sex)

dx_below50 <- data %>% 
  filter(age <50) %>% 
  filter(dx == 1) %>% 
  as_tibble()


data %>% 
  filter(dx == 1) %>% 
  count()
# 625 diagnosed DM or pre-DM

p1 <- dx_below50 %>% 
  ggplot(aes(x = sex)) + 
  geom_bar(aes(y = 100*(..count..)/625)) + 
  ylim(0,40) + 
  theme_classic() +  
  labs(x = "", 
       y = "Percentage of total diagnosed individuals", 
       subtitle = "Age group below 50", 
       caption = "The average age for menopause in USA is 50")



### Menopausal aged women and men with equivalent age

dx_above50 <- data %>% 
  filter(age >50) %>% 
  filter(dx == 1) %>% 
  as_tibble()

  
p2 <- dx_above50 %>% 
  ggplot(aes(x = sex)) + 
  geom_bar(aes(y = 100*(..count..)/625)) + 
  scale_y_continuous(breaks = seq(0,40,10)) + 
  theme_classic() + 
  labs(x = "", 
       y = " ", 
       subtitle = "Age group above 50")

(p1 + p2) + 
  plot_annotation(title = "Pre- and postmenopausal prevalence of diabetes mellitus") 


## Diagnosed vs treated individuals 

dxtx_total <- data %>% 
  filter(dx == 1, 
         tx == 1) %>% 
  as_tibble()

## Only diagnosed individuals is presented in dx_total = 625.
## Of all diagnosed individuals, how many is actually treated with insulin or DM meds? 
dx_total %>% 
  ggplot(aes(x = age)) + 
  geom_histogram(bins = 30, 
                 aes(y = 100*(..count..)/625)) + 
  facet_grid(tx ~ re ) + 
  #scale_x_discrete(breaks = c("0", 
                             # "1"),
                   #labels = c("Not treated",
                              #"Treated")) + 
  labs(x = "Treatment status", 
       y = "Percentage", 
       title = "Treatment status of diagnosed individuals") 


## Treatment status

dx_total %>% 
  ggplot(aes(x = age, 
             y = re, 
             fill = tx)) + 
  geom_density_ridges(alpha = 0.5) +
  scale_fill_viridis_d() 
  

######################################################################

## Financial distribution and age in relation to treatment status 

cols <- c("0" = "black", "1" = "blue")
      
dx_total %>% 
  mutate(age_group = group)
dx_total %>%
  group_by()
  ggplot(aes(x = tx,
             y = income)) + 
  geom_boxplot() +
  facet_wrap(~ age)
  labs(x = "Age", 
       y = "Annual income [USD]", 
       title = "Finance and age in relation to treatment status of all diagnosed individuals")

scale_colour_manual(values = cols,
                    breaks = c("0", "2"),
                    labels = c("Not", "Treated"))

####################################################################
# Income, ethnicity, tx 

dx_total %>% 
  ggplot(aes(x = re, 
             y = income)) + 
  geom_boxplot() + 
  facet_wrap(~ tx) + 
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_text(angle = 25,size = 7)) 



####################################################################
#Linear relations between variables 

data %>% 
  ggplot(aes(x = bmi, 
             y = waist)) + 
  geom_point()
## BMI and waist is correlated.

data %>% 
  ggplot(aes(x = bmi, 
             y = sub)) + 
  geom_point()
## 	Subscapular Skinfold and BMI is correlated. 

data %>% 
  ggplot(aes(x = bmi, 
             y = waist)) + 
  geom_point()
## BMI and waist is correlated 


data %>% 
  ggplot(aes(x = bun, 
             y = SCr)) + 
  geom_point() + 
  scale_x_log10() + 
  scale_y_log10()

data %>% 
  ggplot(aes(x = wt, 
             y = income)) + 
  geom_point()
## Diagnosis in correlation with biochemical parameters 

data %>% 
  ggplot(aes(x = age, 
             y = albumin)) + 
  geom_point() + 
    facet_grid(dx ~ tx)

###################################################
## Mutate() 

##BMI classification 

data <- data %>% 
  mutate(bmi_class = case_when(bmi < 18.5 ~ "underweight",
                               18.5 <= bmi & bmi < 25.0 ~ "normal weight",
                               25.0 <= bmi & bmi < 30.0 ~ "overweight",
                               30.0 <= bmi & bmi < 35.0 ~ "obese",
                               35.0 <= bmi & bmi < 40.0 ~ "severe obesity",
                               40.0 <= bmi & bmi < 50.0 ~ "morbid obesity",
                               50.0 <= bmi ~ "super obese"))
## Body fat percentage 

data <- data %>% 
  mutate(sex_class = case_when(sex == "female" ~ 0,
                               sex == "male" ~ 1),
         bfp = 1.39 * bmi + 0.16 * age - 10.34 * sex_class - 9)

## distribution of BMI 

data %>%  
  count(bmi_class) %>% 
  drop_na(bmi_class) %>% 
  mutate(bmi_class = factor(bmi_class,
                            levels =  c("underweight", "normal weight",
                                        "overweight", "obese",
                                        "severe obesity", "morbid obesity",
                                        "super obese")), 
         Percentage = n / sum(n) * 100) %>%
  ggplot(aes(x = bmi_class, 
             y = Percentage)) +
  geom_col() + 
  labs( x = "BMI classification", 
        title = "Distribution of BMI classification")

data %>% 
  mutate(dx_class = case_when(dx == 0 ~ "Not diagnosed", 
                              dx == 1 ~ "Diagnosed")) %>% 
  ggplot(aes(x = age, 
             y = bmi_class)) + 
  geom_boxplot() + 
  facet_wrap(~ dx_class) + 
  labs(x = "Age", 
      y = "BMI class", 
      title = "BMI and age in relation to diagnosis")


data %>% 
  mutate(dx_class = case_when(dx == 0 ~ "Not diagnosed", 
                              dx == 1 ~ "Diagnosed")) %>% 
  ggplot(aes(x = income, 
             y = bmi_class)) + 
  geom_boxplot() + 
  facet_wrap(~ dx_class)
## Body fat perentage distribution 
## needs to range or classify BFP first. 
  