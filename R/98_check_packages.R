# List of used packages
packages_used = c("tidyverse",
                  "pdist",
                  "broom",
                  "ggplot2",
                  "patchwork",
                  "scales",
                  "ggridges",
                  "corrplot",
                  "stringr",
                  "glmnet",
                  "corrr")

for(p in packages_used){
  if(!require(p,character.only = TRUE)) install.packages(p)
}
