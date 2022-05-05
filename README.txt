###The dataset###
The dataset we have chosen to work with, is the “NHANES glycohemoglobin data”, from the Vanderbilt Biostatistics Datasets. The dataset can be accessed at: https://hbiostat.org/data/

The description of the dataset can be accessed at:
https://hbiostat.org/data/repo/nhgh.html

This dataset contains 6795 observations and 20 variables of which 7 variables contain some NAs. The dataset includes a great span of variables such as sex, age, ethnicity, income and a lot of variables relating to body measurements such as bmi, skinfolds as well as albumin, glycohemoglobin and creatinine levels. Finally, the dataset contains variables indicating whether each individual in the dataset is diagnosed with diabetes or not, and whether they are treated for it or not. 




###Generated data for additions###
The tab-separated file "diagnose_and_treatment.tsv" in ./data/_raw has been generated with the purpose of adding an extra variables classifying individuals based on both their diagnosis and treatment status which is used for the PCA. This dataset were joined together with the raw dataset nhgh.tsv in the script 02_clean.R




###Final datasets used for modelling and visualisations###
The final datasets generated after running 01_load.R, 02_clean.R and 03_augment.R are placed in ./data. These are used for different purposes in the process of visualisation and modelling data (using exploratory data analysis - should we write this?), and include:

- 03_nhgh_clean_aug.tsv
- 03_nhgh_clean_aug_1k.tsv
- 03_nhgh_clean_aug_2k.tsv

The datasets comprise the same overall information, however in the files with the extensions _1k and _2k, one-out-of-k coding have been applied to some variables.




###Scripts###

#00_doit.R#
This script runs the entire project. As there are several visualisations kept within some of the _analysis scripts, these should be viewed separately if the user wants to take a closer look at the visualisations. 


#01_load.R#
This scripts loads in the raw dataset nhgh.tsv in ./data/_raw


#02_clean.R#
This script is used for cleaning particular variables (such as age and income) as well as for imputing NAs based on a KNN approach.

This script will take around 10 minutes to run (depending on the server load on the cloud or the computer it is run upon), due to two reasons:
- For each observation where there is NAs, we have implemented a KNN function, which takes about 20-30 seconds to run for each variable containing NAs.
- the rowrise-operator from dplyr used for replacing NAs is slow as it essentially turns the calculations into a for-loop.


#03_augment.R#




###Packages###
The following packages should be installed for running the entire project:

