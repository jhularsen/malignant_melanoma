# Packages ----------------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(finalfit)
library(skimr)
library(here)
library(survival)
library(survminer)
library(broom)
library(cmprsk)
library(patchwork)
library(knitr)

# Sourcing scripts --------------------------------------------------------
source(here("scripts", "data_cleaning.R"))
#load(here("data", "melanoma_final.rda")) 
#loads the R object saved in melanoma_final.rda (aka the cleaned melanoma dataset from the data_cleaning.R script)
#It is quicker than sourcing the data_cleaning.R script, but loads the dataset which was created the last time I ran data_cleaning.R
source(here("scripts", "figures.R"))
source(here("scripts", "analysis.R"))
