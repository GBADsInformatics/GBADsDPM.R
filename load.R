setwd("/Users/jarrettphillips/desktop/GBADs Postdoc/GBADs R Package/R")

# install.packages("mc2d) # for 2D Monte Carlo simulations
# install.packages("truncnorm") # for truncated Normal distribution
# install.packages("yaml") # for importing YAML files

library(mc2d)
library(truncnorm)
library(yaml)

source("rpert.R")
source("read_params.R")
source("run_compartmental_model.R")

file_path <- "/Users/jarrettphillips/desktop/GBADs Postdoc/Ethiopia AHLE/YAML Parameter Files/tbp_cattle/cattle_trial_CLM_all_mortality_zero.yaml"
# file_path <- "/Users/jarrettphillips/desktop/GBADs Postdoc/Ethiopia AHLE/YAML Parameter Files/params_cattle.yaml"
# file_path <- "/Users/jarrettphillips/desktop/GBADs Postdoc/Ethiopia AHLE/YAML Parameter Files/params_small_ruminants.yaml"
# file_path <- "/Users/jarrettphillips/desktop/GBADs Postdoc/Ethiopia AHLE/YAML Parameter Files/params_poultry.yaml"

file_type <- "yaml" # this will always be the case 

seed_value <- NULL # set random seed for reproducibility - can be an integer of any length

read_params(file_path = file_path, file_type = file_type)

# system.time(run_compartmental_model()) # measure CPU time

