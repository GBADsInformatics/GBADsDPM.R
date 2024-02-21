setwd("/Users/jarrettphillips/Desktop/GBADs Postdoc/GBADs DPM R Package/R")

# install.packages("mc2d) # for 2D Monte Carlo simulations
# install.packages("truncnorm") # for truncated Normal distribution
# install.packages("yaml") # for importing YAML files

library(mc2d)
library(truncnorm)
library(yaml)

source("rpert.R")
source("read_params.R")
source("run_model.R")

file_path <- "/Users/jarrettphillips/desktop/GBADs Postdoc/Ethiopia AHLE/YAML Parameter Files/params_cattle.yaml"
file_type <- "yaml"

seed_value <- NULL # set random seed for reproducibility
read_params(file_path = file_path, file_type = file_type)


