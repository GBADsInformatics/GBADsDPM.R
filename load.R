setwd("/Users/jarrettphillips/Desktop/GBADs Postdoc/GBADs DPM R Package/R")

# install.packages("mc2d) # for 2D Monte Carlo simulations
# install.packages("truncnorm") # for truncated Normal distribution
# install.packages("yaml") # for importing YAML files

library(mc2d)
library(truncnorm)
library(yaml)

source("rpert.R")
source("read_params.R")
source("run_compartmental_model.R")

file_path <- file.choose()
file_type <- "yaml"

seed_value <- NULL # set random seed for reproducibility
read_params(file_path = file_path, file_type = file_type)

run_compartmental_model()
