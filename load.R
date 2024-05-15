setwd("/Users/jarrettphillips/Desktop/GBADs Postdoc/GBADs DPM R Package/R")

# install.packages("mc2d) # for 2D Monte Carlo simulations
# install.packages("truncnorm") # for truncated Normal distribution
# install.packages("yaml") # for importing YAML files

library(mc2d)
library(truncnorm)
library(yaml)
library(rstudioapi)
library(tools)
# library(foreach)
# library(doParallel)

source("rpert.R")
source("read_params.R")
source("run_compartmental_model.R")
source("setup.R")

file_path <- selectDirectory()
seed_value <- NULL

setup(file_path = file_path, seed_value = seed_value)
