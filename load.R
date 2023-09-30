# Set working directory

setwd("/Users/jarrettphillips/desktop/GBADs Postdoc/GBADs R Package/R")

# Install required packages

# install.packages("mc2d) # for 2D Monte Carlo simulations
# install.packages("truncnorm") # for truncated Normal distribution
# install.packages("yaml") # for importing YAML files

# Load required packages

library(mc2d)
library(truncnorm)
library(yaml)

# Import GBADS functions

source("rpert.R")
source("read_params.R")

# Specify required arguments

file_path <- "/Users/jarrettphillips/desktop/GBADs Postdoc/Ethiopia AHLE/YAML Parameter Files/params_cattle.yaml"
file_type <- "yaml"
species <- "cattle"

# Import scenario paramter file

read_params(file_path = file_path, file_type = file_type)