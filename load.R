setwd("/Users/jarrettphillips/Desktop/GBADs Postdoc/GBADs DPM R Package/R")

# install.packages("mc2d") # for 2D Monte Carlo simulations
# install.packages("truncnorm") # for truncated Normal distribution
# install.packages("yaml") # for importing YAML files
# install.packages("rstudioapi")
# install.packages("tools")
# install.packages("foreach")
# install.packages("doParallel")

library(mc2d)
library(truncnorm)
library(yaml)
library(rstudioapi)
library(tools)
# library(foreach)
# library(doParallel)

# library(GBADsDPM)

source("rpert.R")
source("read_params.R")
source("run_compartmental_model_V7.2_StochasticStartPop_Girma.R")
source("setup.R")

file_path <- selectDirectory()
# file_path <- "/Users/jarrettphillips/Desktop/Test 1"
seed_value <- NULL
parallel <- FALSE

# ptm <- proc.time()
setup(file_path = file_path, seed_value = seed_value, parallel = parallel)
# proc.time() - ptm