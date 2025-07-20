setwd("./R")

# install.packages("mc2d")
# install.packages("truncnorm")
# install.packages("yaml")
# install.packages("rstudioapi")
# install.packages("tools")
# install.packages("foreach")
# install.packages("doParallel")

library(mc2d)
library(truncnorm)
library(yaml)
library(rstudioapi)
library(tools)

source("rpert.R")
source("read_params.R")
source("run_compartmental_model_V7.2_StochasticStartPop_Girma.R")
source("setup.R")

args <- commandArgs(trailingOnly = TRUE)

file_path <- ifelse(length(args) >= 1, args[[1]], ".")
# seed_value <- ifelse(length(args) >= 2, as.numeric(args[[2]]), NULL)
seed_value <- ifelse(length(args) >= 2 && args[[2]] != "NULL" && args[[2]] != "", as.numeric(args[[2]]), NULL)

output <- ifelse(length(args) >= 3, args[[3]], "summary")
parallel <- ifelse(length(args) >= 4, as.logical(args[[4]]), FALSE)

cat("Running model with the following settings:\n")
cat("  File path: ", file_path, "\n")
cat("  Seed:      ", seed_value, "\n")
cat("  Output:    ", output, "\n")
cat("  Parallel:  ", parallel, "\n")

ptm <- proc.time()
results <- setup(file_path = file_path, seed_value = seed_value, parallel = parallel)
cat("Model completed in ", round((proc.time() - ptm)[3], 2), " seconds.\n")


