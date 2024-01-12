setwd("/Users/jarrettphillips/desktop")

# Variables to replace
variables_to_replace <- c("NF", "NM", "JF", "JM")
new_variables <- c("JF", "JM", "SubAF", "SubAM")

# Replace variables in the function code
modified_code <- deparse(run_compartmental_model)
for (i in seq_along(variables_to_replace)) {
  modified_code <- gsub(variables_to_replace[i], new_variables[i], modified_code)
}

# Create a new function with replaced variables
new_fun <- eval(parse(text = modified_code))

# Save function to file in working directory
dump("new_fun", file = "fun.R")
