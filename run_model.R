#' @title
#' Run the AHLE compartmental agent-based simulation model 
#' 
#' @description
#' Runs scenario simulations using parameters imported via \code{read_params()}
#' 
#' @example 
#' # run_model()
#' 

run_model <- function() {
  
  calculate_mu <- function(part, prolif) {
    return ((sample(part, size = 10000, replace = TRUE) * sample(prolif, size = 10000, replace = TRUE)) / 12)
  }
  
  calculate_dry_matter_requirements <- function(lw, prpn, dm_req_prpn) {
    return (dm_req_prpn * lw * prpn)
  }
  
  calculate_purchased_feed <- function(kg_dm_req, lskeepers_purch_feed, feed_paid_for, dm_in_feed) {
    return (kg_dm_req * lskeepers_purch_feed * feed_paid_for / dm_in_feed)
  }
  
  calculate_expenditure_on_feed <- function(kg_feed_purchased, feed_cost_kg) {
    return (kg_feed_purchased * feed_cost_kg)
  }

  # Calculate mu
  Mu <- calculate_mu(part, prolif)
  
  # Calculate dry matter requirements
  kg_DM_req_NF <- calculate_dry_matter_requirements(lwNF, prpn_lskeepers_purch_feed, DM_req_prpn_NF)
  kg_DM_req_NM <- calculate_dry_matter_requirements(lwNM, prpn_lskeepers_purch_feed, DM_req_prpn_NM)
  kg_DM_req_JF <- calculate_dry_matter_requirements(lwJF, prpn_lskeepers_purch_feed, DM_req_prpn_JF)
  kg_DM_req_JM <- calculate_dry_matter_requirements(lwJM, prpn_lskeepers_purch_feed, DM_req_prpn_JM)
  kg_DM_req_AF <- calculate_dry_matter_requirements(lwAF, prpn_lskeepers_purch_feed, DM_req_prpn_AF)
  kg_DM_req_AM <- calculate_dry_matter_requirements(lwAM, prpn_lskeepers_purch_feed, DM_req_prpn_AM)
    
  # Calculate purchased feed
  DM_purch_NF <- calculate_purchased_feed(kg_DM_req_NF, prpn_lskeepers_purch_feed, prpn_feed_paid_for, DM_in_feed)
  DM_purch_NM <- calculate_purchased_feed(kg_DM_req_NM, prpn_lskeepers_purch_feed, prpn_feed_paid_for, DM_in_feed)
  DM_purch_JF <- calculate_purchased_feed(kg_DM_req_JF, prpn_lskeepers_purch_feed, prpn_feed_paid_for, DM_in_feed)
  DM_purch_JM <- calculate_purchased_feed(kg_DM_req_JM, prpn_lskeepers_purch_feed, prpn_feed_paid_for, DM_in_feed)
  DM_purch_AF <- calculate_purchased_feed(kg_DM_req_AF, prpn_lskeepers_purch_feed, prpn_feed_paid_for, DM_in_feed)
  DM_purch_AM <- calculate_purchased_feed(kg_DM_req_AM, prpn_lskeepers_purch_feed, prpn_feed_paid_for, DM_in_feed)
  
  # Calculate KG Feed purchased
  KG_Feed_purchased_NF <- DM_purch_NF / DM_in_feed
  KG_Feed_purchased_NM <- DM_purch_NM / DM_in_feed
  KG_Feed_purchased_JF <- DM_purch_JF / DM_in_feed
  KG_Feed_purchased_JM <- DM_purch_JM / DM_in_feed
  KG_Feed_purchased_AF <- DM_purch_AF / DM_in_feed
  KG_Feed_purchased_AM <- DM_purch_AM / DM_in_feed
  
  # Calculate expenditure on feed
  Expenditure_on_feed_NF <- calculate_expenditure_on_feed(KG_Feed_purchased_NF, Feed_cost_kg)
  Expenditure_on_feed_NM <- calculate_expenditure_on_feed(KG_Feed_purchased_NM, Feed_cost_kg)
  Expenditure_on_feed_JF <- calculate_expenditure_on_feed(KG_Feed_purchased_JF, Feed_cost_kg)
  Expenditure_on_feed_JM <- calculate_expenditure_on_feed(KG_Feed_purchased_JM, Feed_cost_kg)
  Expenditure_on_feed_AF <- calculate_expenditure_on_feed(KG_Feed_purchased_AF, Feed_cost_kg)
  Expenditure_on_feed_AM <- calculate_expenditure_on_feed(KG_Feed_purchased_AM, Feed_cost_kg)
  
  if (species == "cattle") {
    kg_DM_req_O <- calculate_dry_matter_requirements(lwO, prpn_lskeepers_purch_feed, DM_req_prpn_O)
    DM_purch_O <- calculate_purchased_feed(kg_DM_req_O, prpn_lskeepers_purch_feed, prpn_feed_paid_for, DM_in_feed)
    KG_Feed_purchased_O <- DM_purch_O / DM_in_feed
    Expenditure_on_feed_O <- calculate_expenditure_on_feed(KG_Feed_purchased_O, Feed_cost_kg)
  }
  
  # List of variable categories'
  categories <- c(
    "NumNF",
    "NumNM",
    "NumJF",
    "NumJM",
    "NumAF",
    "NumAM",
    "NumN",
    
    "Births", 
    
    "Growth_NF", 
    "Growth_NM", 
    "Growth_JF", 
    "Growth_JM", 
    
    "Deaths_NF", 
    "Deaths_NM", 
    "Deaths_JF", 
    "Deaths_JM", 
    "Deaths_AF", 
    "Deaths_AM", 
    
    "Culls_AF", 
    "Culls_AM", 
    
    "Cumulative_culls_AM", 
    
    "offtake_NF", 
    "offtake_NM", 
    "offtake_JF", 
    "offtake_JM", 
    "offtake_AF", 
    "offtake_AM", 
    
    "Cumulative_draught_income", 
    
    "Monthly_mortality", 
    "Total_Mortality",
    "Total_Mortality_NF", 
    "Total_Mortality_NM", 
    "Total_Mortality_JF", 
    "Total_Mortality_JM", 
    "Total_Mortality_AF", 
    "Total_Mortality_AM", 
    
    "Value_of_Total_Mortality", 
    "Value_of_Total_Mortality_NF", 
    "Value_of_Total_Mortality_NM", 
    "Value_of_Total_Mortality_JF", 
    "Value_of_Total_Mortality_JM", 
    "Value_of_Total_Mortality_AF", 
    "Value_of_Total_Mortality_AM", 
    
    "Quant_Liveweight_kg", 
    "Quant_Liveweight_kg_NF", 
    "Quant_Liveweight_kg_NM", 
    "Quant_Liveweight_kg_JF", 
    "Quant_Liveweight_kg_JM", 
    "Quant_Liveweight_kg_AF", 
    "Quant_Liveweight_kg_AM", 
    "Quant_Meat_kg", 
    
    "Num_Offtake", 
    "Num_Offtake_NF", 
    "Num_Offtake_NM", 
    "Num_Offtake_JF", 
    "Num_Offtake_JM", 
    "Num_Offtake_AF", 
    "Num_Offtake_AM", 
    
    "Offtake_Liveweight_kg", 
    "Offtake_Liveweight_kg_JF", 
    "Offtake_Liveweight_kg_JM", 
    "Offtake_Liveweight_kg_AF", 
    "Offtake_Liveweight_kg_AM", 
    
    "Pop_growth", 
    "Pop_growth_NF", 
    "Pop_growth_NM", 
    "Pop_growth_JF", 
    "Pop_growth_JM", 
    "Pop_growth_AF", 
    "Pop_growth_AM", 
    
    "Monthly_growth_rate", 
    "Monthly_pop_growth", 
    
    "Quant_Manure", 
    "Quant_Manure_NF", 
    "Quant_Manure_NM", 
    "Quant_Manure_JF", 
    "Quant_Manure_JM", 
    "Quant_Manure_AF", 
    "Quant_Manure_AM", 
    
    "Quant_Hides", 
    "Quant_Hides_JF", 
    "Quant_Hides_JM", 
    "Quant_Hides_AF", 
    "Quant_Hides_AM", 
    "Quant_Milk",
    
    "Cumulative_Dry_Matter", 
    "Cumulative_Dry_Matter_NF", 
    "Cumulative_Dry_Matter_NM", 
    "Cumulative_Dry_Matter_JF", 
    "Cumulative_Dry_Matter_JM", 
    "Cumulative_Dry_Matter_AF", 
    "Cumulative_Dry_Matter_AM", 
    
    "Monthly_DM", 
    
    "Value_Offtake", 
    "Value_Offtake_NF", 
    "Value_Offtake_NM", 
    "Value_Offtake_JF", 
    "Value_Offtake_JM", 
    "Value_Offtake_AF", 
    "Value_Offtake_AM", 
    
    "Value_Herd_Increase", 
    "Value_Herd_Increase_NF", 
    "Value_Herd_Increase_NM", 
    "Value_Herd_Increase_JF", 
    "Value_Herd_Increase_JM", 
    "Value_Herd_Increase_AF", 
    "Value_Herd_Increase_AM", 
    
    "Total_Value_increase", 
    "Total_Value_increase_NF", 
    "Total_Value_increase_NM", 
    "Total_Value_increase_JF", 
    "Total_Value_increase_JM", 
    "Total_Value_increase_AF", 
    "Total_Value_increase_AM", 
    
    "Feed_cost", 
    "Feed_cost_NF", 
    "Feed_cost_NM", 
    "Feed_cost_JF", 
    "Feed_cost_JM",
    "Feed_cost_AF", 
    "Feed_cost_AM", 
    
    "Labour_cost", 
    "Labour_cost_NF", 
    "Labour_cost_NM", 
    "Labour_cost_JF", 
    "Labour_cost_JM", 
    "Labour_cost_AF", 
    "Labour_cost_AM", 
    
    "Health_cost", 
    "Health_cost_NF", 
    "Health_cost_NM", 
    "Health_cost_JF", 
    "Health_cost_JM", 
    "Health_cost_AF", 
    "Health_cost_AM", 
    
    "Capital_cost", 
    "Capital_cost_NF", 
    "Capital_cost_NM", 
    "Capital_cost_JF", 
    "Capital_cost_JM", 
    "Capital_cost_AF", 
    "Capital_cost_AM", 
    
    "Infrastructure_cost", 
    "Infrastructure_cost_NF", 
    "Infrastructure_cost_NM", 
    "Infrastructure_cost_JF", 
    "Infrastructure_cost_JM", 
    "Infrastructure_cost_AF", 
    "Infrastructure_cost_AM", 
    
    "Total_expenditure", 
    "Total_expenditure_NF", 
    "Total_expenditure_NM", 
    "Total_expenditure_JF", 
    "Total_expenditure_JM", 
    "Total_expenditure_AF", 
    "Total_expenditure_AM"
  )
  
  if (species == "cattle") {
    vector_categories <- append(categories, c("NumO",
                         "Oxen_J", 
                         "Oxen_A",
                         "Deaths_O", 
                         "Culls_O", 
                         "Offtake_O",
                         "Total_Mortality_O",
                         "Value_of_Total_Mortality_O",
                         "Quant_Liveweight_kg_O",
                         "Num_Offtake_O",
                         "Offtake_Liveweight_kg_O", 
                         "Pop_growth_O", 
                         "Quant_Manure_O", 
                         "Quant_Hides_O",
                         "Cumulative_Dry_Matter_O",
                         "Value_Offtake_O", 
                         "Value_Herd_Increase_O", 
                         "Total_Value_increase_O", 
                         "Feed_cost_O",
                         "Labour_cost_O", 
                         "Health_cost_O",
                         "Capital_cost_O",
                         "Infrastructure_cost_O", 
                         "Total_expenditure_O"))
  } else if (species == "small ruminants") {
    vector_categories <- append(vector_categories, "Quant_Wool")
  } else {
    # poultry
    vector_categories <- append(vector_categories, c("Quant_Eggs_sold",
                         "Quant_Eggs_consumed",
                         "Value_Eggs_sold",
                         "Value_Eggs_consumed",
                         ))
  }
  
  # Initialize a list to store the vectors
  res_vec <- list()
  
  # Loop through categories and create matrices
  for (vec in vector_categories) {
    res_vec[[vec]] <- rep(0, Num_months)
  }
  
  matrix_categories <- c("NumNF",
                         "NumNM",
                         "NumJF",
                         "NumJM",
                         "NumAF",
                         "NumAM",
                         "NumN",
                         
                         "Monthly_mortality", 
                         "Total_Mortality",
                         "Total_Mortality_NF", 
                         "Total_Mortality_NM", 
                         "Total_Mortality_JF", 
                         "Total_Mortality_JM", 
                         "Total_Mortality_AF", 
                         "Total_Mortality_AM", 
                         
                         "Value_of_Total_Mortality", 
                         "Value_of_Total_Mortality_NF", 
                         "Value_of_Total_Mortality_NM", 
                         "Value_of_Total_Mortality_JF", 
                         "Value_of_Total_Mortality_JM", 
                         "Value_of_Total_Mortality_AF", 
                         "Value_of_Total_Mortality_AM", 
                         
                         "Quant_Liveweight_kg", 
                         "Quant_Liveweight_kg_NF", 
                         "Quant_Liveweight_kg_NM", 
                         "Quant_Liveweight_kg_JF", 
                         "Quant_Liveweight_kg_JM", 
                         "Quant_Liveweight_kg_AF", 
                         "Quant_Liveweight_kg_AM", 
                         
                         "Cumulative_draught_income",
                         
                         "Quant_Meat_kg", 
                         
                         "Num_Offtake", 
                         "Num_Offtake_NF", 
                         "Num_Offtake_NM", 
                         "Num_Offtake_JF", 
                         "Num_Offtake_JM", 
                         "Num_Offtake_AF", 
                         "Num_Offtake_AM", 
                         
                         "Offtake_Liveweight_kg", 
                         "Offtake_Liveweight_kg_JF", 
                         "Offtake_Liveweight_kg_JM", 
                         "Offtake_Liveweight_kg_AF", 
                         "Offtake_Liveweight_kg_AM", 
    
                         "Pop_growth", 
                         "Pop_growth_NF", 
                         "Pop_growth_NM", 
                         "Pop_growth_JF", 
                         "Pop_growth_JM", 
                         "Pop_growth_AF", 
                         "Pop_growth_AM", 
                         
                         "Monthly_growth_rate", 
                         "Monthly_pop_growth", 
                         
                         "Quant_Manure", 
                         "Quant_Manure_NF", 
                         "Quant_Manure_NM", 
                         "Quant_Manure_JF", 
                         "Quant_Manure_JM", 
                         "Quant_Manure_AF", 
                         "Quant_Manure_AM", 
                         
                         "Value_manure",
                         "Value_manure_NF",
                         "Value_manure_NM",
                         "Value_Manure_JF", 
                         "Value_Manure_JM", 
                         "Value_Manure_AF", 
                         "Value_Manure_AM", 
                         
                         "Quant_Hides", 
                         "Quant_Hides_JF", 
                         "Quant_Hides_JM", 
                         "Quant_Hides_AF", 
                         "Quant_Hides_AM", 
                         
                         "Value_Hides", 
                         "Value_Hides_JF", 
                         "Value_Hides_JM", 
                         "Value_Hides_AF", 
                         "Value_Hides_AM", 
                         
                         "Quant_Milk",
                         "Value_Milk",
                         
                         "Cumulative_Dry_Matter", 
                         "Cumulative_Dry_Matter_NF", 
                         "Cumulative_Dry_Matter_NM", 
                         "Cumulative_Dry_Matter_JF", 
                         "Cumulative_Dry_Matter_JM", 
                         "Cumulative_Dry_Matter_AF", 
                         "Cumulative_Dry_Matter_AM", 
                         
                         "Monthly_DM",
                         
                         "Value_Offtake", 
                         "Value_Offtake_NF", 
                         "Value_Offtake_NM", 
                         "Value_Offtake_JF", 
                         "Value_Offtake_JM", 
                         "Value_Offtake_AF", 
                         "Value_Offtake_AM", 
                         
                         "Value_Herd_Increase", 
                         "Value_Herd_Increase_NF", 
                         "Value_Herd_Increase_NM", 
                         "Value_Herd_Increase_JF", 
                         "Value_Herd_Increase_JM", 
                         "Value_Herd_Increase_AF", 
                         "Value_Herd_Increase_AM", 
                         
                         "Total_Value_increase", 
                         "Total_Value_increase_NF", 
                         "Total_Value_increase_NM", 
                         "Total_Value_increase_JF", 
                         "Total_Value_increase_JM", 
                         "Total_Value_increase_AF", 
                         "Total_Value_increase_AM", 
                         
                         "Production_value_herd_offtake_hide_manure",
                         "Production_value_herd_offtake_hide_manure_NF",
                         "Production_value_herd_offtake_hide_manure_NM",
                         "Production_value_herd_offtake_hide_manure_JF",
                         "Production_value_herd_offtake_hide_manure_JM",
                         "Production_value_herd_offtake_hide_manure_AF",
                         "Production_value_herd_offtake_hide_manure_AM",
                         
                         "Feed_cost", 
                         "Feed_cost_NF", 
                         "Feed_cost_NM", 
                         "Feed_cost_JF", 
                         "Feed_cost_JM",
                         "Feed_cost_AF", 
                         "Feed_cost_AM", 
                         
                         "Labour_cost", 
                         "Labour_cost_NF", 
                         "Labour_cost_NM", 
                         "Labour_cost_JF", 
                         "Labour_cost_JM", 
                         "Labour_cost_AF", 
                         "Labour_cost_AM", 
                         
                         "Health_cost", 
                         "Health_cost_NF", 
                         "Health_cost_NM", 
                         "Health_cost_JF", 
                         "Health_cost_JM", 
                         "Health_cost_AF", 
                         "Health_cost_AM", 
                         
                         "Capital_cost", 
                         "Capital_cost_NF", 
                         "Capital_cost_NM", 
                         "Capital_cost_JF", 
                         "Capital_cost_JM", 
                         "Capital_cost_AF", 
                         "Capital_cost_AM", 
                         
                         "Infrastructure_cost", 
                         "Infrastructure_cost_NF", 
                         "Infrastructure_cost_NM", 
                         "Infrastructure_cost_JF", 
                         "Infrastructure_cost_JM", 
                         "Infrastructure_cost_AF", 
                         "Infrastructure_cost_AM", 
                         
                         "Total_expenditure", 
                         "Total_expenditure_NF", 
                         "Total_expenditure_NM", 
                         "Total_expenditure_JF", 
                         "Total_expenditure_JM", 
                         "Total_expenditure_AF", 
                         "Total_expenditure_AM"
                         
  )
  
  if (species == "cattle") {
    matrix_categories <- append(matrix_categories, c("NumO",
                                                     "Total_Mortality_O", 
                                                     "Value_of_Total_Mortality_O",
                                                     "Quant_Liveweight_kg_O",
                                                     "Num_Offtake_O", 
                                                     "Offtake_Liveweight_kg_O", 
                                                     "Pop_growth_O", 
                                                     "Quant_Manure_O",
                                                     "Value_Manure_O",
                                                     "Quant_Hides_O",
                                                     "Value_Hides_O", 
                                                     "Cumulative_Dry_Matter_O", 
                                                     "Value_Offtake_O", 
                                                     "Value_Herd_Increase_O",
                                                     "Total_Value_increase_O",
                                                     "Production_value_herd_offtake_hide_manure_O",
                                                     "Feed_cost_O", 
                                                     "Labour_cost_O", 
                                                     "Health_cost_O", 
                                                     "Capital_cost_O", 
                                                     "Infrastructure_cost_O", 
                                                     "Total_expenditure_O"))
  } else if (species == "small ruminants") {
    matrix_categories <- append(matrix_categories, "Quant_Wool")
  } else {
    # poultry
    matrix_categories <- append(matrix_categories, c("Quant_Eggs_consumed",
                                                     "Quant_Eggs_sold",
                                                     "Value_Eggs_consumed",
                                                     "Value_Eggs_sold"))
  }

  # Initialize a list to store the matrices
  res_mat <- list()
  
  for (mat in matrix_categories) {
    res_mat[[mat]] <- matrix(0, nrow = nruns, ncol = Num_months)
  }

  for (i in 1:nruns) {
    # Total population is sum of age*sex segments
    Nt0 <- sum(N_NF_t0, N_NM_t0, N_JF_t0, N_JM_t0, N_AF_t0, N_AM_t0)
    
    if (species == "cattle") {
      Nt0 <- Nt0 + N_O_t0
    }
    
    # Define population variables and set initial values from function arguments
    N <- Nt0
    age_sex_groups <- c("NF", "NM", "JF", "JM", "AF", "AM")
    
    if (species == "cattle") {
      age_sex_groups <- append(age_sex_groups, "O")
    }

    for (group in age_sex_groups) {
      var_name <- group  # Store the variable name in a separate variable
      value <- get(paste0("N_", group, "_t0"))  # Get the value from the corresponding object
      assign(var_name, value)  # Assign the value to the variable with the desired name
    }
    
    # Age sex group prop of pop at t0 - this ratio should probably stay the same
    prop_groups <- c(pNF_t0 = NF/N, 
                     pJF_t0 = JF/N, 
                     pAF_t0 = AF/N,
                     pNM_t0 = NM/N, 
                     pJM_t0 = JM/N, 
                     pAM_t0 = AM/N)
    
    if (species == "cattle") {
      prop_groups <- c(prop_groups, pO_t0 = O/N)
    }
    
    Culls <- 0
    Num_dead <- 0
    Liveweight_kg <- 0
    Offtake <- 0
    Offtake_liveweight <- 0
    Manure_kg <- 0
    Hides <- 0
    Milk <- 0
    Meat_kg <- 0
    Draught_income <- 0
    Cumulative_DM <- 0
    Monthly_Dry_Matter <- 0
    Population_growth_rate <- 0
    Monthly_growth_rate <- 0
    Monthly_pop_growth <- 0
    Value_Offt <- 0
    Value_herd_inc <- 0
    Feed <- 0
    Labour <- 0
    Health <- 0
    Capital <- 0
    
    production_vars <- c("Num_dead", 
                         "Liveweight_kg",
                         "Offtake",
                         "Offtake_Liveweight",
                         "Manure_kg",
                         "Hides",
                         "Cumulative_DM",
                         "Value_offt",
                         "Value_herd_inc",
                         "Feed",
                         "Labour", 
                         "Health",
                         "Capital")
    
    for (group in age_sex_groups) {
      for (prod_var in production_vars) {
        assign(paste(prod_var, group, sep = "_"), 0)
      }
    }
    
    for (month in 1:Num_months) {
      
      # check and fix these - only 12 elements instead of nruns elements filled
      res_vec$Births[month] <- sample(Mu, 1) * AF

      res_vec$Deaths_NF[month] <- sample(AlphaN, 1) * NF
      res_vec$Deaths_JF[month] <- sample(AlphaJ, 1) * JF
      res_vec$Deaths_AF[month] <- sample(AlphaF, 1) * AF

      res_vec$Deaths_NM[month] <- sample(AlphaN, 1) * NM
      res_vec$Deaths_JM[month] <- sample(AlphaJ, 1) * JM
      res_vec$Deaths_AM[month] <- sample(AlphaM, 1) * AM

      res_vec$offtake_NF[month] <- sample(GammaNF, 1) * NF
      res_vec$offtake_NM[month] <- sample(GammaNM, 1) * NM

      res_vec$offtake_JF[month] <- sample(GammaJF, 1) * JF
      res_vec$offtake_AF[month] <- sample(GammaAF, 1) * AF
      res_vec$offtake_JM[month] <- sample(GammaJM, 1) * JM
      res_vec$offtake_AM[month] <- sample(GammaAM, 1) * AM

      res_vec$Growth_NF[month] <- sample(Beta_N, 1) * NF
      res_vec$Growth_JF[month] <- sample(Beta_J, 1) * JF
      res_vec$Growth_NM[month] <- sample(Beta_N, 1) * NM
      res_vec$Growth_JM[month] <- sample(Beta_J, 1) * JM

      res_vec$Culls_AF[month] <- sample(CullF, 1) * AF
      res_vec$Culls_AM[month] <- sample(CullM, 1) * AM
      
      res_vec$NumNF[month] <- NF + res_vec$Births[month] * 0.5 - res_vec$Deaths_NF[month] - res_vec$Growth_NF[month] - res_vec$offtake_NF[month]
      res_vec$NumJF[month] <- JF + res_vec$Growth_NF[month] - res_vec$Growth_JF[month] - res_vec$offtake_JF[month] - res_vec$Deaths_JF[month]
      res_vec$NumAF[month] <- AF + res_vec$Growth_JF[month] - res_vec$offtake_AF[month] - res_vec$Deaths_AF[month] - res_vec$Culls_AF[month]

      res_vec$NumNM[month] <- NM + res_vec$Births[month] * 0.5 - res_vec$Growth_NM[month] - res_vec$Deaths_NM[month] - res_vec$offtake_NM[month]
      res_vec$NumJM[month] <- JM + res_vec$Growth_NM[month] - res_vec$Growth_JM[month] - res_vec$offtake_JM[month] - res_vec$Deaths_JM[month]
      res_vec$NumAM[month] <- AM + res_vec$Growth_JM[month] - res_vec$offtake_AM[month] - res_vec$Deaths_AM[month] - res_vec$Culls_AM[month]
      
      res_vec$NumN[month] = sum(res_vec$NumNF[month],
                                res_vec$NumJF[month],
                                res_vec$NumAF[month],
                                res_vec$NumNM[month],
                                res_vec$NumJM[month],
                                res_vec$NumAM[month])
      
      if (species == "cattle") {
        res_vec$Deaths_O[month] <- sample(AlphaO, 1) * O
        res_vec$Offtake_O[month] <- sample(GammaO, 1) * O
        res_vec$Culls_O[month] <- sample(CullO, 1) * O
        res_vec$Oxen_A[month] <- sample(castration_rate, 1) * AM
        res_vec$NumO[month] <- O + res_vec$Oxen_A[month] - res_vec$Offtake_O[month] - res_vec$Deaths_O[month] - res_vec$Culls_O[month]
        res_vec$NumAM[month] <- res_vec$NumAM[month] - res_vec$Oxen_A[month]
        res_vec$NumN[month] <- res_vec$NumN[month] + res_vec$NumO[month]
      }

      NF <- res_vec$NumNF[month]
      JF <- res_vec$NumJF[month]
      AF <- res_vec$NumAF[month]
      NM <- res_vec$NumNM[month]
      JM <- res_vec$NumJM[month]
      AM <- res_vec$NumAM[month]
      N <- res_vec$NumN[month]
      
      res_vec$Total_Mortality_NF[month] <- Num_dead_NF + res_vec$Deaths_NF[month]
      Num_dead_NF <- res_vec$Total_Mortality_NF[month]
      res_vec$Total_Mortality_NM[month] <- Num_dead_NM + res_vec$Deaths_NM[month]
      Num_dead_NM <- res_vec$Total_Mortality_NM[month]
      res_vec$Total_Mortality_JF[month] <- Num_dead_JF + res_vec$Deaths_JF[month]
      Num_dead_JF <- res_vec$Total_Mortality_JF[month]
      res_vec$Total_Mortality_JM[month] <- Num_dead_JM + res_vec$Deaths_JM[month]
      Num_dead_JM <- res_vec$Total_Mortality_JM[month]
      res_vec$Total_Mortality_AF[month] <- Num_dead_AF + res_vec$Deaths_AF[month]
      Num_dead_AF <- res_vec$Total_Mortality_AF[month]
      res_vec$Total_Mortality_AM[month] <- Num_dead_AM + res_vec$Deaths_AM[month]
      Num_dead_AM <- res_vec$Total_Mortality_AM[month]
      
      res_vec$Total_Mortality[month] <- sum(res_vec$Total_Mortality_NF[month],
                                            res_vec$Total_Mortality_NM[month], 
                                            res_vec$Total_Mortality_JF[month],
                                            res_vec$Total_Mortality_JM[month], 
                                            res_vec$Total_Mortality_AF[month],
                                            res_vec$Total_Mortality_AM[month]) 
      
      if (species == "cattle") {
        O <- res_vec$NumO[month]
        res_vec$Total_Mortality_O[month] <- Num_dead_O + res_vec$Deaths_O[month]
        Num_dead_O <- res_vec$Total_Mortality_O[month]
        res_vec$Total_Mortality[month] <- res_vec$Total_Mortality[month] + res_vec$Total_Mortality_O[month]
      }
      
      ### Fix This
      
      # res_vec$Value_of_Total_Mortality_NF[month] <- res_vec$Total_Mortality_NF[month] * fvNF
      # res_vec$Value_of_Total_Mortality_NM[month] <- res_vec$Total_Mortality_NM[month] * fvNM
      # res_vec$Value_of_Total_Mortality_JF[month] <- res_vec$Total_Mortality_JF[month] * fvJF
      # res_vec$Value_of_Total_Mortality_JM[month] <- res_vec$Total_Mortality_JM[month] * fvJM
      # res_vec$Value_of_Total_Mortality_AF[month] <- res_vec$Total_Mortality_AF[month] * fvAF
      # res_vec$Value_of_Total_Mortality_AM[month] <- res_vec$Total_Mortality_AM[month] * fvAM
      # 
      # res_vec$Value_of_Total_Mortality[month] <- sum(res_vec$Value_of_Total_Mortality_NF[month],
      #                                            res_vec$Value_of_Total_Mortality_NM[month],
      #                                            res_vec$Value_of_Total_Mortality_JF[month],
      #                                            res_vec$Value_of_Total_Mortality_JM[month],
      #                                            res_vec$Value_of_Total_Mortality_AF[month],
      #                                            res_vec$Value_of_Total_Mortality_AM[month])
      # 
      # 
      # if (species == "cattle") {
      #   res_vec$Value_of_Total_Mortality_O[month] <- res_vec$Total_Mortality_O[month] * fvO
      #   res_vec$Value_of_Total_Mortality[month] <- res_vec$Value_of_Total_Mortality[month] + res_vec$Value_of_Total_Mortality_O[month]
      # }
      
      res_vec$Pop_growth[month] <-  res_vec$NumN[month] - Nt0
      res_vec$Pop_growth_NF[month] <- NF - N_NF_t0
      res_vec$Pop_growth_NM[month] <- NM - N_NM_t0
      res_vec$Pop_growth_JF[month] <- JF - N_JF_t0
      res_vec$Pop_growth_JM[month] <- JM - N_JM_t0
      res_vec$Pop_growth_AF[month] <- AF - N_AF_t0
      res_vec$Pop_growth_AM[month] <- AM - N_AM_t0
      
      if (species == "cattle") {
        res_vec$Pop_growth_O[month] <- O - N_O_t0
      }
      
      res_vec$Quant_Liveweight_kg_NF[month] <- NF * sample(lwNF, 1)
      res_vec$Quant_Liveweight_kg_NM[month] <- NM * sample(lwNM, 1)
      res_vec$Quant_Liveweight_kg_JF[month] <- JF * sample(lwJF, 1)
      res_vec$Quant_Liveweight_kg_JM[month] <- JM * sample(lwJM, 1)
      res_vec$Quant_Liveweight_kg_AF[month] <- AF * sample(lwAF, 1)
      res_vec$Quant_Liveweight_kg_AM[month] <- AM * sample(lwAM, 1)
      
      res_vec$Quant_Liveweight_kg[month] <- sum(res_vec$Quant_Liveweight_kg_NF[month],
                                            res_vec$Quant_Liveweight_kg_NM[month],
                                            res_vec$Quant_Liveweight_kg_JF[month],
                                            res_vec$Quant_Liveweight_kg_JM[month],
                                            res_vec$Quant_Liveweight_kg_AF[month],
                                            res_vec$Quant_Liveweight_kg_AM[month])

      res_vec$Num_Offtake_NF[month] <- Offtake_NF + res_vec$offtake_NF[month]
      res_vec$Num_Offtake_NM[month] <- Offtake_NM + res_vec$offtake_NM[month]
      res_vec$Num_Offtake_JF[month] <- Offtake_JF + res_vec$offtake_JF[month]
      res_vec$Num_Offtake_JM[month] <- Offtake_JM + res_vec$offtake_JM[month]
      res_vec$Num_Offtake_AF[month] <- Offtake_AF + res_vec$offtake_AF[month]
      res_vec$Num_Offtake_AM[month] <- Offtake_AM + res_vec$offtake_AM[month] + res_vec$Culls_AM[month]
      
      if (species == "cattle"){
        res_vec$Quant_Liveweight_kg_O[month] <- O * sample(lwO, 1)
        res_vec$Quant_Liveweight_kg[month] <- res_vec$Quant_Liveweight_kg[month] + res_vec$Quant_Liveweight_kg_O[month]
        res_vec$Num_Offtake_O[month] <- Offtake_O + res_vec$Offtake_O[month] + res_vec$Culls_O[month]
      }
      
      Offtake_NF <- res_vec$Num_Offtake_NF[month]
      Offtake_NM <- res_vec$Num_Offtake_NM[month]
      Offtake_JF <- res_vec$Num_Offtake_JF[month]
      Offtake_JM <- res_vec$Num_Offtake_JM[month]
      Offtake_AF <- res_vec$Num_Offtake_AF[month]
      Offtake_AM <- res_vec$Num_Offtake_AM[month]
      
      res_vec$Num_Offtake[month] <- sum(res_vec$Num_Offtake_NF[month],
                                    res_vec$Num_Offtake_NM[month],
                                    res_vec$Num_Offtake_JF[month],
                                    res_vec$Num_Offtake_JM[month],
                                    res_vec$Num_Offtake_AF[month],
                                    res_vec$Num_Offtake_AM[month])
      
      if (species == "cattle") {
        Offtake_O <- res_vec$Num_Offtake_O[month]
        res_vec$Num_Offtake[month] <- res_vec$Num_Offtake[month] + res_vec$Num_Offtake_O[month]
      }
      
      Offtake <- res_vec$Num_Offtake[month]

      res_vec$Offtake_Liveweight_kg_JF[month] <- sample(lwJF, 1) * Offtake_JF
      res_vec$Offtake_Liveweight_kg_JM[month] <- sample(lwJM, 1) * Offtake_JM
      res_vec$Offtake_Liveweight_kg_AF[month] <- sample(lwAF, 1) * Offtake_AF
      res_vec$Offtake_Liveweight_kg_AM[month] <- sample(lwAM, 1) * Offtake_AM
      
      res_vec$Offtake_Liveweight_kg[month] <- sum(res_vec$Offtake_Liveweight_kg_JF[month],
                                              res_vec$Offtake_Liveweight_kg_JM[month],
                                              res_vec$Offtake_Liveweight_kg_AF[month],
                                              res_vec$Offtake_Liveweight_kg_AM[month])

      
      res_vec$Quant_Meat_kg[month] = Meat_kg + sum(res_vec$Offtake_Liveweight_kg_JF[month], 
                                                  res_vec$Offtake_Liveweight_kg_JM[month],
                                                  res_vec$Offtake_Liveweight_kg_AF[month],
                                                  res_vec$Offtake_Liveweight_kg_AM[month]) * ccy
      
      if (species == "cattle") { 
        res_vec$Offtake_Liveweight_kg_O[month] <- sample(lwO, 1) * Offtake_O
        res_vec$Offtake_Liveweight_kg[month] <-  res_vec$Offtake_Liveweight_kg[month] + res_vec$Offtake_Liveweight_kg_O[month]
        res_vec$Quant_Meat_kg[month] <- res_vec$Quant_Meat_kg[month] + res_vec$Offtake_Liveweight_kg_O[month] * ccy
      }
      
      Meat_kg <- res_vec$Quant_Meat_kg[month]
      
      res_vec$Cumulative_draught_income[month] <- Draught_income + O * sample(draught_rate, 1) * draught_day_value * 30
      Draught_income <-  res_vec$Cumulative_draught_income[month]
      
      res_vec$Quant_Hides_JF[month] <- Hides_JF +  res_vec$Deaths_JF[month] * hides_rate_mor
      res_vec$Quant_Hides_JM[month] <- Hides_JM + res_vec$Deaths_JM[month] * hides_rate_mor
      res_vec$Quant_Hides_AF[month] <- Hides_AF + res_vec$Deaths_AF[month] * hides_rate_mor
      res_vec$Quant_Hides_AM[month] <- Hides_AM + res_vec$Deaths_AM[month] * hides_rate_mor
      
      Hides_JF <- res_vec$Quant_Hides_JF[month]
      Hides_JM <- res_vec$Quant_Hides_JM[month]
      Hides_AF <- res_vec$Quant_Hides_AF[month]
      Hides_AM <- res_vec$Quant_Hides_AM[month]
      
      res_vec$Quant_Hides[month] <- sum(res_vec$Quant_Hides_JF[month],
                                       res_vec$Quant_Hides_JM[month],
                                       res_vec$Quant_Hides_AF[month],
                                       res_vec$Quant_Hides_AM[month])
      
      if (species == "cattle") {
        res_vec$Quant_Hides_O[month] = Hides_O + res_vec$Deaths_O[month] * hides_rate_mor 
        Hides_O <- res_vec$Quant_Hides_O[month]
        res_vec$Quant_Hides[month] <- res_vec$Quant_Hides[month] + res_vec$Quant_Hides_O[month]
      }
      
      Hides <- res_vec$Quant_Hides[month]
      
      res_vec$Quant_Milk[month] <- Milk + AF * sample(part, 1)/12 * prop_F_milked * sample(lac_duration, 1) * sample(avg_daily_yield_ltr, 1) 
      
      Milk <- res_vec$Quant_Milk[month]

      res_vec$Quant_Manure_NF[month] <- Manure_kg_NF + NF * sample(Man_N, 1) * 30  
      res_vec$Quant_Manure_NM[month] <- Manure_kg_NM + NM * sample(Man_N, 1) * 30 
      res_vec$Quant_Manure_JF[month] <- Manure_kg_JF + JF * sample(Man_J, 1) * 30
      res_vec$Quant_Manure_JM[month] <- Manure_kg_JM + JM * sample(Man_J, 1) * 30 
      res_vec$Quant_Manure_AF[month] <- Manure_kg_AF + AF * sample(Man_A, 1) * 30
      res_vec$Quant_Manure_AM[month] <- Manure_kg_AM + AM * sample(Man_A, 1) * 30
      
      Manure_kg_NF <-  res_vec$Quant_Manure_NF[month]
      Manure_kg_NM <-  res_vec$Quant_Manure_NM[month]
      Manure_kg_JF <-  res_vec$Quant_Manure_JF[month]
      Manure_kg_JM <-  res_vec$Quant_Manure_JM[month]
      Manure_kg_AF <-  res_vec$Quant_Manure_AF[month]
      Manure_kg_AM <-  res_vec$Quant_Manure_AM[month]
      
      res_vec$Quant_Manure[month] <- sum(res_vec$Quant_Manure_NF[month],
                                         res_vec$Quant_Manure_NM[month],
                                         res_vec$Quant_Manure_JF[month],
                                         res_vec$Quant_Manure_JM[month],
                                         res_vec$Quant_Manure_AF[month],
                                         res_vec$Quant_Manure_AM[month])
      
      if (species == "cattle") {
        res_vec$Quant_Manure_O[month] <- Manure_kg_O + O * sample(Man_A, 1) * 30 
        Manure_kg_O <-  res_vec$Quant_Manure_O[month]
        res_vec$Quant_Manure[month] <- res_vec$Quant_Manure[month] + res_vec$Quant_Manure_O[month]
      }
      
      Manure_kg <- res_vec$Quant_Manure[month]
      
      res_vec$Cumulative_Dry_Matter_NF[month] <- Cumulative_DM_NF + NF * sample(kg_DM_req_NF, 1) * 30 
      res_vec$Cumulative_Dry_Matter_NM[month] <- Cumulative_DM_NM + NM * sample(kg_DM_req_NM, 1) * 30
      res_vec$Cumulative_Dry_Matter_JF[month] <- Cumulative_DM_JF + JF * sample(kg_DM_req_JF, 1) * 30 
      res_vec$Cumulative_Dry_Matter_JM[month] <- Cumulative_DM_JM + JM * sample(kg_DM_req_JM, 1) * 30 
      res_vec$Cumulative_Dry_Matter_AF[month] <- Cumulative_DM_AF + AF * sample(kg_DM_req_AF, 1) * 30
      res_vec$Cumulative_Dry_Matter_AM[month] <- Cumulative_DM_AM + AM * sample(kg_DM_req_AM, 1) * 30
      
      Cumulative_DM_NF <- res_vec$Cumulative_Dry_Matter_NF[month]
      Cumulative_DM_NM <- res_vec$Cumulative_Dry_Matter_NM[month]
      Cumulative_DM_JF <- res_vec$Cumulative_Dry_Matter_JF[month]
      Cumulative_DM_JM <- res_vec$Cumulative_Dry_Matter_JM[month]
      Cumulative_DM_AF <- res_vec$Cumulative_Dry_Matter_AF[month]
      Cumulative_DM_AM <- res_vec$Cumulative_Dry_Matter_AM[month]
      
      res_vec$Cumulative_Dry_Matter[month] <- sum(res_vec$Cumulative_Dry_Matter_NF[month],
                                                  res_vec$Cumulative_Dry_Matter_NM[month],
                                                  res_vec$Cumulative_Dry_Matter_JF[month],
                                                  res_vec$Cumulative_Dry_Matter_JM[month],
                                                  res_vec$Cumulative_Dry_Matter_AF[month],
                                                  res_vec$Cumulative_Dry_Matter_AM[month])
      
      if (species == "cattle") {
        res_vec$Cumulative_Dry_Matter_O[month] <- Cumulative_DM_O + O * sample(kg_DM_req_O, 1) * 30
        Cumulative_DM_O <- res_vec$Cumulative_Dry_Matter_O[month]
        res_vec$Cumulative_Dry_Matter[month] <-  res_vec$Cumulative_Dry_Matter[month] +  res_vec$Cumulative_Dry_Matter_O[month]
      }
      
      Cumulative_DM <- res_vec$Cumulative_Dry_Matter[month]
      
      ### Offtake value ###
      
      res_vec$Value_Offtake_JF[month] <- sample(fvJF, 1) * Offtake_JF 
      Value_offt_JF <- res_vec$Value_Offtake_JF[month]
      
      res_vec$Value_Offtake_JM[month] <- sample(fvJM, 1) * Offtake_JM
      Value_offt_JM <- res_vec$Value_Offtake_JM[month]
      
      res_vec$Value_Offtake_AF[month] <- sample(fvAF, 1) * Offtake_AF
      Value_offt_AF <-  res_vec$Value_Offtake_AF[month]
      
      res_vec$Value_Offtake_AM[month] <- sample(fvAM, 1) * Offtake_AM  
      Value_offt_AM <-  res_vec$Value_Offtake_AM[month]
      
      res_vec$Value_Offtake[month] = sum(res_vec$Value_Offtake_JF[month],
                                         res_vec$Value_Offtake_JM[month],
                                         res_vec$Value_Offtake_AF[month],
                                         res_vec$Value_Offtake_AM[month])
     
       if (species == "cattle") {
         res_vec$Value_Offtake_O[month] <- sample(fvO, 1) * Offtake_O
         Value_offt_O <- res_vec$Value_Offtake_O[month]
         res_vec$Value_Offtake[month] <- res_vec$Value_Offtake[month] + res_vec$Value_Offtake_O[month]
       }
      
      Value_offt <- res_vec$Value_Offtake[month] 
      
      ### Herd increase value ###
      
      res_vec$Value_Herd_Increase_NF[month] <- (NF - N_NF_t0) * sample(fvNF, 1)
      Value_herd_inc_NF <- res_vec$Value_Herd_Increase_NF[month]
      
      res_vec$Value_Herd_Increase_NM[month] <- (NM - N_NM_t0) * sample(fvNM, 1)
      Value_herd_inc_NM <- res_vec$Value_Herd_Increase_NM[month]
      
      res_vec$Value_Herd_Increase_JF[month] <- (JF - N_JF_t0) * sample(fvJF, 1)
      Value_herd_inc_JF <- res_vec$Value_Herd_Increase_JF[month]
      
      res_vec$Value_Herd_Increase_JM[month] <- (JM - N_JM_t0) * sample(fvJM, 1)
      Value_herd_inc_JM <- res_vec$Value_Herd_Increase_JM[month]
      
      res_vec$Value_Herd_Increase_AF[month] <- (AF - N_AF_t0) * sample(fvAF, 1)
      Value_herd_inc_AF <- res_vec$Value_Herd_Increase_AF[month]
      
      res_vec$Value_Herd_Increase_AM[month] <- (AM - N_AM_t0) * sample(fvAM, 1)
      Value_herd_inc_AM <- res_vec$Value_Herd_Increase_AM[month]
      
      res_vec$Value_Herd_Increase[month] <- sum(res_vec$Value_Herd_Increase_NF[month],
                                                res_vec$Value_Herd_Increase_NM[month],
                                                res_vec$Value_Herd_Increase_JF[month],
                                                res_vec$Value_Herd_Increase_JM[month],
                                                res_vec$Value_Herd_Increase_AF[month],
                                                res_vec$Value_Herd_Increase_AM[month])
      
      if (species == "cattle") {
        res_vec$Value_Herd_Increase_O[month] <- (O - N_O_t0) * (sample(fvO, 1))
        Value_herd_inc_O <- res_vec$Value_Herd_Increase_O[month]
        res_vec$Value_Herd_Increase[month] <- res_vec$Value_Herd_Increase[month] + res_vec$Value_Herd_Increase_O[month]
      }
      
      Value_herd_inc <- res_vec$Value_Herd_Increase[month]
      
      ### Total value increase ###
      
      res_vec$Total_Value_increase[month] <- Value_herd_inc + Value_offt
      res_vec$Total_Value_increase_NF[month] <- Value_herd_inc_NF 
      res_vec$Total_Value_increase_NM[month] <- Value_herd_inc_NM 
      res_vec$Total_Value_increase_JF[month] <- Value_herd_inc_JF + Value_offt_JF
      res_vec$Total_Value_increase_JM[month] <- Value_herd_inc_JM + Value_offt_JM
      res_vec$Total_Value_increase_AF[month] <- Value_herd_inc_AF + Value_offt_AF
      res_vec$Total_Value_increase_AM[month] <- Value_herd_inc_AM + Value_offt_AM
      
      if (species == "cattle") {
        res_vec$Total_Value_increase_O[month] <- Value_herd_inc_O + Value_offt_O
        
      }
      
      ### Feed cost ### 
      
      res_vec$Feed_cost_NF[month] <- Feed_NF + NF * sample(Expenditure_on_feed_NF, 1) * 30 
      Feed_NF <- res_vec$Feed_cost_NF[month]
      res_vec$Feed_cost_NM[month] <- Feed_NM + NM * sample(Expenditure_on_feed_NM, 1) * 30 
      Feed_NM <- res_vec$Feed_cost_NM[month]
      res_vec$Feed_cost_JF[month] <- Feed_JF + JF * sample(Expenditure_on_feed_JF, 1) * 30
      Feed_JF <- res_vec$Feed_cost_JF[month]
      res_vec$Feed_cost_JM[month] <- Feed_JM + JM * sample(Expenditure_on_feed_JM, 1) * 30
      Feed_JM <- res_vec$Feed_cost_JM[month]
      res_vec$Feed_cost_AF[month] <- Feed_AF + AF * sample(Expenditure_on_feed_AF, 1) * 30
      Feed_AF <- res_vec$Feed_cost_AF[month]
      res_vec$Feed_cost_AM[month] <- Feed_AM + AM * sample(Expenditure_on_feed_AM, 1) * 30 
      Feed_AM <- res_vec$Feed_cost_AM[month]
     
      res_vec$Feed_cost[month] <- sum(res_vec$Feed_cost_NF[month],
                                      res_vec$Feed_cost_NM[month],
                                      res_vec$Feed_cost_JF[month],
                                      res_vec$Feed_cost_JM[month],
                                      res_vec$Feed_cost_AF[month],
                                      res_vec$Feed_cost_AM[month])
      
      if (species == "cattle") {
        res_vec$Feed_cost_O[month] <- Feed_O + O * sample(Expenditure_on_feed_O, 1) * 30
        Feed_O <- res_vec$Feed_cost_O[month]
        res_vec$Feed_cost[month] <- res_vec$Feed_cost[month] + res_vec$Feed_cost_O[month]
      }
      
      Feed <- res_vec$Feed_cost[month]
      
      ### Labour cost ###
      
      res_vec$Labour_cost_NF[month] <- Labour_NF + NF * sample(Labour_cattle, 1) * lab_non_health 
      res_vec$Labour_cost_NM[month] <- Labour_NM + NM * sample(Labour_cattle, 1) * lab_non_health  
      res_vec$Labour_cost_JF[month] <- Labour_JF + JF * sample(Labour_cattle, 1) * lab_non_health  
      res_vec$Labour_cost_JM[month] <- Labour_JM + JM * sample(Labour_cattle, 1) * lab_non_health  
      res_vec$Labour_cost_AF[month] <- Labour_AF + AF * sample(Labour_cattle, 1) * lab_non_health + AF * prop_F_milked * sample(Labour_dairy, 1)  
      res_vec$Labour_cost_AM[month] <- Labour_AM + AM * sample(Labour_cattle, 1) * lab_non_health 
      
      Labour_NF <- res_vec$Labour_cost_NF[month]
      Labour_NM <- res_vec$Labour_cost_NM[month]
      Labour_JF <- res_vec$Labour_cost_JF[month]
      Labour_JM <- res_vec$Labour_cost_JM[month]
      Labour_AF <- res_vec$Labour_cost_AF[month]
      Labour_AM <- res_vec$Labour_cost_AM[month]
      
      res_vec$Labour_cost[month] <- sum(res_vec$Labour_cost_NF[month],
                                        res_vec$Labour_cost_NM[month],
                                        res_vec$Labour_cost_JF[month],
                                        res_vec$Labour_cost_JM[month],
                                        res_vec$Labour_cost_AF[month],
                                        res_vec$Labour_cost_AM[month])
      Labour =  res_vec$Labour_cost[month]
      
      if (species == "cattle") {
        res_vec$Labour_cost_O[month] <- Labour_O + O * sample(Labour_cattle, 1) * lab_non_health + O * sample(Labour_Oxen, 1)
        Labour_O <- res_vec$Labour_cost_O[month]
        res_vec$Labour_cost[month] <- res_vec$Labour_cost[month] + res_vec$Labour_cost_O[month]
      }
      
      ### Health cost ###
      
      res_vec$Health_cost_NF[month] <- Health_NF + NF * sample(Health_exp_prev, 1) + NF * sample(Health_exp_treatment, 1) 
      res_vec$Health_cost_NM[month] <- Health_NM + NM * sample(Health_exp_prev, 1) + NM * sample(Health_exp_treatment, 1) 
      res_vec$Health_cost_JF[month] <- Health_JF + JF * sample(Health_exp_prev, 1) + JF * sample(Health_exp_treatment, 1)
      res_vec$Health_cost_JM[month] <- Health_JM + JM * sample(Health_exp_prev, 1) + JM * sample(Health_exp_treatment, 1)
      res_vec$Health_cost_AF[month] <- Health_AF + AF * sample(Health_exp_prev, 1) + AF * sample(Health_exp_treatment, 1)
      res_vec$Health_cost_AM[month] <- Health_AM + AM * sample(Health_exp_prev, 1) + AM * sample(Health_exp_treatment, 1)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
      Health_NF <- res_vec$Health_cost_NF[month]
      Health_NM <- res_vec$Health_cost_NM[month]
      Health_JF <- res_vec$Health_cost_JF[month]
      Health_JM <- res_vec$Health_cost_JM[month]
      Health_AF <- res_vec$Health_cost_AF[month]
      Health_AM <- res_vec$Health_cost_AM[month]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
      res_vec$Health_cost[month] <- sum(res_vec$Health_cost_NF[month],
                                        res_vec$Health_cost_NM[month],
                                        res_vec$Health_cost_JF[month],
                                        res_vec$Health_cost_JM[month],
                                        res_vec$Health_cost_AF[month],
                                        res_vec$Health_cost_AM[month])
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
     Health = res_vec$Health_cost[month]
                                                                                                                                                                                                                                                                                                                                                                                                                                               
      if (species == "cattle") {
        res_vec$Health_cost_O[month] <- Health_O + O * sample(Health_exp_prev, 1) + O * sample(Health_exp_treatment, 1) 
        Health_O <- res_vec$Health_cost_O[month]                                                                                
        res_vec$Health_cost[month] <-  res_vec$Health_cost[month] + res_vec$Health_cost_O[month]
      } 
     
      ### Capital cost ###
     
     res_vec$Capital_cost_NF[month] <- res_vec$NumNF[1] * sample(fvNF, 1) * Interest_rate 
     Capital_NF <- res_vec$Capital_cost_NF[month]
     
     res_vec$Capital_cost_NM[month] <- res_vec$NumNM[1] * sample(fvNM, 1) * Interest_rate  
     Capital_NM <- res_vec$Capital_cost_NM[month]
     
     res_vec$Capital_cost_JF[month] <-res_vec$NumJF[1] * sample(fvJF, 1) * Interest_rate  
     Capital_JF = res_vec$Capital_cost_JF[month]
     
     res_vec$Capital_cost_JM[month] <- res_vec$NumJM[1] * sample(fvJM, 1) * Interest_rate  
     Capital_JM <- res_vec$Capital_cost_JM[month]
     
     res_vec$Capital_cost_AF[month] <- res_vec$NumAF[1] * sample(fvAF, 1) * Interest_rate  
     Capital_AF <- res_vec$Capital_cost_AF[month]
     
     res_vec$Capital_cost_AM[month] <- res_vec$NumAM[1] * sample(fvAM, 1) * Interest_rate  
     Capital_AM <- res_vec$Capital_cost_AM[month]
     
     res_vec$Capital_cost[month] <- sum(res_vec$Capital_cost_NF[month],
                                       res_vec$Capital_cost_NM[month],
                                       res_vec$Capital_cost_JF[month],
                                       res_vec$Capital_cost_JM[month],
                                       res_vec$Capital_cost_AF[month],
                                       res_vec$Capital_cost_AM[month])
     
     if (species == "cattle") {
       res_vec$Capital_cost_O[month] <- res_vec$NumO[1] * sample(fvO, 1) * Interest_rate
       Capital_O <- res_vec$Capital_cost_O[month]
       res_vec$Capital_cost[month] <- res_vec$Capital_cost[month] + res_vec$Capital_cost_O[month]
     }
     
     Capital <- res_vec$Capital_cost[month]
     
     ### Infrastructure cost ###
     
     res_vec$Infrastructure_cost_NF[month] <- N_NF_t0 * sample(Infrastructure_per_head, 1)
     res_vec$Infrastructure_cost_NM[month] <- N_NM_t0 * sample(Infrastructure_per_head, 1)
     res_vec$Infrastructure_cost_JF[month] <- N_JF_t0 * sample(Infrastructure_per_head, 1)
     res_vec$Infrastructure_cost_JM[month] <- N_JM_t0 * sample(Infrastructure_per_head, 1)
     res_vec$Infrastructure_cost_AF[month] <- N_AF_t0 * sample(Infrastructure_per_head, 1)
     res_vec$Infrastructure_cost_AM[month] <- N_AM_t0 * sample(Infrastructure_per_head, 1)
     
     res_vec$Infrastructure_cost[month] <- sum(res_vec$Infrastructure_cost_NF[month],
                                               res_vec$Infrastructure_cost_NM[month],
                                               res_vec$Infrastructure_cost_JF[month],
                                               res_vec$Infrastructure_cost_JM[month],
                                               res_vec$Infrastructure_cost_AF[month],
                                               res_vec$Infrastructure_cost_AM[month])
     
     if (species == "cattle") {
       res_vec$Infrastructure_cost_O[month] <- N_O_t0 * sample(Infrastructure_per_head, 1)
       res_vec$Infrastructure_cost[month] <- res_vec$Infrastructure_cost[month] + res_vec$Infrastructure_cost_O[month]
     }
     
     # Total expenditure ###
     
     res_vec$Total_expenditure[month] =  res_vec$Feed_cost[month] + Health + Labour + Capital + res_vec$Infrastructure_cost[month]
     
     res_vec$Total_expenditure_NF[month] =  Feed_NF + Health_NF + Labour_NF + Capital_NF + res_vec$Infrastructure_cost_NF[month]
     res_vec$Total_expenditure_NM[month] =  Feed_NM + Health_NM + Labour_NM + Capital_NM + res_vec$Infrastructure_cost_NM[month]
     res_vec$Total_expenditure_JF[month] =  Feed_JF + Health_JF + Labour_JF + Capital_JF + res_vec$Infrastructure_cost_JF[month]
     res_vec$Total_expenditure_JM[month] =  Feed_JM + Health_JM + Labour_JM + Capital_JM + res_vec$Infrastructure_cost_JM[month]
     res_vec$Total_expenditure_AF[month] =  Feed_AF + Health_AF + Labour_AF + Capital_AF + res_vec$Infrastructure_cost_AF[month]
     res_vec$Total_expenditure_AM[month] =  Feed_AM + Health_AM + Labour_AM + Capital_AM + res_vec$Infrastructure_cost_AM[month]
     
     if (species == "cattle"){
       res_vec$Total_expenditure_O[month] =  Feed_O + Health_O + Labour_O + Capital_O + res_vec$Infrastructure_cost_O[month]
     }

    } # end Num_months loop
    
    ### Fill matrices ###
    
    res_mat$NumNF[i, ] <- res_vec$NumNF
    res_mat$NumJF[i, ] <- res_vec$NumJF
    res_mat$NumAF[i, ] <- res_vec$NumAF
    res_mat$NumNM[i, ] <- res_vec$NumNM
    res_mat$NumJM[i, ] <- res_vec$NumJM
    res_mat$NumAM[i, ] <- res_vec$NumAM
    res_mat$NumN[i, ] <- res_vec$NumN
    
    res_mat$Monthly_mortality[i, ] <- res_vec$Monthly_mortality
    res_mat$Total_Mortality[i, ] <- res_vec$Total_Mortality
    
    res_mat$Total_Mortality_NF[i, ] <- res_vec$Total_Mortality_NF
    res_mat$Total_Mortality_NM[i, ] <- res_vec$Total_Mortality_NM
    res_mat$Total_Mortality_JF[i, ] <- res_vec$Total_Mortality_JF
    res_mat$Total_Mortality_JM[i, ] <- res_vec$Total_Mortality_JM
    res_mat$Total_Mortality_AF[i, ] <- res_vec$Total_Mortality_AF
    res_mat$Total_Mortality_AM[i, ] <- res_vec$Total_Mortality_AM
    
    res_mat$Value_of_Total_Mortality[i, ] <- res_vec$Value_of_Total_Mortality
    
    res_mat$Value_of_Total_Mortality_NF[i, ] <- res_vec$Value_of_Total_Mortality_NF
    res_mat$Value_of_Total_Mortality_NM[i, ] <- res_vec$Value_of_Total_Mortality_NM
    res_mat$Value_of_Total_Mortality_JF[i, ] <- res_vec$Value_of_Total_Mortality_JF
    res_mat$Value_of_Total_Mortality_JM[i, ] <- res_vec$Value_of_Total_Mortality_JM
    res_mat$Value_of_Total_Mortality_AF[i, ] <- res_vec$Value_of_Total_Mortality_AF
    res_mat$ Value_of_Total_Mortality_AM[i, ] <- res_vec$Value_of_Total_Mortality_AM
    
    res_mat$Quant_Liveweight_kg[i, ] <- res_vec$Quant_Liveweight_kg
    
    res_mat$Quant_Liveweight_kg_NF[i, ] <- res_vec$Quant_Liveweight_kg_NF
    res_mat$Quant_Liveweight_kg_NM[i, ] <- res_vec$Quant_Liveweight_kg_NM
    res_mat$Quant_Liveweight_kg_JF[i, ] <- res_vec$Quant_Liveweight_kg_JF
    res_mat$Quant_Liveweight_kg_JM[i, ] <- res_vec$Quant_Liveweight_kg_JM
    res_mat$Quant_Liveweight_kg_AF[i, ] <- res_vec$Quant_Liveweight_kg_AF
    res_mat$Quant_Liveweight_kg_AM[i, ] <- res_vec$Quant_Liveweight_kg_AM
    
    res_mat$Cumulative_draught_income[i,] <- res_vec$Cumulative_draught_income

    res_mat$Quant_Meat_kg[i, ] <- res_vec$Quant_Meat_kg
    
    res_mat$Num_Offtake[i, ] <- res_vec$Num_Offtake
    
    res_mat$Num_Offtake_NF[i, ] <- res_vec$Num_Offtake_NF
    res_mat$Num_Offtake_NM[i, ] <- res_vec$Num_Offtake_NM
    res_mat$Num_Offtake_JF[i, ] <- res_vec$Num_Offtake_JF
    res_mat$Num_Offtake_JM[i, ] <- res_vec$Num_Offtake_JM
    res_mat$Num_Offtake_AF[i, ] <- res_vec$Num_Offtake_AF
    res_mat$Num_Offtake_AM[i, ] <- res_vec$Num_Offtake_AM
    
    res_mat$Offtake_Liveweight_kg[i, ] <- res_vec$Offtake_Liveweight_kg
    
    res_mat$Offtake_Liveweight_kg_JF[i, ] <- res_vec$Offtake_Liveweight_kg_JF
    res_mat$Offtake_Liveweight_kg_JM[i, ] <- res_vec$Offtake_Liveweight_kg_JM
    res_mat$Offtake_Liveweight_kg_AF[i, ] <- res_vec$Offtake_Liveweight_kg_AF
    res_mat$Offtake_Liveweight_kg_AM[i, ] <- res_vec$Offtake_Liveweight_kg_AM
    
    res_mat$Pop_growth[i, ] <- res_vec$Pop_growth
    
    res_mat$Pop_growth_NF[i, ] <- res_vec$Pop_growth_NF
    res_mat$Pop_growth_NM[i, ] <- res_vec$Pop_growth_NM
    res_mat$Pop_growth_JF[i, ] <- res_vec$Pop_growth_JF
    res_mat$Pop_growth_JM[i, ] <- res_vec$Pop_growth_JM
    res_mat$Pop_growth_AF[i, ] <- res_vec$Pop_growth_AF
    res_mat$Pop_growth_AM[i, ] <- res_vec$Pop_growth_AM
    
    res_mat$Monthly_growth_rate[i, ] <- res_vec$Monthly_growth_rate
    res_mat$Monthly_pop_growth[i, ] <- res_vec$Monthly_pop_growth
    
    res_mat$Quant_Manure[i, ] <- res_vec$Quant_Manure
    res_mat$Quant_Manure_NF[i, ] <- res_vec$Quant_Manure_NF
    res_mat$Quant_Manure_NM[i, ] <- res_vec$Quant_Manure_NM
    res_mat$Quant_Manure_JF[i, ] <- res_vec$Quant_Manure_JF
    res_mat$Quant_Manure_JM[i, ] <- res_vec$Quant_Manure_JM
    res_mat$Quant_Manure_AF[i, ] <- res_vec$Quant_Manure_AF
    res_mat$Quant_Manure_AM[i, ] <- res_vec$Quant_Manure_AM
    
    res_mat$Quant_Hides[i, ] <- res_vec$Quant_Hides
    
    res_mat$Quant_Hides_JF[i, ] <- res_vec$Quant_Hides_JF
    res_mat$Quant_Hides_JM[i, ] <- res_vec$Quant_Hides_JM
    res_mat$Quant_Hides_AF[i, ] <- res_vec$Quant_Hides_AF
    res_mat$Quant_Hides_AM[i, ] <- res_vec$Quant_Hides_AM
    
    res_mat$Quant_Milk[i, ] <- res_vec$Quant_Milk
    res_mat$Quant_Wool[i, ] <- res_vec$Quant_Wool
    
    res_mat$Cumulative_Dry_Matter[i, ] <- res_vec$Cumulative_Dry_Matter
    
    res_mat$Cumulative_Dry_Matter_NF[i, ] <- res_vec$Cumulative_Dry_Matter_NF
    res_mat$Cumulative_Dry_Matter_NM[i, ] <- res_vec$Cumulative_Dry_Matter_NM
    res_mat$Cumulative_Dry_Matter_JF[i, ] <- res_vec$Cumulative_Dry_Matter_JF
    res_mat$Cumulative_Dry_Matter_JM[i, ] <- res_vec$Cumulative_Dry_Matter_JM
    res_mat$Cumulative_Dry_Matter_AF[i, ] <- res_vec$Cumulative_Dry_Matter_AF
    res_mat$Cumulative_Dry_Matter_AM[i, ] <- res_vec$Cumulative_Dry_Matter_AM
    
    res_mat$Monthly_DM[i, ] <- res_vec$Monthly_DM

    res_mat$Value_Offtake[i, ] <- res_vec$Value_Offtake
    res_mat$Value_Offtake_NF[i, ] <- res_vec$Value_Offtake_NF
    res_mat$Value_Offtake_NM[i, ] <- res_vec$Value_Offtake_NM
    
    res_mat$Value_Offtake_JF[i, ] <- res_vec$Value_Offtake_JF
    res_mat$Value_Offtake_JM[i, ] <- res_vec$Value_Offtake_JM
    res_mat$Value_Offtake_AF[i, ] <- res_vec$Value_Offtake_AF
    res_mat$Value_Offtake_AM[i, ] <- res_vec$Value_Offtake_AM
    
    res_mat$Value_Herd_Increase[i, ] <- res_vec$Value_Herd_Increase
    
    res_mat$Value_Herd_Increase_NF[i, ] <- res_vec$Value_Herd_Increase_NF
    res_mat$Value_Herd_Increase_NM[i, ] <- res_vec$Value_Herd_Increase_NM
    res_mat$Value_Herd_Increase_JF[i, ] <- res_vec$Value_Herd_Increase_JF
    res_mat$Value_Herd_Increase_JM[i, ] <- res_vec$Value_Herd_Increase_JM
    res_mat$Value_Herd_Increase_AF[i, ] <- res_vec$Value_Herd_Increase_AF
    res_mat$Value_Herd_Increase_AM[i, ] <- res_vec$Value_Herd_Increase_AM
    
    res_mat$Total_Value_increase[i, ] <- res_vec$Total_Value_increase
    
    res_mat$Total_Value_increase_NF[i, ] <- res_vec$Total_Value_increase_NF
    res_mat$Total_Value_increase_NM[i, ] <- res_vec$Total_Value_increase_NM
    res_mat$Total_Value_increase_JF[i, ] <- res_vec$Total_Value_increase_JF
    res_mat$Total_Value_increase_JM[i, ] <- res_vec$Total_Value_increase_JM
    res_mat$Total_Value_increase_AF[i, ] <- res_vec$Total_Value_increase_AF
    res_mat$Total_Value_increase_AM[i, ] <- res_vec$Total_Value_increase_AM
    
    res_mat$Feed_cost[i, ] <- res_vec$Feed_cost
    
    res_mat$Feed_cost_NF[i, ] <- res_vec$Feed_cost_NF
    res_mat$Feed_cost_NM[i, ] <- res_vec$Feed_cost_NM
    res_mat$Feed_cost_JF[i, ] <- res_vec$Feed_cost_JF
    res_mat$Feed_cost_JM[i, ] <- res_vec$Feed_cost_JM
    res_mat$Feed_cost_AF[i, ] <- res_vec$Feed_cost_AF
    res_mat$Feed_cost_AM[i, ] <- res_vec$Feed_cost_AM
    
    res_mat$Labour_cost[i, ] <- res_vec$Labour_cost
    
    res_mat$Labour_cost_NF[i, ] <- res_vec$Labour_cost_NF
    res_mat$Labour_cost_NM[i, ] <- res_vec$Labour_cost_NM
    res_mat$Labour_cost_JF[i, ] <- res_vec$Labour_cost_JF
    res_mat$Labour_cost_JM[i, ] <- res_vec$Labour_cost_JM
    res_mat$Labour_cost_AF[i, ] <- res_vec$Labour_cost_AF
    res_mat$Labour_cost_AM[i, ] <- res_vec$Labour_cost_AM
    
    res_mat$Health_cost[i, ] <- res_vec$Health_cost
    
    res_mat$Health_cost_NF[i, ] <- res_vec$Health_cost_NF
    res_mat$Health_cost_NM[i, ] <- res_vec$Health_cost_NM
    res_mat$Health_cost_JF[i, ] <- res_vec$Health_cost_JF
    res_mat$Health_cost_JM[i, ] <- res_vec$Health_cost_JM
    res_mat$Health_cost_AF[i, ] <- res_vec$Health_cost_AF
    res_mat$Health_cost_AM[i, ] <- res_vec$Health_cost_AM
    
    res_mat$Capital_cost[i, ] <- res_vec$Capital_cost
    
    res_mat$Capital_cost_NF[i, ] <- res_vec$Capital_cost_NF
    res_mat$Capital_cost_NM[i, ] <- res_vec$Capital_cost_NM
    res_mat$Capital_cost_JF[i, ] <- res_vec$Capital_cost_JF
    res_mat$Capital_cost_JM[i, ] <- res_vec$Capital_cost_JM
    res_mat$Capital_cost_AF[i, ] <- res_vec$Capital_cost_AF
    res_mat$Capital_cost_AM[i, ] <- res_vec$Capital_cost_AM
    
    res_mat$Infrastructure_cost[i, ] <- res_vec$Infrastructure_cost
    res_mat$Infrastructure_cost_NF[i, ] <- res_vec$Infrastructure_cost_NF
    res_mat$Infrastructure_cost_NM[i, ] <- res_vec$Infrastructure_cost_NM
    res_mat$Infrastructure_cost_JF[i, ] <- res_vec$Infrastructure_cost_JF
    res_mat$Infrastructure_cost_JM[i, ] <- res_vec$Infrastructure_cost_JM
    res_mat$Infrastructure_cost_AF[i, ] <- res_vec$Infrastructure_cost_AF
    res_mat$Infrastructure_cost_AM[i, ] <- res_vec$Infrastructure_cost_AM
    
    res_mat$Total_expenditure[i, ] <- res_vec$Total_expenditure

    res_mat$Total_expenditure_NF[i, ] <- res_vec$Total_expenditure_NF
    res_mat$Total_expenditure_NM[i, ] <- res_vec$Total_expenditure_NM
    res_mat$Total_expenditure_JF[i, ] <- res_vec$Total_expenditure_JF
    res_mat$Total_expenditure_JM[i, ] <- res_vec$Total_expenditure_JM
    res_mat$Total_expenditure_AF[i, ] <- res_vec$Total_expenditure_AF
    res_mat$Total_expenditure_AM[i, ] <- res_vec$Total_expenditure_AM
    
    if (species == "cattle") {
      res_mat$NumO[i, ] <- res_vec$NumO
      res_mat$Total_Mortality_O[i, ] <- res_vec$Total_Mortality_O
      res_mat$Total_Mortality_O[i, ] <- res_vec$Total_Mortality_O
      res_mat$Value_of_Total_Mortality_O[i, ] <- res_vec$Value_of_Total_Mortality_O
      res_mat$Quant_Liveweight_kg_O[i, ] <- res_vec$Quant_Liveweight_kg_O
      res_mat$Num_Offtake_O[i, ] <- res_vec$Num_Offtake_O
      res_mat$Offtake_Liveweight_kg_O[i, ] <- res_vec$Offtake_Liveweight_kg_O
      res_mat$Pop_growth_O[i, ] <- res_vec$Pop_growth_O
      res_mat$Quant_Manure_O[i, ] <- res_vec$Quant_Manure_O
      res_mat$Quant_Hides_O[i, ] <- res_vec$Quant_Hides_O
      res_mat$Cumulative_Dry_Matter_O[i, ] <- res_vec$Cumulative_Dry_Matter_O
      res_mat$Value_Offtake_O[i, ] <- res_vec$Value_Offtake_O
      res_mat$Value_Herd_Increase_O[i, ] <- res_vec$Value_Herd_Increase_O
      res_mat$Total_Value_increase_O[i, ] <- res_vec$Total_Value_increase_O
      res_mat$Feed_cost_O[i, ] <- res_vec$Feed_cost_O
      res_mat$Labour_cost_O[i, ] <- res_vec$Labour_cost_O
      res_mat$Health_cost_O[i, ] <- res_vec$Health_cost_O
      res_mat$Capital_cost_O[i, ] <- res_vec$Capital_cost_O
      res_mat$Infrastructure_cost_O[i, ] <- res_vec$Infrastructure_cost_O
      res_mat$Total_expenditure_O[i, ] <- res_vec$Total_expenditure_O
    }
    
  } # end nruns loop
  
  Total_number_change_NF <- res_mat$Num_Offtake_NF + res_mat$Pop_growth_NF
  Total_number_change_NM <- res_mat$Num_Offtake_NM + res_mat$Pop_growth_NM
  Total_number_change_JF <- res_mat$Num_Offtake_JF + res_mat$Pop_growth_JF
  Total_number_change_JM <- res_mat$Num_Offtake_JM + res_mat$Pop_growth_JM
  Total_number_change_AF <- res_mat$Num_Offtake_AF + res_mat$Pop_growth_AF
  Total_number_change_AM <- res_mat$Num_Offtake_AM + res_mat$Pop_growth_AM
  
  Total_number_change <- sum(Total_number_change_NF,
                             Total_number_change_NM,
                             Total_number_change_JF,
                             Total_number_change_JM,
                             Total_number_change_AF,
                             Total_number_change_AM)

  Value_Milk <- res_mat$Quant_Milk * milk_value_ltr
  
  Value_Hides_JF <- res_mat$Quant_Hides_JF * sample(hides_value, 1)
  Value_Hides_JM <- res_mat$Quant_Hides_JM * sample(hides_value, 1)
  Value_Hides_AF <- res_mat$Quant_Hides_AF * sample(hides_value, 1)
  Value_Hides_AM <- res_mat$Quant_Hides_AM * sample(hides_value, 1)
  
  Value_Hides <- sum(Value_Hides_JF,
                     Value_Hides_JM,
                     Value_Hides_AF,
                     Value_Hides_AM)
                     
  Value_Manure <- res_mat$Quant_Manure * Man_value
  Value_Manure_NF <- res_mat$Quant_Manure_NF * Man_value
  Value_Manure_NM <- res_mat$Quant_Manure_NM * Man_value
  Value_Manure_JF <- res_mat$Quant_Manure_JF * Man_value
  Value_Manure_JM <- res_mat$Quant_Manure_JM * Man_value
  Value_Manure_AF <- res_mat$Quant_Manure_AF * Man_value
  Value_Manure_AM <- res_mat$Quant_Manure_AM * Man_value
  
  Production_value_herd_offtake_hide_manure_NF <- res_mat$Total_Value_increase_NF + res_mat$Value_Manure_NF
  Production_value_herd_offtake_hide_manure_NM <- res_mat$Total_Value_increase_NM + res_mat$Value_Manure_NM
  Production_value_herd_offtake_hide_manure_JF <- res_mat$Total_Value_increase_JF + res_mat$Value_Manure_JF + res_mat$Value_Hides_JF
  Production_value_herd_offtake_hide_manure_JM <- res_mat$Total_Value_increase_JM + res_mat$Value_Manure_JM + Value_Hides_JM
  Production_value_herd_offtake_hide_manure_AF <- res_mat$Total_Value_increase_AF + res_mat$Value_Manure_AF + res_mat$Value_Hides_AF + res_mat$Value_Milk
  Production_value_herd_offtake_hide_manure_AM <- res_mat$Total_Value_increase_AM + res_mat$Value_Manure_AM + res_mat$Value_Hides_AM
  
  Production_value_herd_offtake_hide_manure <- sum(Production_value_herd_offtake_hide_manure_NF,
                                                 Production_value_herd_offtake_hide_manure_NM, 
                                                 Production_value_herd_offtake_hide_manure_JF,
                                                 Production_value_herd_offtake_hide_manure_JM + 
                                                 Production_value_herd_offtake_hide_manure_AF,
                                                 Production_value_herd_offtake_hide_manure_AM)
  
  Gross_margin <- res_mat$Production_value_herd_offtake_hide_manure - res_mat$Total_expenditure
  Gross_margin_NF <- res_mat$Production_value_herd_offtake_hide_manure_NF - res_mat$Total_expenditure_NF
  Gross_margin_NM <- res_mat$Production_value_herd_offtake_hide_manure_NM - res_mat$Total_expenditure_NM
  Gross_margin_JF <- res_mat$Production_value_herd_offtake_hide_manure_JF - res_mat$Total_expenditure_JF
  Gross_margin_JM <- res_mat$Production_value_herd_offtake_hide_manure_JM - res_mat$Total_expenditure_JM
  Gross_margin_AF <- res_mat$Production_value_herd_offtake_hide_manure_AF - res_mat$Total_expenditure_AF
  Gross_margin_AM <- res_mat$Production_value_herd_offtake_hide_manure_AM - res_mat$Total_expenditure_AM
  
  Num_Offtake_N <- res_mat$Num_Offtake_NF + res_mat$Num_Offtake_NM
  Num_Offtake_J <- res_mat$Num_Offtake_JF + res_mat$Num_Offtake_JM
  
  Pop_growth_N <- res_mat$Pop_growth_NF + res_mat$Pop_growth_NM
  Pop_growth_J_M <- res_mat$Pop_growth_JF + res_mat$Pop_growth_JM

  Total_number_change_N <- res_mat$Num_Offtake_N + res_mat$Pop_growth_N
  Total_number_change_J <- res_mat$Num_Offtake_J + res_mat$Pop_growth_J
  
  Total_Mortality_N <- res_mat$Total_Mortality_NF + res_mat$Total_Mortality_NM
  Total_Mortality_J <- res_mat$Total_Mortality_JF + res_mat$Total_Mortality_JM
  
  Value_of_Total_Mortality_N <- res_mat$Value_of_Total_Mortality_NF + res_mat$Value_of_Total_Mortality_NM
  Value_of_Total_Mortality_J <- res_mat$Value_of_Total_Mortality_JF + res_mat$Value_of_Total_Mortality_JM
  
  Quant_Liveweight_kg_J <- res_mat$Quant_Liveweight_kg_JF + res_mat$Quant_Liveweight_kg_JM
  
  Quant_Manure_N <- res_mat$Quant_Manure_NF + res_mat$Quant_Manure_NM
  Quant_Manure_J <- res_mat$Quant_Manure_JF + res_mat$Quant_Manure_JM
  
  Quant_Hides_J <- res_mat$Quant_Hides_JF + res_mat$Quant_Hides_JM
  
  Cumulative_Dry_Matter_N <- res_mat$Cumulative_Dry_Matter_NF + res_mat$Cumulative_Dry_Matter_NM
  Cumulative_Dry_Matter_J <- res_mat$Cumulative_Dry_Matter_JF + res_mat$Cumulative_Dry_Matter_JM
  
  Value_Offtake_N <- res_mat$Value_Offtake_NF + res_mat$Value_Offtake_NM
  Value_Offtake_J <- res_mat$Value_Offtake_JF + res_mat$Value_Offtake_JM
  
  Value_Herd_Increase_N <- res_mat$Value_Herd_Increase_NF + res_mat$Value_Herd_Increase_NM
  Value_Herd_Increase_J <- res_mat$Value_Herd_Increase_JF + res_mat$Value_Herd_Increase_JM
  
  Total_Value_increase_N <- res_mat$Total_Value_increase_NF + res_mat$Total_Value_increase_NM
  Total_Value_increase_J <- res_mat$Total_Value_increase_JF + res_mat$Total_Value_increase_JM
  
  Value_Manure_N <- res_mat$Value_Manure_NF + res_mat$Value_Manure_NM
  Value_Manure_J <- res_mat$Value_Manure_JF + res_mat$Value_Manure_JM
  
  Value_Hides_J <- res_mat$Value_Hides_JF + res_mat$Value_Hides_JM
  
  Production_value_herd_offteake_hide_manure_N <- res_mat$Production_value_herd_offtake_hide_manure_NF + res_mat$Production_value_herd_offtake_hide_manure_NM
  Production_value_herd_offteake_hide_manure_J <- res_mat$Production_value_herd_offtake_hide_manure_JF + res_mat$Production_value_herd_offtake_hide_manure_JM
  
  Feed_cost_N <- res_mat$Feed_cost_NF + res_mat$Feed_cost_NM
  Feed_cost_J <- res_mat$Feed_cost_JF + res_mat$Feed_cost_JM
  
  Labour_cost_N <- res_mat$Labour_cost_NF + res_mat$Labour_cost_NM
  Labour_cost_J <- res_mat$Labour_cost_JF + res_mat$Labour_cost_JM
  
  Health_cost_N <- res_mat$Health_cost_NF + res_mat$Health_cost_NM
  Health_cost_J <- res_mat$Health_cost_JF + res_mat$Health_cost_JM
  
  Capital_cost_N <- res_mat$Capital_cost_NF + res_mat$Capital_cost_NM
  Capital_cost_J <- res_mat$Capital_cost_JF + res_mat$Capital_cost_JM
  
  Infrastructure_cost_N <- res_mat$Infrastructure_cost_NF + res_mat$Infrastructure_cost_NM
  Infrastructure_cost_J <- res_mat$Infrastructure_cost_JF + res_mat$Infrastructure_cost_JM
  
  Total_expenditure_N <- res_mat$Total_expenditure_N + res_mat$Total_expenditure_NM
  Total_expenditure_J <- res_mat$Total_expenditure_JF + res_mat$Total_expenditure_JM
  
  Gross_margin_N <- res_mat$Gross_margin_NF + res_mat$Gross_margin_NM
  Gross_margin_J <- res_mat$Gross_margin_JF + res_mat$Gross_margin_JM
  
  if (species == "cattle") {
    Total_number_change_O <- res_mat$Num_Offtake_O + res_mat$Pop_growth_O
    Total_number_change <- res_mat$Total_number_change + res_mat$Total_number_change_O
    Value_Hides_O <- res_mat$Quant_Hides_AM * sample(hides_value, 1)
    Value_Hides <- res_mat$Value_Hides + res_mat$Value_Hides_O
    Value_Manure_O <- res_mat$Quant_Manure_O * Man_value
    Production_value_herd_offtake_hide_manure_O <- res_mat$Total_Value_increase_O + res_mat$Value_Manure_O + res_mat$Value_Hides_O + res_mat$Cumulative_draught_income
    Production_value_herd_offtake_hide_manure <- res_mat$Production_value_herd_offtake_hide_manure + res_mat$Production_value_herd_offtake_hide_manure_O
    Gross_margin_O <- res_mat$Production_value_herd_offteake_hide_man_O - res_mat$Total_expenditure_O
  }
  
  # =================================================================
  # Summarize items and build data frame
  # =================================================================

  summary_df_updated <- build_summary_df(
    items_to_summarize = c("Num Offtake" = "Num_Offtake",
                           "Cumulative Pop Growth" = "Pop_growth",
                           "Total Number Increase" = "Total_number_change",
                           "Total Mortality" = "Total_Mortality",
                           "Value of Total Mortality" = "Value_of_Total_Mortality",
                           "Population Liveweight (kg)" = "Quant_Liveweight_kg",
                           "Offtake Liveweight (kg)" = "Offtake_Liveweight_kg",
                           "Meat (kg)" = "Quant_Meat_kg",
                           "Manure" = "Quant_Manure",
                           "Hides" = "Quant_Hides",
                           "Milk" = "Quant_Milk",
                           "Wool" = "Quant_Wool",
                           "Cumulative Dry Matter" = "Cumulative_Dry_Matter",
                           "Value of Offtake" = "Value_Offtake",
                           "Value of Draught" = "Cumulative_draught_income",
                           "Value of Herd Increase" = "Value_Herd_Increase",
                           "Value of Herd Increase and Offtake" = "Total_Value_increase",
                           "Value of Manure" = "Value_Manure",
                           "Value of Hides" = "Value_Hides",
                           "Value of Milk" = "Value_Milk",
                           "Total Production Value" = "Production_value_herd_offtake_hide_manure",
                           "Feed Cost" = "Feed_cost",
                           "Labour Cost" = "Labour_cost",
                           "Health Cost" = "Health_cost",
                           "Capital Cost" = "Capital_cost",
                           "Infrastructure Cost" = "Infrastructure_cost",
                           "Total Expenditure" = "Total_expenditure",
                           "Gross Margin" = "Gross_margin"
    )
  )
  print('Compartmental model finished.')
  return(list(Gross_margin_M[,12], summary_df_updated))
}
  
} # end function

