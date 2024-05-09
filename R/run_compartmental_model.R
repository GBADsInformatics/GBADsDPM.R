#' @title
#' Run the DPM/AHLE compartmental simulation model 
#' 
#' @description
#' Runs simulations for scenarios using parameters imported via \code{read_params(file_path)}. 
#' If desired, users can set a random seed to ensure reproducibility 
#' 
#' @example 
#' # run_compartmental_model(seed_value = NULL)
#' 

run_compartmental_model <- function(seed_value = NULL) {
  
  set.seed(seed_value)
  
  ### Functions ###
  
  calculate_mu <- function(part, prolif) {
    return ((sample(part, size = nruns, replace = TRUE) * sample(prolif, size = nruns, replace = TRUE)) / 12)
  }
    
  calculate_purchased_feed <- function(kg_dm_req, lskeepers_purch_feed, feed_paid_for, dm_in_feed) {
    return (kg_dm_req * lskeepers_purch_feed * feed_paid_for / dm_in_feed)
  }
  
  calculate_expenditure_on_feed <- function(kg_feed_purchased, feed_cost_kg) {
    return (kg_feed_purchased * feed_cost_kg)
  }
  
  calculate_dry_matter_requirements <- function(lw, prpn, dm_req_prpn) {
    return (dm_req_prpn * lw * prpn)
  }

  if (species == "cattle" || species == "smallruminants") {
    
    kg_DM_req_JF <- calculate_dry_matter_requirements(lwJF, prpn_lskeepers_purch_feed, DM_req_prpn_JF)
    kg_DM_req_JM <- calculate_dry_matter_requirements(lwJM, prpn_lskeepers_purch_feed, DM_req_prpn_JM)
    kg_DM_req_SubAF <- calculate_dry_matter_requirements(lwSubAF, prpn_lskeepers_purch_feed, DM_req_prpn_SubAF)
    kg_DM_req_SubAM <- calculate_dry_matter_requirements(lwSubAM, prpn_lskeepers_purch_feed, DM_req_prpn_SubAM)
    kg_DM_req_AF <- calculate_dry_matter_requirements(lwAF, prpn_lskeepers_purch_feed, DM_req_prpn_AF)
    kg_DM_req_AM <- calculate_dry_matter_requirements(lwAM, prpn_lskeepers_purch_feed, DM_req_prpn_AM)
  
    DM_purch_JF <- calculate_purchased_feed(kg_DM_req_JF, prpn_lskeepers_purch_feed, prpn_feed_paid_for, DM_in_feed)
    DM_purch_JM <- calculate_purchased_feed(kg_DM_req_JM, prpn_lskeepers_purch_feed, prpn_feed_paid_for, DM_in_feed)
    DM_purch_SubAF <- calculate_purchased_feed(kg_DM_req_SubAF, prpn_lskeepers_purch_feed, prpn_feed_paid_for, DM_in_feed)
    DM_purch_SubAM <- calculate_purchased_feed(kg_DM_req_SubAM, prpn_lskeepers_purch_feed, prpn_feed_paid_for, DM_in_feed)
    DM_purch_AF <- calculate_purchased_feed(kg_DM_req_AF, prpn_lskeepers_purch_feed, prpn_feed_paid_for, DM_in_feed)
    DM_purch_AM <- calculate_purchased_feed(kg_DM_req_AM, prpn_lskeepers_purch_feed, prpn_feed_paid_for, DM_in_feed)
  
    KG_Feed_purchased_JF <- DM_purch_JF / DM_in_feed
    KG_Feed_purchased_JM <- DM_purch_JM / DM_in_feed
    KG_Feed_purchased_SubAF <- DM_purch_SubAF / DM_in_feed
    KG_Feed_purchased_SubAM <- DM_purch_SubAM / DM_in_feed
    KG_Feed_purchased_AF <- DM_purch_AF / DM_in_feed
    KG_Feed_purchased_AM <- DM_purch_AM / DM_in_feed
  
    Expenditure_on_feed_JF <- calculate_expenditure_on_feed(KG_Feed_purchased_JF, Feed_cost_kg)
    Expenditure_on_feed_JM <- calculate_expenditure_on_feed(KG_Feed_purchased_JM, Feed_cost_kg)
    Expenditure_on_feed_SubAF <- calculate_expenditure_on_feed(KG_Feed_purchased_SubAF, Feed_cost_kg)
    Expenditure_on_feed_SubAM <- calculate_expenditure_on_feed(KG_Feed_purchased_SubAM, Feed_cost_kg)
    Expenditure_on_feed_AF <- calculate_expenditure_on_feed(KG_Feed_purchased_AF, Feed_cost_kg)
    Expenditure_on_feed_AM <- calculate_expenditure_on_feed(KG_Feed_purchased_AM, Feed_cost_kg)
  
  }
  
  if (species == "cattle") {
    kg_DM_req_Ox <- calculate_dry_matter_requirements(lwOx, prpn_lskeepers_purch_feed, DM_req_prpn_Ox)
    DM_purch_Ox <- calculate_purchased_feed(kg_DM_req_Ox, prpn_lskeepers_purch_feed, prpn_feed_paid_for, DM_in_feed)
    KG_Feed_purchased_Ox <- DM_purch_Ox / DM_in_feed
    Expenditure_on_feed_Ox <- calculate_expenditure_on_feed(KG_Feed_purchased_Ox, Feed_cost_kg)
  }
  
  ### Variable categories ### 
  
  vector_categories <- c(
    "Num_JF",
    "Num_JM",
    "Num_SubAF",
    "Num_SubAM",
    "Num_AF",
    "Num_AM",
    "Num_N",
    
    "Births", 
    
    "Growth_JF", 
    "Growth_JM", 
    "Growth_SubAF", 
    "Growth_SubAM", 
    
    "Deaths_JF", 
    "Deaths_JM", 
    "Deaths_SubAF", 
    "Deaths_SubAM", 
    "Deaths_AF", 
    "Deaths_AM", 
    
    "Culls_AF", 
    "Culls_AM", 
    
    "Cumulative_culls_AM", 
    
    "Offtake_JF", 
    "Offtake_JM", 
    "Offtake_SubAF", 
    "Offtake_SubAM", 
    "Offtake_AF", 
    "Offtake_AM", 
    
    "Cumulative_draught_income", 
    
    "Monthly_mortality", 
    "Total_mortality",
    "Total_mortality_JF", 
    "Total_mortality_JM", 
    "Total_mortality_SubAF", 
    "Total_mortality_SubAM", 
    "Total_mortality_AF", 
    "Total_mortality_AM", 
    
    "Value_of_total_mortality", 
    "Value_of_total_mortality_JF", 
    "Value_of_total_mortality_JM", 
    "Value_of_total_mortality_SubAF", 
    "Value_of_total_mortality_SubAM", 
    "Value_of_total_mortality_AF", 
    "Value_of_total_mortality_AM", 
    
    "Quantity_liveweight_kg", 
    "Quantity_liveweight_kg_JF", 
    "Quantity_liveweight_kg_JM", 
    "Quantity_liveweight_kg_SubAF", 
    "Quantity_liveweight_kg_SubAM", 
    "Quantity_liveweight_kg_AF", 
    "Quantity_liveweight_kg_AM", 
    
    "Quantity_meat_kg", 
    
    "Num_offtake", 
    "Num_offtake_SubAF", 
    "Num_offtake_SubAM", 
    "Num_offtake_SubAF", 
    "Num_offtake_SubAM", 
    "Num_offtake_AF", 
    "Num_offtake_AM", 
    
    "Offtake_liveweight_kg", 
    "Offtake_liveweight_kg_SubAF", 
    "Offtake_liveweight_kg_SubAM", 
    "Offtake_liveweight_kg_AF", 
    "Offtake_liveweight_kg_AM", 
    
    "Pop_growth", 
    "Pop_growth_SubAF", 
    "Pop_growth_SubAM", 
    "Pop_growth_SubAF", 
    "Pop_growth_SubAM", 
    "Pop_growth_AF", 
    "Pop_growth_AM", 
    
    "Monthly_growth_rate", 
    "Monthly_pop_growth", 
    
    "Quantity_manure", 
    "Quantity_manure_JF", 
    "Quantity_manure_JM", 
    "Quantity_manure_SubAF", 
    "Quantity_manure_SubAM", 
    "Quantity_manure_AF", 
    "Quantity_manure_AM", 
    
    "Quantity_hides", 
    "Quantity_hides_SubAF", 
    "Quantity_hides_SubAM", 
    "Quantity_hides_AF", 
    "Quantity_hides_AM", 
    
    "Quantity_milk",
    
    "Cumulative_dry_matter", 
    "Cumulative_dry_matter_JF", 
    "Cumulative_dry_matter_JM", 
    "Cumulative_dry_matter_SubAF", 
    "Cumulative_dry_matter_SubAM", 
    "Cumulative_dry_matter_AF", 
    "Cumulative_dry_matter_AM", 
    
    "Monthly_DM", 
    
    "Value_offtake", 
    "Value_offtake_JF", 
    "Value_offtake_JM", 
    "Value_offtake_SubAF", 
    "Value_offtake_SubAM", 
    "Value_offtake_AF", 
    "Value_offtake_AM", 
    
    "Value_herd_increase", 
    "Value_herd_increase_JF", 
    "Value_herd_increase_JM", 
    "Value_herd_increase_SubAF", 
    "Value_herd_increase_SubAM", 
    "Value_herd_increase_AF", 
    "Value_herd_increase_AM", 
    
    "Total_value_increase", 
    "Total_value_increase_JF", 
    "Total_value_increase_JM", 
    "Total_value_increase_SubAF", 
    "Total_value_increase_SubAM", 
    "Total_value_increase_AF", 
    "Total_value_increase_AM", 
    
    "Feed_cost", 
    "Feed_cost_JF", 
    "Feed_cost_JM", 
    "Feed_cost_SubAF", 
    "Feed_cost_SubAM",
    "Feed_cost_AF", 
    "Feed_cost_AM", 
    
    "Labour_cost", 
    "Labour_cost_JF", 
    "Labour_cost_JM", 
    "Labour_cost_SubAF", 
    "Labour_cost_SubAM", 
    "Labour_cost_AF", 
    "Labour_cost_AM", 
    
    "Health_cost", 
    "Health_cost_JF", 
    "Health_cost_JM", 
    "Health_cost_SubAF", 
    "Health_cost_SubAM", 
    "Health_cost_AF", 
    "Health_cost_AM", 
    
    "Capital_cost", 
    "Capital_cost_JF", 
    "Capital_cost_JM", 
    "Capital_cost_SubAF", 
    "Capital_cost_SubAM", 
    "Capital_cost_AF", 
    "Capital_cost_AM", 
    
    "Infrastructure_cost", 
    "Infrastructure_cost_JF", 
    "Infrastructure_cost_JM", 
    "Infrastructure_cost_SubAF", 
    "Infrastructure_cost_SubAM", 
    "Infrastructure_cost_AF", 
    "Infrastructure_cost_AM", 
    
    "Total_expenditure", 
    "Total_expenditure_JF", 
    "Total_expenditure_JM", 
    "Total_expenditure_SubAF", 
    "Total_expenditure_SubAM", 
    "Total_expenditure_AF", 
    "Total_expenditure_AM"
  )
  
  if (species == "cattle") { # oxen
    vector_categories <- append(vector_categories, c(
                         "NumOx",
                         "Oxen_J", 
                         "Oxen_A",
                         "Deaths_Ox", 
                         "Culls_Ox", 
                         "Offtake_Ox",
                         "Total_mortality_Ox",
                         "Value_of_total_mortality_Ox",
                         "Quantity_liveweight_kg_Ox",
                         "Num_offtake_Ox",
                         "Offtake_liveweight_kg_Ox", 
                         "Pop_growth_Ox", 
                         "Quantity_manure_Ox", 
                         "Quantity_hides_Ox",
                         "Cumulative_dry_matter_Ox",
                         "Value_offtake_Ox", 
                         "Value_herd_increase_Ox", 
                         "Total_value_increase_Ox", 
                         "Feed_cost_Ox",
                         "Labour_cost_Ox", 
                         "Health_cost_Ox",
                         "Capital_cost_Ox",
                         "Infrastructure_cost_Ox", 
                         "Total_expenditure_Ox"))
  } else if (species == "smallruminants") {
    # vector_categories <- append(vector_categories, "Quantity_Wool")
  } else {
    # poultry
    vector_categories <- append(vector_categories, c(
                          "Feed_requirement",
                          "Feed_requirement_JF",
                          "Feed_requirement_JM",
                          "Feed_requirement_SubAF",
                          "Feed_requirement_SubAM",
                          "Feed_requirement_AF",
                          "Feed_requirement_AM",
                          
                          "Feed_purchased",
                          "Feed_purchased_JF",
                          "Feed_purchased_JM",
                          "Feed_purchased_SubAF",
                          "Feed_purchased_SubAM",
                          "Feed_purchased_AF",
                          "Feed_purchased_AM",
                         
                         "Quantity_meat_kg",
                         "Quantity_eggs_sold",
                         "Quantity_eggs_consumed",
                         
                         "Value_eggs_sold",
                         "Value_eggs_consumed"
                         ))
  }
  
  res_vec <- list()
  
  for (vec in vector_categories) {
    res_vec[[vec]] <- rep(0, Num_months)
  }
  
  matrix_categories <- c("Num_JF",
                         "Num_JM",
                         "Num_SubAF",
                         "Num_SubAM",
                         "Num_AF",
                         "Num_AM",
                         "Num_N",
                         
                         "Monthly_mortality", 
                         
                         "Total_mortality",
                         "Total_mortality_JF", 
                         "Total_mortality_JM", 
                         "Total_mortality_SubAF", 
                         "Total_mortality_SubAM", 
                         "Total_mortality_AF", 
                         "Total_mortality_AM", 
                         
                         "Value_of_total_mortality", 
                         "Value_of_total_mortality_JF", 
                         "Value_of_total_mortality_JM", 
                         "Value_of_total_mortality_SubAF", 
                         "Value_of_total_mortality_SubAM", 
                         "Value_of_total_mortality_AF", 
                         "Value_of_total_mortality_AM", 
                         
                         "Quantity_liveweight_kg", 
                         "Quantity_liveweight_kg_JF", 
                         "Quantity_liveweight_kg_JM", 
                         "Quantity_liveweight_kg_SubAF", 
                         "Quantity_liveweight_kg_SubAM", 
                         "Quantity_liveweight_kg_AF", 
                         "Quantity_liveweight_kg_AM", 
                         
                         "Cumulative_draught_income",
                         
                         "Quantity_meat_kg", 
                         
                         "Num_offtake", 
                         "Num_offtake_SubAF", 
                         "Num_offtake_SubAM", 
                         "Num_offtake_SubAF", 
                         "Num_offtake_SubAM", 
                         "Num_offtake_AF", 
                         "Num_offtake_AM", 
                         
                         "Offtake_liveweight_kg", 
                         "Offtake_liveweight_kg_SubAF", 
                         "Offtake_liveweight_kg_SubAM", 
                         "Offtake_liveweight_kg_AF", 
                         "Offtake_liveweight_kg_AM", 
    
                         "Pop_growth", 
                         "Pop_growth_SubAF", 
                         "Pop_growth_SubAM", 
                         "Pop_growth_SubAF", 
                         "Pop_growth_SubAM", 
                         "Pop_growth_AF", 
                         "Pop_growth_AM", 
                         
                         "Monthly_growth_rate", 
                         "Monthly_pop_growth", 
                         
                         "Quantity_manure", 
                         "Quantity_manure_JF", 
                         "Quantity_manure_JM", 
                         "Quantity_manure_SubAF", 
                         "Quantity_manure_SubAM", 
                         "Quantity_manure_AF", 
                         "Quantity_manure_AM", 
                         
                         "Value_manure",
                         "Value_manure_JF",
                         "Value_manure_JM",
                         "Value_Manure_SubAF", 
                         "Value_Manure_SubAM", 
                         "Value_Manure_AF", 
                         "Value_Manure_AM", 
                         
                         "Quantity_hides", 
                         "Quantity_hides_SubAF", 
                         "Quantity_hides_SubAM", 
                         "Quantity_hides_AF", 
                         "Quantity_hides_AM", 
                         
                         "Value_hides", 
                         "Value_hides_SubAF", 
                         "Value_hides_SubAM", 
                         "Value_hides_AF", 
                         "Value_hides_AM", 
                         
                         "Quantity_milk",
                         
                         "Value_Milk",
                         
                         "Cumulative_dry_matter", 
                         "Cumulative_dry_matter_JF", 
                         "Cumulative_dry_matter_JM", 
                         "Cumulative_dry_matter_SubAF", 
                         "Cumulative_dry_matter_SubAM", 
                         "Cumulative_dry_matter_AF", 
                         "Cumulative_dry_matter_AM", 
                         
                         "Monthly_DM",
                         
                         "Value_offtake", 
                         "Value_offtake_JF", 
                         "Value_offtake_JM", 
                         "Value_offtake_SubAF", 
                         "Value_offtake_SubAM", 
                         "Value_offtake_AF", 
                         "Value_offtake_AM", 
                         
                         "Value_herd_increase", 
                         "Value_herd_increase_JF", 
                         "Value_herd_increase_JM", 
                         "Value_herd_increase_SubAF", 
                         "Value_herd_increase_SubAM", 
                         "Value_herd_increase_AF", 
                         "Value_herd_increase_AM", 
                         
                         "Total_value_increase", 
                         "Total_value_increase_JF", 
                         "Total_value_increase_JM", 
                         "Total_value_increase_SubAF", 
                         "Total_value_increase_SubAM", 
                         "Total_value_increase_AF", 
                         "Total_value_increase_AM", 
                         
                         "Production_value_herd_offtake_hide_manure",
                         "Production_value_herd_offtake_hide_manure_JF",
                         "Production_value_herd_offtake_hide_manure_JM",
                         "Production_value_herd_offtake_hide_manure_SubAF",
                         "Production_value_herd_offtake_hide_manure_SubAM",
                         "Production_value_herd_offtake_hide_manure_AF",
                         "Production_value_herd_offtake_hide_manure_AM",
                         
                         "Feed_cost", 
                         "Feed_cost_JF", 
                         "Feed_cost_JM", 
                         "Feed_cost_SubAF", 
                         "Feed_cost_SubAM",
                         "Feed_cost_AF", 
                         "Feed_cost_AM", 
                         
                         "Labour_cost", 
                         "Labour_cost_JF", 
                         "Labour_cost_JM", 
                         "Labour_cost_SubAF", 
                         "Labour_cost_SubAM", 
                         "Labour_cost_AF", 
                         "Labour_cost_AM", 
                         
                         "Health_cost", 
                         "Health_cost_JF", 
                         "Health_cost_JM", 
                         "Health_cost_SubAF", 
                         "Health_cost_SubAM", 
                         "Health_cost_AF", 
                         "Health_cost_AM", 
                         
                         "Capital_cost", 
                         "Capital_cost_JF", 
                         "Capital_cost_JM", 
                         "Capital_cost_SubAF", 
                         "Capital_cost_SubAM", 
                         "Capital_cost_AF", 
                         "Capital_cost_AM", 
                         
                         "Infrastructure_cost", 
                         "Infrastructure_cost_JF", 
                         "Infrastructure_cost_JM", 
                         "Infrastructure_cost_SubAF", 
                         "Infrastructure_cost_SubAM", 
                         "Infrastructure_cost_AF", 
                         "Infrastructure_cost_AM", 
                         
                         "Total_expenditure", 
                         "Total_expenditure_JF", 
                         "Total_expenditure_JM", 
                         "Total_expenditure_SubAF", 
                         "Total_expenditure_SubAM", 
                         "Total_expenditure_AF", 
                         "Total_expenditure_AM"
                         
  )
  
  if (species == "cattle") { # oxen
    matrix_categories <- append(matrix_categories, c("NumOx",
                                                     "Total_mortality_Ox", 
                                                     "Value_of_total_mortality_Ox",
                                                     "Quantity_liveweight_kg_Ox",
                                                     "Num_offtake_Ox", 
                                                     "Offtake_liveweight_kg_Ox", 
                                                     "Pop_growth_Ox", 
                                                     "Quantity_manure_Ox",
                                                     "Value_Manure_Ox",
                                                     "Quantity_hides_Ox",
                                                     "Value_hides_Ox", 
                                                     "Cumulative_dry_matter_Ox", 
                                                     "Value_offtake_Ox", 
                                                     "Value_herd_increase_Ox",
                                                     "Total_value_increase_Ox",
                                                     "Production_value_herd_offtake_hide_manure_Ox",
                                                     "Feed_cost_Ox", 
                                                     "Labour_cost_Ox", 
                                                     "Health_cost_Ox", 
                                                     "Capital_cost_Ox", 
                                                     "Infrastructure_cost_Ox", 
                                                     "Total_expenditure_Ox"))
  } else if (species == "smallruminants") {
    # wool not implemented yet by Gemma
    # matrix_categories <- append(matrix_categories, "Quantity_Wool")
  } else {
    # poultry
    matrix_categories <- append(matrix_categories, c("Quantity_eggs_consumed",
                                                     "Quantity_eggs_sold",
                                                     
                                                     "Value_eggs_consumed",
                                                     "Value_eggs_sold",
                                                     
                                                     "Feed_requirement",
                                                     "Feed_requirement_JF",
                                                     "Feed_requirement_JM",
                                                     "Feed_requirement_SubAF",
                                                     "Feed_requirement_SubAM",
                                                     "Feed_requirement_AF",
                                                     "Feed_requirement_AM",
                                                     
                                                     "Feed_purchased",
                                                     "Feed_purchased_JF",
                                                     "Feed_purchased_JM",
                                                     "Feed_purchased_SubAF",
                                                     "Feed_purchased_SubAM",
                                                     "Feed_purchased_AF",
                                                     "Feed_purchased_AM"))
  }

  # Initialize a list to store the matrices
  res_mat <- list()
  
  for (mat in matrix_categories) {
    res_mat[[mat]] <- matrix(0, nrow = nruns, ncol = Num_months)
  }

  for (i in 1:nruns) {
    # Total population is sum of age*sex segments
    Nt0 <- sum(N_JF_t0, N_JM_t0, N_SubAF_t0, N_SubAM_t0, N_AF_t0, N_AM_t0)
    
    if (species == "cattle") {
      Nt0 <- Nt0 + N_Ox_t0 
    }
    
    # Define population variables and set initial values from function arguments
    N <- Nt0
    age_sex_groups <- c("JF", "JM", "SubAF", "SubAM", "AF", "AM")
    
    if (species == "cattle") {
      age_sex_groups <- append(age_sex_groups, "Ox")
    }

    for (group in age_sex_groups) {
      var_name <- group  
      value <- get(paste0("N_", group, "_t0"))  
      assign(var_name, value)  
    }
    
    prop_groups <- c(pJF_t0 = JF/N, 
                     pSubAF_t0 = SubAF/N, 
                     pAF_t0 = AF/N,
                     pJM_t0 = JM/N, 
                     pSubAM_t0 = SubAM/N, 
                     pAM_t0 = AM/N)
    
    if (species == "cattle") {
      prop_groups <- c(prop_groups, pO_t0 = Ox/N)
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
    Value_offtake <- 0
    Value_herd_inc <- 0
    Eggs_sold <- 0
    Eggs_consumed <- 0
    Feed <- 0
    Labour <- 0
    Health <- 0
    Capital <- 0
    
    production_vars <- c("Num_dead", 
                         "Liveweight_kg",
                         "Offtake",
                         "Offtake_liveweight",
                         "Manure_kg",
                         "Hides",
                         "Cumulative_DM",
                         "Value_offtake",
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
      
      if (species == "cattle" || species == "smallruminants") {
        Mu <- calculate_mu(part, prolif)
        
        res_vec$Births[month] <- sample(Mu, 1) * AF
      } else {
        # poultry
        res_vec$Births[month] <- AF * sample(prop_females_laying, 1) * 
                                             sample(lay_rate, 1) * 
                                             sample(egg_brood_rate, 1) * 
                                             sample(hatch_rate, 1)
      }
      
      if (species == "cattle" || species == "smallruminants" || species == "poultry") {
        res_vec$Deaths_JF[month] <- sample(AlphaJ, 1) * JF
        res_vec$Deaths_SubAF[month] <- sample(AlphaSubA, 1) * SubAF
        res_vec$Deaths_AF[month] <- sample(AlphaF, 1) * AF
        res_vec$Deaths_JM[month] <- sample(AlphaJ, 1) * JM
        res_vec$Deaths_SubAM[month] <- sample(AlphaSubA, 1) * SubAM
        res_vec$Deaths_AM[month] <- sample(AlphaM, 1) * AM
      } 
      
      if (species == "cattle" || species == "poultry") {
        res_vec$Offtake_JF[month] <- sample(GammaJF, 1) * JF
        res_vec$Offtake_JM[month] <- sample(GammaJM, 1) * JM
        res_vec$Offtake_SubAF[month] <- sample(GammaSubAF, 1) * SubAF
        res_vec$Offtake_AF[month] <- sample(GammaAF, 1) * AF
        res_vec$Offtake_SubAM[month] <- sample(GammaSubAM, 1) * SubAM
        res_vec$Offtake_AM[month] <- sample(GammaAM, 1) * AM
      } else if (species == "smallruminants") {
        res_vec$Offtake_SubAF[month] <- sample(GammaF, 1) * SubAF
        res_vec$offtake_AF[month] <- sample(GammaF, 1) * AF
        res_vec$Offtake_SubAM[month] <- sample(GammaM, 1) * SubAM
        res_vec$Offtake_AM[month] <- sample(GammaM, 1) * AM
      }
      
      if (species == "cattle" || species == "poultry") {
        res_vec$Growth_JF[month] <- sample(Beta_J, 1) * JF
        res_vec$Growth_SubAF[month] <- sample(Beta_SubA, 1) * SubAF
        res_vec$Growth_JM[month] <- sample(Beta_J, 1) * JM
        res_vec$Growth_SubAM[month] <- sample(Beta_SubA, 1) * SubAM
      } else if (species == "snall_ruminants") {
        res_vec$Growth_JF[month] <- sample(Beta, 1) * JF
        res_vec$Growth_SubAF[month] <- sample(Beta, 1) * SubAF
        res_vec$Growth_JM[month] <- sample(Beta, 1) * JM
        res_vec$Growth_SubAM[month] <- sample(Beta, 1) * SubAM
      }

      if (species == "cattle" || species == "smallruminants" || species == "poultry") {
        
        res_vec$Culls_AF[month] <- sample(CullF, 1) * AF
        res_vec$Culls_AM[month] <- sample(CullM, 1) * AM
      }
      
      if (species == "cattle" || species == "smallruminants" || species == "poultry") {
        
        res_vec$Num_JF[month] <- JF + res_vec$Births[month] * 0.5 - res_vec$Deaths_JF[month] - res_vec$Growth_JF[month] - res_vec$Offtake_JF[month]
        res_vec$Num_SubAF[month] <- SubAF + res_vec$Growth_JF[month] - res_vec$Growth_SubAF[month] - res_vec$Offtake_SubAF[month] - res_vec$Deaths_SubAF[month]
        res_vec$Num_AF[month] <- AF + res_vec$Growth_SubAF[month] - res_vec$Offtake_AF[month] - res_vec$Deaths_AF[month] - res_vec$Culls_AF[month]
        
        res_vec$Num_JM[month] <- JM + res_vec$Births[month] * 0.5 - res_vec$Growth_JM[month] - res_vec$Deaths_JM[month] - res_vec$Offtake_JM[month]
        res_vec$Num_SubAM[month] <- SubAM + res_vec$Growth_JM[month] - res_vec$Growth_SubAM[month] - res_vec$Offtake_SubAM[month] - res_vec$Deaths_SubAM[month]
        res_vec$Num_AM[month] <- AM + res_vec$Growth_SubAM[month] - res_vec$Offtake_AM[month] - res_vec$Deaths_AM[month] - res_vec$Culls_AM[month]
        
        res_vec$Num_N[month] <- sum(res_vec$Num_JF[month],
                                    res_vec$Num_SubAF[month],
                                    res_vec$Num_AF[month],
                                    res_vec$Num_JM[month],
                                    res_vec$Num_SubAM[month],
                                    res_vec$Num_AM[month])
        
      }
      
      
      if (species == "cattle") {
        res_vec$Deaths_Ox[month] <- sample(AlphaOx, 1) * Ox
        res_vec$Offtake_Ox[month] <- sample(GammaOx, 1) * Ox
        res_vec$Culls_Ox[month] <- sample(CullOx, 1) * Ox
        res_vec$Oxen_A[month] <- sample(castration_rate, 1) * AM
        res_vec$NumOx[month] <- Ox + res_vec$Oxen_A[month] - res_vec$Offtake_Ox[month] - res_vec$Deaths_Ox[month] - res_vec$Culls_Ox[month]
        res_vec$Num_AM[month] <- res_vec$Num_AM[month] - res_vec$Oxen_A[month]
        res_vec$Num_N[month] <- res_vec$Num_N[month] + res_vec$NumOx[month]
      }
      
      if (species == "cattle" || species == "smallruminants" || species == "poultry") {
        JF <- res_vec$Num_JF[month]
        SubAF <- res_vec$Num_SubAF[month]
        AF <- res_vec$Num_AF[month]
        JM <- res_vec$Num_JM[month]
        SubAM <- res_vec$Num_SubAM[month]
        AM <- res_vec$Num_AM[month]
        N <- res_vec$Num_N[month]
      
        res_vec$Total_mortality_JF[month] <- Num_dead_JF + res_vec$Deaths_JF[month]
        Num_dead_JF <- res_vec$Total_mortality_JF[month]
        res_vec$Total_mortality_JM[month] <- Num_dead_JM + res_vec$Deaths_JM[month]
        Num_dead_JM <- res_vec$Total_mortality_JM[month]
        res_vec$Total_mortality_SubAF[month] <- Num_dead_SubAF + res_vec$Deaths_SubAF[month]
        Num_dead_SubAF <- res_vec$Total_mortality_SubAF[month]
        res_vec$Total_mortality_SubAM[month] <- Num_dead_SubAM + res_vec$Deaths_SubAM[month]
        Num_dead_SubAM <- res_vec$Total_mortality_SubAM[month]
        res_vec$Total_mortality_AF[month] <- Num_dead_AF + res_vec$Deaths_AF[month]
        Num_dead_AF <- res_vec$Total_mortality_AF[month]
        res_vec$Total_mortality_AM[month] <- Num_dead_AM + res_vec$Deaths_AM[month]
        Num_dead_AM <- res_vec$Total_mortality_AM[month]
      
        res_vec$Total_mortality[month] <- sum(res_vec$Total_mortality_JF[month],
                                              res_vec$Total_mortality_JM[month], 
                                              res_vec$Total_mortality_SubAF[month],
                                              res_vec$Total_mortality_SubAM[month], 
                                              res_vec$Total_mortality_AF[month],
                                              res_vec$Total_mortality_AM[month]) 
      }
      
      if (species == "cattle") {
        Ox <- res_vec$NumOx[month]
        res_vec$Total_mortality_Ox[month] <- Num_dead_Ox + res_vec$Deaths_Ox[month]
        Num_dead_Ox <- res_vec$Total_mortality_Ox[month]
        res_vec$Total_mortality[month] <- res_vec$Total_mortality[month] + res_vec$Total_mortality_Ox[month]
      }
      
      # Value of Total Mortality
      
      if (species == "cattle" || species == "smallruminants" || species == "poultry") {
        res_vec$Value_of_total_mortality_JF[month] <- res_vec$Total_mortality_JF[month] * sample(fvJF, 1)
        res_vec$Value_of_total_mortality_JM[month] <- res_vec$Total_mortality_JM[month] * sample(fvJM, 1)
        res_vec$Value_of_total_mortality_SubAF[month] <- res_vec$Total_mortality_SubAF[month] * sample(fvSubAF, 1)
        res_vec$Value_of_total_mortality_SubAM[month] <- res_vec$Total_mortality_SubAM[month] * sample(fvSubAM, 1)
        res_vec$Value_of_total_mortality_AF[month] <- res_vec$Total_mortality_AF[month] * sample(fvAF, 1)
        res_vec$Value_of_total_mortality_AM[month] <- res_vec$Total_mortality_AM[month] * sample(fvAM, 1)

        res_vec$Value_of_total_mortality[month] <- sum(res_vec$Value_of_total_mortality_JF[month],
                                                       res_vec$Value_of_total_mortality_JM[month],
                                                       res_vec$Value_of_total_mortality_SubAF[month],
                                                       res_vec$Value_of_total_mortality_SubAM[month],
                                                       res_vec$Value_of_total_mortality_AF[month],
                                                       res_vec$Value_of_total_mortality_AM[month])
        
      }


      if (species == "cattle") {
        res_vec$Value_of_total_mortality_Ox[month] <- res_vec$Total_mortality_Ox[month] * sample(fvOx, 1)
        res_vec$Value_of_total_mortality[month] <- res_vec$Value_of_total_mortality[month] + res_vec$Value_of_total_mortality_Ox[month]
      }
      
      if (species == "cattle" || species == "smallruminants" || species == "poultry") {
        res_vec$Pop_growth[month] <- N - Nt0
        res_vec$Pop_growth_SubAF[month] <- JF - N_JF_t0
        res_vec$Pop_growth_SubAM[month] <- JM - N_JM_t0
        res_vec$Pop_growth_SubAF[month] <- SubAF - N_SubAF_t0
        res_vec$Pop_growth_SubAM[month] <- SubAM - N_SubAM_t0
        res_vec$Pop_growth_AF[month] <- AF - N_AF_t0
        res_vec$Pop_growth_AM[month] <- AM - N_AM_t0
      }
      
      if (species == "cattle") {
        res_vec$Pop_growth_Ox[month] <- Ox - N_Ox_t0
      }
      
      if (species == "cattle" || species == "smallruminants") {
        res_vec$Quantity_liveweight_kg_JF[month] <- JF * sample(lwJF, 1)
        res_vec$Quantity_liveweight_kg_JM[month] <- JM * sample(lwJM, 1)
        res_vec$Quantity_liveweight_kg_SubAF[month] <- SubAF * sample(lwSubAF, 1)
        res_vec$Quantity_liveweight_kg_SubAM[month] <- SubAM * sample(lwSubAM, 1)
        res_vec$Quantity_liveweight_kg_AF[month] <- AF * sample(lwAF, 1)
        res_vec$Quantity_liveweight_kg_AM[month] <- AM * sample(lwAM, 1)
      }
        
        res_vec$Quantity_liveweight_kg[month] <- sum(res_vec$Quantity_liveweight_kg_JF[month],
                                                  res_vec$Quantity_liveweight_kg_JM[month],
                                                  res_vec$Quantity_liveweight_kg_SubAF[month],
                                                  res_vec$Quantity_liveweight_kg_SubAM[month],
                                                  res_vec$Quantity_liveweight_kg_AF[month],
                                                  res_vec$Quantity_liveweight_kg_AM[month])
        
        res_vec$Num_offtake_SubAF[month] <- Offtake_JF + res_vec$Offtake_JF[month]
        res_vec$Num_offtake_SubAM[month] <- Offtake_JM + res_vec$Offtake_JM[month]
        res_vec$Num_offtake_SubAF[month] <- Offtake_SubAF + res_vec$Offtake_SubAF[month]
        res_vec$Num_offtake_SubAM[month] <- Offtake_SubAM + res_vec$Offtake_SubAM[month]
        res_vec$Num_offtake_AF[month] <- Offtake_AF + res_vec$Offtake_AF[month]
        res_vec$Num_offtake_AM[month] <- Offtake_AM + res_vec$Offtake_AM[month] + res_vec$Culls_AM[month]
        
      }
      
      if (species == "cattle") {
        res_vec$Quantity_liveweight_kg_Ox[month] <- Ox * sample(lwOx, 1)
        res_vec$Quantity_liveweight_kg[month] <- res_vec$Quantity_liveweight_kg[month] + res_vec$Quantity_liveweight_kg_Ox[month]
        res_vec$Num_offtake_Ox[month] <- Offtake_Ox + res_vec$Offtake_Ox[month] + res_vec$Culls_Ox[month]
      }
    
      if (species == "cattle" || species == "smallruminants" || species == "poultry") {
      
      Offtake_JF <- res_vec$Num_offtake_SubAF[month]
      Offtake_JM <- res_vec$Num_offtake_SubAM[month]
      Offtake_SubAF <- res_vec$Num_offtake_SubAF[month]
      Offtake_SubAM <- res_vec$Num_offtake_SubAM[month]
      Offtake_AF <- res_vec$Num_offtake_AF[month]
      Offtake_AM <- res_vec$Num_offtake_AM[month]
      
      res_vec$Num_offtake[month] <- sum(res_vec$Num_offtake_SubAF[month],
                                    res_vec$Num_offtake_SubAM[month],
                                    res_vec$Num_offtake_SubAF[month],
                                    res_vec$Num_offtake_SubAM[month],
                                    res_vec$Num_offtake_AF[month],
                                    res_vec$Num_offtake_AM[month])
      }
      
      if (species == "cattle") {
        Offtake_Ox <- res_vec$Num_offtake_Ox[month]
        res_vec$Num_offtake[month] <- res_vec$Num_offtake[month] + res_vec$Num_offtake_Ox[month]
      }
    
      if (species == "cattle" || species == "smallruminants" || species == "poultry") {
        Offtake <- res_vec$Num_offtake[month]
        res_vec$Offtake_liveweight_kg_SubAF[month] <- sample(lwSubAF, 1) * Offtake_SubAF
        res_vec$Offtake_liveweight_kg_SubAM[month] <- sample(lwSubAM, 1) * Offtake_SubAM
        res_vec$Offtake_liveweight_kg_AF[month] <- sample(lwAF, 1) * Offtake_AF
        res_vec$Offtake_liveweight_kg_AM[month] <- sample(lwAM, 1) * Offtake_AM
      
        res_vec$Offtake_liveweight_kg[month] <- sum(res_vec$Offtake_liveweight_kg_SubAF[month],
                                                    res_vec$Offtake_liveweight_kg_SubAM[month],
                                                    res_vec$Offtake_liveweight_kg_AF[month],
                                                    res_vec$Offtake_liveweight_kg_AM[month])
      }

      if (species == "cattle" || species == "smallruminants" || species == "poultry") {
        res_vec$Quantity_meat_kg[month] <- sum(res_vec$Offtake_liveweight_kg_SubAF[month], 
                                               res_vec$Offtake_liveweight_kg_SubAM[month],
                                               res_vec$Offtake_liveweight_kg_AF[month],
                                               res_vec$Offtake_liveweight_kg_AM[month]) * ccy
        
      }
      
 
      if (species == "cattle") { 
        res_vec$Offtake_liveweight_kg_Ox[month] <- sample(lwOx, 1) * Offtake_Ox
        res_vec$Offtake_liveweight_kg[month] <-  res_vec$Offtake_liveweight_kg[month] + res_vec$Offtake_liveweight_kg_Ox[month]
        res_vec$Quantity_meat_kg[month] <- res_vec$Quantity_meat_kg[month] + res_vec$Offtake_liveweight_kg_Ox[month] * ccy
      }
      
      if (species == "cattle" || species == "smallruminants" || species == "poultry") {
        Meat_kg <- res_vec$Quantity_meat_kg[month]
      }
      
      if (species == "cattle") {
        res_vec$Cumulative_draught_income[month] <- Draught_income + Ox * sample(draught_rate, 1) * draught_day_value * 30
        Draught_income <- res_vec$Cumulative_draught_income[month]
      }
      
      if (species == "cattle" || species == "smallruminants") {
        res_vec$Quantity_hides_SubAF[month] <- Hides_SubAF +  res_vec$Deaths_SubAF[month] * hides_rate_mor
        res_vec$Quantity_hides_SubAM[month] <- Hides_SubAM + res_vec$Deaths_SubAM[month] * hides_rate_mor
        res_vec$Quantity_hides_AF[month] <- Hides_AF + res_vec$Deaths_AF[month] * hides_rate_mor
        res_vec$Quantity_hides_AM[month] <- Hides_AM + res_vec$Deaths_AM[month] * hides_rate_mor
        
        Hides_SubAF <- res_vec$Quantity_hides_SubAF[month]
        Hides_SubAM <- res_vec$Quantity_hides_SubAM[month]
        Hides_AF <- res_vec$Quantity_hides_AF[month]
        Hides_AM <- res_vec$Quantity_hides_AM[month]
        
        res_vec$Quantity_hides[month] <- sum(res_vec$Quantity_hides_SubAF[month],
                                          res_vec$Quantity_hides_SubAM[month],
                                          res_vec$Quantity_hides_AF[month],
                                          res_vec$Quantity_hides_AM[month])
        
        Hides <- res_vec$Quantity_hides[month]
        
        res_vec$Quantity_milk[month] <- Milk + AF * sample(part, 1)/12 * prop_F_milked * sample(lac_duration, 1) * sample(avg_daily_yield_ltr, 1) 
        
        Milk <- res_vec$Quantity_milk[month]
        
        res_vec$Quantity_manure_JF[month] <- Manure_kg_JF + JF * sample(Man_J, 1) * 30  
        res_vec$Quantity_manure_JM[month] <- Manure_kg_JM + JM * sample(Man_J, 1) * 30 
        res_vec$Quantity_manure_SubAF[month] <- Manure_kg_SubAF + SubAF * sample(Man_SubA, 1) * 30
        res_vec$Quantity_manure_SubAM[month] <- Manure_kg_SubAM + SubAM * sample(Man_SubA, 1) * 30 
        res_vec$Quantity_manure_AF[month] <- Manure_kg_AF + AF * sample(Man_A, 1) * 30
        res_vec$Quantity_manure_AM[month] <- Manure_kg_AM + AM * sample(Man_A, 1) * 30
        
        Manure_kg_JF <- res_vec$Quantity_manure_JF[month]
        Manure_kg_JM <- res_vec$Quantity_manure_JM[month]
        Manure_kg_SubAF <- res_vec$Quantity_manure_SubAF[month]
        Manure_kg_SubAM <- res_vec$Quantity_manure_SubAM[month]
        Manure_kg_AF <- res_vec$Quantity_manure_AF[month]
        Manure_kg_AM <- res_vec$Quantity_manure_AM[month]
        
        res_vec$Quantity_manure[month] <- sum(res_vec$Quantity_manure_JF[month],
                                           res_vec$Quantity_manure_JM[month],
                                           res_vec$Quantity_manure_SubAF[month],
                                           res_vec$Quantity_manure_SubAM[month],
                                           res_vec$Quantity_manure_AF[month],
                                           res_vec$Quantity_manure_AM[month])
        
      }
      
      
      if (species == "cattle") {
        res_vec$Quantity_hides_Ox[month] <- Hides_Ox + res_vec$Deaths_Ox[month] * hides_rate_mor 
        Hides_Ox <- res_vec$Quantity_hides_Ox[month]
        res_vec$Quantity_hides[month] <- res_vec$Quantity_hides[month] + res_vec$Quantity_hides_Ox[month]
        
        res_vec$Quantity_manure_Ox[month] <- Manure_kg_Ox + Ox * sample(Man_A, 1) * 30 
        Manure_kg_Ox <- res_vec$Quantity_manure_Ox[month]
        res_vec$Quantity_manure[month] <- res_vec$Quantity_manure[month] + res_vec$Quantity_manure_Ox[month]
        
        Manure_kg <- res_vec$Quantity_manure[month]
        
        res_vec$Cumulative_dry_matter_JF[month] <- Cumulative_DM_JF + JF * sample(kg_DM_req_JF, 1) * 30 
        res_vec$Cumulative_dry_matter_JM[month] <- Cumulative_DM_JM + JM * sample(kg_DM_req_JM, 1) * 30
        res_vec$Cumulative_dry_matter_SubAF[month] <- Cumulative_DM_SubAF + SubAF * sample(kg_DM_req_SubAF, 1) * 30 
        res_vec$Cumulative_dry_matter_SubAM[month] <- Cumulative_DM_SubAM + SubAM * sample(kg_DM_req_SubAM, 1) * 30 
        res_vec$Cumulative_dry_matter_AF[month] <- Cumulative_DM_AF + AF * sample(kg_DM_req_AF, 1) * 30
        res_vec$Cumulative_dry_matter_AM[month] <- Cumulative_DM_AM + AM * sample(kg_DM_req_AM, 1) * 30
        
        Cumulative_DM_JF <- res_vec$Cumulative_dry_matter_JF[month]
        Cumulative_DM_JM <- res_vec$Cumulative_dry_matter_JM[month]
        Cumulative_DM_SubAF <- res_vec$Cumulative_dry_matter_SubAF[month]
        Cumulative_DM_SubAM <- res_vec$Cumulative_dry_matter_SubAM[month]
        Cumulative_DM_AF <- res_vec$Cumulative_dry_matter_AF[month]
        Cumulative_DM_AM <- res_vec$Cumulative_dry_matter_AM[month]
        
        res_vec$Cumulative_dry_matter[month] <- sum(res_vec$Cumulative_dry_matter_JF[month],
                                                    res_vec$Cumulative_dry_matter_JM[month],
                                                    res_vec$Cumulative_dry_matter_SubAF[month],
                                                    res_vec$Cumulative_dry_matter_SubAM[month],
                                                    res_vec$Cumulative_dry_matter_AF[month],
                                                    res_vec$Cumulative_dry_matter_AM[month])
        
        res_vec$Cumulative_dry_matter_Ox[month] <- Cumulative_DM_Ox + Ox * sample(kg_DM_req_Ox, 1) * 30
        Cumulative_DM_Ox <- res_vec$Cumulative_dry_matter_Ox[month]
        res_vec$Cumulative_dry_matter[month] <- res_vec$Cumulative_dry_matter[month] +  res_vec$Cumulative_dry_matter_Ox[month]
        
      }
      
    if (species == "cattle" || species == "smallruminants") {
      Cumulative_DM <- res_vec$Cumulative_dry_matter[month]
    }
    
    if (species == "cattle" || species == "smallruminants" || species == "poultry") {
      
      res_vec$Value_offtake_SubAF[month] <- sample(fvSubAF, 1) * Offtake_SubAF 
      Value_offtake_SubAF <- res_vec$Value_offtake_SubAF[month]
      
      res_vec$Value_offtake_SubAM[month] <- sample(fvSubAM, 1) * Offtake_SubAM
      Value_offtake_SubAM <- res_vec$Value_offtake_SubAM[month]
      
      res_vec$Value_offtake_AF[month] <- sample(fvAF, 1) * Offtake_AF
      Value_offtake_AF <- res_vec$Value_offtake_AF[month]
      
      res_vec$Value_offtake_AM[month] <- sample(fvAM, 1) * Offtake_AM  
      Value_offtake_AM <- res_vec$Value_offtake_AM[month]
      
      res_vec$Value_offtake[month] <- sum(res_vec$Value_offtake_SubAF[month],
                                         res_vec$Value_offtake_SubAM[month],
                                         res_vec$Value_offtake_AF[month],
                                         res_vec$Value_offtake_AM[month])
    }
     
       if (species == "cattle") {
         res_vec$Value_offtake_Ox[month] <- sample(fvOx, 1) * Offtake_Ox
         Value_offtake_Ox <- res_vec$Value_offtake_Ox[month]
         res_vec$Value_offtake[month] <- res_vec$Value_offtake[month] + res_vec$Value_offtake_Ox[month]
       }
    
    if (species == "cattle" || species == "smallruminants" || species == "poultry") {
      
      Value_offtake <- res_vec$Value_offtake[month] 
      
      res_vec$Value_herd_increase_JF[month] <- (JF - N_JF_t0) * sample(fvJF, 1)
      Value_herd_inc_JF <- res_vec$Value_herd_increase_JF[month]
      
      res_vec$Value_herd_increase_JM[month] <- (JM - N_JM_t0) * sample(fvJM, 1)
      Value_herd_inc_JM <- res_vec$Value_herd_increase_JM[month]
      
      res_vec$Value_herd_increase_SubAF[month] <- (SubAF - N_SubAF_t0) * sample(fvSubAF, 1)
      Value_herd_inc_SubAF <- res_vec$Value_herd_increase_SubAF[month]
      
      res_vec$Value_herd_increase_SubAM[month] <- (SubAM - N_SubAM_t0) * sample(fvSubAM, 1)
      Value_herd_inc_SubAM <- res_vec$Value_herd_increase_SubAM[month]
      
      res_vec$Value_herd_increase_AF[month] <- (AF - N_AF_t0) * sample(fvAF, 1)
      Value_herd_inc_AF <- res_vec$Value_herd_increase_AF[month]
      
      res_vec$Value_herd_increase_AM[month] <- (AM - N_AM_t0) * sample(fvAM, 1)
      Value_herd_inc_AM <- res_vec$Value_herd_increase_AM[month]
      
      res_vec$Value_herd_increase[month] <- sum(res_vec$Value_herd_increase_JF[month],
                                                res_vec$Value_herd_increase_JM[month],
                                                res_vec$Value_herd_increase_SubAF[month],
                                                res_vec$Value_herd_increase_SubAM[month],
                                                res_vec$Value_herd_increase_AF[month],
                                                res_vec$Value_herd_increase_AM[month])
      
    }
      
      if (species == "cattle") {
        res_vec$Value_herd_increase_Ox[month] <- (Ox - N_Ox_t0) * (sample(fvOx, 1))
        Value_herd_inc_Ox <- res_vec$Value_herd_increase_Ox[month]
        res_vec$Value_herd_increase[month] <- res_vec$Value_herd_increase[month] + res_vec$Value_herd_increase_Ox[month]
      }
    
    if (species == "cattle" || species == "smallruminants" || species == "poultry") {
      
      Value_herd_inc <- res_vec$Value_herd_increase[month]
      
      res_vec$Total_value_increase[month] <- Value_herd_inc + Value_offtake
      res_vec$Total_value_increase_JF[month] <- Value_herd_inc_JF 
      res_vec$Total_value_increase_JM[month] <- Value_herd_inc_JM 
      res_vec$Total_value_increase_SubAF[month] <- Value_herd_inc_SubAF + Value_offtake_SubAF
      res_vec$Total_value_increase_SubAM[month] <- Value_herd_inc_SubAM + Value_offtake_SubAM
      res_vec$Total_value_increase_AF[month] <- Value_herd_inc_AF + Value_offtake_AF
      res_vec$Total_value_increase_AM[month] <- Value_herd_inc_AM + Value_offtake_AM
      
    }
      
      if (species == "cattle") {
        res_vec$Total_value_increase_Ox[month] <- Value_herd_inc_Ox + Value_offtake_Ox
        
        ### Feed cost ### 
        
        res_vec$Feed_cost_JF[month] <- Feed_JF + JF * sample(Expenditure_on_feed_JF, 1) * 30 
        Feed_JF <- res_vec$Feed_cost_JF[month]
        res_vec$Feed_cost_JM[month] <- Feed_JM + JM * sample(Expenditure_on_feed_JM, 1) * 30 
        Feed_JM <- res_vec$Feed_cost_JM[month]
        res_vec$Feed_cost_SubAF[month] <- Feed_SubAF + SubAF * sample(Expenditure_on_feed_SubAF, 1) * 30
        Feed_SubAF <- res_vec$Feed_cost_SubAF[month]
        res_vec$Feed_cost_SubAM[month] <- Feed_SubAM + SubAM * sample(Expenditure_on_feed_SubAM, 1) * 30
        Feed_SubAM <- res_vec$Feed_cost_SubAM[month]
        res_vec$Feed_cost_AF[month] <- Feed_AF + AF * sample(Expenditure_on_feed_AF, 1) * 30
        Feed_AF <- res_vec$Feed_cost_AF[month]
        res_vec$Feed_cost_AM[month] <- Feed_AM + AM * sample(Expenditure_on_feed_AM, 1) * 30 
        Feed_AM <- res_vec$Feed_cost_AM[month]
        
        res_vec$Feed_cost[month] <- sum(res_vec$Feed_cost_JF[month],
                                        res_vec$Feed_cost_JM[month],
                                        res_vec$Feed_cost_SubAF[month],
                                        res_vec$Feed_cost_SubAM[month],
                                        res_vec$Feed_cost_AF[month],
                                        res_vec$Feed_cost_AM[month])
        
        Feed <- res_vec$Feed_cost[month]
        
        if (species == "cattle" || species == "smallruminants") {
          res_vec$Labour_cost_JF[month] <- Labour_JF + JF * sample(Labour_cattle, 1) * lab_non_health 
          res_vec$Labour_cost_JM[month] <- Labour_JM + JM * sample(Labour_cattle, 1) * lab_non_health  
          res_vec$Labour_cost_SubAF[month] <- Labour_SubAF + SubAF * sample(Labour_cattle, 1) * lab_non_health  
          res_vec$Labour_cost_SubAM[month] <- Labour_SubAM + SubAM * sample(Labour_cattle, 1) * lab_non_health  
          res_vec$Labour_cost_AF[month] <- Labour_AF + AF * sample(Labour_cattle, 1) * lab_non_health + AF * prop_F_milked * sample(Labour_dairy, 1)  
          res_vec$Labour_cost_AM[month] <- Labour_AM + AM * sample(Labour_cattle, 1) * lab_non_health 
        } else {
          # poultry
          res_vec$Labour_cost_JF[month] = Labour_JF + JF * sample(Labour_poultry, 1) * lab_non_health 
          res_vec$Labour_cost_JM[month] = Labour_JM +JM * (sample(Labour_poultry, 1)) * lab_non_health  
          res_vec$Labour_cost_SubAFF[month] = Labour_SubAF + SubAF * sample(Labour_poultry, 1) * lab_non_health  
          res_vec$Labour_cost_SubAM[month] = Labour_SubAM + SubAM * sample(Labour_poultry, 1) * lab_non_health  
          res_vec$Labour_cost_AF[month] = Labour_AF + AF * sample(Labour_poultry, 1) * lab_non_health   
          res_vec$Labour_cost_AM[month] = Labour_AM + AM * sample(Labour_poultry, 1) * lab_non_health  
        }
        
        if (species == "cattle" || species == "smallruminants" || species == "poultry") {
          Labour_JF <- res_vec$Labour_cost_JF[month]
          Labour_JM <- res_vec$Labour_cost_JM[month]
          Labour_SubAF <- res_vec$Labour_cost_SubAF[month]
          Labour_SubAM <- res_vec$Labour_cost_SubAM[month]
          Labour_AF <- res_vec$Labour_cost_AF[month]
          Labour_AM <- res_vec$Labour_cost_AM[month]
        
          res_vec$Labour_cost[month] <- sum(res_vec$Labour_cost_JF[month],
                                            res_vec$Labour_cost_JM[month],
                                            res_vec$Labour_cost_SubAF[month],
                                            res_vec$Labour_cost_SubAM[month],
                                            res_vec$Labour_cost_AF[month],
                                            res_vec$Labour_cost_AM[month])
        
          Labour =  res_vec$Labour_cost[month]
        
        }
        
        if (species == "cattle") {
          res_vec$Feed_cost_Ox[month] <- Feed_Ox + Ox * sample(Expenditure_on_feed_Ox, 1) * 30
          Feed_Ox <- res_vec$Feed_cost_Ox[month]
          res_vec$Feed_cost[month] <- res_vec$Feed_cost[month] + res_vec$Feed_cost_Ox[month]
          Feed <- res_vec$Feed_cost[month]
          res_vec$Labour_cost_Ox[month] <- Labour_Ox + Ox * sample(Labour_cattle, 1) * lab_non_health + Ox * sample(Labour_Oxen, 1)
          Labour_Ox <- res_vec$Labour_cost_Ox[month]
          res_vec$Labour_cost[month] <- res_vec$Labour_cost[month] + res_vec$Labour_cost_Ox[month]
        }
        
        if (species == "cattle" || species == "poultry") {
    
        res_vec$Health_cost_JF[month] <- Health_JF + JF * sample(Health_exp_prev, 1) + JF * sample(Health_exp_treatment, 1) 
        res_vec$Health_cost_JM[month] <- Health_JM + JM * sample(Health_exp_prev, 1) + JM * sample(Health_exp_treatment, 1) 
        res_vec$Health_cost_SubAF[month] <- Health_SubAF + SubAF * sample(Health_exp_prev, 1) + SubAF * sample(Health_exp_treatment, 1)
        res_vec$Health_cost_SubAM[month] <- Health_SubAM + SubAM * sample(Health_exp_prev, 1) + SubAM * sample(Health_exp_treatment, 1)
        res_vec$Health_cost_AF[month] <- Health_AF + AF * sample(Health_exp_prev, 1) + AF * sample(Health_exp_treatment, 1)
        res_vec$Health_cost_AM[month] <- Health_AM + AM * sample(Health_exp_prev, 1) + AM * sample(Health_exp_treatment, 1)
        
      }
      
      if (species == "smallruminants") {
        res_vec$Health_cost_JF[month] = Health_JF + JF * sample(Health_exp, 1)
        res_vec$Health_cost_JM[month] = Health_JM + JM * sample(Health_exp, 1)
        res_vec$Health_cost_SubAF[month] = Health_SubAF + SubAF * sample(Health_exp, 1) 
        res_vec$Health_cost_SubAMM[month] = Health_SubAM + SubAM * sample(Health_exp, 1)
        res_vec$Health_cost_AF[month] = Health_AF + AF * sample(Health_exp, 1) 
        res_vec$Health_cost_AM[month] = Health_AM + AM * sample(Health_exp, 1) 
      }
        
        if (species == "cattle" || species == "smallruminants" || species == "poultry") {
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
        Health_JF <- res_vec$Health_cost_JF[month]
        Health_JM <- res_vec$Health_cost_JM[month]
        Health_SubAF <- res_vec$Health_cost_SubAF[month]
        Health_SubAM <- res_vec$Health_cost_SubAM[month]
        Health_AF <- res_vec$Health_cost_AF[month]
        Health_AM <- res_vec$Health_cost_AM[month]
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
        res_vec$Health_cost[month] <- sum(res_vec$Health_cost_JF[month],
                                        res_vec$Health_cost_JM[month],
                                        res_vec$Health_cost_SubAF[month],
                                        res_vec$Health_cost_SubAM[month],
                                        res_vec$Health_cost_AF[month],
                                        res_vec$Health_cost_AM[month])
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
        Health = res_vec$Health_cost[month]
     
      }
                                                                                                                                                                                                                                                                                                                                                                                                                                               
      if (species == "cattle") {
        res_vec$Health_cost_Ox[month] <- Health_Ox + Ox * sample(Health_exp_prev, 1) + Ox * sample(Health_exp_treatment, 1) 
        Health_Ox <- res_vec$Health_cost_Ox[month]                                                                                
        res_vec$Health_cost[month] <-  res_vec$Health_cost[month] + res_vec$Health_cost_Ox[month]
      } 
        
      if (species == "cattle" || species == "smallruminants" || species == "poultry") {
        res_vec$Capital_cost_JF[month] <- res_vec$Num_JF[1] * sample(fvJF, 1) * Interest_rate 
        Capital_JF <- res_vec$Capital_cost_JF[month]
     
        res_vec$Capital_cost_JM[month] <- res_vec$Num_JM[1] * sample(fvJM, 1) * Interest_rate  
        Capital_JM <- res_vec$Capital_cost_JM[month]
     
        res_vec$Capital_cost_SubAF[month] <-res_vec$Num_SubAF[1] * sample(fvSubAF, 1) * Interest_rate  
        Capital_SubAF = res_vec$Capital_cost_SubAF[month]
     
        res_vec$Capital_cost_SubAM[month] <- res_vec$Num_SubAM[1] * sample(fvSubAM, 1) * Interest_rate  
        Capital_SubAM <- res_vec$Capital_cost_SubAM[month]
     
        res_vec$Capital_cost_AF[month] <- res_vec$Num_AF[1] * sample(fvAF, 1) * Interest_rate  
        Capital_AF <- res_vec$Capital_cost_AF[month]
     
        res_vec$Capital_cost_AM[month] <- res_vec$Num_AM[1] * sample(fvAM, 1) * Interest_rate  
        Capital_AM <- res_vec$Capital_cost_AM[month]
     
        res_vec$Capital_cost[month] <- sum(res_vec$Capital_cost_JF[month],
                                       res_vec$Capital_cost_JM[month],
                                       res_vec$Capital_cost_SubAF[month],
                                       res_vec$Capital_cost_SubAM[month],
                                       res_vec$Capital_cost_AF[month],
                                       res_vec$Capital_cost_AM[month])
      }
     
     if (species == "cattle") {
       res_vec$Capital_cost_Ox[month] <- res_vec$NumOx[1] * sample(fvOx, 1) * Interest_rate
       Capital_Ox <- res_vec$Capital_cost_Ox[month]
       res_vec$Capital_cost[month] <- res_vec$Capital_cost[month] + res_vec$Capital_cost_Ox[month]
     }
        
    if (species == "cattle" || species == "smallruminants" || species == "poultry") {
      Capital <- res_vec$Capital_cost[month]
    }
        
    if (species == "cattle" || species == "smallruminants" || species == "poultry") {
     
     res_vec$Infrastructure_cost_JF[month] <- N_JF_t0 * sample(Infrastructure_per_head, 1)
     res_vec$Infrastructure_cost_JM[month] <- N_JM_t0 * sample(Infrastructure_per_head, 1)
     res_vec$Infrastructure_cost_SubAF[month] <- N_SubAF_t0 * sample(Infrastructure_per_head, 1)
     res_vec$Infrastructure_cost_SubAM[month] <- N_SubAM_t0 * sample(Infrastructure_per_head, 1)
     res_vec$Infrastructure_cost_AF[month] <- N_AF_t0 * sample(Infrastructure_per_head, 1)
     res_vec$Infrastructure_cost_AM[month] <- N_AM_t0 * sample(Infrastructure_per_head, 1)
     
     res_vec$Infrastructure_cost[month] <- sum(res_vec$Infrastructure_cost_JF[month],
                                               res_vec$Infrastructure_cost_JM[month],
                                               res_vec$Infrastructure_cost_SubAF[month],
                                               res_vec$Infrastructure_cost_SubAM[month],
                                               res_vec$Infrastructure_cost_AF[month],
                                               res_vec$Infrastructure_cost_AM[month])
    }
     
     if (species == "cattle") {
       res_vec$Infrastructure_cost_Ox[month] <- N_Ox_t0 * sample(Infrastructure_per_head, 1)
       res_vec$Infrastructure_cost[month] <- res_vec$Infrastructure_cost[month] + res_vec$Infrastructure_cost_Ox[month]
     }
        
    if (species == "cattle" || species == "smallruminants" || species == "poultry") {
     
     res_vec$Total_expenditure[month] <- res_vec$Feed_cost[month] + Health + Labour + Capital + res_vec$Infrastructure_cost[month]
     
     res_vec$Total_expenditure_JF[month] <- Feed_JF + Health_JF + Labour_JF + Capital_JF + res_vec$Infrastructure_cost_JF[month]
     res_vec$Total_expenditure_JM[month] <-  Feed_JM + Health_JM + Labour_JM + Capital_JM + res_vec$Infrastructure_cost_JM[month]
     res_vec$Total_expenditure_SubAF[month] <- Feed_SubAF + Health_SubAF + Labour_SubAF + Capital_SubAF + res_vec$Infrastructure_cost_SubAF[month]
     res_vec$Total_expenditure_SubAM[month] <-  Feed_SubAM + Health_SubAM + Labour_SubAM + Capital_SubAM + res_vec$Infrastructure_cost_SubAM[month]
     res_vec$Total_expenditure_AF[month] <-  Feed_AF + Health_AF + Labour_AF + Capital_AF + res_vec$Infrastructure_cost_AF[month]
     res_vec$Total_expenditure_AM[month] <- Feed_AM + Health_AM + Labour_AM + Capital_AM + res_vec$Infrastructure_cost_AM[month]
     
    }
     
     
     if (species == "cattle") {
       res_vec$Total_expenditure_Ox[month] <- Feed_Ox + Health_Ox + Labour_Ox + Capital_Ox + res_vec$Infrastructure_cost_Ox[month]
     }
     
     if (species == "poultry") {
       res_vec$Quantity_eggs_sold[month] <- Eggs_sold + AF * sample(prop_females_laying, 1) * sample(lay_rate, 1) * sample(egg_sale_rate,1) 
       res_vec$Quantity_eggs_consumed[month] <- Eggs_consumed + AF * sample(prop_females_laying, 1) * sample(lay_rate, 1) * sample(egg_consumption_rate, 1)
       
       Eggs_sold <- res_vec$Quantity_eggs_sold[month]
       Eggs_consumed <- res_vec$Quantity_eggs_consumed[month]
       
       res_vec$Value_eggs_sold[month] <- res_vec$Quantity_eggs_sold[month] * sample(egg_price, 1) 
       res_vec$Value_eggs_consumed[month] <- res_vec$Quantity_eggs_consumed[month] * sample(egg_price, 1) 
       
       res_vec$Feed_requirement_JF[month] <- Feed_JF + JF * sample(feed_req_prpn_JF, 1)
       res_vec$Feed_requirement_JM[month] <- Feed_JM + JM * sample(feed_req_prpn_JM, 1) 
       res_vec$Feed_requirement_SubAF[month] <- Feed_SubAF + SubAF * sample(feed_req_prpn_SubAF, 1)
       res_vec$Feed_requirement_SubAMM[month] <- Feed_SubAM + SubAM * sample(feed_req_prpn_SubAM, 1)
       res_vec$Feed_requirement_AF[month] <- Feed_AF + AF * sample(feed_req_prpn_AF, 1)
       res_vec$Feed_requirement_AM[month] <- Feed_AM + AM * sample(feed_req_prpn_AM, 1)
       
       Feed_JF <- res_vec$Feed_requirement_JF[month]
       Feed_JM <- res_vec$Feed_requirement_JM[month]
       Feed_SubAF <- res_vec$Feed_requirement_SubAF[month]
       Feed_SubAM <- res_vec$Feed_requirement_SubAM[month]
       Feed_AF <- res_vec$Feed_requirement_AF[month]
       Feed_AM <- res_vec$Feed_requirement_AM[month]
       
       res_vec$Feed_requirement[month] <- sum(res_vec$Feed_requirement_JF[month],
                                              res_vec$Feed_requirement_JM[month],
                                              res_vec$Feed_requirement_SubAF[month][month],
                                              res_vec$Feed_requirement_SubAM[month],
                                              res_vec$Feed_requirement_AF[month],
                                              res_vec$Feed_requirement_AM[month])
       
       Feed <- res_vec$Feed_requirement[month]
       
       res_vec$Feed_purchased_JF[month] <- res_vec$Feed_requirement_JF[month] * sample(prpn_lskeepers_purch_feed, 1) * sample(prpn_feed_paid_for, 1)
       res_vec$Feed_purchased_JM[month] <- res_vec$Feed_requirement_JM[month] * sample(prpn_lskeepers_purch_feed, 1) * sample(prpn_feed_paid_for, 1)
       res_vec$Feed_purchased_SubAF[month] <- res_vec$Feed_requirement_SubAF[month] * sample(prpn_lskeepers_purch_feed, 1) * sample(prpn_feed_paid_for, 1)
       res_vec$Feed_purchased_SubAM[month] <- res_vec$Feed_requirement_SubAM[month] * sample(prpn_lskeepers_purch_feed, 1) * sample(prpn_feed_paid_for, 1)
       res_vec$Feed_purchased_AF[month] <- res_vec$Feed_requirement_AF[month] * sample(prpn_lskeepers_purch_feed, 1) * sample(prpn_feed_paid_for, 1)
       res_vec$Feed_purchased_AM[month] <- res_vec$Feed_requirement_AM[month] * sample(prpn_lskeepers_purch_feed, 1) * sample(prpn_feed_paid_for, 1)
       
       res_vec$Feed_purchased <- sum(res_vec$Feed_purchased_NF[month],
                                     res_vec$Feed_purchased_NM[month],
                                     res_vec$Feed_purchased_JF[month],
                                     res_vec$Feed_purchased_JM[month],
                                     res_vec$Feed_purchased_AF[month],
                                     res_vec$Feed_purchased_AM[month])
       
       
     }

    } # end Num_months loop
    
    res_mat$Num_JF[i, ] <- res_vec$Num_JF
    res_mat$Num_SubAF[i, ] <- res_vec$Num_SubAF
    res_mat$Num_AF[i, ] <- res_vec$Num_AF
    res_mat$Num_JM[i, ] <- res_vec$Num_JM
    res_mat$Num_SubAM[i, ] <- res_vec$Num_SubAM
    res_mat$Num_AM[i, ] <- res_vec$Num_AM
    res_mat$Num_N[i, ] <- res_vec$Num_N
    
    res_mat$Monthly_mortality[i, ] <- res_vec$Monthly_mortality
    res_mat$Total_mortality[i, ] <- res_vec$Total_mortality
    
    res_mat$Total_mortality_JF[i, ] <- res_vec$Total_mortality_JF
    res_mat$Total_mortality_JM[i, ] <- res_vec$Total_mortality_JM
    res_mat$Total_mortality_SubAF[i, ] <- res_vec$Total_mortality_SubAF
    res_mat$Total_mortality_SubAM[i, ] <- res_vec$Total_mortality_SubAM
    res_mat$Total_mortality_AF[i, ] <- res_vec$Total_mortality_AF
    res_mat$Total_mortality_AM[i, ] <- res_vec$Total_mortality_AM
    
    res_mat$Value_of_total_mortality[i, ] <- res_vec$Value_of_total_mortality
    
    res_mat$Value_of_total_mortality_JF[i, ] <- res_vec$Value_of_total_mortality_JF
    res_mat$Value_of_total_mortality_JM[i, ] <- res_vec$Value_of_total_mortality_JM
    res_mat$Value_of_total_mortality_SubAF[i, ] <- res_vec$Value_of_total_mortality_SubAF
    res_mat$Value_of_total_mortality_SubAM[i, ] <- res_vec$Value_of_total_mortality_SubAM
    res_mat$Value_of_total_mortality_AF[i, ] <- res_vec$Value_of_total_mortality_AF
    res_mat$ Value_of_total_mortality_AM[i, ] <- res_vec$Value_of_total_mortality_AM
    
    res_mat$Quantity_liveweight_kg[i, ] <- res_vec$Quantity_liveweight_kg
    
    res_mat$Quantity_liveweight_kg_JF[i, ] <- res_vec$Quantity_liveweight_kg_JF
    res_mat$Quantity_liveweight_kg_JM[i, ] <- res_vec$Quantity_liveweight_kg_JM
    res_mat$Quantity_liveweight_kg_SubAF[i, ] <- res_vec$Quantity_liveweight_kg_SubAF
    res_mat$Quantity_liveweight_kg_SubAM[i, ] <- res_vec$Quantity_liveweight_kg_SubAM
    res_mat$Quantity_liveweight_kg_AF[i, ] <- res_vec$Quantity_liveweight_kg_AF
    res_mat$Quantity_liveweight_kg_AM[i, ] <- res_vec$Quantity_liveweight_kg_AM
    
    res_mat$Cumulative_draught_income[i,] <- res_vec$Cumulative_draught_income

    res_mat$Quantity_meat_kg[i, ] <- res_vec$Quantity_meat_kg
    
    res_mat$Num_offtake[i, ] <- res_vec$Num_offtake
    
    res_mat$Num_offtake_SubAF[i, ] <- res_vec$Num_offtake_SubAF
    res_mat$Num_offtake_SubAM[i, ] <- res_vec$Num_offtake_SubAM
    res_mat$Num_offtake_SubAF[i, ] <- res_vec$Num_offtake_SubAF
    res_mat$Num_offtake_SubAM[i, ] <- res_vec$Num_offtake_SubAM
    res_mat$Num_offtake_AF[i, ] <- res_vec$Num_offtake_AF
    res_mat$Num_offtake_AM[i, ] <- res_vec$Num_offtake_AM
    
    res_mat$Offtake_liveweight_kg[i, ] <- res_vec$Offtake_liveweight_kg
    
    res_mat$Offtake_liveweight_kg_SubAF[i, ] <- res_vec$Offtake_liveweight_kg_SubAF
    res_mat$Offtake_liveweight_kg_SubAM[i, ] <- res_vec$Offtake_liveweight_kg_SubAM
    res_mat$Offtake_liveweight_kg_AF[i, ] <- res_vec$Offtake_liveweight_kg_AF
    res_mat$Offtake_liveweight_kg_AM[i, ] <- res_vec$Offtake_liveweight_kg_AM
    
    res_mat$Pop_growth[i, ] <- res_vec$Pop_growth
    
    res_mat$Pop_growth_SubAF[i, ] <- res_vec$Pop_growth_SubAF
    res_mat$Pop_growth_SubAM[i, ] <- res_vec$Pop_growth_SubAM
    res_mat$Pop_growth_SubAF[i, ] <- res_vec$Pop_growth_SubAF
    res_mat$Pop_growth_SubAM[i, ] <- res_vec$Pop_growth_SubAM
    res_mat$Pop_growth_AF[i, ] <- res_vec$Pop_growth_AF
    res_mat$Pop_growth_AM[i, ] <- res_vec$Pop_growth_AM
    
    res_mat$Monthly_growth_rate[i, ] <- res_vec$Monthly_growth_rate
    res_mat$Monthly_pop_growth[i, ] <- res_vec$Monthly_pop_growth
    
    res_mat$Quantity_manure[i, ] <- res_vec$Quantity_manure
    res_mat$Quantity_manure_JF[i, ] <- res_vec$Quantity_manure_JF
    res_mat$Quantity_manure_JM[i, ] <- res_vec$Quantity_manure_JM
    res_mat$Quantity_manure_SubAF[i, ] <- res_vec$Quantity_manure_SubAF
    res_mat$Quantity_manure_SubAM[i, ] <- res_vec$Quantity_manure_SubAM
    res_mat$Quantity_manure_AF[i, ] <- res_vec$Quantity_manure_AF
    res_mat$Quantity_manure_AM[i, ] <- res_vec$Quantity_manure_AM
    
    res_mat$Quantity_hides[i, ] <- res_vec$Quantity_hides
    
    res_mat$Quantity_hides_SubAF[i, ] <- res_vec$Quantity_hides_SubAF
    res_mat$Quantity_hides_SubAM[i, ] <- res_vec$Quantity_hides_SubAM
    res_mat$Quantity_hides_AF[i, ] <- res_vec$Quantity_hides_AF
    res_mat$Quantity_hides_AM[i, ] <- res_vec$Quantity_hides_AM
    
    res_mat$Quantity_milk[i, ] <- res_vec$Quantity_milk
    # res_mat$Quantity_Wool[i, ] <- res_vec$Quantity_Wool
    
    res_mat$Cumulative_dry_matter[i, ] <- res_vec$Cumulative_dry_matter
    
    res_mat$Cumulative_dry_matter_JF[i, ] <- res_vec$Cumulative_dry_matter_JF
    res_mat$Cumulative_dry_matter_JM[i, ] <- res_vec$Cumulative_dry_matter_JM
    res_mat$Cumulative_dry_matter_SubAF[i, ] <- res_vec$Cumulative_dry_matter_SubAF
    res_mat$Cumulative_dry_matter_SubAM[i, ] <- res_vec$Cumulative_dry_matter_SubAM
    res_mat$Cumulative_dry_matter_AF[i, ] <- res_vec$Cumulative_dry_matter_AF
    res_mat$Cumulative_dry_matter_AM[i, ] <- res_vec$Cumulative_dry_matter_AM
    
    res_mat$Monthly_DM[i, ] <- res_vec$Monthly_DM

    res_mat$Value_offtake[i, ] <- res_vec$Value_offtake
    res_mat$Value_offtake_JF[i, ] <- res_vec$Value_offtake_JF
    res_mat$Value_offtake_JM[i, ] <- res_vec$Value_offtake_JM
    
    res_mat$Value_offtake_SubAF[i, ] <- res_vec$Value_offtake_SubAF
    res_mat$Value_offtake_SubAM[i, ] <- res_vec$Value_offtake_SubAM
    res_mat$Value_offtake_AF[i, ] <- res_vec$Value_offtake_AF
    res_mat$Value_offtake_AM[i, ] <- res_vec$Value_offtake_AM
    
    res_mat$Value_herd_increase[i, ] <- res_vec$Value_herd_increase
    
    res_mat$Value_herd_increase_JF[i, ] <- res_vec$Value_herd_increase_JF
    res_mat$Value_herd_increase_JM[i, ] <- res_vec$Value_herd_increase_JM
    res_mat$Value_herd_increase_SubAF[i, ] <- res_vec$Value_herd_increase_SubAF
    res_mat$Value_herd_increase_SubAM[i, ] <- res_vec$Value_herd_increase_SubAM
    res_mat$Value_herd_increase_AF[i, ] <- res_vec$Value_herd_increase_AF
    res_mat$Value_herd_increase_AM[i, ] <- res_vec$Value_herd_increase_AM
    
    res_mat$Total_value_increase[i, ] <- res_vec$Total_value_increase
    
    res_mat$Total_value_increase_JF[i, ] <- res_vec$Total_value_increase_JF
    res_mat$Total_value_increase_JM[i, ] <- res_vec$Total_value_increase_JM
    res_mat$Total_value_increase_SubAF[i, ] <- res_vec$Total_value_increase_SubAF
    res_mat$Total_value_increase_SubAM[i, ] <- res_vec$Total_value_increase_SubAM
    res_mat$Total_value_increase_AF[i, ] <- res_vec$Total_value_increase_AF
    res_mat$Total_value_increase_AM[i, ] <- res_vec$Total_value_increase_AM
    
    res_mat$Feed_cost[i, ] <- res_vec$Feed_cost
    
    res_mat$Feed_cost_JF[i, ] <- res_vec$Feed_cost_JF
    res_mat$Feed_cost_JM[i, ] <- res_vec$Feed_cost_JM
    res_mat$Feed_cost_SubAF[i, ] <- res_vec$Feed_cost_SubAF
    res_mat$Feed_cost_SubAM[i, ] <- res_vec$Feed_cost_SubAM
    res_mat$Feed_cost_AF[i, ] <- res_vec$Feed_cost_AF
    res_mat$Feed_cost_AM[i, ] <- res_vec$Feed_cost_AM
    
    res_mat$Labour_cost[i, ] <- res_vec$Labour_cost
    
    res_mat$Labour_cost_JF[i, ] <- res_vec$Labour_cost_JF
    res_mat$Labour_cost_JM[i, ] <- res_vec$Labour_cost_JM
    res_mat$Labour_cost_SubAF[i, ] <- res_vec$Labour_cost_SubAF
    res_mat$Labour_cost_SubAM[i, ] <- res_vec$Labour_cost_SubAM
    res_mat$Labour_cost_AF[i, ] <- res_vec$Labour_cost_AF
    res_mat$Labour_cost_AM[i, ] <- res_vec$Labour_cost_AM
    
    res_mat$Health_cost[i, ] <- res_vec$Health_cost
    
    res_mat$Health_cost_JF[i, ] <- res_vec$Health_cost_JF
    res_mat$Health_cost_JM[i, ] <- res_vec$Health_cost_JM
    res_mat$Health_cost_SubAF[i, ] <- res_vec$Health_cost_SubAF
    res_mat$Health_cost_SubAM[i, ] <- res_vec$Health_cost_SubAM
    res_mat$Health_cost_AF[i, ] <- res_vec$Health_cost_AF
    res_mat$Health_cost_AM[i, ] <- res_vec$Health_cost_AM
    
    res_mat$Capital_cost[i, ] <- res_vec$Capital_cost
    
    res_mat$Capital_cost_JF[i, ] <- res_vec$Capital_cost_JF
    res_mat$Capital_cost_JM[i, ] <- res_vec$Capital_cost_JM
    res_mat$Capital_cost_SubAF[i, ] <- res_vec$Capital_cost_SubAF
    res_mat$Capital_cost_SubAM[i, ] <- res_vec$Capital_cost_SubAM
    res_mat$Capital_cost_AF[i, ] <- res_vec$Capital_cost_AF
    res_mat$Capital_cost_AM[i, ] <- res_vec$Capital_cost_AM
    
    res_mat$Infrastructure_cost[i, ] <- res_vec$Infrastructure_cost
    res_mat$Infrastructure_cost_JF[i, ] <- res_vec$Infrastructure_cost_JF
    res_mat$Infrastructure_cost_JM[i, ] <- res_vec$Infrastructure_cost_JM
    res_mat$Infrastructure_cost_SubAF[i, ] <- res_vec$Infrastructure_cost_SubAF
    res_mat$Infrastructure_cost_SubAM[i, ] <- res_vec$Infrastructure_cost_SubAM
    res_mat$Infrastructure_cost_AF[i, ] <- res_vec$Infrastructure_cost_AF
    res_mat$Infrastructure_cost_AM[i, ] <- res_vec$Infrastructure_cost_AM
    
    res_mat$Total_expenditure[i, ] <- res_vec$Total_expenditure

    res_mat$Total_expenditure_JF[i, ] <- res_vec$Total_expenditure_JF
    res_mat$Total_expenditure_JM[i, ] <- res_vec$Total_expenditure_JM
    res_mat$Total_expenditure_SubAF[i, ] <- res_vec$Total_expenditure_SubAF
    res_mat$Total_expenditure_SubAM[i, ] <- res_vec$Total_expenditure_SubAM
    res_mat$Total_expenditure_AF[i, ] <- res_vec$Total_expenditure_AF
    res_mat$Total_expenditure_AM[i, ] <- res_vec$Total_expenditure_AM
    
    if (species == "cattle") {
      res_mat$NumOx[i, ] <- res_vec$NumOx
      res_mat$Total_mortality_Ox[i, ] <- res_vec$Total_mortality_Ox
      res_mat$Total_mortality_Ox[i, ] <- res_vec$Total_mortality_Ox
      res_mat$Value_of_total_mortality_Ox[i, ] <- res_vec$Value_of_total_mortality_Ox
      res_mat$Quantity_liveweight_kg_Ox[i, ] <- res_vec$Quantity_liveweight_kg_Ox
      res_mat$Num_offtake_Ox[i, ] <- res_vec$Num_offtake_Ox
      res_mat$Offtake_liveweight_kg_Ox[i, ] <- res_vec$Offtake_liveweight_kg_Ox
      res_mat$Pop_growth_Ox[i, ] <- res_vec$Pop_growth_Ox
      res_mat$Quantity_manure_Ox[i, ] <- res_vec$Quantity_manure_Ox
      res_mat$Quantity_hides_Ox[i, ] <- res_vec$Quantity_hides_Ox
      res_mat$Cumulative_dry_matter_Ox[i, ] <- res_vec$Cumulative_dry_matter_Ox
      res_mat$Value_offtake_Ox[i, ] <- res_vec$Value_offtake_Ox
      res_mat$Value_herd_increase_Ox[i, ] <- res_vec$Value_herd_increase_Ox
      res_mat$Total_value_increase_Ox[i, ] <- res_vec$Total_value_increase_Ox
      res_mat$Feed_cost_Ox[i, ] <- res_vec$Feed_cost_Ox
      res_mat$Labour_cost_Ox[i, ] <- res_vec$Labour_cost_Ox
      res_mat$Health_cost_Ox[i, ] <- res_vec$Health_cost_Ox
      res_mat$Capital_cost_Ox[i, ] <- res_vec$Capital_cost_Ox
      res_mat$Infrastructure_cost_Ox[i, ] <- res_vec$Infrastructure_cost_Ox
      res_mat$Total_expenditure_Ox[i, ] <- res_vec$Total_expenditure_Ox
    }
    
    if (species == "poultry") {
      res_mat$Quantity_eggs_consumed[i, ] <- res_vec$Quantity_eggs_consumed
      res_mat$Quantity_eggs_sold[i, ] <- res_vec$Quantity_eggs_sold
      
      res_mat$Value_eggs_consumed[i, ] <- res_vec$Value_eggs_consumed
      res_mat$Value_eggs_sold[i, ] <- res_vec$Value_eggs_sold
      
      res_mat$Feed_requirement[i, ] <- res_vec$Feed_requirement
    }
    
  } # end nruns loop
  
  Total_number_change_JF <- res_mat$Num_offtake_SubAF + res_mat$Pop_growth_SubAF
  Total_number_change_JM <- res_mat$Num_offtake_SubAM + res_mat$Pop_growth_SubAM
  Total_number_change_SubAF <- res_mat$Num_offtake_SubAF + res_mat$Pop_growth_SubAF
  Total_number_change_SubAM <- res_mat$Num_offtake_SubAM + res_mat$Pop_growth_SubAM
  Total_number_change_AF <- res_mat$Num_offtake_AF + res_mat$Pop_growth_AF
  Total_number_change_AM <- res_mat$Num_offtake_AM + res_mat$Pop_growth_AM
  
  Total_number_change <- sum(Total_number_change_JF,
                             Total_number_change_JM,
                             Total_number_change_SubAF,
                             Total_number_change_SubAM,
                             Total_number_change_AF,
                             Total_number_change_AM)
  
  if (species == "cattle" || species == "smallruminants") {
    
    Value_hides_SubAF <- res_mat$Quantity_hides_SubAF * sample(hides_value, 1)
    Value_hides_SubAM <- res_mat$Quantity_hides_SubAM * sample(hides_value, 1)
    Value_hides_AF <- res_mat$Quantity_hides_AF * sample(hides_value, 1)
    Value_hides_AM <- res_mat$Quantity_hides_AM * sample(hides_value, 1)
    
    Value_hides <- sum(Value_hides_SubAF,
                       Value_hides_SubAM,
                       Value_hides_AF,
                       Value_hides_AM)
    
    Quantity_hides_SubA <- res_mat$Quantity_hides_SubAF + res_mat$Quantity_hides_SubAM
    
    Value_hides_SubA <- res_mat$Value_hides_SubAF + res_mat$Value_hides_SubAM
    
    Value_Manure <- res_mat$Quantity_manure * Man_value
    Value_Manure_JF <- res_mat$Quantity_manure_JF * Man_value
    Value_Manure_JM <- res_mat$Quantity_manure_JM * Man_value
    Value_Manure_SubAF <- res_mat$Quantity_manure_SubAF * Man_value
    Value_Manure_SubAM <- res_mat$Quantity_manure_SubAM * Man_value
    Value_Manure_AF <- res_mat$Quantity_manure_AF * Man_value
    Value_Manure_AM <- res_mat$Quantity_manure_AM * Man_value
    
    Production_value_herd_offtake_hide_manure_JF <- res_mat$Total_value_increase_JF + res_mat$Value_Manure_JF
    Production_value_herd_offtake_hide_manure_JM <- res_mat$Total_value_increase_JM + res_mat$Value_Manure_JM
    Production_value_herd_offtake_hide_manure_SubAF <- res_mat$Total_value_increase_SubAF + res_mat$Value_Manure_SubAF + res_mat$Value_hides_SubAF
    Production_value_herd_offtake_hide_manure_SubAM <- res_mat$Total_value_increase_SubAM + res_mat$Value_Manure_SubAM + Value_hides_SubAM
    Production_value_herd_offtake_hide_manure_AF <- res_mat$Total_value_increase_AF + res_mat$Value_Manure_AF + res_mat$Value_hides_AF + res_mat$Value_Milk
    Production_value_herd_offtake_hide_manure_AM <- res_mat$Total_value_increase_AM + res_mat$Value_Manure_AM + res_mat$Value_hides_AM
    
    Production_value_herd_offtake_hide_manure <- sum(Production_value_herd_offtake_hide_manure_JF,
                                                     Production_value_herd_offtake_hide_manure_JM, 
                                                     Production_value_herd_offtake_hide_manure_SubAF,
                                                     Production_value_herd_offtake_hide_manure_SubAM + 
                                                     Production_value_herd_offtake_hide_manure_AF,
                                                     Production_value_herd_offtake_hide_manure_AM)
    
  }
  
  
  
  if (species == "poultry") {
    Production_value_herd_offtake_hide_manure_AF <- res_mat$Total_value_increase_AF + res_mat$Value_eggs_sold + res_mat$Value_eggs_consumed
  }
  
  Gross_margin <- res_mat$Production_value_herd_offtake_hide_manure - res_mat$Total_expenditure
  Gross_margin_JF <- res_mat$Production_value_herd_offtake_hide_manure_JF - res_mat$Total_expenditure_JF
  Gross_margin_JM <- res_mat$Production_value_herd_offtake_hide_manure_JM - res_mat$Total_expenditure_JM
  Gross_margin_SubAF <- res_mat$Production_value_herd_offtake_hide_manure_SubAF - res_mat$Total_expenditure_SubAF
  Gross_margin_SubAM <- res_mat$Production_value_herd_offtake_hide_manure_SubAM - res_mat$Total_expenditure_SubAM
  Gross_margin_AF <- res_mat$Production_value_herd_offtake_hide_manure_AF - res_mat$Total_expenditure_AF
  Gross_margin_AM <- res_mat$Production_value_herd_offtake_hide_manure_AM - res_mat$Total_expenditure_AM
  
  Num_offtake_J <- res_mat$Num_offtake_SubAF + res_mat$Num_offtake_SubAM
  Num_offtake_SubA <- res_mat$Num_offtake_SubAF + res_mat$Num_offtake_SubAM
  
  Pop_growth_J <- res_mat$Pop_growth_SubAF + res_mat$Pop_growth_SubAM
  Pop_growth_SubA <- res_mat$Pop_growth_SubAF + res_mat$Pop_growth_SubAM

  Total_number_change_J <- res_mat$Num_offtake_J + res_mat$Pop_growth_J
  Total_number_change_SubA <- res_mat$Num_offtake_SubA + res_mat$Pop_growth_SubA
  
  Total_mortality_J <- res_mat$Total_mortality_JF + res_mat$Total_mortality_JM
  Total_mortality_SubA <- res_mat$Total_mortality_SubAF + res_mat$Total_mortality_SubAM
  
  Value_of_total_mortality_J <- res_mat$Value_of_total_mortality_JF + res_mat$Value_of_total_mortality_JM
  Value_of_total_mortality_SubA <- res_mat$Value_of_total_mortality_SubAF + res_mat$Value_of_total_mortality_SubAM
  
  Quantity_liveweight_kg_SubA <- res_mat$Quantity_liveweight_kg_SubAF + res_mat$Quantity_liveweight_kg_SubAM
  
  Quantity_manure_J <- res_mat$Quantity_manure_JF + res_mat$Quantity_manure_JM
  Quantity_manure_SubA <- res_mat$Quantity_manure_SubAF + res_mat$Quantity_manure_SubAM
  
  Cumulative_dry_matter_J <- res_mat$Cumulative_dry_matter_JF + res_mat$Cumulative_dry_matter_JM
  Cumulative_dry_matter_SubA <- res_mat$Cumulative_dry_matter_SubAF + res_mat$Cumulative_dry_matter_SubAM
  
  Value_offtake_J <- res_mat$Value_offtake_JF + res_mat$Value_offtake_JM
  Value_offtake_SubA <- res_mat$Value_offtake_SubAF + res_mat$Value_offtake_SubAM
  
  Value_herd_increase_J <- res_mat$Value_herd_increase_JF + res_mat$Value_herd_increase_JM
  Value_herd_increase_SubA <- res_mat$Value_herd_increase_SubAF + res_mat$Value_herd_increase_SubAM
  
  Total_value_increase_J <- res_mat$Total_value_increase_JF + res_mat$Total_value_increase_JM
  Total_value_increase_SubA <- res_mat$Total_value_increase_SubAF + res_mat$Total_value_increase_SubAM
  
  Value_Manure_J <- res_mat$Value_Manure_JF + res_mat$Value_Manure_JM
  Value_Manure_SubA <- res_mat$Value_Manure_SubAF + res_mat$Value_Manure_SubAM
  
  
  Production_value_herd_offtake_hide_manure_J <- res_mat$Production_value_herd_offtake_hide_manure_JF + res_mat$Production_value_herd_offtake_hide_manure_JM
  Production_value_herd_offtake_hide_manure_SubA <- res_mat$Production_value_herd_offtake_hide_manure_SubAF + res_mat$Production_value_herd_offtake_hide_manure_SubAM
  
  Feed_cost_J <- res_mat$Feed_cost_JF + res_mat$Feed_cost_JM
  Feed_cost_SubA <- res_mat$Feed_cost_SubAF + res_mat$Feed_cost_SubAM
  
  Labour_cost_J <- res_mat$Labour_cost_JF + res_mat$Labour_cost_JM
  Labour_cost_SubA <- res_mat$Labour_cost_SubAF + res_mat$Labour_cost_SubAM
  
  Health_cost_J <- res_mat$Health_cost_JF + res_mat$Health_cost_JM
  Health_cost_SubA <- res_mat$Health_cost_SubAF + res_mat$Health_cost_SubAM
  
  Capital_cost_J <- res_mat$Capital_cost_JF + res_mat$Capital_cost_JM
  Capital_cost_SubA <- res_mat$Capital_cost_SubAF + res_mat$Capital_cost_SubAM
  
  Infrastructure_cost_J <- res_mat$Infrastructure_cost_JF + res_mat$Infrastructure_cost_JM
  Infrastructure_cost_SubA <- res_mat$Infrastructure_cost_SubAF + res_mat$Infrastructure_cost_SubAM
  
  Total_expenditure_J <- res_mat$Total_expenditure_N + res_mat$Total_expenditure_JM
  Total_expenditure_SubA <- res_mat$Total_expenditure_SubAF + res_mat$Total_expenditure_SubAM
  
  Gross_margin_J <- res_mat$Gross_margin_JF + res_mat$Gross_margin_JM
  Gross_margin_SubA <- res_mat$Gross_margin_SubAF + res_mat$Gross_margin_SubAM
  
  if (species == "cattle" || species == "smallruminants") {
    Value_Milk <- res_mat$Quantity_milk * milk_value_ltr
  }
  
  if (species == "cattle") {
    Total_number_change_Ox <- res_mat$Num_offtake_Ox + res_mat$Pop_growth_Ox
    Total_number_change <- res_mat$Total_number_change + res_mat$Total_number_change_Ox
    Value_hides_Ox <- res_mat$Quantity_hides_AM * sample(hides_value, 1)
    Value_hides <- res_mat$Value_hides + res_mat$Value_hides_Ox
    Value_Manure_Ox <- res_mat$Quantity_manure_Ox * Man_value
    Production_value_herd_offtake_hide_manure_Ox <- res_mat$Total_value_increase_Ox + res_mat$Value_Manure_Ox + res_mat$Value_hides_Ox + res_mat$Cumulative_draught_income
    Production_value_herd_offtake_hide_manure <- res_mat$Production_value_herd_offtake_hide_manure + res_mat$Production_value_herd_offtake_hide_manure_Ox
    Gross_margin_Ox <- res_mat$Production_value_herd_offtake_hide_man_Ox - res_mat$Total_expenditure_Ox
  }
  
  
  ### output results to a CSV file ###
  
  # apply_summary_last_column <- function(mat) {
  #   mat_summary <- summary(mat[, ncol(mat)])
  #   mat_sd <- sd(mat[, ncol(mat)])
  #   mat_summary <- c(mat_summary, SD = mat_sd)
  # }
  # 
  # summary_list <- lapply(res_mat, apply_summary_last_column)
  # 
  # df <- as.data.frame(do.call(rbind, summary_list))
  # 
  # df$Variable <- rownames(df)
  # 
  # df <- df[, c("Variable", "Min.", "1st Qu.", "Median", "Mean", "SD", "3rd Qu.", "Max.")]
  # df
  
  apply_last_column <- function(mat) {
    mat_last_column <- mat[, ncol(mat), drop = TRUE]  
    return(mat_last_column)
  }
  
  mat_list <- lapply(res_mat, apply_last_column)
  
  df <- as.data.frame(do.call(rbind, mat_list))
  
  rownames(df) <- names(mat_list)
  

} # end function

