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
  
  if (species == "cattle") {
    kg_DM_req_O <- calculate_dry_matter_requirements(lwO, prpn_lskeepers_purch_feed, DM_req_prpn_O)
  }
  
  # Calculate purchased feed
  DM_purch_NF <- calculate_purchased_feed(kg_DM_req_NF, prpn_lskeepers_purch_feed, prpn_feed_paid_for, DM_in_feed)
  DM_purch_NM <- calculate_purchased_feed(kg_DM_req_NM, prpn_lskeepers_purch_feed, prpn_feed_paid_for, DM_in_feed)
  DM_purch_JF <- calculate_purchased_feed(kg_DM_req_JF, prpn_lskeepers_purch_feed, prpn_feed_paid_for, DM_in_feed)
  DM_purch_JM <- calculate_purchased_feed(kg_DM_req_JM, prpn_lskeepers_purch_feed, prpn_feed_paid_for, DM_in_feed)
  DM_purch_AF <- calculate_purchased_feed(kg_DM_req_AF, prpn_lskeepers_purch_feed, prpn_feed_paid_for, DM_in_feed)
  DM_purch_AM <- calculate_purchased_feed(kg_DM_req_AM, prpn_lskeepers_purch_feed, prpn_feed_paid_for, DM_in_feed)
  
  if (species == "cattle") {
    DM_purch_O <- calculate_purchased_feed(kg_DM_req_O, prpn_lskeepers_purch_feed, prpn_feed_paid_for, DM_in_feed)
  }
  
  # Calculate KG Feed purchased
  KG_Feed_purchased_NF <- DM_purch_NF / DM_in_feed
  KG_Feed_purchased_NM <- DM_purch_NM / DM_in_feed
  KG_Feed_purchased_JF <- DM_purch_JF / DM_in_feed
  KG_Feed_purchased_JM <- DM_purch_JM / DM_in_feed
  KG_Feed_purchased_AF <- DM_purch_AF / DM_in_feed
  KG_Feed_purchased_AM <- DM_purch_AM / DM_in_feed
  
  if (species == "cattle") {
    KG_Feed_purchased_O <- DM_purch_O / DM_in_feed
  }
  
  # Calculate expenditure on feed
  Expenditure_on_feed_NF <- calculate_expenditure_on_feed(KG_Feed_purchased_NF, Feed_cost_kg)
  Expenditure_on_feed_NM <- calculate_expenditure_on_feed(KG_Feed_purchased_NM, Feed_cost_kg)
  Expenditure_on_feed_JF <- calculate_expenditure_on_feed(KG_Feed_purchased_JF, Feed_cost_kg)
  Expenditure_on_feed_JM <- calculate_expenditure_on_feed(KG_Feed_purchased_JM, Feed_cost_kg)
  Expenditure_on_feed_AF <- calculate_expenditure_on_feed(KG_Feed_purchased_AF, Feed_cost_kg)
  Expenditure_on_feed_AM <- calculate_expenditure_on_feed(KG_Feed_purchased_AM, Feed_cost_kg)
  
  if (species == "cattle") {
    Expenditure_on_feed_O <- calculate_expenditure_on_feed(KG_Feed_purchased_O, Feed_cost_kg)
  }
  
  # List of variable categories'
  categories <- c(
    "Num",
    "NumNF",
    "NumNM",
    "NumJF",
    "NumJM",
    "NumAF",
    "NumAM",
    
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
    
    "Offtake_NF", 
    "Offtake_NM", 
    "Offtake_JF", 
    "Offtake_JM", 
    "Offtake_AF", 
    "Offtake_AM", 
    
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
    categories <- append(categories, c("Oxen_J", 
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
    categories <- append(categories, "Quant_Wool")
  } else {
    # poultry
    categories <- append(categories, c("Prop_females_laying",
                         "Lay_rate",
                         "Egg_brood_rate",
                         "Egg_sale_rate",
                         "Egg_consumption_rate",
                         "Hatch_rate",
                         "Egg_price"))
  }
  
  # Initialize a list to store the matrices
  res <- list()
  
  # Loop through categories and create matrices
  for (cat in categories) {
    res[[cat]] <- matrix(0, nrow = nruns, ncol = Num_months)
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
    
    culls <- 0
    
    Num_dead <- rep(0, length(age_sex_groups))
    
    ## Create empty variables to be used for calculating production
    production_vars <- c("Liveweight_kg", "Offtake", "Offtake_Liveweight", "Manure_kg", "Hides", "Milk", "Meat_kg", "Draught_income",
                         "Cumulative_DM", "Monthly_Dry_Matter", "Population_growth_rate", "Monthly_growth_rate", "Monthly_pop_growth",
                         "Value_offt", "Value_herd_inc", "Feed", "Labour", "Health", "Capital")
    
    if (species == "small ruminants") {
      production_vars <- append(production_vars, "Wool")
    }
    
    for (var in production_vars) {
      assign(var, rep(0, length(age_sex_groups)))
    }
    
    for (month in 1:Num_months) {
      # check and fix these - only 12 elements instead of nruns elements filled
      res$Births[month] <- sample(Mu, 1) * AF 
      
      res$Deaths_NF[month] <- sample(AlphaN, 1) * NF
      res$Deaths_JF[month] <- sample(AlphaJ, 1) * JF 
      res$deaths_AF[month] <- sample(AlphaF, 1) * AF
      
      res$Deaths_NM[month] <- sample(AlphaN, 1) * NM
      res$Deaths_JM[month] <- sample(AlphaJ, 1) * JM
      res$Deaths_AM[month] <- sample(AlphaM, 1) * AM
      res$Deaths_O[month] <- sample(AlphaO, 1) * O
      
      res$Offtake_NF[month] <- (sample(GammaNF, 1) * NF) #
      res$fftake_NM[month] <- (sample(GammaNM, 1) * NM)
      
      res$Offtake_JF[month] <- sample(GammaJF, 1) * JF 
      res$Offtake_AF[month] <- sample(GammaAF, 1) * AF
      res$Offtake_JM[month] <- sample(GammaJM, 1) * JM 
      res$Offtake_AM[month] <- sample(GammaAM, 1) * AM
      res$Offtake_O[month] <- sample(GammaO, 1) * O
      
      res$Oxen_A[month] <- sample(castration_rate, 1) * AM
      
      res$Growth_NF[month] <- sample(Beta_N, 1) * NF
      res$Growth_JF[month] <- sample(Beta_J, 1) * JF
      res$Growth_NM[month] <- sample(Beta_N, 1) * NM
      res$Growth_JM[month] <- sample(Beta_J, 1) * JM
      
      res$Culls_AF[month] <- sample(CullF, 1) * AF
      res$Culls_AM[month] <- sample(CullM, 1) * AM
      res$Culls_O[month] <- sample(CullO, 1) * O
      
      res$NumNF[month] = NF + res$Births[month] * 0.5 - res$Deaths_NF[month] - res$Growth_NF[month] - res$Offtake_NF[month]
      res$NumJF[month] = JF + res$Growth_NF[month] - res$Growth_JF[month] - res$Offtake_JF[month] - res$Deaths_JF[month]
      res$NumAF[month] = AF + res$Growth_JF[month] - res$Offtake_AF[month] - res$Deaths_AF[month] - res$Culls_AF[month]
      
      res$NumNM[month] = NM + res$Births[month] * 0.5 - growth_NM[month] - res$Deaths_NM[month] - res$Offtake_NM[month]
      res$NumJM[month] = JM + res$Growth_NM[month] - res$Growth_JM[month] - res$Offtake_JM[month] - res$Deaths_JM[month] 
      res$NumAM[month] = AM + res$Growth_JM[month] - res$Offtake_AM[month] - res$Deaths_AM[month] - res$Culls_AM[month] - res$Oxen_A[month]
      res$NumO[month] = O + res$Oxen_A[month] - res$Offtake_O[month] - res$Deaths_O[month] - res$Culls_O[month]
      
      res$numN[month] = res$NumNF[month] + res$umJF[month] + res$NumAF[month] + res$NumNM[month] + res$NumJM[month] + res$NumAM[month] + res$NumO[month]
      
      
    } # end Num_months loop
    
    
  } # end nruns loop
  
} # end function


