### GEMMA EDITS VERSION 3 ###

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

run_compartmental_model <- function(output = "summary") {
  
  ### Functions ###
  
  calculate_mu <- function(part, prolif) {
    return ((sample(part, size = nruns, replace = TRUE) * sample(prolif, size = nruns, replace = TRUE)) / Num_timesteps)
  }
    
  calculate_purchased_feed <- function(kg_dm_req, lskeepers_purch_feed, feed_paid_for, dm_in_feed) {
    return (kg_dm_req * lskeepers_purch_feed * feed_paid_for / dm_in_feed)
  }
  
  calculate_expenditure_on_feed <- function(kg_feed_purchased, feed_cost_kg) {
    return (kg_feed_purchased * feed_cost_kg)
  }
  
  ## removed prpn here as this is simply calculating dm required as bw multiplied by dm requ per kg bw
  calculate_dry_matter_requirements <- function(lw, dm_req_prpn) {
    return (dm_req_prpn * lw)
  }

 
  ######## ADD FEED COSTS in here to maintain stochasticity.     #######
  ### kg_DM_req is a matrix of 10000 options for the possible amount of DM required per animal/day
  ## all of these values are matrices of 10,000 options to be sampled from later in the code
  
  if (species == "cattle" || species == "smallruminants" || species == "equids") {
    
    kg_DM_req_JF <- calculate_dry_matter_requirements(lwJF, DM_req_prpn_JF)
    kg_DM_req_JM <- calculate_dry_matter_requirements(lwJM, DM_req_prpn_JM)
    kg_DM_req_SubAF <- calculate_dry_matter_requirements(lwSubAF, DM_req_prpn_SubAF)
    kg_DM_req_SubAM <- calculate_dry_matter_requirements(lwSubAM, DM_req_prpn_SubAM)
    kg_DM_req_AF <- calculate_dry_matter_requirements(lwAF, DM_req_prpn_AF)
    kg_DM_req_AM <- calculate_dry_matter_requirements(lwAM, DM_req_prpn_AM)
  }
  
  
  if (species == "poultry") {
    kg_DM_req_JF <- DM_req_prpn_JF
    kg_DM_req_JM <- DM_req_prpn_JM
    kg_DM_req_SubAF <- DM_req_prpn_SubAF
    kg_DM_req_SubAM <- DM_req_prpn_SubAM
    kg_DM_req_AF <- DM_req_prpn_AF
    kg_DM_req_AM <- DM_req_prpn_AM
  }
  
  if (species == "cattle" || species == "smallruminants" || species == "poultry") {
    
    KG_Feed_purchased_JF <- calculate_purchased_feed(kg_DM_req_JF, prpn_lskeepers_purch_feed, prpn_feed_paid_for, DM_in_feed)
    KG_Feed_purchased_JM <- calculate_purchased_feed(kg_DM_req_JM, prpn_lskeepers_purch_feed, prpn_feed_paid_for, DM_in_feed)
    KG_Feed_purchased_SubAF <- calculate_purchased_feed(kg_DM_req_SubAF, prpn_lskeepers_purch_feed, prpn_feed_paid_for, DM_in_feed)
    KG_Feed_purchased_SubAM <- calculate_purchased_feed(kg_DM_req_SubAM, prpn_lskeepers_purch_feed, prpn_feed_paid_for, DM_in_feed)
    KG_Feed_purchased_AF <- calculate_purchased_feed(kg_DM_req_AF, prpn_lskeepers_purch_feed, prpn_feed_paid_for, DM_in_feed)
    KG_Feed_purchased_AM <- calculate_purchased_feed(kg_DM_req_AM, prpn_lskeepers_purch_feed, prpn_feed_paid_for, DM_in_feed)
    
    Expenditure_on_feed_JF <- calculate_expenditure_on_feed(KG_Feed_purchased_JF, Feed_cost_kg)
    Expenditure_on_feed_JM <- calculate_expenditure_on_feed(KG_Feed_purchased_JM, Feed_cost_kg)
    Expenditure_on_feed_SubAF <- calculate_expenditure_on_feed(KG_Feed_purchased_SubAF, Feed_cost_kg)
    Expenditure_on_feed_SubAM <- calculate_expenditure_on_feed(KG_Feed_purchased_SubAM, Feed_cost_kg)
    Expenditure_on_feed_AF <- calculate_expenditure_on_feed(KG_Feed_purchased_AF, Feed_cost_kg)
    Expenditure_on_feed_AM <- calculate_expenditure_on_feed(KG_Feed_purchased_AM, Feed_cost_kg)
    
  }
  
  if (species == "equids") {
    
    KG_Feed_purchased_partial_HF_JF <- calculate_purchased_feed(kg_DM_req_JF, prpn_lskeepers_partial_feed, lskeepers_partial_feed_prpn_fd_purch, DM_in_feed)
    KG_Feed_purchased_partial_HF_JM <- calculate_purchased_feed(kg_DM_req_JM, prpn_lskeepers_partial_feed, lskeepers_partial_feed_prpn_fd_purch, DM_in_feed)
    KG_Feed_purchased_partial_HF_SubAF <- calculate_purchased_feed(kg_DM_req_SubAF, prpn_lskeepers_partial_feed, lskeepers_partial_feed_prpn_fd_purch, DM_in_feed)
    KG_Feed_purchased_partial_HF_SubAM <- calculate_purchased_feed(kg_DM_req_SubAM, prpn_lskeepers_partial_feed, lskeepers_partial_feed_prpn_fd_purch, DM_in_feed)
    KG_Feed_purchased_partial_HF_AF <- calculate_purchased_feed(kg_DM_req_AF, prpn_lskeepers_partial_feed, lskeepers_partial_feed_prpn_fd_purch, DM_in_feed)
    KG_Feed_purchased_partial_HF_AM <- calculate_purchased_feed(kg_DM_req_AM, prpn_lskeepers_partial_feed, lskeepers_partial_feed_prpn_fd_purch, DM_in_feed)
    
    KG_Feed_purchased_fully_HF_JF <- calculate_purchased_feed(kg_DM_req_JF, prpn_lskeepers_all_handfeed, lskeepers_all_handfeed_prpn_fd_purch, DM_in_feed)
    KG_Feed_purchased_fully_HF_JM <- calculate_purchased_feed(kg_DM_req_JM, prpn_lskeepers_all_handfeed, lskeepers_all_handfeed_prpn_fd_purch, DM_in_feed)
    KG_Feed_purchased_fully_HF_SubAF <- calculate_purchased_feed(kg_DM_req_SubAF, prpn_lskeepers_all_handfeed, lskeepers_all_handfeed_prpn_fd_purch, DM_in_feed)
    KG_Feed_purchased_fully_HF_SubAM <- calculate_purchased_feed(kg_DM_req_SubAM, prpn_lskeepers_all_handfeed, lskeepers_all_handfeed_prpn_fd_purch, DM_in_feed)
    KG_Feed_purchased_fully_HF_AF <- calculate_purchased_feed(kg_DM_req_AF, prpn_lskeepers_all_handfeed, lskeepers_all_handfeed_prpn_fd_purch, DM_in_feed)
    KG_Feed_purchased_fully_HF_AM <- calculate_purchased_feed(kg_DM_req_AM, prpn_lskeepers_all_handfeed, lskeepers_all_handfeed_prpn_fd_purch, DM_in_feed)
    
    
    Expenditure_on_feed_JF <- calculate_expenditure_on_feed(KG_Feed_purchased_partial_HF_JF, Feed_cost_kg) + calculate_expenditure_on_feed(KG_Feed_purchased_fully_HF_JF, Feed_cost_kg)
    Expenditure_on_feed_JM <- calculate_expenditure_on_feed(KG_Feed_purchased_partial_HF_JM, Feed_cost_kg) + calculate_expenditure_on_feed(KG_Feed_purchased_fully_HF_JM, Feed_cost_kg)
    Expenditure_on_feed_SubAF <- calculate_expenditure_on_feed(KG_Feed_purchased_partial_HF_SubAF, Feed_cost_kg) + calculate_expenditure_on_feed(KG_Feed_purchased_fully_HF_SubAF, Feed_cost_kg)
    Expenditure_on_feed_SubAM <- calculate_expenditure_on_feed(KG_Feed_purchased_partial_HF_SubAM, Feed_cost_kg) + calculate_expenditure_on_feed(KG_Feed_purchased_fully_HF_SubAM, Feed_cost_kg)
    Expenditure_on_feed_AF <- calculate_expenditure_on_feed(KG_Feed_purchased_partial_HF_AF, Feed_cost_kg) + calculate_expenditure_on_feed(KG_Feed_purchased_fully_HF_AF, Feed_cost_kg)
    Expenditure_on_feed_AM <- calculate_expenditure_on_feed(KG_Feed_purchased_partial_HF_AM, Feed_cost_kg) + calculate_expenditure_on_feed(KG_Feed_purchased_fully_HF_AM, Feed_cost_kg)
    
  }
  
  if (species == "cattle") {
    
    kg_DM_req_Ox <- calculate_dry_matter_requirements(lwOx, DM_req_prpn_Ox)
    KG_Feed_purchased_Ox <- calculate_purchased_feed(kg_DM_req_Ox, prpn_lskeepers_purch_feed, prpn_feed_paid_for, DM_in_feed)
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
    
 ##   "Cumulative_culls_AM", # remove
    
    "Offtake_JF", 
    "Offtake_JM", 
    "Offtake_SubAF", 
    "Offtake_SubAM", 
    "Offtake_AF", 
    "Offtake_AM", 
    
    "Cumulative_draught_income", 
    
##    "Monthly_mortality", # remove
    "Total_mortality",
    "Total_mortality_JF", 
    "Total_mortality_JM", 
    "Total_mortality_SubAF", 
    "Total_mortality_SubAM", 
    "Total_mortality_AF", 
    "Total_mortality_AM", 
    

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
    "Pop_growth_JF", 
    "Pop_growth_JM", 
    "Pop_growth_SubAF", 
    "Pop_growth_SubAM", 
    "Pop_growth_AF", 
    "Pop_growth_AM", 
    
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
    "Value_manure_SubAF", 
    "Value_manure_SubAM", 
    "Value_manure_AF", 
    "Value_manure_AM", 
       
    "Quantity_hides", 
    "Quantity_hides_SubAF", 
    "Quantity_hides_SubAM", 
    "Quantity_hides_AF", 
    "Quantity_hides_AM", 
    
## ADDED HIDES Vale to vectors
    "Value_hides",

    "Value_hides_SubAF",
    "Value_hides_SubAM",
    "Value_hides_AF",
    "Value_hides_AM",

    "Quantity_milk",
    "Value_milk",  ## MILK ADDED here

    "Cumulative_dry_matter", 
    "Cumulative_dry_matter_JF", 
    "Cumulative_dry_matter_JM", 
    "Cumulative_dry_matter_SubAF", 
    "Cumulative_dry_matter_SubAM", 
    "Cumulative_dry_matter_AF", 
    "Cumulative_dry_matter_AM", 
    
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
                         "Num_Ox",
                         "Oxen_J", 
                         "Oxen_A",
                         "Deaths_Ox", 
                         "Culls_Ox", 
                         "Offtake_Ox",
                         "Total_mortality_Ox",
                         "Quantity_liveweight_kg_Ox",
                         "Num_offtake_Ox",
                         "Offtake_liveweight_kg_Ox", 
                         "Pop_growth_Ox", 
                         "Quantity_manure_Ox", 
                         "Value_manure_Ox", ## added
                         "Quantity_hides_Ox",
                         "Value_hides_Ox",  ## added
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
   
      } 
  
  if (species == "equids") {
      vector_categories <- append(vector_categories, c("Labour_cost_Cart_Driver",
                                                       "Health_cost_vet_JF",
                                                       "Health_cost_vet_JM",
                                                       "Health_cost_vet_SubAF",
                                                       "Health_cost_vet_SubAM",
                                                       "Health_cost_vet_AF",
                                                       "Health_cost_vet_AM",
                                                       
                                                       "Health_cost_trad_JF",
                                                       "Health_cost_trad_JM",
                                                       "Health_cost_trad_SubAF",
                                                       "Health_cost_trad_SubAM",
                                                       "Health_cost_trad_AF",
                                                       "Health_cost_trad_AM",
                                                       
                                                       "Health_cost_selftx_JF",
                                                       "Health_cost_selftx_JM",
                                                       "Health_cost_selftx_SubAF",
                                                       "Health_cost_selftx_SubAM",
                                                       "Health_cost_selftx_AF",
                                                       "Health_cost_selftx_AM",
                                                       
                                                       "Health_cost_combined_JF",
                                                       "Health_cost_combined_JM",
                                                       "Health_cost_combined_SubAF",
                                                       "Health_cost_combined_SubAM",
                                                       "Health_cost_combined_AF",
                                                       "Health_cost_combined_AM",
                                                       
                                                       "Accessory_cost",
                                                       "Accessory_cost_JF",
                                                       "Accessory_cost_JM",
                                                       "Accessory_cost_SubAF",
                                                       "Accessory_cost_SubAM",
                                                       "Accessory_cost_AF",
                                                       "Accessory_cost_AM",
                                                       
                                                       "Cumulative_commercial_income",
                                                       "Cumulative_commercial_income_AF",
                                                       "Cumulative_commercial_income_AM",
                                                       
                                                       "Cumulative_unpaid_income",
                                                       "Cumulative_unpaid_income_SubAF",
                                                       "Cumulative_unpaid_income_SubAM",
                                                       "Cumulative_unpaid_income_AF",
                                                       "Cumulative_unpaid_income_AM",
                                                       
                                                       "Cumulative_combined_income",
                                                       "Cumulative_combined_income_SubAF",
                                                       "Cumulative_combined_income_SubAM",
                                                       "Cumulative_combined_income_AF",
                                                       "Cumulative_combined_income_AM",
                                                       
                                                       "Donkey_power_income",
                                                       "Donkey_power_income_SubAF",
                                                       "Donkey_power_income_SubAM",
                                                       "Donkey_power_income_AF",
                                                       "Donkey_power_income_AM"
                                                      ))  
  
    } else {
    # poultry
    vector_categories <- append(vector_categories, c("Quantity_meat_kg",
                                                     "Quantity_eggs_sold",
                                                     "Quantity_eggs_consumed",
                                                     "Value_eggs_sold",
                                                     "Value_eggs_consumed"
                                                    ))

  }
  
  res_vec <- list()
  
  for (vec in vector_categories) {
    res_vec[[vec]] <- rep(0, Num_timesteps)
  }
  
  matrix_categories <- c("Num_JF",
                         "Num_JM",
                         "Num_SubAF",
                         "Num_SubAM",
                         "Num_AF",
                         "Num_AM",
                         "Num_N",
                         
                         "Total_mortality",
                         "Total_mortality_JF", 
                         "Total_mortality_JM", 
                         "Total_mortality_SubAF", 
                         "Total_mortality_SubAM", 
                         "Total_mortality_AF", 
                         "Total_mortality_AM", 

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
                         "Pop_growth_JF", 
                         "Pop_growth_JM", 
                         "Pop_growth_SubAF", 
                         "Pop_growth_SubAM", 
                         "Pop_growth_AF", 
                         "Pop_growth_AM", 
                        
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
                         "Value_manure_SubAF", 
                         "Value_manure_SubAM", 
                         "Value_manure_AF", 
                         "Value_manure_AM", 
                         
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
                         
                         "Value_milk",
                         
                         "Cumulative_dry_matter", 
                         "Cumulative_dry_matter_JF", 
                         "Cumulative_dry_matter_JM", 
                         "Cumulative_dry_matter_SubAF", 
                         "Cumulative_dry_matter_SubAM", 
                         "Cumulative_dry_matter_AF", 
                         "Cumulative_dry_matter_AM", 
                         
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
                         "Total_expenditure_AM",
               
                         "Gross_margin",
                         "Gross_margin_JF",
                         "Gross_margin_JM",
                         "Gross_margin_SubAF",
                         "Gross_margin_SubAM",
                         "Gross_margin_AF",
                         "Gross_margin_AM")
  
  if (species == "cattle") { # oxen
    matrix_categories <- append(matrix_categories, c("Num_Ox",
                                                     "Total_mortality_Ox", 
                                                     "Quantity_liveweight_kg_Ox",
                                                     "Num_offtake_Ox", 
                                                     "Offtake_liveweight_kg_Ox", 
                                                     "Pop_growth_Ox", 
                                                     "Quantity_manure_Ox",
                                                     "Value_manure_Ox",
                                                     "Quantity_hides_Ox",
                                                     "Value_hides_Ox", ## ADDED VALUE Hide
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
                                                     "Total_expenditure_Ox",
                                                     "Gross_margin_Ox"))
  }  else if (species == "equids") {
      matrix_categories <- append(matrix_categories, c("Labour_cost_Cart_Driver",
                                "Health_cost_vet_JF",
                                "Health_cost_vet_JM",
                                "Health_cost_vet_SubAF",
                                "Health_cost_vet_SubAM",
                                "Health_cost_vet_AF",
                                "Health_cost_vet_AM",
                                
                                "Health_cost_trad_JF",
                                "Health_cost_trad_JM",
                                "Health_cost_trad_SubAF",
                                "Health_cost_trad_SubAM",
                                "Health_cost_trad_AF",
                                "Health_cost_trad_AM",
                                
                                "Health_cost_selftx_JF",
                                "Health_cost_selftx_JM",
                                "Health_cost_selftx_SubAF",
                                "Health_cost_selftx_SubAM",
                                "Health_cost_selftx_AF",
                                "Health_cost_selftx_AM",
                                
                                "Health_cost_combined_JF",
                                "Health_cost_combined_JM",
                                "Health_cost_combined_SubAF",
                                "Health_cost_combined_SubAM",
                                "Health_cost_combined_AF",
                                "Health_cost_combined_AM",
                                
                                "Accessory_cost",
                                "Accessory_cost_JF",
                                "Accessory_cost_JM",
                                "Accessory_cost_SubAF",
                                "Accessory_cost_SubAM",
                                "Accessory_cost_AF",
                                "Accessory_cost_AM",
                                
                                "Cumulative_commercial_income",
                                "Cumulative_commercial_income_AF",
                                "Cumulative_commercial_income_AM",
                                
                                "Cumulative_unpaid_income",
                                "Cumulative_unpaid_income_SubAF",
                                "Cumulative_unpaid_income_SubAM",
                                "Cumulative_unpaid_income_AF",
                                "Cumulative_unpaid_income_AM",
                                
                                "Cumulative_combined_income",
                                "Cumulative_combined_income_SubAF",
                                "Cumulative_combined_income_SubAM",
                                "Cumulative_combined_income_AF",
                                "Cumulative_combined_income_AM",
                                
                                "Donkey_power_income",
                                "Donkey_power_income_SubAF",
                                "Donkey_power_income_SubAM",
                                "Donkey_power_income_AF",
                                "Donkey_power_income_AM")
    )
  
    } else {
    # poultry
    matrix_categories <- append(matrix_categories, c("Quantity_eggs_consumed",
                                                     "Quantity_eggs_sold",
                                                     
                                                     "Value_eggs_consumed",
                                                     "Value_eggs_sold"))
  }

  # Initialize a list to store the matrices
  res_mat <- list()
  
  for (mat in matrix_categories) {
    res_mat[[mat]] <- matrix(0, nrow = nruns, ncol = Num_timesteps)
  }

  for (i in 1:nruns) {
    # Total population is sum of age*sex segments
    # Define population variables and set initial values from function arguments
  
    age_sex_groups <- c("JF", "JM", "SubAF", "SubAM", "AF", "AM")
    
    if (species == "cattle") {
      age_sex_groups <- append(age_sex_groups, "Ox")
    }

    for (group in age_sex_groups) {
      var_name <- group  
      value <- sample(get(paste0("N_", group, "_t0")), 1) ### GEMMA EDITED HERE to sample start population size from a distribution 
      assign(var_name, value)  
      assign(paste0("N_", group, "_t0"), value) ### GEMMA EDITED HERE to ensure N_group_t0 is the same as the group number
    }                                    
    
    # Total population is sum of age*sex segments
    Nt0 <- sum(N_JF_t0, N_JM_t0, N_SubAF_t0, N_SubAM_t0, N_AF_t0, N_AM_t0)
    
    if (species == "cattle") {
      Nt0 <- Nt0 + N_Ox_t0 
    }
    N <- Nt0
    
    
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
    Value_offtake <- 0
    Value_herd_inc <- 0
    Eggs_sold <- 0
    Eggs_consumed <- 0
    Feed <- 0
    Labour <- 0
    Health <- 0
    Capital <- 0
    
    ## adding for equids
    Health_vet <- 0
    Health_trad <- 0
    Health_selftx <- 0
    Health_combined <- 0
    
    Labour_cost_Cart <- 0
    
    commercial_income <- 0
    unpaid_income <- 0
    combined_income <- 0
    
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
                         "Capital",
                         
                         ## for equids
                         "Labour_cost_Cart",
                         
                         "Health_vet",
                         "Health_trad",
                         "Health_selftx",
                         "Health_combined",
                         
                         "commercial_income",
                         "unpaid_income",
                         "combined_income"
                         
                         )
    
    for (group in age_sex_groups) {
      for (prod_var in production_vars) {
        assign(paste(prod_var, group, sep = "_"), 0)
      }
    }
    
    for (month in 1:Num_timesteps) {
      
      if (species == "cattle" || species == "smallruminants" || species == "equids") {
        Mu <- calculate_mu(part, prolif)
        
        res_vec$Births[month] <- sample(Mu, 1) * AF
      } else {
        # poultry
        res_vec$Births[month] <- AF * sample(prop_females_laying, 1) * 
                                             (sample(lay_rate, 1) * timestep_Nof_days)  * 
                                             sample(egg_brood_rate, 1) * 
                                             sample(hatch_rate, 1)
      }
      ## HERE 
      ## Mortality Alpha in the input spreadsheet is now annual risk of mortality OR 
      ## Risk of mortality for the duration of stay (Beta_J for J, Beta_SubA for SubA, 
      ## *CullF/M if duration of stay in adult compartment is < 1 year) because CullF/M 
      ## in the new input values spreadsheet is the duration of stay in adult compartment before culling
      ## but make statement  if this is > 12 the calculation should use 12 and annual mortality risk
      ## in the age sex groups note for juvs and subadults these are the same, should have option for differences
      ## if data allows
      ## Mortality rate equation below is changed
      ## Alpha_ and Beta_ are all age-sex specific now _JF, _JM, _SubAF, _SubAM, _AF, _AM
      
      if (species == "cattle" || species == "equids") {
        ## if statement here that if duration of stay in compartment (Beta_) is less than Num_timesteps (representing 1 year)
        ## replace 1/Beta_  with 1/Num_timesteps so you calculate mortality for the duration of stay in the age
        ## group per timestep correctly if they are in the age-sex compartment for < 1 year
       
         ## I have done this crudely for cattle because we can apply annual mortality to all age/sex groups 
        ## because they are all 12 month or more age-sex compartments but this should be flexible for
        ## future use to the user can define how long they want the age-sex compartments to be
        ## So for for small ruminants and poultry it will have to be 1/Beta_X for Juvenile and SubAdults
        ## mortalitis rather than Num_timesteps 
        
        ## Alpha is now the annual mortality rate OR the mortality rate for animals in the age-sex group
        ## if they are in the age-sex group for less than 1 year
        
        res_vec$Deaths_JF[month] <- (1-(1-sample(Alpha_JF, 1))^(1/Beta_JF)) * JF
        res_vec$Deaths_SubAF[month] <- (1-(1-sample(Alpha_SubAF, 1))^(1/Num_timesteps)) * SubAF 
        res_vec$Deaths_AF[month] <- (1-(1-sample(Alpha_AF, 1))^(1/Num_timesteps))  * AF
                                     
        res_vec$Deaths_JM[month] <- (1-(1-sample(Alpha_JM, 1))^(1/Beta_JM)) * JM
        res_vec$Deaths_SubAM[month] <- (1-(1-sample(Alpha_SubAM, 1))^(1/Num_timesteps)) * SubAM
        ## Again apply an if statement here as this calculation using 12 is applying an annual mortality
        ## rate, so if CullM is < Num_timesteps then replace Num_timesteps with CullM
        res_vec$Deaths_AM[month] <- (1-(1-sample(Alpha_AM, 1))^(1/Num_timesteps)) * AM
      } 
      
      ### GEMMA HAVING A GO AT DOING SPECIFIC MORTALITY FOR SR AND POULTRY
      ## Assumption in juv and sub adult cats <1 year. in adult cat > 1 year
      if (species == "smallruminants" || species == "poultry" ) {

        res_vec$Deaths_JF[month] <- (1-(1-sample(Alpha_JF, 1))^(1/Beta_JF)) * JF
        res_vec$Deaths_SubAF[month] <- (1-(1-sample(Alpha_SubAF, 1))^(1/Beta_SubAF)) * SubAF 
        res_vec$Deaths_AF[month] <- (1-(1-sample(Alpha_AF, 1))^(1/Num_timesteps))  * AF
        
        res_vec$Deaths_JM[month] <- (1-(1-sample(Alpha_JM, 1))^(1/Beta_JM)) * JM
        res_vec$Deaths_SubAM[month] <- (1-(1-sample(Alpha_SubAM, 1))^(1/Beta_SubAM)) * SubAM
        res_vec$Deaths_AM[month] <- (1-(1-sample(Alpha_AM, 1))^(1/Num_timesteps)) * AM
      } 
      
      
      ## These Offtake equations are changed so that small ruminants is the same as others
      ## NOTE UPDATE Small Ruminants inputs spreadsheet for Gamma_JF and Gamma_JM and Gamma_SubAF and Gamma_SubAM
      ## Offtake rate equations changed to be the risk of offtake during the time spent in that age group
      ## to the power of 1/12 of if Beta_ (duration of stay in this time step)  <num_timesteps (a year), 
      ## use Num_timesteps
      if (species == "cattle" ) {
        res_vec$Offtake_JF[month] <- (1-(1-Gamma_JF)^(1/Beta_JF)) * JF
        res_vec$Offtake_JM[month] <- (1-(1-Gamma_JM)^(1/Beta_JM)) * JM
        res_vec$Offtake_SubAF[month] <- (1-(1-Gamma_SubAF)^(1/Num_timesteps)) * SubAF ## assuming subadults for >= 1 year
        res_vec$Offtake_SubAM[month] <- (1-(1-Gamma_SubAM)^(1/Num_timesteps)) * SubAM ## assuming subadults for >= 1 year
        res_vec$Offtake_AF[month] <- (1-(1-Gamma_AF)^(1/Num_timesteps)) * AF
        res_vec$Offtake_AM[month] <- (1-(1-Gamma_AM)^(1/Num_timesteps)) * AM
        
      } 
      ## seperated for small ruminants and poultry ## Different to cattle because they are in the second age sex group < 1 year
      if (species == "poultry" || species == "smallruminants") {
        res_vec$Offtake_JF[month] <- (1-(1-Gamma_JF)^(1/Beta_JF)) * JF
        res_vec$Offtake_JM[month] <- (1-(1-Gamma_JM)^(1/Beta_JM)) * JM
        res_vec$Offtake_SubAF[month] <- (1-(1-Gamma_SubAF)^(1/Beta_SubAF)) * SubAF ## offtake for subadults assuming <12 months
        res_vec$Offtake_SubAM[month] <- (1-(1-Gamma_SubAM)^(1/Beta_SubAM)) * SubAM ## offtake for subadults assuming <12 months
        res_vec$Offtake_AF[month] <- (1-(1-Gamma_AF)^(1/Num_timesteps)) * AF
        res_vec$Offtake_AM[month] <- (1-(1-Gamma_AM)^(1/Num_timesteps)) * AM
        
      } 
      
      if (species == "equids") {
        res_vec$Offtake_JF[month] <- (1-(1-Gamma_JF)^(1/Num_timesteps)) * JF
        res_vec$Offtake_JM[month] <- (1-(1-Gamma_JM)^(1/Num_timesteps)) * JM
        res_vec$Offtake_SubAF[month] <- (1-(1-Gamma_SubAF)^(1/Num_timesteps)) * SubAF ## offtake for subadults assuming <12 months
        res_vec$Offtake_SubAM[month] <- (1-(1-Gamma_SubAM)^(1/Num_timesteps)) * SubAM ## offtake for subadults assuming <12 months
        res_vec$Offtake_AF[month] <- (1-(1-Gamma_AF)^(1/Num_timesteps)) * AF
        res_vec$Offtake_AM[month] <- (1-(1-Gamma_AM)^(1/Num_timesteps)) * AM
        
      } 
      

       ## HERE Gemma has moved SR into same calculations as cattle and poultry, and need to
      ## change SR intake spreadsheet so they also have two different parameters to enter
      ## for duration of stay in first (juvenile) and second (subadult) age groups
      ## Beta values are duration of stay (number of timesteps) in the age sex group
      ## so the calculation becomes, for each time step, the risk of moving up to the 
      ## next age sex group is 1/duration of stay in compartment 
      ## (stochastic option but point values are used in current spreadsheet)
      
      if (species == "cattle" || species == "poultry" || species == "smallruminants" || species == "equids") {
        res_vec$Growth_JF[month] <- (1/(Beta_JF)) * JF
        res_vec$Growth_JM[month] <- (1/(Beta_JM)) * JM
        res_vec$Growth_SubAF[month] <- (1/(Beta_SubAF)) * SubAF
        res_vec$Growth_SubAM[month] <- (1/(Beta_SubAM)) * SubAM
      } 

      ## HERE Gemma has adapted cull rate to defensively program so input for CullF and CullM
      ## is now the duration of stay (number of time steps spent) in the adult compartment before
      ## before culling so each time step the risk of being culled is 1/duration of stay
      ## similar calculation to above for maturation between compartments ## GEMMA HERE 08/05/24
    
      if (species == "cattle" || species == "smallruminants" || species == "poultry" || species == "equids") {
        
        res_vec$Culls_AF[month] <- 1/(CullF) * AF
        res_vec$Culls_AM[month] <- 1/(CullM) * AM
      }
      
      
      if (species == "smallruminants" || species == "poultry" || species == "equids") {
        
        res_vec$Num_JF[month] <- JF + (res_vec$Births[month] * 0.5) - res_vec$Deaths_JF[month] - res_vec$Growth_JF[month] - res_vec$Offtake_JF[month]
        res_vec$Num_SubAF[month] <- SubAF + res_vec$Growth_JF[month] - res_vec$Growth_SubAF[month] - res_vec$Offtake_SubAF[month] - res_vec$Deaths_SubAF[month]
        res_vec$Num_AF[month] <- AF + res_vec$Growth_SubAF[month] - res_vec$Offtake_AF[month] - res_vec$Deaths_AF[month] - res_vec$Culls_AF[month]
        
        res_vec$Num_JM[month] <- JM + (res_vec$Births[month] * 0.5) - res_vec$Growth_JM[month] - res_vec$Deaths_JM[month] - res_vec$Offtake_JM[month]
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
        
        res_vec$Deaths_Ox[month] <- (1-(1-sample(Alpha_Ox, 1))^(1/Num_timesteps)) * Ox
        res_vec$Offtake_Ox[month] <- (1-(1-sample(Gamma_Ox, 1))^(1/Num_timesteps)) * Ox
        res_vec$Culls_Ox[month] <- (1/CullOx) * Ox
        
        # the proportion of growing sub-adult males that move to be oxen is dependant on castration proportion, 
        # castration proportion is calculated from proportion of oxen out of all adult males in starting population
        res_vec$Oxen_A[month] <- sample(castration_proportion, 1) * res_vec$Growth_SubAM[month]
        
        res_vec$Num_JF[month] <- JF + (res_vec$Births[month] * 0.5) - res_vec$Deaths_JF[month] - res_vec$Growth_JF[month] - res_vec$Offtake_JF[month]
        res_vec$Num_SubAF[month] <- SubAF + res_vec$Growth_JF[month] - res_vec$Growth_SubAF[month] - res_vec$Offtake_SubAF[month] - res_vec$Deaths_SubAF[month]
        res_vec$Num_AF[month] <- AF + res_vec$Growth_SubAF[month] - res_vec$Offtake_AF[month] - res_vec$Deaths_AF[month] - res_vec$Culls_AF[month]
        
        res_vec$Num_JM[month] <- JM + (res_vec$Births[month] * 0.5) - res_vec$Growth_JM[month] - res_vec$Deaths_JM[month] - res_vec$Offtake_JM[month]
        res_vec$Num_SubAM[month] <- SubAM + res_vec$Growth_JM[month] - res_vec$Growth_SubAM[month] - res_vec$Offtake_SubAM[month] - res_vec$Deaths_SubAM[month]
        res_vec$Num_AM[month] <- AM + (res_vec$Growth_SubAM[month]-res_vec$Oxen_A[month]) - res_vec$Offtake_AM[month] - res_vec$Deaths_AM[month] - res_vec$Culls_AM[month]
        
        res_vec$Num_Ox[month] <- Ox + res_vec$Oxen_A[month] - res_vec$Offtake_Ox[month] - res_vec$Deaths_Ox[month] - res_vec$Culls_Ox[month]
        
        res_vec$Num_N[month] <- sum(res_vec$Num_JF[month],
                                    res_vec$Num_SubAF[month],
                                    res_vec$Num_AF[month],
                                    res_vec$Num_JM[month],
                                    res_vec$Num_SubAM[month],
                                    res_vec$Num_AM[month],
                                    res_vec$Num_Ox[month])
      }
      
      
      if (species == "cattle" || species == "smallruminants" || species == "poultry" || species == "equids") {
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
        Ox <- res_vec$Num_Ox[month]
        res_vec$Total_mortality_Ox[month] <- Num_dead_Ox + res_vec$Deaths_Ox[month]
        Num_dead_Ox <- res_vec$Total_mortality_Ox[month]
        res_vec$Total_mortality[month] <- res_vec$Total_mortality[month] + res_vec$Total_mortality_Ox[month]
      }
      
      
      
      if (species == "cattle" || species == "smallruminants" || species == "poultry" || species == "equids") {
        res_vec$Pop_growth[month] <- N - Nt0
        res_vec$Pop_growth_JF[month] <- JF - N_JF_t0
        res_vec$Pop_growth_JM[month] <- JM - N_JM_t0
        res_vec$Pop_growth_SubAF[month] <- SubAF - N_SubAF_t0
        res_vec$Pop_growth_SubAM[month] <- SubAM - N_SubAM_t0
        res_vec$Pop_growth_AF[month] <- AF - N_AF_t0
        res_vec$Pop_growth_AM[month] <- AM - N_AM_t0
      }
      
      if (species == "cattle") {
        res_vec$Pop_growth_Ox[month] <- Ox - N_Ox_t0
      }
      
      if (species == "cattle" || species == "smallruminants" || species == "poultry" || species == "equids") {
        
        liveweight_JF <- sample(lwJF, 1)
        liveweight_JM <- sample(lwJM, 1)
        liveweight_SubAF <- sample(lwSubAF, 1)
        liveweight_SubAM <- sample(lwSubAM, 1)
        liveweight_AF <- sample(lwAF, 1)
        liveweight_AM <- sample(lwAM, 1)
        
        res_vec$Quantity_liveweight_kg_JF[month] <- JF * liveweight_JF
        res_vec$Quantity_liveweight_kg_JM[month] <- JM * liveweight_JM
        res_vec$Quantity_liveweight_kg_SubAF[month] <- SubAF * liveweight_SubAF
        res_vec$Quantity_liveweight_kg_SubAM[month] <- SubAM * liveweight_SubAM
        res_vec$Quantity_liveweight_kg_AF[month] <- AF * liveweight_AF
        res_vec$Quantity_liveweight_kg_AM[month] <- AM * liveweight_AM
      
        
        res_vec$Quantity_liveweight_kg[month] <- sum(res_vec$Quantity_liveweight_kg_JF[month],
                                                  res_vec$Quantity_liveweight_kg_JM[month],
                                                  res_vec$Quantity_liveweight_kg_SubAF[month],
                                                  res_vec$Quantity_liveweight_kg_SubAM[month],
                                                  res_vec$Quantity_liveweight_kg_AF[month],
                                                  res_vec$Quantity_liveweight_kg_AM[month])
        ## some renaming vars here _JF and _JM
        res_vec$Num_offtake_JF[month] <- Offtake_JF + res_vec$Offtake_JF[month]
        res_vec$Num_offtake_JM[month] <- Offtake_JM + res_vec$Offtake_JM[month]
        res_vec$Num_offtake_SubAF[month] <- Offtake_SubAF + res_vec$Offtake_SubAF[month]
        res_vec$Num_offtake_SubAM[month] <- Offtake_SubAM + res_vec$Offtake_SubAM[month]
        res_vec$Num_offtake_AF[month] <- Offtake_AF + res_vec$Offtake_AF[month]
        res_vec$Num_offtake_AM[month] <- Offtake_AM + res_vec$Offtake_AM[month] + res_vec$Culls_AM[month]
        
      }
      
      if (species == "cattle") {
        liveweight_Ox <- sample(lwOx, 1)
        res_vec$Quantity_liveweight_kg_Ox[month] <- Ox * liveweight_Ox
        res_vec$Quantity_liveweight_kg[month] <- res_vec$Quantity_liveweight_kg[month] + res_vec$Quantity_liveweight_kg_Ox[month]
        res_vec$Num_offtake_Ox[month] <- Offtake_Ox + res_vec$Offtake_Ox[month] + res_vec$Culls_Ox[month]
      }
    
      if (species == "cattle" || species == "smallruminants" || species == "poultry" || species == "equids") {
      
      Offtake_JF <- res_vec$Num_offtake_JF[month]
      Offtake_JM <- res_vec$Num_offtake_JM[month]
      Offtake_SubAF <- res_vec$Num_offtake_SubAF[month]
      Offtake_SubAM <- res_vec$Num_offtake_SubAM[month]
      Offtake_AF <- res_vec$Num_offtake_AF[month]
      Offtake_AM <- res_vec$Num_offtake_AM[month]
      
      res_vec$Num_offtake[month] <- sum(res_vec$Num_offtake_JF[month],
                                    res_vec$Num_offtake_JM[month],
                                    res_vec$Num_offtake_SubAF[month],
                                    res_vec$Num_offtake_SubAM[month],
                                    res_vec$Num_offtake_AF[month],
                                    res_vec$Num_offtake_AM[month])
      }
      
      if (species == "cattle") {
        Offtake_Ox <- res_vec$Num_offtake_Ox[month]
        res_vec$Num_offtake[month] <- res_vec$Num_offtake[month] + res_vec$Num_offtake_Ox[month]
      }
    
      if (species == "cattle" || species == "smallruminants" || species == "poultry" || species == "equids") {
        Offtake <- res_vec$Num_offtake[month]
        ## QUESTION HERE for Wudu, it doesn't matter too much now as we don't use meat produced value, 
        ## but here we could preplace liveweight distribution with a finish weight distribution as 
        ## this is what we have done for the Indonesia model - or is this unnecessary in the extensive system?
        res_vec$Offtake_liveweight_kg_SubAF[month] <- liveweight_SubAF * Offtake_SubAF
        res_vec$Offtake_liveweight_kg_SubAM[month] <- liveweight_SubAM * Offtake_SubAM
        res_vec$Offtake_liveweight_kg_AF[month] <- liveweight_AF * Offtake_AF
        res_vec$Offtake_liveweight_kg_AM[month] <- liveweight_AM * Offtake_AM
      
        res_vec$Offtake_liveweight_kg[month] <- sum(res_vec$Offtake_liveweight_kg_SubAF[month],
                                                    res_vec$Offtake_liveweight_kg_SubAM[month],
                                                    res_vec$Offtake_liveweight_kg_AF[month],
                                                    res_vec$Offtake_liveweight_kg_AM[month])
      }

      if (species == "smallruminants" || species == "poultry") {
        res_vec$Quantity_meat_kg[month] <- res_vec$Offtake_liveweight_kg[month] * sample(ccy, 1) ## made ccy possible to be a distribution
        
      }
      
 
      if (species == "cattle") { 
        res_vec$Offtake_liveweight_kg_Ox[month] <- liveweight_Ox * Offtake_Ox
        res_vec$Offtake_liveweight_kg[month] <-  res_vec$Offtake_liveweight_kg[month] + res_vec$Offtake_liveweight_kg_Ox[month]
        res_vec$Quantity_meat_kg[month] <- res_vec$Offtake_liveweight_kg[month] * sample(ccy, 1)
      }
      
      if (species == "cattle" || species == "smallruminants" || species == "poultry") {
        Meat_kg <- res_vec$Quantity_meat_kg[month]
      }
      
      if (species == "cattle") {
      
        res_vec$Cumulative_draught_income[month] <- Draught_income + Ox * sample(draught_rate, 1) * sample(draught_day_value, 1) * timestep_Nof_days
        Draught_income <- res_vec$Cumulative_draught_income[month]
      }
      
      #### EQUIDS Additional income
      # commercial AF and AM
      # combined Sub AF, Sub AM, AF, AM 
      # unpaid Sub AF, Sub AM, AF, AM 
      # total is a sum of all
      
      ## use hours/days worked to get 
      ## (* 1) for current and 
      ## (* by proportional increase in hours (commercial)/days (unpaid and combined) worked per month) for ideal 
      ## the proportional increase is (current_hrs+(ideal_hrs-current_hrs))/current_hrs)
      ## in scenarion spreadsheet current and ideal are same in current scenario, ideal hrs higher in ideal scenario
    
      if (species == "equids") {
        ## Commercial (set working hours to x and y for each age sex group and use to calculate prop increase hours in ideal)
        ## Adult female commercial
        x <- sample(WH_commercial_AF_ideal,1)
        y <- sample(WH_commercial_AF_current,1)       
        
        res_vec$Cumulative_commercial_income_AF[month] <- commercial_income_AF + 
          (AF * sample(prop_commercial_AF, 1) * sample(monthly_income_head_commercial_AF, 1) * ((y+(x - y))/y))  
          
        commercial_income_AF <- res_vec$Cumulative_commercial_income_AF[month]
        
        
        ## Adult male commercial
        x <- sample(WH_commercial_AM_ideal,1)
        y <- sample(WH_commercial_AM_current,1)
        
        res_vec$Cumulative_commercial_income_AM[month] <- commercial_income_AM + 
          (AM * sample(prop_commercial_AM, 1) * sample(monthly_income_head_commercial_AM, 1) * ((y+(x - y))/y))
        
        commercial_income_AM <- res_vec$Cumulative_commercial_income_AM[month]
        
        res_vec$Cumulative_commercial_income[month] <- sum(res_vec$Cumulative_commercial_income_AF[month],
                                                           res_vec$Cumulative_commercial_income_AM[month])
        
        commercial_income <- res_vec$Cumulative_commercial_income[month]
        
        
        ## Total unpaid income
        ## Adult female unpaid
        
        x <- sample(WH_unpaid_AF_ideal,1)
        y <- sample(WH_unpaid_AF_current,1)
        
        res_vec$Cumulative_unpaid_income_AF[month] <- unpaid_income_AF + 
          (AF * sample(prop_unpaid_AF, 1) * sample(monthly_income_head_unpaid_AF, 1) * ((y+(x - y))/y))
        
        unpaid_income_AF <- res_vec$Cumulative_unpaid_income_AF[month]
        
        ## Adult male unpaid
        
        x <- sample(WH_unpaid_AM_ideal,1)
        y <- sample(WH_unpaid_AM_current,1)
        res_vec$Cumulative_unpaid_income_AM[month] <- unpaid_income_AM + 
          (AM * sample(prop_unpaid_AM, 1) * sample(monthly_income_head_unpaid_AM, 1) * ((y+(x - y))/y))
        
        unpaid_income_AM <- res_vec$Cumulative_unpaid_income_AM[month]
        
        ## and subadults female unpaid
        x <- sample(WH_unpaid_SubAF_ideal,1)
        y <- sample(WH_unpaid_SubAF_current,1)
        
        res_vec$Cumulative_unpaid_income_SubAF[month] <- unpaid_income_SubAF + 
          (SubAF * sample(prop_unpaid_SubAF, 1) * sample(monthly_income_head_unpaid_SubAF, 1) * ((y+(x - y))/y))
        
        unpaid_income_SubAF <- res_vec$Cumulative_unpaid_income_SubAF[month]
        
        ## and subadults male unpaid
        x <- sample(WH_unpaid_SubAM_ideal,1)
        y <- sample(WH_unpaid_SubAM_current,1)
        res_vec$Cumulative_unpaid_income_SubAM[month] <- unpaid_income_SubAM + 
          (SubAM * sample(prop_unpaid_SubAM, 1) * sample(monthly_income_head_unpaid_SubAM, 1) * ((y+(x - y))/y))
        
        unpaid_income_SubAM <- res_vec$Cumulative_unpaid_income_SubAM[month]
        
        
        ## Total unpaid income
        res_vec$Cumulative_unpaid_income[month] <- sum(res_vec$Cumulative_unpaid_income_AF[month],
                                                       res_vec$Cumulative_unpaid_income_AM[month],
                                                       res_vec$Cumulative_unpaid_income_SubAF[month],
                                                       res_vec$Cumulative_unpaid_income_SubAM[month])
        
        unpaid_income <- res_vec$Cumulative_unpaid_income[month]
        
        ## Combined income Adult female

        x <- sample(WH_combined_hhcom_AF_ideal,1)
        y <- sample(WH_combined_hhcom_AF_current,1)
        res_vec$Cumulative_combined_income_AF[month] <- combined_income_AF + 
          (AF * sample(prop_combined_hhcom_AF, 1) * sample(monthly_income_head_combined_hhcom_AF, 1) * ((y+(x - y))/y))
        
        combined_income_AF <- res_vec$Cumulative_combined_income_AF[month]
        
        ## Combined income Adult male
        x <- sample(WH_combined_hhcom_AM_ideal,1)
        y <- sample(WH_combined_hhcom_AM_current,1)
        res_vec$Cumulative_combined_income_AM[month] <- combined_income_AM + 
          (AM * sample(prop_combined_hhcom_AM, 1) * sample(monthly_income_head_combined_hhcom_AM, 1) * ((y+(x - y))/y))
        
        combined_income_AM <- res_vec$Cumulative_combined_income_AM[month]
        
        ## Combined income subadults female
        x <- sample(WH_combined_hhcom_SubAF_ideal,1)
        y <- sample(WH_combined_hhcom_SubAF_current,1)
        res_vec$Cumulative_combined_income_SubAF[month] <- combined_income_SubAF + 
          (SubAF * sample(prop_combined_hhcom_SubAF, 1) * sample(monthly_income_head_combined_hhcom_SubAF, 1) * ((y+(x - y))/y))
        
        combined_income_SubAF <- res_vec$Cumulative_combined_income_SubAF[month]
        
        ## Combined income subadults male
        x <- sample(WH_combined_hhcom_SubAM_ideal,1)
        y <- sample(WH_combined_hhcom_SubAM_current,1)
        res_vec$Cumulative_combined_income_SubAM[month] <- combined_income_SubAM + 
          (SubAM * sample(prop_combined_hhcom_SubAM, 1) * sample(monthly_income_head_combined_hhcom_SubAM, 1) * ((y+(x - y))/y))
        
        combined_income_SubAM <- res_vec$Cumulative_combined_income_SubAM[month]
        
        
        ## Total combined income
        res_vec$Cumulative_combined_income[month] <- sum(res_vec$Cumulative_combined_income_AF[month],
                                                       res_vec$Cumulative_combined_income_AM[month],
                                                       res_vec$Cumulative_combined_income_SubAF[month],
                                                       res_vec$Cumulative_combined_income_SubAM[month])
        
        combined_income <- res_vec$Cumulative_combined_income[month]
        
        #### Total donkey income from different age-sex groups
        res_vec$Donkey_power_income_SubAF[month] <- sum(res_vec$Cumulative_unpaid_income_SubAF[month],
                                                  res_vec$Cumulative_combined_income_SubAF[month]) 
        
        res_vec$Donkey_power_income_SubAM[month] <- sum(res_vec$Cumulative_unpaid_income_SubAM[month],
                                                  res_vec$Cumulative_combined_income_SubAM[month]) 
        
        res_vec$Donkey_power_income_AF[month] <- sum(res_vec$Cumulative_commercial_income_AF[month],
                                                  res_vec$Cumulative_unpaid_income_AF[month],
                                                  res_vec$Cumulative_combined_income_AF[month]) 
        
        res_vec$Donkey_power_income_AM[month] <- sum(res_vec$Cumulative_commercial_income_AM[month],
                                                  res_vec$Cumulative_unpaid_income_AM[month],
                                                  res_vec$Cumulative_combined_income_AM[month]) 
        
        res_vec$Donkey_power_income[month] <- sum(res_vec$Cumulative_commercial_income[month],
                                                  res_vec$Cumulative_unpaid_income[month],
                                                  res_vec$Cumulative_combined_income[month]) 
      
        }
 
      
      ## Hides
      if (species == "cattle" || species == "smallruminants") {
        res_vec$Quantity_hides_SubAF[month] <- Hides_SubAF +  res_vec$Deaths_SubAF[month] * sample(hides_rate_mor, 1)
        res_vec$Quantity_hides_SubAM[month] <- Hides_SubAM + res_vec$Deaths_SubAM[month] * sample(hides_rate_mor, 1)
        res_vec$Quantity_hides_AF[month] <- Hides_AF + res_vec$Deaths_AF[month] * sample(hides_rate_mor, 1)
        res_vec$Quantity_hides_AM[month] <- Hides_AM + res_vec$Deaths_AM[month] * sample(hides_rate_mor, 1)
        
        Hides_SubAF <- res_vec$Quantity_hides_SubAF[month]
        Hides_SubAM <- res_vec$Quantity_hides_SubAM[month]
        Hides_AF <- res_vec$Quantity_hides_AF[month]
        Hides_AM <- res_vec$Quantity_hides_AM[month]
        
        res_vec$Quantity_hides[month] <- sum(res_vec$Quantity_hides_SubAF[month],
                                          res_vec$Quantity_hides_SubAM[month],
                                          res_vec$Quantity_hides_AF[month],
                                          res_vec$Quantity_hides_AM[month])
        
        Hides <- res_vec$Quantity_hides[month] 
        
        ## Hides Value.  ### ADDED HIDES VALUE HERE ### so the calculation is stochastic
        res_vec$Value_hides_SubAF[month] <- res_vec$Quantity_hides_SubAF[month] * sample(hides_value, 1)
        res_vec$Value_hides_SubAM[month] <- res_vec$Quantity_hides_SubAM[month] * sample(hides_value, 1)
        res_vec$Value_hides_AF[month] <- res_vec$Quantity_hides_AF[month] * sample(hides_value, 1)
        res_vec$Value_hides_AM[month] <- res_vec$Quantity_hides_AM[month] * sample(hides_value, 1)
        
        res_vec$Value_hides[month] <- sum(res_vec$Value_hides_SubAF[month],
                                          res_vec$Value_hides_SubAM[month],
                                          res_vec$Value_hides_AF[month],
                                          res_vec$Value_hides_AM[month])
        
        ## Changed milk production equation to be dependant on Num_timesteps rather than 12
        
        res_vec$Quantity_milk[month] <- Milk + AF * (sample(part, 1)/Num_timesteps) * sample(prop_F_milked, 1) * sample(lac_duration, 1) * sample(avg_daily_yield_ltr, 1) 
        
        Milk <- res_vec$Quantity_milk[month]
        
        ## ADDED MILK HERE so value calculated stochastically each month and iteration
        res_vec$Value_milk[month] <- res_vec$Quantity_milk[month] * sample(milk_value_ltr, 1)
        
        ## Manure
        ## replace 30 with timestep_Nof_days
        res_vec$Quantity_manure_JF[month] <- Manure_kg_JF + JF * sample(Man_J, 1) * timestep_Nof_days  
        res_vec$Quantity_manure_JM[month] <- Manure_kg_JM + JM * sample(Man_J, 1) * timestep_Nof_days 
        res_vec$Quantity_manure_SubAF[month] <- Manure_kg_SubAF + SubAF * sample(Man_SubA, 1) * timestep_Nof_days
        res_vec$Quantity_manure_SubAM[month] <- Manure_kg_SubAM + SubAM * sample(Man_SubA, 1) * timestep_Nof_days 
        res_vec$Quantity_manure_AF[month] <- Manure_kg_AF + AF * sample(Man_A, 1) * timestep_Nof_days
        res_vec$Quantity_manure_AM[month] <- Manure_kg_AM + AM * sample(Man_A, 1) * timestep_Nof_days
        
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
        
        ### ADD MANURE VALUE here ** THEN add Matrix and make sure oxen done for both
        
        res_vec$Value_manure_JF[month] <- res_vec$Quantity_manure_JF[month] * sample(Man_value, 1)
        res_vec$Value_manure_JM[month] <- res_vec$Quantity_manure_JM[month] * sample(Man_value, 1)
        res_vec$Value_manure_SubAF[month] <- res_vec$Quantity_manure_SubAF[month] * sample(Man_value, 1)
        res_vec$Value_manure_SubAM[month] <- res_vec$Quantity_manure_SubAM[month] * sample(Man_value, 1)
        res_vec$Value_manure_AF[month] <- res_vec$Quantity_manure_AF[month] * sample(Man_value, 1)
        res_vec$Value_manure_AM[month] <- res_vec$Quantity_manure_AM[month] * sample(Man_value, 1)
        
        res_vec$Value_manure[month] <- sum(res_vec$Value_manure_JF[month],
                                              res_vec$Value_manure_JM[month],
                                              res_vec$Value_manure_SubAF[month],
                                              res_vec$Value_manure_SubAM[month],
                                              res_vec$Value_manure_AF[month],
                                              res_vec$Value_manure_AM[month])
      }
      
      
      if (species == "cattle") {
        res_vec$Quantity_hides_Ox[month] <- Hides_Ox + res_vec$Deaths_Ox[month] * sample(hides_rate_mor, 1) 
        Hides_Ox <- res_vec$Quantity_hides_Ox[month]
        res_vec$Quantity_hides[month] <- res_vec$Quantity_hides[month] + res_vec$Quantity_hides_Ox[month]
        
        ## ADDED VALUE HIDES oxen here
        res_vec$Value_hides_Ox[month] <- res_vec$Quantity_hides_Ox[month] * sample(hides_value, 1)
        res_vec$Value_hides[month] <- res_vec$Value_hides[month] +  res_vec$Value_hides_Ox[month]
        
        ## Oxen manure
        res_vec$Quantity_manure_Ox[month] <- Manure_kg_Ox + Ox * sample(Man_A, 1) * timestep_Nof_days 
        Manure_kg_Ox <- res_vec$Quantity_manure_Ox[month]
        res_vec$Quantity_manure[month] <- res_vec$Quantity_manure[month] + res_vec$Quantity_manure_Ox[month]
        Manure_kg <- res_vec$Quantity_manure[month]
        
        ## ADDED VALUE MANURE  here ## NOTE THIS UPDATES VECTOR FOR TOTAL QUANTITY 
         ## SO DONT Double sum up the matrices
        res_vec$Value_manure_Ox[month] <- res_vec$Quantity_manure_Ox[month] * sample(Man_value, 1)
        res_vec$Value_manure[month] <- res_vec$Value_manure[month] + res_vec$Value_manure_Ox[month]
      }
    
        #### Dry matter requirements ######
    if (species == "cattle" || species == "smallruminants") {
        res_vec$Cumulative_dry_matter_JF[month] <- Cumulative_DM_JF + JF * sample(kg_DM_req_JF, 1) * timestep_Nof_days 
        res_vec$Cumulative_dry_matter_JM[month] <- Cumulative_DM_JM + JM * sample(kg_DM_req_JM, 1) * timestep_Nof_days
        res_vec$Cumulative_dry_matter_SubAF[month] <- Cumulative_DM_SubAF + SubAF * sample(kg_DM_req_SubAF, 1) * timestep_Nof_days 
        res_vec$Cumulative_dry_matter_SubAM[month] <- Cumulative_DM_SubAM + SubAM * sample(kg_DM_req_SubAM, 1) * timestep_Nof_days 
        res_vec$Cumulative_dry_matter_AF[month] <- Cumulative_DM_AF + AF * sample(kg_DM_req_AF, 1) * timestep_Nof_days
        res_vec$Cumulative_dry_matter_AM[month] <- Cumulative_DM_AM + AM * sample(kg_DM_req_AM, 1) * timestep_Nof_days
        
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
    }
      ## ADDED extra feed requirements for working equids
      if (species == "equids") {
        res_vec$Cumulative_dry_matter_JF[month] <- Cumulative_DM_JF + JF * sample(kg_DM_req_JF, 1) * timestep_Nof_days 
        res_vec$Cumulative_dry_matter_JM[month] <- Cumulative_DM_JM + JM * sample(kg_DM_req_JM, 1) * timestep_Nof_days
        res_vec$Cumulative_dry_matter_SubAF[month] <- Cumulative_DM_SubAF + SubAF * sample(kg_DM_req_SubAF, 1) * timestep_Nof_days 
        res_vec$Cumulative_dry_matter_SubAM[month] <- Cumulative_DM_SubAM + SubAM * sample(kg_DM_req_SubAM, 1) * timestep_Nof_days 
        res_vec$Cumulative_dry_matter_AF[month] <- Cumulative_DM_AF + (AF * sample(kg_DM_req_AF, 1) * timestep_Nof_days) + (0.97*(AF * sample(kg_DM_req_AF, 1) * timestep_Nof_days))
        res_vec$Cumulative_dry_matter_AM[month] <- Cumulative_DM_AM + (AM * sample(kg_DM_req_AM, 1) * timestep_Nof_days) + (0.97*(AM * sample(kg_DM_req_AM, 1) * timestep_Nof_days))
        
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
      }
    
    if (species == "cattle") {
        res_vec$Cumulative_dry_matter_Ox[month] <- Cumulative_DM_Ox + Ox * sample(kg_DM_req_Ox, 1) * timestep_Nof_days
        Cumulative_DM_Ox <- res_vec$Cumulative_dry_matter_Ox[month]
        res_vec$Cumulative_dry_matter[month] <- res_vec$Cumulative_dry_matter[month] +  res_vec$Cumulative_dry_matter_Ox[month]
      }
      
    if (species == "cattle" || species == "smallruminants") {
      Cumulative_DM <- res_vec$Cumulative_dry_matter[month]
    }

    if (species == "cattle" || species == "smallruminants" || species == "poultry" || species == "equids") {
      
      ## Set the financial value of animals for this iteration
      fin_val_JF <- sample(fvJF, 1)
      fin_val_JM <- sample(fvJM, 1)
      fin_val_SubAF <- sample(fvSubAF, 1)
      fin_val_SubAM <- sample(fvSubAM, 1)
      fin_val_AF <- sample(fvAF, 1)
      fin_val_AM <- sample(fvAM, 1)
      
      
      res_vec$Value_offtake_SubAF[month] <- fin_val_SubAF * Offtake_SubAF 
      Value_offtake_SubAF <- res_vec$Value_offtake_SubAF[month]
      
      res_vec$Value_offtake_SubAM[month] <- fin_val_SubAM * Offtake_SubAM
      Value_offtake_SubAM <- res_vec$Value_offtake_SubAM[month]
      
      res_vec$Value_offtake_AF[month] <- fin_val_AF * Offtake_AF
      Value_offtake_AF <- res_vec$Value_offtake_AF[month]
      
      res_vec$Value_offtake_AM[month] <- fin_val_AM * Offtake_AM  
      Value_offtake_AM <- res_vec$Value_offtake_AM[month]
      
      res_vec$Value_offtake[month] <- sum(res_vec$Value_offtake_SubAF[month],
                                         res_vec$Value_offtake_SubAM[month],
                                         res_vec$Value_offtake_AF[month],
                                         res_vec$Value_offtake_AM[month])
    }
     
       if (species == "cattle") {
         fin_val_Ox <- sample(fvOx, 1)
         res_vec$Value_offtake_Ox[month] <- fin_val_Ox * Offtake_Ox
         
         Value_offtake_Ox <- res_vec$Value_offtake_Ox[month]
         res_vec$Value_offtake[month] <- res_vec$Value_offtake[month] + res_vec$Value_offtake_Ox[month]
       }
    
    if (species == "cattle" || species == "smallruminants" || species == "poultry" || species == "equids") {
      
      Value_offtake <- res_vec$Value_offtake[month] 
      
      res_vec$Value_herd_increase_JF[month] <- (JF - N_JF_t0) * fin_val_JF
      Value_herd_inc_JF <- res_vec$Value_herd_increase_JF[month]
      
      res_vec$Value_herd_increase_JM[month] <- (JM - N_JM_t0) * fin_val_JM
      Value_herd_inc_JM <- res_vec$Value_herd_increase_JM[month]
      
      res_vec$Value_herd_increase_SubAF[month] <- (SubAF - N_SubAF_t0) * fin_val_SubAF
      Value_herd_inc_SubAF <- res_vec$Value_herd_increase_SubAF[month]
      
      res_vec$Value_herd_increase_SubAM[month] <- (SubAM - N_SubAM_t0) * fin_val_SubAM
      Value_herd_inc_SubAM <- res_vec$Value_herd_increase_SubAM[month]
      
      res_vec$Value_herd_increase_AF[month] <- (AF - N_AF_t0) * fin_val_AF
      Value_herd_inc_AF <- res_vec$Value_herd_increase_AF[month]
      
      res_vec$Value_herd_increase_AM[month] <- (AM - N_AM_t0) * fin_val_AM
      Value_herd_inc_AM <- res_vec$Value_herd_increase_AM[month]
      
      res_vec$Value_herd_increase[month] <- sum(res_vec$Value_herd_increase_JF[month],
                                                res_vec$Value_herd_increase_JM[month],
                                                res_vec$Value_herd_increase_SubAF[month],
                                                res_vec$Value_herd_increase_SubAM[month],
                                                res_vec$Value_herd_increase_AF[month],
                                                res_vec$Value_herd_increase_AM[month])
      
    }
      
      if (species == "cattle") {
        res_vec$Value_herd_increase_Ox[month] <- (Ox - N_Ox_t0) * fin_val_Ox
        Value_herd_inc_Ox <- res_vec$Value_herd_increase_Ox[month]
        res_vec$Value_herd_increase[month] <- res_vec$Value_herd_increase[month] + res_vec$Value_herd_increase_Ox[month]
      }
    
    if (species == "cattle" || species == "smallruminants" || species == "poultry" || species == "equids") {
      
      Value_herd_inc <- res_vec$Value_herd_increase[month]
      
      res_vec$Total_value_increase[month] <- Value_herd_inc + Value_offtake ## Total_value already includes oxen
      res_vec$Total_value_increase_JF[month] <- Value_herd_inc_JF 
      res_vec$Total_value_increase_JM[month] <- Value_herd_inc_JM 
      res_vec$Total_value_increase_SubAF[month] <- Value_herd_inc_SubAF + Value_offtake_SubAF
      res_vec$Total_value_increase_SubAM[month] <- Value_herd_inc_SubAM + Value_offtake_SubAM
      res_vec$Total_value_increase_AF[month] <- Value_herd_inc_AF + Value_offtake_AF
      res_vec$Total_value_increase_AM[month] <- Value_herd_inc_AM + Value_offtake_AM
      
    }
      
      if (species == "cattle") {
        res_vec$Total_value_increase_Ox[month] <- Value_herd_inc_Ox + Value_offtake_Ox
      }
        ### Feed cost ### 
      ## Expenditure on feed is calculated using KG of dry matter required 
      ## (maintenance plus work requirement for adults (using body weights and % 
      ## DM required as a proportion of bodyweight)) 
      ## Amount of dry matter purchased is estimated using the % DM of commonly purchased feeds, the prortion of LS keepers 
      ## that partial hand feed (estimate they purchase 50% of required DM) and fully hand feed
      ## and this is multiplied by the local cost per KG of feed
      
    if (species == "cattle" || species == "smallruminants" || species == "poultry" || species == "equids") {
        
        res_vec$Feed_cost_JF[month] <- Feed_JF + JF * sample(Expenditure_on_feed_JF, 1) * timestep_Nof_days 
        Feed_JF <- res_vec$Feed_cost_JF[month]
        
        res_vec$Feed_cost_JM[month] <- Feed_JM + JM * sample(Expenditure_on_feed_JM, 1) * timestep_Nof_days 
        Feed_JM <- res_vec$Feed_cost_JM[month]
       
        res_vec$Feed_cost_SubAF[month] <- Feed_SubAF + SubAF * sample(Expenditure_on_feed_SubAF, 1) * timestep_Nof_days
        Feed_SubAF <- res_vec$Feed_cost_SubAF[month]
        res_vec$Feed_cost_SubAM[month] <- Feed_SubAM + SubAM * sample(Expenditure_on_feed_SubAM, 1) * timestep_Nof_days
        Feed_SubAM <- res_vec$Feed_cost_SubAM[month]
        res_vec$Feed_cost_AF[month] <- Feed_AF + AF * sample(Expenditure_on_feed_AF, 1) * timestep_Nof_days
        Feed_AF <- res_vec$Feed_cost_AF[month]
        res_vec$Feed_cost_AM[month] <- Feed_AM + AM * sample(Expenditure_on_feed_AM, 1) * timestep_Nof_days 
        Feed_AM <- res_vec$Feed_cost_AM[month]
        
        res_vec$Feed_cost[month] <- sum(res_vec$Feed_cost_JF[month],
                                        res_vec$Feed_cost_JM[month],
                                        res_vec$Feed_cost_SubAF[month],
                                        res_vec$Feed_cost_SubAM[month],
                                        res_vec$Feed_cost_AF[month],
                                        res_vec$Feed_cost_AM[month])
        
        Feed <- res_vec$Feed_cost[month]
    }
        
        if (species == "cattle" || species == "smallruminants" || species == "poultry" || species == "equids") {
          res_vec$Labour_cost_JF[month] <- Labour_JF + JF * (sample(Labour_cost_head_JF, 1)/Num_timesteps) * lab_non_health 
          res_vec$Labour_cost_JM[month] <- Labour_JM + JM * (sample(Labour_cost_head_JM, 1)/Num_timesteps) * lab_non_health  
          res_vec$Labour_cost_SubAF[month] <- Labour_SubAF + SubAF * (sample(Labour_cost_head_SubAF, 1)/Num_timesteps) * lab_non_health  
          res_vec$Labour_cost_SubAM[month] <- Labour_SubAM + SubAM * (sample(Labour_cost_head_SubAM, 1)/Num_timesteps) * lab_non_health  
          res_vec$Labour_cost_AF[month] <- Labour_AF + AF * (sample(Labour_cost_head_AF, 1)/Num_timesteps) * lab_non_health  
          res_vec$Labour_cost_AM[month] <- Labour_AM + AM * (sample(Labour_cost_head_AM, 1)/Num_timesteps) * lab_non_health 
         
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
          res_vec$Feed_cost_Ox[month] <- Feed_Ox + Ox * sample(Expenditure_on_feed_Ox, 1) * timestep_Nof_days
          Feed_Ox <- res_vec$Feed_cost_Ox[month]
          res_vec$Feed_cost[month] <- res_vec$Feed_cost[month] + res_vec$Feed_cost_Ox[month]
          Feed <- res_vec$Feed_cost[month]
          
          res_vec$Labour_cost_Ox[month] <- Labour_Ox + Ox * (sample(Labour_cost_head_Oxen, 1)/Num_timesteps) * lab_non_health
          Labour_Ox <- res_vec$Labour_cost_Ox[month]
          res_vec$Labour_cost[month] <- res_vec$Labour_cost[month] + res_vec$Labour_cost_Ox[month]
          Labour =  res_vec$Labour_cost[month]
          
        }
        
      ## NOTE GIRMA ADD these vectors and matrices created
      if (species == "equids"){
        res_vec$Labour_cost_Cart_Driver[month] <- Labour_cost_Cart + (AF * (sample(Labour_cost_head_cd,1)/Num_timesteps) * Prop_cart_AF) + (AM * (sample(Labour_cost_head_cd,1)/Num_timesteps) * Prop_cart_AM)
        Labour_cost_Cart <- res_vec$Labour_cost_Cart_Driver[month]
        res_vec$Labour_cost[month] <- res_vec$Labour_cost[month] + res_vec$Labour_cost_Cart_Driver[month]
        Labour =  res_vec$Labour_cost[month]
        }
      
      ## add cost of caring for and milking milk producting animals
      if (species == "cattle" || species == "smallruminants") {
        res_vec$Labour_cost_AF[month] <- Labour_AF + AF * (sample(prop_F_milked, 1)) * (sample(Labour_cost_head_dairy, 1)/Num_timesteps) 
        Labour_AF <- res_vec$Labour_cost_AF[month]
        res_vec$Labour_cost[month] <- res_vec$Labour_cost[month] + (AF * (sample(prop_F_milked, 1)) * (sample(Labour_cost_head_dairy, 1)/Num_timesteps))
        Labour =  res_vec$Labour_cost[month]
        
      }
      
        if (species == "cattle" || species == "poultry" || species == "smallruminants") {
    
        res_vec$Health_cost_JF[month] <- Health_JF + JF * (sample(Health_exp_prev_JF, 1)/Num_timesteps) + JF * (sample(Health_exp_treatment_JF, 1)/Num_timesteps) 
        res_vec$Health_cost_JM[month] <- Health_JM + JM * (sample(Health_exp_prev_JM, 1)/Num_timesteps) + JM * (sample(Health_exp_treatment_JM, 1)/Num_timesteps) 
        res_vec$Health_cost_SubAF[month] <- Health_SubAF + SubAF * (sample(Health_exp_prev_SubAF, 1)/Num_timesteps) + SubAF * (sample(Health_exp_treatment_SubAF, 1)/Num_timesteps)
        res_vec$Health_cost_SubAM[month] <- Health_SubAM + SubAM * (sample(Health_exp_prev_SubAM, 1)/Num_timesteps) + SubAM * (sample(Health_exp_treatment_SubAM, 1)/Num_timesteps)
        res_vec$Health_cost_AF[month] <- Health_AF + AF * (sample(Health_exp_prev_AF, 1)/Num_timesteps) + AF * (sample(Health_exp_treatment_AF, 1)/Num_timesteps)
        res_vec$Health_cost_AM[month] <- Health_AM + AM * (sample(Health_exp_prev_AM, 1)/Num_timesteps) + AM * (sample(Health_exp_treatment_AM, 1)/Num_timesteps)
        
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
        res_vec$Health_cost_Ox[month] <- Health_Ox + Ox * (sample(Health_exp_prev_Ox, 1)/Num_timesteps) + Ox * (sample(Health_exp_treatment_Ox, 1)/Num_timesteps) 
        Health_Ox <- res_vec$Health_cost_Ox[month]                                                                                
        res_vec$Health_cost[month] <-  res_vec$Health_cost[month] + res_vec$Health_cost_Ox[month]
        Health = res_vec$Health_cost[month]
        
      } 
        
      if (species == "equids") {
        
        res_vec$Health_cost_JF[month] <- Health_JF + JF * (sample(Health_exp_vet_JF, 1)/Num_timesteps) + JF * (sample(Health_exp_trad_JF, 1)/Num_timesteps) +
                                                   + JF * (sample(Health_exp_selftx_JF, 1)/Num_timesteps) + JF * (sample(Health_exp_combined_JF, 1)/Num_timesteps)
        
        res_vec$Health_cost_JM[month] <- Health_JM + JM * (sample(Health_exp_vet_JF, 1)/Num_timesteps) + JM * (sample(Health_exp_trad_JM, 1)/Num_timesteps) +
          + JM * (sample(Health_exp_selftx_JM, 1)/Num_timesteps) + JM * (sample(Health_exp_combined_JM, 1)/Num_timesteps)
        
        res_vec$Health_cost_SubAF[month] <- Health_SubAF + SubAF * (sample(Health_exp_vet_SubAF, 1)/Num_timesteps) + SubAF * (sample(Health_exp_trad_SubAF, 1)/Num_timesteps) +
          + SubAF * (sample(Health_exp_selftx_SubAF, 1)/Num_timesteps) + SubAF * (sample(Health_exp_combined_SubAF, 1)/Num_timesteps)
        
        res_vec$Health_cost_SubAM[month] <- Health_SubAM + SubAM * (sample(Health_exp_vet_SubAM, 1)/Num_timesteps) + SubAM * (sample(Health_exp_trad_SubAM, 1)/Num_timesteps) +
          + SubAM * (sample(Health_exp_selftx_SubAM, 1)/Num_timesteps) + SubAM * (sample(Health_exp_combined_SubAM, 1)/Num_timesteps)
        
        res_vec$Health_cost_AF[month] <- Health_AF + AF * (sample(Health_exp_vet_AF, 1)/Num_timesteps) + AF * (sample(Health_exp_trad_AF, 1)/Num_timesteps) +
          + AF * (sample(Health_exp_selftx_AF, 1)/Num_timesteps) + AF * (sample(Health_exp_combined_AF, 1)/Num_timesteps)
        
        res_vec$Health_cost_AM[month] <- Health_AM + AM * (sample(Health_exp_vet_AM, 1)/Num_timesteps) + AM * (sample(Health_exp_trad_AM, 1)/Num_timesteps) +
          + AM * (sample(Health_exp_selftx_AM, 1)/Num_timesteps) + AM * (sample(Health_exp_combined_AM, 1)/Num_timesteps)
        
        
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
        
        ### Do separately for each health cost type for future exploration these outputs can be analysed
        ## vet
        res_vec$Health_cost_vet_JF[month] <- Health_vet_JF + JF * (sample(Health_exp_vet_JF, 1)/Num_timesteps) 
        res_vec$Health_cost_vet_JM[month] <- Health_vet_JM + JM * (sample(Health_exp_vet_JM, 1)/Num_timesteps) 
        res_vec$Health_cost_vet_SubAF[month] <- Health_vet_SubAF + SubAF * (sample(Health_exp_vet_SubAF, 1)/Num_timesteps) 
        res_vec$Health_cost_vet_SubAM[month] <- Health_vet_SubAM + SubAM * (sample(Health_exp_vet_SubAM, 1)/Num_timesteps) 
        res_vec$Health_cost_vet_AF[month] <- Health_vet_AF + AF * (sample(Health_exp_vet_AF, 1)/Num_timesteps) 
        res_vec$Health_cost_vet_AM[month] <- Health_vet_AM + AM * (sample(Health_exp_vet_AM, 1)/Num_timesteps) 
        
        Health_vet_JF <- res_vec$Health_cost_vet_JF[month]
        Health_vet_JM <- res_vec$Health_cost_vet_JM[month]
        Health_vet_SubAF <- res_vec$Health_cost_vet_SubAF[month]
        Health_vet_SubAM <- res_vec$Health_cost_vet_SubAM[month]
        Health_vet_AF <- res_vec$Health_cost_vet_AF[month]
        Health_vet_AM <- res_vec$Health_cost_vet_AM[month]
        
        res_vec$Health_cost_vet[month] <- sum(res_vec$Health_cost_vet_JF[month],
                                          res_vec$Health_cost_vet_JM[month],
                                          res_vec$Health_cost_vet_SubAF[month],
                                          res_vec$Health_cost_vet_SubAM[month],
                                          res_vec$Health_cost_vet_AM[month],
                                          res_vec$Health_cost_vet_AF[month])
        
        
        Health_vet = res_vec$Health_cost_vet[month]
        
        ## traditional
        res_vec$Health_cost_trad_JF[month] <- Health_trad_JF + JF * (sample(Health_exp_trad_JF, 1)/Num_timesteps) 
        res_vec$Health_cost_trad_JM[month] <- Health_trad_JM + JM * (sample(Health_exp_trad_JM, 1)/Num_timesteps) 
        res_vec$Health_cost_trad_SubAF[month] <- Health_trad_SubAF + SubAF * (sample(Health_exp_trad_SubAF, 1)/Num_timesteps) 
        res_vec$Health_cost_trad_SubAM[month] <- Health_trad_SubAM + SubAM * (sample(Health_exp_trad_SubAM, 1)/Num_timesteps) 
        res_vec$Health_cost_trad_AF[month] <- Health_trad_AF + AF * (sample(Health_exp_trad_AF, 1)/Num_timesteps) 
        res_vec$Health_cost_trad_AM[month] <- Health_trad_AM + AM * (sample(Health_exp_trad_AM, 1)/Num_timesteps) 
        
        Health_trad_JF <- res_vec$Health_cost_trad_JF[month]
        Health_trad_JM <- res_vec$Health_cost_trad_JM[month]
        Health_trad_SubAF <- res_vec$Health_cost_trad_SubAF[month]
        Health_trad_SubAM <- res_vec$Health_cost_trad_SubAM[month]
        Health_trad_AF <- res_vec$Health_cost_trad_AF[month]
        Health_trad_AM <- res_vec$Health_cost_trad_AM[month]
        
        res_vec$Health_cost_trad[month] <- sum(res_vec$Health_cost_trad_JF[month],
                                              res_vec$Health_cost_trad_JM [month],
                                              res_vec$Health_cost_trad_SubAF[month],
                                              res_vec$Health_cost_trad_SubAM[month],
                                              res_vec$Health_cost_trad_AF[month],
                                              res_vec$Health_cost_trad_AM[month])
        
        
        Health_trad = res_vec$Health_cost_trad[month]
        
        ## self treatment
        res_vec$Health_cost_selftx_JF[month] <- Health_selftx_JF + JF * (sample(Health_exp_selftx_JF, 1)/Num_timesteps) 
        res_vec$Health_cost_selftx_JM[month] <- Health_selftx_JM + JM * (sample(Health_exp_selftx_JM, 1)/Num_timesteps) 
        res_vec$Health_cost_selftx_SubAF[month] <- Health_selftx_SubAF + SubAF * (sample(Health_exp_selftx_SubAF, 1)/Num_timesteps) 
        res_vec$Health_cost_selftx_SubAM[month] <- Health_selftx_SubAM + SubAM * (sample(Health_exp_selftx_SubAM, 1)/Num_timesteps) 
        res_vec$Health_cost_selftx_AF[month] <- Health_selftx_AF + AF * (sample(Health_exp_selftx_AF, 1)/Num_timesteps) 
        res_vec$Health_cost_selftx_AM[month] <- Health_selftx_AM + AM * (sample(Health_exp_selftx_AM, 1)/Num_timesteps) 
        
        Health_selftx_JF <- res_vec$Health_cost_selftx_JF[month]
        Health_selftx_JM <- res_vec$Health_cost_selftx_JM[month]
        Health_selftx_SubAF <- res_vec$Health_cost_selftx_SubAF[month]
        Health_selftx_SubAM <- res_vec$Health_cost_selftx_SubAM[month]
        Health_selftx_AF <- res_vec$Health_cost_selftx_AF[month]
        Health_selftx_AM <- res_vec$Health_cost_selftx_AM[month]
        
        res_vec$Health_cost_selftx[month] <- sum(res_vec$Health_cost_selftx_JF[month],
                                               res_vec$Health_cost_selftx_JM[month],
                                               res_vec$Health_cost_selftx_SubAF[month],
                                               res_vec$Health_cost_selftx_SubAM[month],
                                               res_vec$Health_cost_selftx_AF[month],
                                               res_vec$Health_cost_selftx_AM[month])
        
        
        Health_selftx = res_vec$Health_cost_selftx[month]
        
        ## combined
        res_vec$Health_cost_combined_JF[month] <- Health_combined_JF + JF * (sample(Health_exp_combined_JF, 1)/Num_timesteps) 
        res_vec$Health_cost_combined_JM[month] <- Health_combined_JM+ JM * (sample(Health_exp_combined_JM, 1)/Num_timesteps) 
        res_vec$Health_cost_combined_SubAF[month] <- Health_combined_SubAF + SubAF * (sample(Health_exp_combined_SubAF, 1)/Num_timesteps) 
        res_vec$Health_cost_combined_SubAM[month] <- Health_combined_SubAM + SubAM * (sample(Health_exp_combined_SubAM, 1)/Num_timesteps) 
        res_vec$Health_cost_combined_AF[month] <- Health_combined_AF + AF * (sample(Health_exp_combined_AF, 1)/Num_timesteps) 
        res_vec$Health_cost_combined_AM[month] <- Health_combined_AM + AM * (sample(Health_exp_combined_AM, 1)/Num_timesteps) 
        
        Health_combined_JF <- res_vec$Health_cost_combined_JF[month]
        Health_combined_JM <- res_vec$Health_cost_combined_JM[month]
        Health_combined_SubAF <- res_vec$Health_cost_combined_SubAF[month]
        Health_combined_SubAM <- res_vec$Health_cost_combined_SubAM[month]
        Health_combined_AF <- res_vec$Health_cost_combined_AF[month]
        Health_combined_AM <- res_vec$Health_cost_combined_AM[month]
        
        res_vec$Health_cost_combined[month] <- sum(res_vec$Health_cost_combined_JF[month],
                                                 res_vec$Health_cost_combined_JM[month],
                                                 res_vec$Health_cost_combined_SubAF[month],
                                                 res_vec$Health_cost_combined_SubAM[month],
                                                 res_vec$Health_cost_combined_AF[month],
                                                 res_vec$Health_cost_combined_AM[month])
        
        
        Health_combined = res_vec$Health_cost_combined[month]
        
      }
      
      
    ## GEMMA CHANGED these numbers from [1] to set numbers at time zero (eg. N_JF_t0), like infrastructure calculations
    ## below it is better to use the defined starting population numbers rather than the numbers at timestep[1]
    
      if (species == "cattle" || species == "smallruminants" || species == "poultry" || species == "equids") {
        
        res_vec$Capital_cost_JF[month] <- N_JF_t0 * fin_val_JF * Interest_rate 
        Capital_JF <- res_vec$Capital_cost_JF[month]
     
        res_vec$Capital_cost_JM[month] <- N_JM_t0 * fin_val_JM * Interest_rate  
        Capital_JM <- res_vec$Capital_cost_JM[month]
     
        res_vec$Capital_cost_SubAF[month] <-N_SubAF_t0 * fin_val_SubAF * Interest_rate  
        Capital_SubAF = res_vec$Capital_cost_SubAF[month]
     
        res_vec$Capital_cost_SubAM[month] <- N_SubAM_t0 * fin_val_SubAM * Interest_rate  
        Capital_SubAM <- res_vec$Capital_cost_SubAM[month]
     
        res_vec$Capital_cost_AF[month] <- N_AF_t0[1] * fin_val_AF * Interest_rate  
        Capital_AF <- res_vec$Capital_cost_AF[month]
     
        res_vec$Capital_cost_AM[month] <- N_AM_t0 * fin_val_AM * Interest_rate  
        Capital_AM <- res_vec$Capital_cost_AM[month]
     
        res_vec$Capital_cost[month] <- sum(res_vec$Capital_cost_JF[month],
                                       res_vec$Capital_cost_JM[month],
                                       res_vec$Capital_cost_SubAF[month],
                                       res_vec$Capital_cost_SubAM[month],
                                       res_vec$Capital_cost_AF[month],
                                       res_vec$Capital_cost_AM[month])
        
        Capital <- res_vec$Capital_cost[month]
      }
     
     if (species == "cattle") {
       res_vec$Capital_cost_Ox[month] <- N_Ox_t0 * fin_val_Ox * Interest_rate
       Capital_Ox <- res_vec$Capital_cost_Ox[month]
       res_vec$Capital_cost[month] <- res_vec$Capital_cost[month] + res_vec$Capital_cost_Ox[month]
     }
        
    if (species == "cattle" || species == "smallruminants" || species == "poultry" || species == "equids") {
      Capital <- res_vec$Capital_cost[month]
    }
        
    if (species == "cattle" || species == "smallruminants" || species == "poultry" || species == "equids") {
    
       ### For equids this is fixed cost including barn and cart maintenance
     res_vec$Infrastructure_cost_JF[month] <- N_JF_t0 * sample(Infrastructure_per_head_JF, 1)
     res_vec$Infrastructure_cost_JM[month] <- N_JM_t0 * sample(Infrastructure_per_head_JM, 1)
     res_vec$Infrastructure_cost_SubAF[month] <- N_SubAF_t0 * sample(Infrastructure_per_head_SubAF, 1)
     res_vec$Infrastructure_cost_SubAM[month] <- N_SubAM_t0 * sample(Infrastructure_per_head_SubAM, 1)
     res_vec$Infrastructure_cost_AF[month] <- N_AF_t0 * sample(Infrastructure_per_head_AF, 1)
     res_vec$Infrastructure_cost_AM[month] <- N_AM_t0 * sample(Infrastructure_per_head_AM, 1)
     
     res_vec$Infrastructure_cost[month] <- sum(res_vec$Infrastructure_cost_JF[month],
                                               res_vec$Infrastructure_cost_JM[month],
                                               res_vec$Infrastructure_cost_SubAF[month],
                                               res_vec$Infrastructure_cost_SubAM[month],
                                               res_vec$Infrastructure_cost_AF[month],
                                               res_vec$Infrastructure_cost_AM[month])
    }
     
     if (species == "cattle") {
       res_vec$Infrastructure_cost_Ox[month] <- N_Ox_t0 * sample(Infrastructure_per_head_Ox, 1)
       res_vec$Infrastructure_cost[month] <- res_vec$Infrastructure_cost[month] + res_vec$Infrastructure_cost_Ox[month]
     }
      
      ## ACCESSORY COST for equids
      if (species == "equids") {
        
        ### For accessory cost is feeder, harness, ropes etc
        res_vec$Accessory_cost_JF[month] <- N_JF_t0 * sample(Acce_cost_head_JF, 1)
        res_vec$Accessory_cost_JM[month] <- N_JM_t0 * sample(Acce_cost_head_JM, 1)
        res_vec$Accessory_cost_SubAF[month] <- N_SubAF_t0 * sample(Acce_cost_head_SubAF, 1)
        res_vec$Accessory_cost_SubAM[month] <- N_SubAM_t0 * sample(Acce_cost_head_SubAM, 1)
        res_vec$Accessory_cost_AF[month] <- N_AF_t0 * sample(Acce_cost_head_AF, 1)
        res_vec$Accessory_cost_AM[month] <- N_AM_t0 * sample(Acce_cost_head_AM, 1)
        
        res_vec$Accessory_cost[month] <- sum(res_vec$Accessory_cost_JF[month],
                                                  res_vec$Accessory_cost_JM[month],
                                                  res_vec$Accessory_cost_SubAF[month],
                                                  res_vec$Accessory_cost_SubAM[month],
                                                  res_vec$Accessory_cost_AF[month],
                                                  res_vec$Accessory_cost_AM[month])
      }
      
      
        
    if (species == "cattle" || species == "smallruminants" || species == "poultry" ) {
     
     res_vec$Total_expenditure[month] <- res_vec$Feed_cost[month] + Health + Labour + Capital + res_vec$Infrastructure_cost[month]
     
     res_vec$Total_expenditure_JF[month] <- Feed_JF + Health_JF + Labour_JF + Capital_JF + res_vec$Infrastructure_cost_JF[month]
     res_vec$Total_expenditure_JM[month] <-  Feed_JM + Health_JM + Labour_JM + Capital_JM + res_vec$Infrastructure_cost_JM[month]
     res_vec$Total_expenditure_SubAF[month] <- Feed_SubAF + Health_SubAF + Labour_SubAF + Capital_SubAF + res_vec$Infrastructure_cost_SubAF[month]
     res_vec$Total_expenditure_SubAM[month] <-  Feed_SubAM + Health_SubAM + Labour_SubAM + Capital_SubAM + res_vec$Infrastructure_cost_SubAM[month]
     res_vec$Total_expenditure_AF[month] <-  Feed_AF + Health_AF + Labour_AF + Capital_AF + res_vec$Infrastructure_cost_AF[month]
     res_vec$Total_expenditure_AM[month] <- Feed_AM + Health_AM + Labour_AM + Capital_AM + res_vec$Infrastructure_cost_AM[month]
     
    }
      
      if (species == "equids") {
        
        res_vec$Total_expenditure[month] <- res_vec$Feed_cost[month] + Health + Labour + Capital + res_vec$Infrastructure_cost[month] + res_vec$Accessory_cost[month]
        
        res_vec$Total_expenditure_JF[month] <- Feed_JF + Health_JF + Labour_JF + Capital_JF + res_vec$Infrastructure_cost_JF[month] + res_vec$Accessory_cost_AF[month]
        res_vec$Total_expenditure_JM[month] <-  Feed_JM + Health_JM + Labour_JM + Capital_JM + res_vec$Infrastructure_cost_JM[month] + res_vec$Accessory_cost_AM[month]
        res_vec$Total_expenditure_SubAF[month] <- Feed_SubAF + Health_SubAF + Labour_SubAF + Capital_SubAF + res_vec$Infrastructure_cost_SubAF[month] + res_vec$Accessory_cost_SubAF[month]
        res_vec$Total_expenditure_SubAM[month] <-  Feed_SubAM + Health_SubAM + Labour_SubAM + Capital_SubAM + res_vec$Infrastructure_cost_SubAM[month] + res_vec$Accessory_cost_SubAM[month]
        res_vec$Total_expenditure_AF[month] <-  Feed_AF + Health_AF + Labour_AF + Capital_AF + res_vec$Infrastructure_cost_AF[month] + res_vec$Accessory_cost_AF[month]
        res_vec$Total_expenditure_AM[month] <- Feed_AM + Health_AM + Labour_AM + Capital_AM + res_vec$Infrastructure_cost_AM[month] + res_vec$Accessory_cost_AM[month]
        
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
       
     }

      
    } # end Num_timesteps loop
    
    #### GEMMA TO HERE WITH EDITS #####
    
    res_mat$Num_JF[i, ] <- res_vec$Num_JF
    res_mat$Num_SubAF[i, ] <- res_vec$Num_SubAF
    res_mat$Num_AF[i, ] <- res_vec$Num_AF
    res_mat$Num_JM[i, ] <- res_vec$Num_JM
    res_mat$Num_SubAM[i, ] <- res_vec$Num_SubAM
    res_mat$Num_AM[i, ] <- res_vec$Num_AM
    res_mat$Num_N[i, ] <- res_vec$Num_N
    
    res_mat$Total_mortality[i, ] <- res_vec$Total_mortality
    
    res_mat$Total_mortality_JF[i, ] <- res_vec$Total_mortality_JF
    res_mat$Total_mortality_JM[i, ] <- res_vec$Total_mortality_JM
    res_mat$Total_mortality_SubAF[i, ] <- res_vec$Total_mortality_SubAF
    res_mat$Total_mortality_SubAM[i, ] <- res_vec$Total_mortality_SubAM
    res_mat$Total_mortality_AF[i, ] <- res_vec$Total_mortality_AF
    res_mat$Total_mortality_AM[i, ] <- res_vec$Total_mortality_AM
    
    
    res_mat$Quantity_liveweight_kg[i, ] <- res_vec$Quantity_liveweight_kg
    
    res_mat$Quantity_liveweight_kg_JF[i, ] <- res_vec$Quantity_liveweight_kg_JF
    res_mat$Quantity_liveweight_kg_JM[i, ] <- res_vec$Quantity_liveweight_kg_JM
    res_mat$Quantity_liveweight_kg_SubAF[i, ] <- res_vec$Quantity_liveweight_kg_SubAF
    res_mat$Quantity_liveweight_kg_SubAM[i, ] <- res_vec$Quantity_liveweight_kg_SubAM
    res_mat$Quantity_liveweight_kg_AF[i, ] <- res_vec$Quantity_liveweight_kg_AF
    res_mat$Quantity_liveweight_kg_AM[i, ] <- res_vec$Quantity_liveweight_kg_AM
    
    res_mat$Cumulative_draught_income[i, ] <- res_vec$Cumulative_draught_income

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
    
    res_mat$Pop_growth_JF[i, ] <- res_vec$Pop_growth_JF
    res_mat$Pop_growth_JM[i, ] <- res_vec$Pop_growth_JM
    res_mat$Pop_growth_SubAF[i, ] <- res_vec$Pop_growth_SubAF
    res_mat$Pop_growth_SubAM[i, ] <- res_vec$Pop_growth_SubAM
    res_mat$Pop_growth_AF[i, ] <- res_vec$Pop_growth_AF
    res_mat$Pop_growth_AM[i, ] <- res_vec$Pop_growth_AM
    
    res_mat$Quantity_manure[i, ] <- res_vec$Quantity_manure
    res_mat$Quantity_manure_JF[i, ] <- res_vec$Quantity_manure_JF
    res_mat$Quantity_manure_JM[i, ] <- res_vec$Quantity_manure_JM
    res_mat$Quantity_manure_SubAF[i, ] <- res_vec$Quantity_manure_SubAF
    res_mat$Quantity_manure_SubAM[i, ] <- res_vec$Quantity_manure_SubAM
    res_mat$Quantity_manure_AF[i, ] <- res_vec$Quantity_manure_AF
    res_mat$Quantity_manure_AM[i, ] <- res_vec$Quantity_manure_AM
    
    #### ADDED FILL MATRICX FROM MANURE VALUE
    res_mat$Value_manure[i, ] <- res_vec$Value_manure
    res_mat$Value_manure_JF[i, ] <- res_vec$Value_manure_JF
    res_mat$Value_manure_JM[i, ] <- res_vec$Value_manure_JM
    res_mat$Value_manure_SubAF[i, ] <- res_vec$Value_manure_SubAF
    res_mat$Value_manure_SubAM[i, ] <- res_vec$Value_manure_SubAM
    res_mat$Value_manure_AF[i, ] <- res_vec$Value_manure_AF
    res_mat$Value_manure_AM[i, ] <- res_vec$Value_manure_AM
    
    res_mat$Quantity_hides[i, ] <- res_vec$Quantity_hides
    
    res_mat$Quantity_hides_SubAF[i, ] <- res_vec$Quantity_hides_SubAF
    res_mat$Quantity_hides_SubAM[i, ] <- res_vec$Quantity_hides_SubAM
    res_mat$Quantity_hides_AF[i, ] <- res_vec$Quantity_hides_AF
    res_mat$Quantity_hides_AM[i, ] <- res_vec$Quantity_hides_AM
    
    ### ADDED fill MATRIX HIDES VALUE
    res_mat$Value_hides[i, ] <- res_vec$Value_hides
    
    res_mat$Value_hides_SubAF[i, ] <- res_vec$Value_hides_SubAF
    res_mat$Value_hides_SubAM[i, ] <- res_vec$Value_hides_SubAM
    res_mat$Value_hides_AF[i, ] <- res_vec$Value_hides_AF
    res_mat$Value_hides_AM[i, ] <- res_vec$Value_hides_AM    
    
    
    res_mat$Quantity_milk[i, ] <- res_vec$Quantity_milk
    ## MILK VALUEADDED
    res_mat$Value_milk[i, ] <- res_vec$Value_milk 

    # res_mat$Quantity_Wool[i, ] <- res_vec$Quantity_Wool
    
    res_mat$Cumulative_dry_matter[i, ] <- res_vec$Cumulative_dry_matter
    
    res_mat$Cumulative_dry_matter_JF[i, ] <- res_vec$Cumulative_dry_matter_JF
    res_mat$Cumulative_dry_matter_JM[i, ] <- res_vec$Cumulative_dry_matter_JM
    res_mat$Cumulative_dry_matter_SubAF[i, ] <- res_vec$Cumulative_dry_matter_SubAF
    res_mat$Cumulative_dry_matter_SubAM[i, ] <- res_vec$Cumulative_dry_matter_SubAM
    res_mat$Cumulative_dry_matter_AF[i, ] <- res_vec$Cumulative_dry_matter_AF
    res_mat$Cumulative_dry_matter_AM[i, ] <- res_vec$Cumulative_dry_matter_AM
    

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
      res_mat$Num_Ox[i, ] <- res_vec$Num_Ox
      res_mat$Total_mortality_Ox[i, ] <- res_vec$Total_mortality_Ox
      res_mat$Quantity_liveweight_kg_Ox[i, ] <- res_vec$Quantity_liveweight_kg_Ox
      res_mat$Num_offtake_Ox[i, ] <- res_vec$Num_offtake_Ox
      res_mat$Offtake_liveweight_kg_Ox[i, ] <- res_vec$Offtake_liveweight_kg_Ox
      res_mat$Pop_growth_Ox[i, ] <- res_vec$Pop_growth_Ox
      res_mat$Quantity_manure_Ox[i, ] <- res_vec$Quantity_manure_Ox
      res_mat$Value_manure_Ox[i, ] <- res_vec$Value_manure_Ox ## added here
      res_mat$Quantity_hides_Ox[i, ] <- res_vec$Quantity_hides_Ox
      res_mat$Value_hides_Ox[i, ] <- res_vec$Value_hides_Ox ## added here
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
    
    ## EQUIDS ##
    
    if (species == "equids") {
      res_mat$Labour_cost_Cart_Driver[i, ] <- res_vec$Labour_cost_Cart_Driver
      
      res_mat$Health_cost_vet_JF[i, ] <- res_vec$Health_cost_vet_JF
      res_mat$Health_cost_vet_JM[i, ] <- res_vec$Health_cost_vet_JM
      res_mat$Health_cost_vet_SubAF[i, ] <- res_vec$Health_cost_vet_SubAF
      res_mat$Health_cost_vet_SubAM[i, ] <- res_vec$Health_cost_vet_SubAM
      res_mat$Health_cost_vet_AF[i, ] <- res_vec$Health_cost_vet_AF
      res_mat$Health_cost_vet_AM[i, ] <- res_vec$Health_cost_vet_AM
      
      res_mat$Health_cost_trad_JF[i, ] <- res_vec$Health_cost_trad_JF
      res_mat$Health_cost_trad_JM[i, ] <- res_vec$Health_cost_trad_JM
      res_mat$Health_cost_trad_SubAF[i, ] <- res_vec$Health_cost_trad_SubAF
      res_mat$Health_cost_trad_SubAM[i, ] <- res_vec$Health_cost_trad_SubAM
      res_mat$Health_cost_trad_AF[i, ] <- res_vec$Health_cost_trad_AF
      res_mat$Health_cost_trad_AM[i, ] <- res_vec$Health_cost_trad_AM
      
      res_mat$Health_cost_selftx_JF[i, ] <- res_vec$Health_cost_selftx_JF
      res_mat$Health_cost_selftx_JM[i, ] <- res_vec$Health_cost_selftx_JM
      res_mat$Health_cost_selftx_SubAF[i, ] <- res_vec$Health_cost_selftx_SubAF
      res_mat$Health_cost_selftx_SubAM[i, ] <- res_vec$Health_cost_selftx_SubAM
      res_mat$Health_cost_selftx_AF[i, ] <- res_vec$Health_cost_selftx_AF
      res_mat$Health_cost_selftx_AM[i, ] <- res_vec$Health_cost_selftx_AM
      
      res_mat$Health_cost_combined_JF[i, ] <- res_vec$Health_cost_combined_JF
      res_mat$Health_cost_combined_JM[i, ] <- res_vec$Health_cost_combined_JM
      res_mat$Health_cost_combined_SubAF[i, ] <- res_vec$Health_cost_combined_SubAF
      res_mat$Health_cost_combined_SubAM[i, ] <- res_vec$Health_cost_combined_SubAM
      res_mat$Health_cost_combined_AF[i, ] <- res_vec$Health_cost_combined_AF
      res_mat$Health_cost_combined_AM[i, ] <- res_vec$Health_cost_combined_AM
      
      res_mat$Accessory_cost[i, ] <- res_vec$Accessory_cost
      res_mat$Accessory_cost_JF[i, ] <- res_vec$Accessory_cost_JF
      res_mat$Accessory_cost_JM[i, ] <- res_vec$Accessory_cost_JM
      res_mat$Accessory_cost_SubAF[i, ] <- res_vec$Accessory_cost_SubAF
      res_mat$Accessory_cost_SubAM[i, ] <- res_vec$Accessory_cost_SubAM
      res_mat$Accessory_cost_AF[i, ] <- res_vec$Accessory_cost_AF
      res_mat$Accessory_cost_AM[i, ] <- res_vec$Accessory_cost_AM
      
      res_mat$Cumulative_commercial_income[i, ] <- res_vec$Cumulative_commercial_income
      res_mat$Cumulative_commercial_income_AF[i, ] <- res_vec$Cumulative_commercial_income_AF
      res_mat$Cumulative_commercial_income_AM[i, ] <- res_vec$Cumulative_commercial_income_AM
      
      res_mat$Cumulative_unpaid_income[i, ] <- res_vec$Cumulative_unpaid_income
      res_mat$Cumulative_unpaid_income_SubAF[i, ] <- res_vec$Cumulative_unpaid_income_SubAF
      res_mat$Cumulative_unpaid_income_SubAM[i, ] <- res_vec$Cumulative_unpaid_income_SubAM
      res_mat$Cumulative_unpaid_income_AF[i, ] <- res_vec$Cumulative_unpaid_income_AF
      res_mat$Cumulative_unpaid_income_AM[i, ] <- res_vec$Cumulative_unpaid_income_AM
      
      res_mat$Cumulative_combined_income[i, ] <- res_vec$Cumulative_combined_income
      res_mat$Cumulative_combined_income_SubAF[i, ] <- res_vec$Cumulative_combined_income_SubAF
      res_mat$Cumulative_combined_income_SubAM[i, ] <- res_vec$Cumulative_combined_income_SubAM
      res_mat$Cumulative_combined_income_AF[i, ] <- res_vec$Cumulative_combined_income_AF
      res_mat$Cumulative_combined_income_AM[i, ] <- res_vec$Cumulative_combined_income_AM
      
      res_mat$Donkey_power_income[i, ] <- res_vec$Donkey_power_income
      res_mat$Donkey_power_income_SubAF[i, ] <- res_vec$Donkey_power_income_SubAF
      res_mat$Donkey_power_income_SubAM[i, ] <- res_vec$Donkey_power_income_SubAM
      res_mat$Donkey_power_income_AF[i, ] <- res_vec$Donkey_power_income_AF
      res_mat$Donkey_power_income_AM[i, ] <- res_vec$Donkey_power_income_AM
      
    }
    
  } # end nruns loop
  ## some errors here in the script corrected, replaced SubAF with JF etc
  Total_number_change_JF <-  res_mat$Pop_growth_JF
  Total_number_change_JM <-  res_mat$Pop_growth_JM
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
  

  if (species == "cattle" || species == "smallruminants" || species == "equids" || species == "poultry" ) {
  
    Quantity_hides_SubA <- res_mat$Quantity_hides_SubAF + res_mat$Quantity_hides_SubAM
    
    Value_hides_SubA <- res_mat$Value_hides_SubAF + res_mat$Value_hides_SubAM
    

    res_mat$Production_value_herd_offtake_hide_manure_JF <- res_mat$Total_value_increase_JF + res_mat$Value_manure_JF
    res_mat$Production_value_herd_offtake_hide_manure_JM <- res_mat$Total_value_increase_JM + res_mat$Value_manure_JM
    res_mat$Production_value_herd_offtake_hide_manure_SubAF <- res_mat$Total_value_increase_SubAF + res_mat$Value_manure_SubAF + res_mat$Value_hides_SubAF
    res_mat$Production_value_herd_offtake_hide_manure_SubAM <- res_mat$Total_value_increase_SubAM + res_mat$Value_manure_SubAM + res_mat$Value_hides_SubAM
    res_mat$Production_value_herd_offtake_hide_manure_AF <- res_mat$Total_value_increase_AF + res_mat$Value_manure_AF + res_mat$Value_hides_AF 
    res_mat$Production_value_herd_offtake_hide_manure_AM <- res_mat$Total_value_increase_AM + res_mat$Value_manure_AM + res_mat$Value_hides_AM
    
    res_mat$Production_value_herd_offtake_hide_manure  <- (res_mat$Production_value_herd_offtake_hide_manure_JF +
                                                             res_mat$Production_value_herd_offtake_hide_manure_JM +
                                                             res_mat$Production_value_herd_offtake_hide_manure_SubAF +
                                                             res_mat$Production_value_herd_offtake_hide_manure_SubAM +
                                                             res_mat$Production_value_herd_offtake_hide_manure_AF +
                                                             res_mat$Production_value_herd_offtake_hide_manure_AM)
    
  }
  
  if (species == "cattle" || species == "smallruminants") {
    res_mat$Production_value_herd_offtake_hide_manure_AF <- res_mat$Production_value_herd_offtake_hide_manure_AF + res_mat$Value_milk 
  
    res_mat$Production_value_herd_offtake_hide_manure  <- (res_mat$Production_value_herd_offtake_hide_manure_JF +
                                                             res_mat$Production_value_herd_offtake_hide_manure_JM +
                                                             res_mat$Production_value_herd_offtake_hide_manure_SubAF +
                                                             res_mat$Production_value_herd_offtake_hide_manure_SubAM +
                                                             res_mat$Production_value_herd_offtake_hide_manure_AF +
                                                             res_mat$Production_value_herd_offtake_hide_manure_AM)
    }
  
  if (species == "cattle") {
    Total_number_change_Ox <- res_mat$Num_offtake_Ox + res_mat$Pop_growth_Ox
    Total_number_change <- res_mat$Total_number_change + res_mat$Total_number_change_Ox
    
    res_mat$Production_value_herd_offtake_hide_manure_Ox <- res_mat$Total_value_increase_Ox + res_mat$Value_manure_Ox + res_mat$Value_hides_Ox + res_mat$Cumulative_draught_income
    
    # Add Draught income to production value for cattle
    res_mat$Production_value_herd_offtake_hide_manure <- res_mat$Production_value_herd_offtake_hide_manure + res_mat$Production_value_herd_offtake_hide_manure_Ox
     }
    
  ### !!! POULTRY NEEDS MORE INCOME categories!!!
  if (species == "poultry") {
    res_mat$Production_value_herd_offtake_hide_manure_AF <- res_mat$Total_value_increase_AF + res_mat$Value_eggs_sold + res_mat$Value_eggs_consumed
    
    res_mat$Production_value_herd_offtake_hide_manure  <- (res_mat$Production_value_herd_offtake_hide_manure_JF +
                                                             res_mat$Production_value_herd_offtake_hide_manure_JM +
                                                             res_mat$Production_value_herd_offtake_hide_manure_SubAF +
                                                             res_mat$Production_value_herd_offtake_hide_manure_SubAM +
                                                             res_mat$Production_value_herd_offtake_hide_manure_AF +
                                                             res_mat$Production_value_herd_offtake_hide_manure_AM)
    }
  
  if (species == "equids"){
    res_mat$Production_value_herd_offtake_hide_manure_SubAF <- res_mat$Production_value_herd_offtake_hide_manure_SubAF + res_mat$Donkey_power_income_SubAF
    res_mat$Production_value_herd_offtake_hide_manure_SubAM <- res_mat$Production_value_herd_offtake_hide_manure_SubAM + res_mat$Donkey_power_income_SubAM
    res_mat$Production_value_herd_offtake_hide_manure_AF <- res_mat$Production_value_herd_offtake_hide_manure_AF + res_mat$Donkey_power_income_AF
    res_mat$Production_value_herd_offtake_hide_manure_AM <- res_mat$Production_value_herd_offtake_hide_manure_AM + res_mat$Donkey_power_income_AM
    
    res_mat$Production_value_herd_offtake_hide_manure  <- (res_mat$Production_value_herd_offtake_hide_manure_JF +
                                                             res_mat$Production_value_herd_offtake_hide_manure_JM +
                                                             res_mat$Production_value_herd_offtake_hide_manure_SubAF +
                                                             res_mat$Production_value_herd_offtake_hide_manure_SubAM +
                                                             res_mat$Production_value_herd_offtake_hide_manure_AF +
                                                             res_mat$Production_value_herd_offtake_hide_manure_AM)
  }
  #### CALCULATING THE GROSS MARGINS #####
  
  res_mat$Gross_margin <- res_mat$Production_value_herd_offtake_hide_manure - res_mat$Total_expenditure
  
  res_mat$Gross_margin_JF <- res_mat$Production_value_herd_offtake_hide_manure_JF - res_mat$Total_expenditure_JF
  res_mat$Gross_margin_JM <- res_mat$Production_value_herd_offtake_hide_manure_JM - res_mat$Total_expenditure_JM
  res_mat$Gross_margin_SubAF <- res_mat$Production_value_herd_offtake_hide_manure_SubAF - res_mat$Total_expenditure_SubAF
  res_mat$Gross_margin_SubAM <- res_mat$Production_value_herd_offtake_hide_manure_SubAM - res_mat$Total_expenditure_SubAM
  res_mat$Gross_margin_AF <- res_mat$Production_value_herd_offtake_hide_manure_AF - res_mat$Total_expenditure_AF
  res_mat$Gross_margin_AM <- res_mat$Production_value_herd_offtake_hide_manure_AM - res_mat$Total_expenditure_AM
  
  if (species == "cattle") {
    
    res_mat$Gross_margin_Ox <- res_mat$Production_value_herd_offtake_hide_manure_Ox - res_mat$Total_expenditure_Ox
  }
  
  
  #@@ output results to a CSV file ###
  
  if (output == "cumulative total") {
    ## output from last month (cumulative total) ##
    
    apply_last_column <- function(mat) {
      mat_last_column <- mat[, ncol(mat), drop = TRUE]  
      mat_last_column
    }
    
    mat_list <- lapply(res_mat, apply_last_column)
    
    df <- as.data.frame(do.call(rbind, mat_list))
    
    rownames(df) <- names(mat_list)
    colnames(df) <- paste("Run", 1:ncol(df))
    df
  } else {
    ## Summary statistics ##
    
    apply_summary_last_column <- function(mat) {
      mat_summary <- summary(mat[, ncol(mat)])
      mat_sd <- sd(mat[, ncol(mat)])
      mat_summary <- c(mat_summary, SD = mat_sd)
    }
    
    summary_list <- lapply(res_mat, apply_summary_last_column)
    
    df <- as.data.frame(do.call(rbind, summary_list))
    
    df$Variable <- rownames(df)
    
    df <- df[, c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.", "SD")]
    df # summary statistics across all months
    
  } 
  

} # end function


