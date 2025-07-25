### GEMMA EDITS VERSION 6 ###

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

run_compartmental_model <- function() {
  
  set.seed(seed_value)
  
  ### Functions ###

  ## removed function because code wasnt running  
#  calculate_mu <- function(part, prolif) {
#    return ((sample(part, size = nruns, replace = TRUE) * sample(prolif, size = nruns, replace = TRUE)) / Num_timesteps)
#  }
    
  calculate_purchased_feed <- function(kg_dm_req, lskeepers_purch_feed, feed_paid_for, dm_in_feed) {
    return (kg_dm_req * lskeepers_purch_feed * feed_paid_for / dm_in_feed)
  }
  
  calculate_feed_required_based_on_DM_content <- function(kg_dm_req, dm_in_feed){
    return(kg_dm_req/dm_in_feed)
  }
  
  calculate_expenditure_on_feed <- function(kg_feed_purchased, feed_cost_kg) {
    return (kg_feed_purchased * feed_cost_kg)
  }
  
  calculate_ME_required <- function(lw, Number, MEreqPerKG, timestep_days){
    return((lw^0.75) * Number * MEreqPerKG * timestep_days)
  }
  
  ## removed prpn here as this is simply calculating dm required as bw multiplied by dm requ per kg bw
  calculate_dry_matter_requirements <- function(lw, dm_req_prpn) {
    return (dm_req_prpn * lw)
  }

 
  ######## ADD FEED COSTS in here to maintain stochasticity.     #######
  ### kg_DM_req is a matrix of 10000 options for the possible amount of DM required per animal/day
  ## all of these values are matrices of 10,000 options to be sampled from later in the code
  
  if (species == "cattle" || species == "smallruminants" || species == "equids" || species == "swine") {
    
    kg_DM_req_JF <- calculate_dry_matter_requirements(lwJF, DM_req_prpn_JF)
    kg_DM_req_JM <- calculate_dry_matter_requirements(lwJM, DM_req_prpn_JM)
    kg_DM_req_SubAF <- calculate_dry_matter_requirements(lwSubAF, DM_req_prpn_SubAF)
    kg_DM_req_SubAM <- calculate_dry_matter_requirements(lwSubAM, DM_req_prpn_SubAM)
    kg_DM_req_AF <- calculate_dry_matter_requirements(lwAF, DM_req_prpn_AF)
    kg_DM_req_AM <- calculate_dry_matter_requirements(lwAM, DM_req_prpn_AM)
  }
  
  if (species == "swine") {
    kg_DM_req_GF <- calculate_dry_matter_requirements(lwGF, DM_req_prpn_GF)
    kg_DM_req_GM <- calculate_dry_matter_requirements(lwGM, DM_req_prpn_GM)
    kg_DM_req_GltF <- calculate_dry_matter_requirements(lwGltF, DM_req_prpn_GltF)
    kg_DM_req_GltM <- calculate_dry_matter_requirements(lwGltM, DM_req_prpn_GltM)
  }
  
  if (species == "poultry") {
    kg_DM_req_JF <- DM_req_prpn_JF
    kg_DM_req_JM <- DM_req_prpn_JM
    kg_DM_req_SubAF <- DM_req_prpn_SubAF
    kg_DM_req_SubAM <- DM_req_prpn_SubAM
    kg_DM_req_AF <- DM_req_prpn_AF
    kg_DM_req_AM <- DM_req_prpn_AM
  }
  
 
  
  if (species == "cattle" || species == "smallruminants" || species == "poultry" || species == "swine") {
    
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
  
  if (species == "swine") {
    KG_Feed_purchased_GF <- calculate_purchased_feed(kg_DM_req_GF, prpn_lskeepers_purch_feed, prpn_feed_paid_for, DM_in_feed)
    KG_Feed_purchased_GM <- calculate_purchased_feed(kg_DM_req_GM, prpn_lskeepers_purch_feed, prpn_feed_paid_for, DM_in_feed)
    KG_Feed_purchased_GltF <- calculate_purchased_feed(kg_DM_req_GltF, prpn_lskeepers_purch_feed, prpn_feed_paid_for, DM_in_feed)
    KG_Feed_purchased_GltM <- calculate_purchased_feed(kg_DM_req_GltM, prpn_lskeepers_purch_feed, prpn_feed_paid_for, DM_in_feed)
    
    Expenditure_on_feed_GF <- calculate_expenditure_on_feed(KG_Feed_purchased_GF, Feed_cost_kg)
    Expenditure_on_feed_GM <- calculate_expenditure_on_feed(KG_Feed_purchased_GM, Feed_cost_kg)
    Expenditure_on_feed_GltF <- calculate_expenditure_on_feed(KG_Feed_purchased_GltF, Feed_cost_kg)
    Expenditure_on_feed_GltM <- calculate_expenditure_on_feed(KG_Feed_purchased_GltM, Feed_cost_kg)
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
    "Num_offtake_AF", 
    "Num_offtake_AM", 
    
    "Offtake_liveweight_kg", 
    "Offtake_liveweight_kg_JF", 
    "Offtake_liveweight_kg_JM", 
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
    "milk_ME_MJ",
    "milk_DM_req",
    "Value_milk",  ## milk ADDED here

    "Cumulative_dry_matter", 
    "Cumulative_dry_matter_JF", 
    "Cumulative_dry_matter_JM", 
    "Cumulative_dry_matter_SubAF", 
    "Cumulative_dry_matter_SubAM", 
    "Cumulative_dry_matter_AF", 
    "Cumulative_dry_matter_AM", 


"Feed_required_KG", 
"Feed_required_KG_JF", 
"Feed_required_KG_JM", 
"Feed_required_KG_SubAF", 
"Feed_required_KG_SubAM", 
"Feed_required_KG_AF", 
"Feed_required_KG_AM", 

"Purchased_Feed_KG", 
"Purchased_Feed_KG_JF", 
"Purchased_Feed_KG_JM", 
"Purchased_Feed_KG_SubAF", 
"Purchased_Feed_KG_SubAM", 
"Purchased_Feed_KG_AF", 
"Purchased_Feed_KG_AM", 

"Foraged_Feed_KG", 
"Foraged_Feed_KG_JF", 
"Foraged_Feed_KG_JM", 
"Foraged_Feed_KG_SubAF", 
"Foraged_Feed_KG_SubAM", 
"Foraged_Feed_KG_AF", 
"Foraged_Feed_KG_AM", 

"Foraged_Feed_Value", 
"Foraged_Feed_Value_JF", 
"Foraged_Feed_Value_JM", 
"Foraged_Feed_Value_SubAF", 
"Foraged_Feed_Value_SubAM", 
"Foraged_Feed_Value_AF", 
"Foraged_Feed_Value_AM", 

"ME_required_MJ",
"ME_required_MJ_JF",
"ME_required_MJ_JM",
"ME_required_MJ_SubAF",
"ME_required_MJ_SubAM",
"ME_required_MJ_AF",
"ME_required_MJ_AM",
    
    "Value_offtake", 
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

                         "Feed_required_KG_Ox", 
                         "Purchased_Feed_KG_Ox", 
                         "Foraged_Feed_KG_Ox", 
                         "Foraged_Feed_Value_Ox",
                         "ME_required_MJ_Ox",
                         
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
  
  if (species == "swine") { # swine extras
    vector_categories <- append(vector_categories, c(
      "Num_GF",
      "Num_GM",
      "Deaths_GF", 
      "Deaths_GM", 
      "Offtake_GF",
      "Offtake_GM",
      "Total_mortality_GF",
      "Total_mortality_GM",
      "Quantity_liveweight_kg_GF",
      "Quantity_liveweight_kg_GM",
      "Num_offtake_GF",
      "Num_offtake_GM",
      "Offtake_liveweight_kg_GF", 
      "Offtake_liveweight_kg_GM", 
      "Pop_growth_GF", 
      "Pop_growth_GM", 
      "Quantity_manure_GF", 
      "Quantity_manure_GM", 
      "Value_manure_GF", ## added
      "Value_manure_GM", ## added
      "Quantity_hides_GF",
      "Quantity_hides_GM",
      "Value_hides_GF",  ## added
      "Value_hides_GM",  ## added
      "Cumulative_dry_matter_GF",
      "Cumulative_dry_matter_GM",
      
      
      "Feed_required_KG_GF", 
      "Feed_required_KG_GM", 
      "Purchased_Feed_KG_GF", 
      "Purchased_Feed_KG_GM", 
      "Foraged_Feed_KG_GF", 
      "Foraged_Feed_KG_GM", 
      "Foraged_Feed_Value_GF",
      "Foraged_Feed_Value_GM",
      "ME_required_MJ_GF",
      "ME_required_MJ_GM",
      
      "Value_offtake_GF", 
      "Value_offtake_GM", 
      "Value_herd_increase_GF", 
      "Value_herd_increase_GM", 
      "Total_value_increase_GF", 
      "Total_value_increase_GM", 
      "Feed_cost_GF",
      "Feed_cost_GM",
      "Labour_cost_GF", 
      "Labour_cost_GM", 
      "Health_cost_GF",
      "Health_cost_GM",
      "Capital_cost_GF",
      "Capital_cost_GM",
      "Infrastructure_cost_GF", 
      "Infrastructure_cost_GM", 
      "Total_expenditure_GF",
      "Total_expenditure_GM",
      
      ## and gilts
      "Num_GltF",
      "Num_GltM",
      "Deaths_GltF", 
      "Deaths_GltM", 
      "Growth_GltF",
      "Growth_GltM",
      "Offtake_GltF",
      "Offtake_GltM",
      "Total_mortality_GltF",
      "Total_mortality_GltM",
      "Quantity_liveweight_kg_GltF",
      "Quantity_liveweight_kg_GltM",
      "Num_offtake_GltF",
      "Num_offtake_GltM",
      "Offtake_liveweight_kg_GltF", 
      "Offtake_liveweight_kg_GltM", 
      "Pop_growth_GltF", 
      "Pop_growth_GltM", 
      "Quantity_manure_GltF", 
      "Quantity_manure_GltM", 
      "Value_manure_GltF", ## added
      "Value_manure_GltM", ## added
      "Quantity_hides_GltF",
      "Quantity_hides_GltM",
      "Value_hides_GltF",  ## added
      "Value_hides_GltM",  ## added
      "Cumulative_dry_matter_GltF",
      "Cumulative_dry_matter_GltM",
      
      "Feed_required_KG_GltF", 
      "Feed_required_KG_GltM", 
      "Purchased_Feed_KG_GltF", 
      "Purchased_Feed_KG_GltM", 
      "Foraged_Feed_KG_GltF", 
      "Foraged_Feed_KG_GltM", 
      "Foraged_Feed_Value_GltF",
      "Foraged_Feed_Value_GltM",
      "ME_required_MJ_GltF",
      "ME_required_MJ_GltM",
      
      "Value_offtake_GltF", 
      "Value_offtake_GltM", 
      "Value_herd_increase_GltF", 
      "Value_herd_increase_GltM", 
      "Total_value_increase_GltF", 
      "Total_value_increase_GltM", 
      "Feed_cost_GltF",
      "Feed_cost_GltM",
      "Labour_cost_GltF", 
      "Labour_cost_GltM", 
      "Health_cost_GltF",
      "Health_cost_GltM",
      "Capital_cost_GltF",
      "Capital_cost_GltM",
      "Infrastructure_cost_GltF", 
      "Infrastructure_cost_GltM", 
      "Total_expenditure_GltF",
      "Total_expenditure_GltM"
      ))
    
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
                                                       
                                                       "donkey_power_income",
                                                       "donkey_power_income_SubAF",
                                                       "donkey_power_income_SubAM",
                                                       "donkey_power_income_AF",
                                                       "donkey_power_income_AM",
                                                       
                                                       "working_hours_commercial",
                                                       "working_hours_commercial_AF",
                                                       "working_hours_commercial_AM",
                                                       
                                                       "working_hours_unpaid",
                                                       "working_hours_unpaid_SubAF",
                                                       "working_hours_unpaid_SubAM",
                                                       "working_hours_unpaid_AF",
                                                       "working_hours_unpaid_AM",
                                                       
                                                       "working_hours_combined",
                                                       "working_hours_combined_SubAF",
                                                       "working_hours_combined_SubAM",
                                                       "working_hours_combined_AF",
                                                       "working_hours_combined_AM",
                                                       
                                                       "working_hours"
                                                       
                                                      ))  
  
    } else {
    # poultry
    vector_categories <- append(vector_categories, c(
                         
                         "Quantity_meat_kg",
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
                         "Num_offtake_JF", 
                         "Num_offtake_JM", 
                         "Num_offtake_SubAF", 
                         "Num_offtake_SubAM", 
                         "Num_offtake_AF", 
                         "Num_offtake_AM", 
                         
                         "Offtake_liveweight_kg", 
                         "Offtake_liveweight_kg_JF", 
                         "Offtake_liveweight_kg_JM",  
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
                         "milk_ME_MJ",
                         "milk_DM_req",
                         
                         "Value_milk",
                         
                         "Cumulative_dry_matter", 
                         "Cumulative_dry_matter_JF", 
                         "Cumulative_dry_matter_JM", 
                         "Cumulative_dry_matter_SubAF", 
                         "Cumulative_dry_matter_SubAM", 
                         "Cumulative_dry_matter_AF", 
                         "Cumulative_dry_matter_AM", 
                         
                         "Cumulative_Methane_Prod_KG",
                         "Cumulative_Methane_Prod_KG_JF",
                         "Cumulative_Methane_Prod_KG_JM",
                         "Cumulative_Methane_Prod_KG_SubAF",
                         "Cumulative_Methane_Prod_KG_SubAM",
                         "Cumulative_Methane_Prod_KG_AF",
                         "Cumulative_Methane_Prod_KG_AM",
                         
                         "Feed_required_KG", 
                         "Feed_required_KG_JF", 
                         "Feed_required_KG_JM", 
                         "Feed_required_KG_SubAF", 
                         "Feed_required_KG_SubAM", 
                         "Feed_required_KG_AF", 
                         "Feed_required_KG_AM", 
                         
                         "Purchased_Feed_KG", 
                         "Purchased_Feed_KG_JF", 
                         "Purchased_Feed_KG_JM", 
                         "Purchased_Feed_KG_SubAF", 
                         "Purchased_Feed_KG_SubAM", 
                         "Purchased_Feed_KG_AF", 
                         "Purchased_Feed_KG_AM", 
                         
                         "Foraged_Feed_KG", 
                         "Foraged_Feed_KG_JF", 
                         "Foraged_Feed_KG_JM", 
                         "Foraged_Feed_KG_SubAF", 
                         "Foraged_Feed_KG_SubAM", 
                         "Foraged_Feed_KG_AF", 
                         "Foraged_Feed_KG_AM", 
                         
                         "Foraged_Feed_Value", 
                         "Foraged_Feed_Value_JF", 
                         "Foraged_Feed_Value_JM", 
                         "Foraged_Feed_Value_SubAF", 
                         "Foraged_Feed_Value_SubAM", 
                         "Foraged_Feed_Value_AF", 
                         "Foraged_Feed_Value_AM", 
                         
                         "ME_required_MJ",
                         "ME_required_MJ_JF",
                         "ME_required_MJ_JM",
                         "ME_required_MJ_SubAF",
                         "ME_required_MJ_SubAM",
                         "ME_required_MJ_AF",
                         "ME_required_MJ_AM",
                         
                         "Value_offtake", 
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
               "Gross_margin_AM"
               
  )
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
                                                     "Cumulative_Methane_Prod_KG_Ox",
                                                     
                                                     "Feed_required_KG_Ox", 
                                                     "Purchased_Feed_KG_Ox", 
                                                     "Foraged_Feed_KG_Ox", 
                                                     "Foraged_Feed_Value_Ox", 
                                                     "ME_required_MJ_Ox",
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
  }  
  
  if (species == "swine") { # swine extras
    matrix_categories <- append(matrix_categories, c("Num_GF",
      "Num_GM",
      "Deaths_GF", 
      "Deaths_GM", 
      "Total_mortality_GF",
      "Total_mortality_GM",
      "Quantity_liveweight_kg_GF",
      "Quantity_liveweight_kg_GM",
      "Num_offtake_GF",
      "Num_offtake_GM",
      "Offtake_liveweight_kg_GF", 
      "Offtake_liveweight_kg_GM", 
      "Pop_growth_GF", 
      "Pop_growth_GM", 
      "Quantity_manure_GF", 
      "Quantity_manure_GM", 
      "Value_manure_GF", ## added
      "Value_manure_GM", ## added
      "Quantity_hides_GF",
      "Quantity_hides_GM",
      "Value_hides_GF",  ## added
      "Value_hides_GM",  ## added
      "Cumulative_dry_matter_GF",
      "Cumulative_dry_matter_GM",
      
      "Cumulative_Methane_Prod_KG_GF",
      "Cumulative_Methane_Prod_KG_GM",
      "Cumulative_Methane_Prod_KG_GltF",
      "Cumulative_Methane_Prod_KG_GltM",
      
      "Feed_required_KG_GF", 
      "Feed_required_KG_GM", 
      "Purchased_Feed_KG_GF", 
      "Purchased_Feed_KG_GM", 
      "Foraged_Feed_KG_GF", 
      "Foraged_Feed_KG_GM", 
      "Foraged_Feed_Value_GF",
      "Foraged_Feed_Value_GM",
      "ME_required_MJ_GF",
      "ME_required_MJ_GM",
      
      "Value_offtake_GF", 
      "Value_offtake_GM", 
      "Value_herd_increase_GF", 
      "Value_herd_increase_GM", 
      "Total_value_increase_GF", 
      "Total_value_increase_GM", 
      "Feed_cost_GF",
      "Feed_cost_GM",
      "Labour_cost_GF", 
      "Labour_cost_GM", 
      "Health_cost_GF",
      "Health_cost_GM",
      "Capital_cost_GF",
      "Capital_cost_GM",
      "Infrastructure_cost_GF", 
      "Infrastructure_cost_GM", 
      "Total_expenditure_GF",
      "Total_expenditure_GM",
      
      ## and gilts
      "Num_GltF",
      "Num_GltM",
      "Deaths_GltF", 
      "Deaths_GltM", 
     
      "Offtake_GltF",
      "Offtake_GltM",
      "Total_mortality_GltF",
      "Total_mortality_GltM",
      "Quantity_liveweight_kg_GltF",
      "Quantity_liveweight_kg_GltM",
      "Num_offtake_GltF",
      "Num_offtake_GltM",
      "Offtake_liveweight_kg_GltF", 
      "Offtake_liveweight_kg_GltM", 
      "Pop_growth_GltF", 
      "Pop_growth_GltM", 
      "Quantity_manure_GltF", 
      "Quantity_manure_GltM", 
      "Value_manure_GltF", ## added
      "Value_manure_GltM", ## added
      "Quantity_hides_GltF",
      "Quantity_hides_GltM",
      "Value_hides_GltF",  ## added
      "Value_hides_GltM",  ## added
      "Cumulative_dry_matter_GltF",
      "Cumulative_dry_matter_GltM",
     
       "Feed_required_KG_GltF", 
      "Feed_required_KG_GltM", 
      "Purchased_Feed_KG_GltF", 
      "Purchased_Feed_KG_GltM", 
      "Foraged_Feed_KG_GltF", 
      "Foraged_Feed_KG_GltM", 
      "Foraged_Feed_Value_GltF",
      "Foraged_Feed_Value_GltM",
      "ME_required_MJ_GltF",
      "ME_required_MJ_GltM",
     
       "Value_offtake_GltF", 
      "Value_offtake_GltM", 
      "Value_herd_increase_GltF", 
      "Value_herd_increase_GltM", 
      "Total_value_increase_GltF", 
      "Total_value_increase_GltM", 
      "Feed_cost_GltF",
      "Feed_cost_GltM",
      "Labour_cost_GltF", 
      "Labour_cost_GltM", 
      "Health_cost_GltF",
      "Health_cost_GltM",
      "Capital_cost_GltF",
      "Capital_cost_GltM",
      "Infrastructure_cost_GltF", 
      "Infrastructure_cost_GltM", 
      "Total_expenditure_GltF",
      "Total_expenditure_GltM"
      ))
    
  } 
  
  
  if (species == "equids") {
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
                                
                                "donkey_power_income",
                                "donkey_power_income_SubAF",
                                "donkey_power_income_SubAM",
                                "donkey_power_income_AF",
                                "donkey_power_income_AM",
                                
                                "working_hours_commercial",
                                "working_hours_commercial_AF",
                                "working_hours_commercial_AM",
                                
                                "working_hours_unpaid",
                                "working_hours_unpaid_SubAF",
                                "working_hours_unpaid_SubAM",
                                "working_hours_unpaid_AF",
                                "working_hours_unpaid_AM",
                                
                                "working_hours_combined",
                                "working_hours_combined_SubAF",
                                "working_hours_combined_SubAM",
                                "working_hours_combined_AF",
                                "working_hours_combined_AM",
                                
                                "working_hours",
                                
                                "Enterprise_budget")
    )
  
    } else {
    # poultry
    matrix_categories <- append(matrix_categories, c("Quantity_eggs_consumed",
                                                     "Quantity_eggs_sold",
                                                     
                                                     "Value_eggs_consumed",
                                                     "Value_eggs_sold"
                              ))
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
   
     if (species == "swine") {
      age_sex_groups <- append(age_sex_groups, c("GF", "GM", "GltF", "GltM"))
    }
    
    for (group in age_sex_groups) {
      var_name <- group  
      value <- sample(get(paste0("N_", group, "_t0")),1) ### GEMMA EDITED HERE to sample start population size from a distribution 
      assign(var_name, value)  
      assign(paste0("N_", group), value) ### GEMMA EDITED HERE to ensure N_group_t0 is the same as the group number
    }                                    
    
    # Total population is sum of age*sex segments
    Nt0 <- sum(N_JF, N_JM, N_SubAF, N_SubAM, N_AF, N_AM) ## removed _t0
    
    if (species == "cattle") {
      Nt0 <- Nt0 + N_Ox
    }
    if (species == "swine") {
      Nt0 <- Nt0 + N_GF + N_GM + N_GltF + N_GltM
    }
    
    N <- Nt0
    
    ## list of defined values used in model
    Culls <- 0
    Num_dead <- 0
    Liveweight_kg <- 0
    Offtake <- 0
    Offtake_liveweight <- 0
    Manure_kg <- 0
    Hides <- 0
    milk <- 0
    monthly_milk <- 0
    Meat_kg <- 0
    Draught_income <- 0
    Cumulative_DM <- 0
    Total_ME_MJ <- 0
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
    
    commercial_hours <- 0
    unpaid_hours <- 0
    combined_hours <- 0
    
    production_vars <- c("Num_dead", 
                         "Liveweight_kg",
                         "Offtake",
                         "Offtake_liveweight",
                         "Manure_kg",
                         "Hides",
                         "Cumulative_DM",
                         "Total_ME_MJ",
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
                         "combined_income",
                         
                         "commercial_hours",
                         "unpaid_hours",
                         "combined_hours"
                         
                         )
    
    for (group in age_sex_groups) {
      for (prod_var in production_vars) {
        assign(paste(prod_var, group, sep = "_"), 0)
      }
    }
    
    
    for (month in 1:Num_timesteps) {
      
      if (species == "cattle" || species == "smallruminants" || species == "equids" || species == "swine") {
        Mu <- sample(part, 1) * sample(prolif, 1) / Num_timesteps
        
        res_vec$Births[month] <- Mu * AF
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
    
        if (species == "cattle" || species == "equids") {
        
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
      if (species == "smallruminants" || species == "poultry" || species == "swine") {

        res_vec$Deaths_JF[month] <- (1-(1-sample(Alpha_JF, 1))^(1/Beta_JF)) * JF
        res_vec$Deaths_SubAF[month] <- (1-(1-sample(Alpha_SubAF, 1))^(1/Beta_SubAF)) * SubAF 
        res_vec$Deaths_AF[month] <- (1-(1-sample(Alpha_AF, 1))^(1/Num_timesteps))  * AF
        
        res_vec$Deaths_JM[month] <- (1-(1-sample(Alpha_JM, 1))^(1/Beta_JM)) * JM
        res_vec$Deaths_SubAM[month] <- (1-(1-sample(Alpha_SubAM, 1))^(1/Beta_SubAM)) * SubAM
        res_vec$Deaths_AM[month] <- (1-(1-sample(Alpha_AM, 1))^(1/Num_timesteps)) * AM
      } 
      
       ## add extra mortalities for growers, dependent on duration of stay in the age sex groups
      if (species == "swine") {    
        res_vec$Deaths_GF[month] <- (1-(1-sample(Alpha_GF, 1))^(1/Beta_GF)) * GF
        res_vec$Deaths_GM[month] <- (1-(1-sample(Alpha_GM, 1))^(1/Beta_GM)) * GM
        res_vec$Deaths_GltF[month] <- (1-(1-sample(Alpha_GltF, 1))^(1/Beta_GltF)) * GltF
        res_vec$Deaths_GltM[month] <- (1-(1-sample(Alpha_GltM, 1))^(1/Beta_GltM)) * GltM
      }
      
      if (species == "cattle"){
        res_vec$Deaths_Ox[month] <- (1-(1-sample(Alpha_Ox, 1))^(1/Num_timesteps)) * Ox
      }
      
      ## Write new population size minus the deaths for all!!
      
      if (species == "cattle" || species == "equids" || species == "poultry" || species == "smallruminants" || species == "swine") {
        
        res_vec$Num_JF[month] <- JF - res_vec$Deaths_JF[month]
        res_vec$Num_JM[month] <- JM - res_vec$Deaths_JM[month]
        res_vec$Num_SubAF[month] <- SubAF - res_vec$Deaths_SubAF[month]
        res_vec$Num_SubAM[month] <- SubAM - res_vec$Deaths_SubAM[month]
        res_vec$Num_AF[month] <- AF - res_vec$Deaths_AF[month]
        res_vec$Num_AM[month] <- AM - res_vec$Deaths_AM[month]
        
        JF <- res_vec$Num_JF[month]
        SubAF <- res_vec$Num_SubAF[month]
        AF <- res_vec$Num_AF[month]
        JM <- res_vec$Num_JM[month]
        SubAM <- res_vec$Num_SubAM[month]
        AM <- res_vec$Num_AM[month] 
        
      }
      
      if (species == "cattle") {
        res_vec$Num_Ox[month] <- Ox - res_vec$Deaths_Ox[month]
        Ox <- res_vec$Num_Ox[month] 
      }
      
      if (species == "swine"){
        res_vec$Num_GF[month] <- GF - res_vec$Deaths_GF[month]
        res_vec$Num_GM[month] <- GM - res_vec$Deaths_GM[month]
        res_vec$Num_GltF[month] <- GltF - res_vec$Deaths_GltF[month]
        res_vec$Num_GltM[month] <- GltM - res_vec$Deaths_GltM[month]
      
        GF <- res_vec$Num_GF[month]
        GM <- res_vec$Num_GM[month]
        GltF <- res_vec$Num_GltF[month]
        GltM <- res_vec$Num_GltM[month]
      }
        
      
      ## UPDATE GAMMA OFFTAKE IS NOW LINEAR RATHEHER THAN EXPONENTIAL BECAUSE 
      ## EXPONENTIAL DOES NOT WORK WITH HIGH OFFTAKE RATES
      ## These Offtake equations are changed so that small ruminants is the same as others
      ## NOTE UPDATE Small Ruminants inputs spreadsheet for Gamma_JF and Gamma_JM and Gamma_SubAF and Gamma_SubAM
      ## Offtake rate equations changed to be the risk of offtake during the time spent in that age group
      ## to the power of 1/12 of if Beta_ (duration of stay in this time step)  <num_timesteps (a year), 
      ## use Num_timesteps
      if (species == "cattle" ) {
        res_vec$Offtake_SubAF[month] <- (Gamma_SubAF)*(1/Num_timesteps) * SubAF ## assuming subadults for >= 1 year
        res_vec$Offtake_SubAM[month] <- (Gamma_SubAM)*(1/Num_timesteps) * SubAM ## assuming subadults for >= 1 year
        res_vec$Offtake_AF[month] <- (Gamma_AF)*(1/Num_timesteps) * AF
        res_vec$Offtake_AM[month] <- (Gamma_AM)*(1/Num_timesteps) * AM
        res_vec$Offtake_Ox[month] <- (Gamma_Ox)*(1/Num_timesteps) * Ox
      } 
      ## seperated for small ruminants and poultry ## Different to cattle because they are in the second age sex group < 1 year
      if (species == "poultry" || species == "smallruminants" || species == "swine") {
        res_vec$Offtake_SubAF[month] <- (Gamma_SubAF)*(1/Beta_SubAM) * SubAF ## offtake for subadults assuming <12 months
        res_vec$Offtake_SubAM[month] <- (Gamma_SubAM)*(1/Beta_SubAM) * SubAM ## offtake for subadults assuming <12 months
        res_vec$Offtake_AF[month] <- (Gamma_AF)*(1/Num_timesteps) * AF
        res_vec$Offtake_AM[month] <- (Gamma_AM)*(1/Num_timesteps) * AM
        
      } 
      
      if (species == "swine"){
        res_vec$Offtake_GF[month] <- (Gamma_GF)*(1/Beta_GF) * GF ## offtake for growers assuming <12 months
        res_vec$Offtake_GM[month] <- (Gamma_GM)*(1/Beta_GM) * GM ## offtake for growers assuming <12 months
        res_vec$Offtake_GltF[month] <- (Gamma_GltF)*(1/Beta_GltF) * GltF ## offtake for growers assuming <12 months
        res_vec$Offtake_GltM[month] <- (Gamma_GltM)*(1/Beta_GltM) * GltM ## offtake for growers assuming <12 months
        
      }
      
      if (species == "equids") { 
        res_vec$Offtake_SubAF[month] <- (Gamma_SubAF)*(1/Num_timesteps) * SubAF ## offtake for subadults assuming <12 months
        res_vec$Offtake_SubAM[month] <- (Gamma_SubAM)*(1/Num_timesteps) * SubAM ## offtake for subadults assuming <12 months
        res_vec$Offtake_AF[month] <- (Gamma_AF)*(1/Num_timesteps) * AF
        res_vec$Offtake_AM[month] <- (Gamma_AM)*(1/Num_timesteps) * AM
        
      } 
      
      ## Then recalculate population size for the offtakes...
      
      if (species == "cattle" || species == "equids" || species == "poultry" || species == "smallruminants" || species == "swine") {

        res_vec$Num_SubAF[month] <- SubAF - res_vec$Offtake_SubAF[month]
        res_vec$Num_SubAM[month] <- SubAM - res_vec$Offtake_SubAM[month]
        res_vec$Num_AF[month] <- AF - res_vec$Offtake_AF[month]
        res_vec$Num_AM[month] <- AM - res_vec$Offtake_AM[month]
        
        SubAF <- res_vec$Num_SubAF[month]
        AF <- res_vec$Num_AF[month]
        SubAM <- res_vec$Num_SubAM[month]
        AM <- res_vec$Num_AM[month] 
      }
      
      if (species == "cattle") {
        res_vec$Num_Ox[month] <- Ox - res_vec$Offtake_Ox[month]
        
        Ox <- res_vec$Num_Ox[month] 
      }
      
      if (species == "swine"){
        res_vec$Num_GF[month] <- GF - res_vec$Offtake_GF[month]
        res_vec$Num_GM[month] <- GM - res_vec$Offtake_GM[month]
        res_vec$Num_GltF[month] <- GltF - res_vec$Offtake_GltF[month]
        res_vec$Num_GltM[month] <- GltM - res_vec$Offtake_GltM[month]
     
        GF <- res_vec$Num_GF[month]
        GM <- res_vec$Num_GM[month]
        GltF <- res_vec$Num_GltF[month]
        GltM <- res_vec$Num_GltM[month]
      }
      
    ##  Now remove culls
      if (species == "cattle" || species == "smallruminants" || species == "poultry" || species == "equids" || species == "swine") {
        
        res_vec$Culls_AF[month] <- 1/(CullF) * AF
        res_vec$Culls_AM[month] <- 1/(CullM) * AM
        
        res_vec$Num_AF[month] <- AF - res_vec$Culls_AF[month]
        res_vec$Num_AM[month] <- AM - res_vec$Culls_AM[month]
        
        AF <- res_vec$Num_AF[month]
        AM <- res_vec$Num_AM[month]
      }
      
      if (species == "cattle"){
      res_vec$Culls_Ox[month] <- 1/(CullOx) * Ox
        res_vec$Num_Ox[month] <- Ox - res_vec$Culls_Ox[month]
        Ox <- res_vec$Num_Ox[month]
              }

      ## Now growth can occur for the remaining animals in each age/sex group 
       ## HERE Gemma has moved SR into same calculations as cattle and poultry, and need to
      ## change SR intake spreadsheet so they also have two different parameters to enter
      ## for duration of stay in first (juvenile) and second (subadult) age groups
      ## Beta values are duration of stay (number of timesteps) in the age sex group
      ## so the calculation becomes, for each time step, the risk of moving up to the 
      ## next age sex group is 1/duration of stay in compartment 
      ## (stochastic option but point values are used in current spreadsheet)
      
      if (species == "cattle" || species == "poultry" || species == "smallruminants" || species == "equids" || species == "swine") {
        res_vec$Growth_JF[month] <- (1/(Beta_JF)) * JF
        res_vec$Growth_JM[month] <- (1/(Beta_JM)) * JM
        res_vec$Growth_SubAF[month] <- (1/(Beta_SubAF)) * SubAF
        res_vec$Growth_SubAM[month] <- (1/(Beta_SubAM)) * SubAM
      } 

      if (species == "swine"){
        res_vec$Growth_GltF[month] <- (1/(Beta_GltF)) * GltF
        res_vec$Growth_GltM[month] <- (1/(Beta_GltM)) * GltM
      }
      ## HERE Gemma has adapted cull rate to defensively program so input for CullF and CullM
      ## is now the duration of stay (number of time steps spent) in the adult compartment before
      ## before culling so each time step the risk of being culled is 1/duration of stay
      ## similar calculation to above for maturation between compartments ## GEMMA HERE 08/05/24
    
      
      if (species == "swine") {
        
        res_vec$Num_JF[month] <- JF + (res_vec$Births[month] * 0.5) - res_vec$Growth_JF[month] 
        res_vec$Num_SubAF[month] <- SubAF + res_vec$Growth_JF[month] - res_vec$Growth_SubAF[month] 
        res_vec$Num_GF[month] <- GF + (res_vec$Growth_SubAF[month] * (1-Prop_kept_breeding_F))  
        res_vec$Num_GltF[month] <- GltF + (res_vec$Growth_SubAF[month] * Prop_kept_breeding_F) - res_vec$Growth_GltF[month] 
        res_vec$Num_AF[month] <- AF + res_vec$Growth_GltF[month] 
        
        res_vec$Num_JM[month] <- JM + (res_vec$Births[month] * 0.5) - res_vec$Growth_JM[month] 
        res_vec$Num_SubAM[month] <- SubAM + res_vec$Growth_JM[month] - res_vec$Growth_SubAM[month] 
        res_vec$Num_GM[month] <- GM + (res_vec$Growth_SubAM[month] * (1-Prop_kept_breeding_M)) 
        res_vec$Num_GltM[month] <- GltM + (res_vec$Growth_SubAM[month] * Prop_kept_breeding_M) - res_vec$Growth_GltM[month] 
        res_vec$Num_AM[month] <- AM + res_vec$Growth_GltM[month] 
        
        res_vec$Num_N[month] <- sum(res_vec$Num_JF[month],
                                    res_vec$Num_SubAF[month],
                                    res_vec$Num_GF[month],
                                    res_vec$Num_GltF[month],
                                    res_vec$Num_AF[month],
                                    res_vec$Num_JM[month],
                                    res_vec$Num_SubAM[month],
                                    res_vec$Num_GM[month],
                                    res_vec$Num_GltM[month],
                                    res_vec$Num_AM[month])
        
        
        JF <- res_vec$Num_JF[month]
        SubAF <- res_vec$Num_SubAF[month]
        GF <- res_vec$Num_GF[month]
        GltF <- res_vec$Num_GltF[month]
        AF <- res_vec$Num_AF[month]
        
        JM <- res_vec$Num_JM[month]
        SubAM <- res_vec$Num_SubAM[month]
        GM <- res_vec$Num_GM[month]
        GltM <- res_vec$Num_GltM[month]
        AM <- res_vec$Num_AM[month]
        
        N <- res_vec$Num_N[month]
        
      }
      
      if (species == "smallruminants" || species == "poultry" || species == "equids") {
        
        res_vec$Num_JF[month] <- JF + (res_vec$Births[month] * 0.5) - res_vec$Growth_JF[month] 
        res_vec$Num_SubAF[month] <- SubAF + res_vec$Growth_JF[month] - res_vec$Growth_SubAF[month] 
        res_vec$Num_AF[month] <- AF + res_vec$Growth_SubAF[month] 
        
        res_vec$Num_JM[month] <- JM + (res_vec$Births[month] * 0.5) - res_vec$Growth_JM[month] 
        res_vec$Num_SubAM[month] <- SubAM + res_vec$Growth_JM[month] - res_vec$Growth_SubAM[month] 
        res_vec$Num_AM[month] <- AM + res_vec$Growth_SubAM[month] 
        
        res_vec$Num_N[month] <- sum(res_vec$Num_JF[month],
                                    res_vec$Num_SubAF[month],
                                    res_vec$Num_AF[month],
                                    res_vec$Num_JM[month],
                                    res_vec$Num_SubAM[month],
                                    res_vec$Num_AM[month])
        
        JF <- res_vec$Num_JF[month]
        SubAF <- res_vec$Num_SubAF[month]
        AF <- res_vec$Num_AF[month]
        JM <- res_vec$Num_JM[month]
        SubAM <- res_vec$Num_SubAM[month]
        AM <- res_vec$Num_AM[month]
        
        N <- res_vec$Num_N[month]
        
      }
      
      
      if (species == "cattle") {
        
        
        # the proportion of growing sub-adult males that move to be oxen is dependant on castration proportion, 
        # castration proportion is calculated from proportion of oxen out of all adult males in starting population
        res_vec$Oxen_A[month] <- sample(castration_proportion, 1) * res_vec$Growth_SubAM[month]
        
        res_vec$Num_JF[month] <- JF + (res_vec$Births[month] * 0.5) - res_vec$Growth_JF[month] 
        res_vec$Num_SubAF[month] <- SubAF + res_vec$Growth_JF[month] - res_vec$Growth_SubAF[month] 
        res_vec$Num_AF[month] <- AF + res_vec$Growth_SubAF[month] 
        
        res_vec$Num_JM[month] <- JM + (res_vec$Births[month] * 0.5) - res_vec$Growth_JM[month] 
        res_vec$Num_SubAM[month] <- SubAM + res_vec$Growth_JM[month] - res_vec$Growth_SubAM[month] 
        res_vec$Num_AM[month] <- AM + (res_vec$Growth_SubAM[month]-res_vec$Oxen_A[month]) 
        
        res_vec$Num_Ox[month] <- Ox + res_vec$Oxen_A[month]  
        
        res_vec$Num_N[month] <- sum(res_vec$Num_JF[month],
                                    res_vec$Num_SubAF[month],
                                    res_vec$Num_AF[month],
                                    res_vec$Num_JM[month],
                                    res_vec$Num_SubAM[month],
                                    res_vec$Num_AM[month],
                                    res_vec$Num_Ox[month])
        
        JF <- res_vec$Num_JF[month]
        SubAF <- res_vec$Num_SubAF[month]
        AF <- res_vec$Num_AF[month]
        JM <- res_vec$Num_JM[month]
        SubAM <- res_vec$Num_SubAM[month]
        AM <- res_vec$Num_AM[month]
        Ox <- res_vec$Num_Ox[month]
        
        N <- res_vec$Num_N[month]
      }
      
      ## add swine population dynamics
      
      if (species == "cattle" || species == "smallruminants" || species == "poultry" || species == "equids" || species =="swine") {
        
      
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
      
      if (species == "swine"){

        res_vec$Total_mortality_GF[month] <- Num_dead_GF + res_vec$Deaths_GF[month]
        Num_dead_GF <- res_vec$Total_mortality_GF[month]
        
        res_vec$Total_mortality_GM[month] <- Num_dead_GM + res_vec$Deaths_GM[month]
        Num_dead_GM <- res_vec$Total_mortality_GM[month]
       
        res_vec$Total_mortality_GltF[month] <- Num_dead_GltF + res_vec$Deaths_GltF[month]
        Num_dead_GltF <- res_vec$Total_mortality_GltF[month]
       
        res_vec$Total_mortality_GltM[month] <- Num_dead_GltM + res_vec$Deaths_GltM[month]
        Num_dead_GltM <- res_vec$Total_mortality_GltM[month]
        
        res_vec$Total_mortality[month] <- res_vec$Total_mortality[month] + 
                                                res_vec$Total_mortality_GF[month] +
          res_vec$Total_mortality_GM[month] +
          res_vec$Total_mortality_GltF[month] +
          res_vec$Total_mortality_GltM[month] 
          
      }
      
      
      
      if (species == "cattle" || species == "smallruminants" || species == "poultry" || species == "equids" || species == "swine") {
        res_vec$Pop_growth[month] <- N - Nt0
        res_vec$Pop_growth_JF[month] <- JF - N_JF
        res_vec$Pop_growth_JM[month] <- JM - N_JM
        res_vec$Pop_growth_SubAF[month] <- SubAF - N_SubAF
        res_vec$Pop_growth_SubAM[month] <- SubAM - N_SubAM
        res_vec$Pop_growth_AF[month] <- AF - N_AF
        res_vec$Pop_growth_AM[month] <- AM - N_AM
      }
      
      if (species == "cattle") {
        res_vec$Pop_growth_Ox[month] <- Ox - N_Ox
      }
      
      if (species == "swine"){
        res_vec$Pop_growth_GF[month] <- GF - N_GF
        res_vec$Pop_growth_GM[month] <- GM - N_GM
        res_vec$Pop_growth_GltF[month] <- GltF - N_GltF
        res_vec$Pop_growth_GltM[month] <- GltM - N_GltM
      }
      
      if (species == "cattle" || species == "smallruminants" || species == "poultry" || species == "equids" || species == "swine") {
        
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
        res_vec$Num_offtake_SubAF[month] <- Offtake_SubAF + res_vec$Offtake_SubAF[month]
        res_vec$Num_offtake_SubAM[month] <- Offtake_SubAM + res_vec$Offtake_SubAM[month]
        res_vec$Num_offtake_AF[month] <- Offtake_AF + res_vec$Offtake_AF[month]
        res_vec$Num_offtake_AM[month] <- Offtake_AM + res_vec$Offtake_AM[month] 
        # here Girma removed res_vec$Culls_AM[month] for equids but for cattles res_vec$Culls_AM[month]was add because Gemma/wudu assume cull males have some value similar to offtake  
        
      }
      
      if (species == "swine"){
        liveweight_GF <- sample(lwGF, 1)
        liveweight_GM <- sample(lwGM, 1)
        liveweight_GltF <- sample(lwGltF, 1)
        liveweight_GltM <- sample(lwGltM, 1)
        
        res_vec$Quantity_liveweight_kg_GF[month] <- GF * liveweight_GF
        res_vec$Quantity_liveweight_kg_GM[month] <- GM * liveweight_GM
        res_vec$Quantity_liveweight_kg_GltF[month] <- GltF * liveweight_GltF
        res_vec$Quantity_liveweight_kg_GltM[month] <- GltM * liveweight_GltM
        
        res_vec$Quantity_liveweight_kg[month] <- res_vec$Quantity_liveweight_kg[month] + 
          res_vec$Quantity_liveweight_kg_GF[month] +
          res_vec$Quantity_liveweight_kg_GM[month] +
          res_vec$Quantity_liveweight_kg_GltF[month] +
          res_vec$Quantity_liveweight_kg_GltM[month] 
        
        res_vec$Num_offtake_GF[month] <- Offtake_GF + res_vec$Offtake_GF[month]
        res_vec$Num_offtake_GM[month] <- Offtake_GM + res_vec$Offtake_GM[month] 
        res_vec$Num_offtake_GltF[month] <- Offtake_GltF + res_vec$Offtake_GltF[month]
        res_vec$Num_offtake_GltM[month] <- Offtake_GltM + res_vec$Offtake_GltM[month] 
      }
      
      if (species == "cattle") {
        liveweight_Ox <- sample(lwOx, 1)
        res_vec$Quantity_liveweight_kg_Ox[month] <- Ox * liveweight_Ox
        res_vec$Quantity_liveweight_kg[month] <- res_vec$Quantity_liveweight_kg[month] + res_vec$Quantity_liveweight_kg_Ox[month]
       
         ## for cattle the number of culled adult males are added to the number 
        ## offtake because they hold some value (for Ethiopia this made most sense)
        res_vec$Num_offtake_AM[month] <- res_vec$Num_offtake_AM[month] + res_vec$Culls_AM[month]
        res_vec$Num_offtake_Ox[month] <- Offtake_Ox + res_vec$Offtake_Ox[month] + res_vec$Culls_Ox[month]
      }
    
      
      
      
      
      if (species == "cattle" || species == "smallruminants" || species == "poultry" || species == "equids" || species == "swine") {
      
      Offtake_SubAF <- res_vec$Num_offtake_SubAF[month]
      Offtake_SubAM <- res_vec$Num_offtake_SubAM[month]
      Offtake_AF <- res_vec$Num_offtake_AF[month]
      Offtake_AM <- res_vec$Num_offtake_AM[month]
      
      res_vec$Num_offtake[month] <- sum(res_vec$Num_offtake_SubAF[month],
                                    res_vec$Num_offtake_SubAM[month],
                                    res_vec$Num_offtake_AF[month],
                                    res_vec$Num_offtake_AM[month])
      }
      
      if (species == "swine") {
        Offtake_GF <- res_vec$Num_offtake_GF[month]
        Offtake_GM <- res_vec$Num_offtake_GM[month]
        Offtake_GltF <- res_vec$Num_offtake_GltF[month]
        Offtake_GltM <- res_vec$Num_offtake_GltM[month]
        
        res_vec$Num_offtake[month] <- res_vec$Num_offtake[month] + 
          res_vec$Num_offtake_GF[month] +
          res_vec$Num_offtake_GM[month] +
          res_vec$Num_offtake_GltF[month] +
          res_vec$Num_offtake_GltM[month] 
      }
      
      if (species == "cattle") {
        Offtake_Ox <- res_vec$Num_offtake_Ox[month]
        res_vec$Num_offtake[month] <- res_vec$Num_offtake[month] + res_vec$Num_offtake_Ox[month]
      }
      
      if (species == "cattle" || species == "smallruminants" || species == "poultry" || species == "equids" || species == "swine") {
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
      
      if(species == "swine") { ## Here swine finish weight is used as the liveweight of offtake from the growers
        res_vec$Offtake_liveweight_kg_GF[month] <- sample(fw_GF, 1) * Offtake_GF
        res_vec$Offtake_liveweight_kg_GM[month] <- sample(fw_GM, 1) * Offtake_GM
        res_vec$Offtake_liveweight_kg_GltF[month] <- liveweight_GltF * Offtake_GltF
        res_vec$Offtake_liveweight_kg_GltM[month] <- liveweight_GltM * Offtake_GltM
        
        res_vec$Offtake_liveweight_kg[month] <- res_vec$Offtake_liveweight_kg[month] +
          res_vec$Offtake_liveweight_kg_GF[month] +
          res_vec$Offtake_liveweight_kg_GM[month] +
          res_vec$Offtake_liveweight_kg_GltF[month] +
          res_vec$Offtake_liveweight_kg_GltM[month]
        
        res_vec$Quantity_meat_kg[month] <- res_vec$Offtake_liveweight_kg[month] * sample(ccy, 1)
        
      }
 
      if (species == "cattle") { 
        res_vec$Offtake_liveweight_kg_Ox[month] <- liveweight_Ox * Offtake_Ox
        res_vec$Offtake_liveweight_kg[month] <-  res_vec$Offtake_liveweight_kg[month] + res_vec$Offtake_liveweight_kg_Ox[month]
        res_vec$Quantity_meat_kg[month] <- res_vec$Offtake_liveweight_kg[month] * sample(ccy, 1)
      }
      
      if (species == "cattle" || species == "smallruminants" || species == "poultry" || species == "swine") {
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
        res_vec$donkey_power_income_SubAF[month] <- sum(res_vec$Cumulative_unpaid_income_SubAF[month],
                                                  res_vec$Cumulative_combined_income_SubAF[month]) 
        
        res_vec$donkey_power_income_SubAM[month] <- sum(res_vec$Cumulative_unpaid_income_SubAM[month],
                                                  res_vec$Cumulative_combined_income_SubAM[month]) 
        
        res_vec$donkey_power_income_AF[month] <- sum(res_vec$Cumulative_commercial_income_AF[month],
                                                  res_vec$Cumulative_unpaid_income_AF[month],
                                                  res_vec$Cumulative_combined_income_AF[month]) 
        
        res_vec$donkey_power_income_AM[month] <- sum(res_vec$Cumulative_commercial_income_AM[month],
                                                  res_vec$Cumulative_unpaid_income_AM[month],
                                                  res_vec$Cumulative_combined_income_AM[month]) 
        
        res_vec$donkey_power_income[month] <- sum(res_vec$Cumulative_commercial_income[month],
                                                  res_vec$Cumulative_unpaid_income[month],
                                                  res_vec$Cumulative_combined_income[month]) 
      
        }
 
      ## Working hours
      if (species == "equids") {
        ## commercial
        res_vec$working_hours_commercial_AF[month] <- commercial_hours_AF + (res_vec$Num_AF[month] * prop_commercial_AF) * days_worked_month_commercial_AF * (sample(WH_commercial_AF_ideal,1) / 60)
        res_vec$working_hours_commercial_AM[month] <- commercial_hours_AM + (res_vec$Num_AM[month] * prop_commercial_AM) * days_worked_month_commercial_AM * (sample(WH_commercial_AM_ideal,1) / 60)
        
        res_vec$working_hours_commercial[month] <- res_vec$working_hours_commercial_AF[month] + res_vec$working_hours_commercial_AM[month]
      
        commercial_hours_AF <- res_vec$working_hours_commercial_AF[month]
        commercial_hours_AM <- res_vec$working_hours_commercial_AM[month]
        
        ## unpaid
        res_vec$working_hours_unpaid_SubAF[month] <- unpaid_hours_SubAF + (res_vec$Num_SubAF[month] * prop_unpaid_SubAF) * days_worked_month_unpaid_SubAF * (sample(WH_unpaid_SubAF_ideal,1) / 60)
        res_vec$working_hours_unpaid_SubAM[month] <- unpaid_hours_SubAM + (res_vec$Num_SubAM[month] * prop_unpaid_SubAM) * days_worked_month_unpaid_SubAM * (sample(WH_unpaid_SubAM_ideal,1) / 60)
        res_vec$working_hours_unpaid_AF[month] <- unpaid_hours_AF + (res_vec$Num_AF[month] * prop_unpaid_AF) * days_worked_month_unpaid_AF * (sample(WH_unpaid_AF_ideal,1) / 60)
        res_vec$working_hours_unpaid_AM[month] <- unpaid_hours_AM + (res_vec$Num_AM[month] * prop_unpaid_AM) * days_worked_month_unpaid_AM * (sample(WH_unpaid_AM_ideal,1) / 60)
        
        res_vec$working_hours_unpaid[month] <- res_vec$working_hours_unpaid_SubAF[month] + res_vec$working_hours_unpaid_SubAM[month] + res_vec$working_hours_unpaid_AF[month] + res_vec$working_hours_unpaid_AM[month]
        
        unpaid_hours_SubAF <- res_vec$working_hours_unpaid_SubAF[month]
        unpaid_hours_SubAM <- res_vec$working_hours_unpaid_SubAM[month]
        unpaid_hours_AF <- res_vec$working_hours_unpaid_AF[month]
        unpaid_hours_AM <- res_vec$working_hours_unpaid_AM[month]
        
        ## combined
        ## combined
        res_vec$working_hours_combined_SubAF[month] <- combined_hours_SubAF + (res_vec$Num_SubAF[month] * prop_combined_hhcom_SubAF) * days_worked_month_combined_SubAF * (sample(WH_combined_hhcom_SubAF_ideal,1) / 60)
        res_vec$working_hours_combined_SubAM[month] <- combined_hours_SubAM + (res_vec$Num_SubAM[month] * prop_combined_hhcom_SubAM) * days_worked_month_combined_SubAM * (sample(WH_combined_hhcom_SubAM_ideal,1) / 60)
        res_vec$working_hours_combined_AF[month] <- combined_hours_AF + (res_vec$Num_AF[month] * prop_combined_hhcom_AF) * days_worked_month_combined_AF * (sample(WH_combined_hhcom_AF_ideal,1) / 60)
        res_vec$working_hours_combined_AM[month] <- combined_hours_AM + (res_vec$Num_AM[month] * prop_combined_hhcom_AM) * days_worked_month_combined_AM * (sample(WH_combined_hhcom_AM_ideal,1) / 60)
        
        res_vec$working_hours_combined[month] <- res_vec$working_hours_combined_SubAF[month] + res_vec$working_hours_combined_SubAM[month] + res_vec$working_hours_combined_AF[month] + res_vec$working_hours_combined_AM[month]
        
        combined_hours_SubAF <- res_vec$working_hours_combined_SubAF[month]
        combined_hours_SubAM <- res_vec$working_hours_combined_SubAM[month]
        combined_hours_AF <- res_vec$working_hours_combined_AF[month]
        combined_hours_AM <- res_vec$working_hours_combined_AM[month]
        
        ## all
        res_vec$working_hours[month] <- res_vec$working_hours_combined[month] + res_vec$working_hours_unpaid[month] + res_vec$working_hours_commercial[month]
        
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
        monthly_milk <- AF * (sample(part, 1)/Num_timesteps) * sample(prop_F_milked, 1) * sample(lac_duration, 1) * sample(avg_daily_yield_ltr, 1) 
        res_vec$Quantity_milk[month] <- milk + monthly_milk
        
        milk <- res_vec$Quantity_milk[month]
        
        res_vec$milk_ME_MJ[month] <- res_vec$Quantity_milk[month] * milk_MEreqPerKG
        res_vec$milk_DM_req[month] <- res_vec$Quantity_milk[month] * milk_DM_req_perKG
        
        ## ADDED milk HERE so value calculated stochastically each month and iteration
        res_vec$Value_milk[month] <- res_vec$Quantity_milk[month] * sample(milk_value_ltr, 1)
      }
      
      if (species == "cattle" ||species == "equids" || species == "smallruminants" || species == "swine") {
        
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
      
      if (species == "swine") {
        res_vec$Quantity_manure_GF[month] <- Manure_kg_GF + GF * sample(Man_G, 1) * timestep_Nof_days  
        res_vec$Quantity_manure_GM[month] <- Manure_kg_GM + GM * sample(Man_G, 1) * timestep_Nof_days 
        res_vec$Quantity_manure_GltF[month] <- Manure_kg_GltF + GltF * sample(Man_Glt, 1) * timestep_Nof_days  
        res_vec$Quantity_manure_GltM[month] <- Manure_kg_GltM + GltM * sample(Man_Glt, 1) * timestep_Nof_days 
        
        Manure_kg_GF <- res_vec$Quantity_manure_GF[month]
        Manure_kg_GM <- res_vec$Quantity_manure_GM[month]
        Manure_kg_GltF <- res_vec$Quantity_manure_GltF[month]
        Manure_kg_GltM <- res_vec$Quantity_manure_GltM[month]
        
        res_vec$Quantity_manure[month] <- res_vec$Quantity_manure[month] +
          res_vec$Quantity_manure_GF[month] +
          res_vec$Quantity_manure_GM[month] +
          res_vec$Quantity_manure_GltF[month] +
          res_vec$Quantity_manure_GltM[month]
        
        res_vec$Value_manure_GF[month] <- res_vec$Quantity_manure_GF[month] * sample(Man_value, 1)
        res_vec$Value_manure_GM[month] <- res_vec$Quantity_manure_GM[month] * sample(Man_value, 1)
        res_vec$Value_manure_GltF[month] <- res_vec$Quantity_manure_GltF[month] * sample(Man_value, 1)
        res_vec$Value_manure_GltM[month] <- res_vec$Quantity_manure_GltM[month] * sample(Man_value, 1)
        
        res_vec$Value_manure[month] <- res_vec$Value_manure[month] +
          res_vec$Value_manure_GF[month] +
          res_vec$Value_manure_GM[month] +
          res_vec$Value_manure_GltF[month] +
          res_vec$Value_manure_GltM[month]
        
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
    if (species == "cattle" || species == "smallruminants" || species == "swine") {
        res_vec$Cumulative_dry_matter_JF[month] <- Cumulative_DM_JF + (JF * sample(kg_DM_req_JF, 1) * timestep_Nof_days) 
        res_vec$Cumulative_dry_matter_JM[month] <- Cumulative_DM_JM + (JM * sample(kg_DM_req_JM, 1) * timestep_Nof_days)
        res_vec$Cumulative_dry_matter_SubAF[month] <- Cumulative_DM_SubAF + (SubAF * sample(kg_DM_req_SubAF, 1) * timestep_Nof_days) 
        res_vec$Cumulative_dry_matter_SubAM[month] <- Cumulative_DM_SubAM + (SubAM * sample(kg_DM_req_SubAM, 1) * timestep_Nof_days) 
        res_vec$Cumulative_dry_matter_AF[month] <- Cumulative_DM_AF + (AF * sample(kg_DM_req_AF, 1) * timestep_Nof_days) + (monthly_milk * milk_DM_req_perKG)
        res_vec$Cumulative_dry_matter_AM[month] <- Cumulative_DM_AM + (AM * sample(kg_DM_req_AM, 1) * timestep_Nof_days)
        
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
      
      if (species == "swine"){
        res_vec$Cumulative_dry_matter_GF[month] <- Cumulative_DM_GF + GF * sample(kg_DM_req_GF, 1) * timestep_Nof_days 
        res_vec$Cumulative_dry_matter_GM[month] <- Cumulative_DM_GM + GM * sample(kg_DM_req_GM, 1) * timestep_Nof_days
        res_vec$Cumulative_dry_matter_GltF[month] <- Cumulative_DM_GltF + GltF * sample(kg_DM_req_GltF, 1) * timestep_Nof_days 
        res_vec$Cumulative_dry_matter_GltM[month] <- Cumulative_DM_GltM + GltM * sample(kg_DM_req_GltM, 1) * timestep_Nof_days
        
        Cumulative_DM_GF <- res_vec$Cumulative_dry_matter_GF[month]
        Cumulative_DM_GM <- res_vec$Cumulative_dry_matter_GM[month]
        Cumulative_DM_GltF <- res_vec$Cumulative_dry_matter_GltF[month]
        Cumulative_DM_GltM <- res_vec$Cumulative_dry_matter_GltM[month]
        
        res_vec$Cumulative_dry_matter[month] <- res_vec$Cumulative_dry_matter[month] +
          res_vec$Cumulative_dry_matter_GF[month] +
          res_vec$Cumulative_dry_matter_GM[month] +
          res_vec$Cumulative_dry_matter_GltF[month] +
          res_vec$Cumulative_dry_matter_GltM[month]
        
      }
      
      
      ## ADDED extra feed requirements for working equids
      if (species == "equids") {
        res_vec$Cumulative_dry_matter_JF[month] <- Cumulative_DM_JF + JF * sample(kg_DM_req_JF, 1) * timestep_Nof_days 
        res_vec$Cumulative_dry_matter_JM[month] <- Cumulative_DM_JM + JM * sample(kg_DM_req_JM, 1) * timestep_Nof_days
        res_vec$Cumulative_dry_matter_SubAF[month] <- Cumulative_DM_SubAF + SubAF * sample(kg_DM_req_SubAF, 1) * timestep_Nof_days 
        res_vec$Cumulative_dry_matter_SubAM[month] <- Cumulative_DM_SubAM + SubAM * sample(kg_DM_req_SubAM, 1) * timestep_Nof_days 
        res_vec$Cumulative_dry_matter_AF[month] <- Cumulative_DM_AF + (AF * sample(kg_DM_req_AF, 1) * timestep_Nof_days) + (0.97*(AF * sample(kg_DM_req_AF, 1) * timestep_Nof_days)) # + (monthly_milk * milk_DM_req_perKG)
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
      
      if (species == "cattle" || species == "smallruminants" || species == "equids" || species == "swine" || species == "poultry") {
        
        ## KG of feed required based on dry matter used by each animal in the sytem
        res_vec$ME_required_MJ_JF[month] <-  Total_ME_MJ_JF + calculate_ME_required(liveweight_JF, JF, MEreqPerKG_JF, timestep_Nof_days)
        res_vec$ME_required_MJ_JM[month] <-  Total_ME_MJ_JM + calculate_ME_required(liveweight_JM, JM, MEreqPerKG_JM, timestep_Nof_days)
        res_vec$ME_required_MJ_SubAF[month] <-  Total_ME_MJ_SubAF + calculate_ME_required(liveweight_SubAF, SubAF, MEreqPerKG_SubAF, timestep_Nof_days)
        res_vec$ME_required_MJ_SubAM[month] <-  Total_ME_MJ_SubAM + calculate_ME_required(liveweight_SubAM, SubAM, MEreqPerKG_SubAM, timestep_Nof_days)
        res_vec$ME_required_MJ_AF[month] <-  Total_ME_MJ_AF + calculate_ME_required(liveweight_AF, AF, MEreqPerKG_AF, timestep_Nof_days)
        res_vec$ME_required_MJ_AM[month] <-  Total_ME_MJ_AM + calculate_ME_required(liveweight_AM, AM, MEreqPerKG_AM, timestep_Nof_days)
        
        Total_ME_MJ_JF <- res_vec$ME_required_MJ_JF[month]
        Total_ME_MJ_JM <- res_vec$ME_required_MJ_JM[month]
        Total_ME_MJ_SubAF <- res_vec$ME_required_MJ_SubAF[month]
        Total_ME_MJ_SubAM <- res_vec$ME_required_MJ_SubAM[month]
        Total_ME_MJ_AF <- res_vec$ME_required_MJ_AF[month]
        Total_ME_MJ_AM <- res_vec$ME_required_MJ_AM[month]
        
        res_vec$ME_required_MJ <- sum(res_vec$ME_required_MJ_JF[month], res_vec$ME_required_MJ_JM[month],
                              res_vec$ME_required_MJ_SubAF[month], res_vec$ME_required_MJ_SubAM[month],
                              res_vec$ME_required_MJ_AF[month], res_vec$ME_required_MJ_AM[month])
        
      }
      
      
      ##### add calculations here for feed required, purchased feed, foraged feed, value of foraged feed
      if (species == "cattle" || species == "smallruminants" ||species == "swine") {
        
        ## KG of feed required based on dry matter used by each animal in the sytem
        res_vec$Feed_required_KG_JF[month] <-  calculate_feed_required_based_on_DM_content(res_vec$Cumulative_dry_matter_JF[month], sample(DM_in_feed, 1))
        res_vec$Feed_required_KG_JM[month] <-  calculate_feed_required_based_on_DM_content(res_vec$Cumulative_dry_matter_JM[month], sample(DM_in_feed, 1))
        res_vec$Feed_required_KG_SubAF[month] <-  calculate_feed_required_based_on_DM_content(res_vec$Cumulative_dry_matter_SubAF[month], sample(DM_in_feed, 1))
        res_vec$Feed_required_KG_SubAM[month] <-  calculate_feed_required_based_on_DM_content(res_vec$Cumulative_dry_matter_SubAM[month], sample(DM_in_feed, 1))
        res_vec$Feed_required_KG_AF[month] <-  calculate_feed_required_based_on_DM_content(res_vec$Cumulative_dry_matter_AF[month], sample(DM_in_feed, 1))
        res_vec$Feed_required_KG_AM[month] <-  calculate_feed_required_based_on_DM_content(res_vec$Cumulative_dry_matter_AM[month], sample(DM_in_feed, 1))
        
        res_vec$Feed_required_KG[month] <- sum(res_vec$Feed_required_KG_JF[month],
                                               res_vec$Feed_required_KG_JM[month],
                                               res_vec$Feed_required_KG_SubAF[month],
                                               res_vec$Feed_required_KG_SubAM[month],
                                               res_vec$Feed_required_KG_AF[month],
                                               res_vec$Feed_required_KG_AM[month])
        
        ## KG of Feed required that is purchased
        res_vec$Purchased_Feed_KG_JF[month] <- res_vec$Feed_required_KG_JF[month] * prpn_lskeepers_purch_feed * prpn_feed_paid_for
        res_vec$Purchased_Feed_KG_JM[month] <- res_vec$Feed_required_KG_JM[month] * prpn_lskeepers_purch_feed * prpn_feed_paid_for
        res_vec$Purchased_Feed_KG_SubAF[month] <- res_vec$Feed_required_KG_SubAF[month] * prpn_lskeepers_purch_feed * prpn_feed_paid_for
        res_vec$Purchased_Feed_KG_SubAM[month] <- res_vec$Feed_required_KG_SubAM[month] * prpn_lskeepers_purch_feed * prpn_feed_paid_for
        res_vec$Purchased_Feed_KG_AF[month] <- res_vec$Feed_required_KG_AF[month] * prpn_lskeepers_purch_feed * prpn_feed_paid_for
        res_vec$Purchased_Feed_KG_AM[month] <- res_vec$Feed_required_KG_AM[month] * prpn_lskeepers_purch_feed * prpn_feed_paid_for
        
        res_vec$Purchased_Feed_KG[month] <- sum(res_vec$Purchased_Feed_KG_JF[month],
                                               res_vec$Purchased_Feed_KG_JM[month],
                                               res_vec$Purchased_Feed_KG_SubAF[month],
                                               res_vec$Purchased_Feed_SubAM[month],
                                               res_vec$Purchased_Feed_KG_AF[month],
                                               res_vec$Purchased_Feed_KG_AM[month])
        
        ## KG of non-purchased (foraged feed) used by system
        res_vec$Foraged_Feed_KG_JF[month] <- res_vec$Feed_required_KG_JF[month] - res_vec$Purchased_Feed_KG_JF[month]
        res_vec$Foraged_Feed_KG_JM[month] <- res_vec$Feed_required_KG_JM[month] - res_vec$Purchased_Feed_KG_JM[month]
        res_vec$Foraged_Feed_KG_SubAF[month] <- res_vec$Feed_required_KG_SubAF[month] - res_vec$Purchased_Feed_KG_SubAF[month]
        res_vec$Foraged_Feed_KG_SubAM[month] <- res_vec$Feed_required_KG_SubAM[month] - res_vec$Purchased_Feed_KG_SubAM[month]
        res_vec$Foraged_Feed_KG_AF[month] <- res_vec$Feed_required_KG_AF[month] - res_vec$Purchased_Feed_KG_AF[month]
        res_vec$Foraged_Feed_KG_AM[month] <- res_vec$Feed_required_KG_AM[month] - res_vec$Purchased_Feed_KG_AM[month]
        
        res_vec$Foraged_Feed_KG[month] <- sum(res_vec$Foraged_Feed_KG_JF[month],
                                              res_vec$Foraged_Feed_KG_JM[month],
                                              res_vec$Foraged_Feed_KG_SubAM[month],
                                              res_vec$Foraged_Feed_KG_SubAF[month],
                                              res_vec$Foraged_Feed_KG_AF[month],
                                              res_vec$Foraged_Feed_KG_AM[month])
        
        ## Total value (at market value per kg) of non-purchased feed
        feed_cost_run <- sample(Feed_cost_kg, 1)

        res_vec$Foraged_Feed_Value_JF[month] <- res_vec$Foraged_Feed_KG_JF[month] * feed_cost_run
        res_vec$Foraged_Feed_Value_JM[month] <- res_vec$Foraged_Feed_KG_JM[month] * feed_cost_run
        res_vec$Foraged_Feed_Value_SubAF[month] <- res_vec$Foraged_Feed_KG_SubAF[month] * feed_cost_run
        res_vec$Foraged_Feed_Value_SubAM[month] <- res_vec$Foraged_Feed_KG_SubAM[month] * feed_cost_run
        res_vec$Foraged_Feed_Value_AF[month] <- res_vec$Foraged_Feed_KG_AF[month] * feed_cost_run
        res_vec$Foraged_Feed_Value_AM[month] <- res_vec$Foraged_Feed_KG_AM[month] * feed_cost_run
     
        res_vec$Foraged_Feed_Value[month] <- sum(res_vec$Foraged_Feed_Value_JF[month],
                                                    res_vec$Foraged_Feed_Value_JM[month],
                                                    res_vec$Foraged_Feed_Value_SubAF[month],
                                                    res_vec$Foraged_Feed_Value_SubAM[month],
                                                    res_vec$Foraged_Feed_Value_AF[month],
                                                    res_vec$Foraged_Feed_Value_AM[month])
         }
      
      # GIRMA added this calculations here for feed required, purchased feed (for partially grazed and primary fodder fed types of donkeys in Ethiopia), foraged feed, value of foraged feed for equids
      
      if (species == "equids"){  
        
        ## KG of feed required based on dry matter used by each animal in the sytem
        res_vec$Feed_required_KG_JF[month] <-  calculate_feed_required_based_on_DM_content(res_vec$Cumulative_dry_matter_JF[month], sample(DM_in_feed, 1))
        res_vec$Feed_required_KG_JM[month] <-  calculate_feed_required_based_on_DM_content(res_vec$Cumulative_dry_matter_JM[month], sample(DM_in_feed, 1))
        res_vec$Feed_required_KG_SubAF[month] <-  calculate_feed_required_based_on_DM_content(res_vec$Cumulative_dry_matter_SubAF[month], sample(DM_in_feed, 1))
        res_vec$Feed_required_KG_SubAM[month] <-  calculate_feed_required_based_on_DM_content(res_vec$Cumulative_dry_matter_SubAM[month], sample(DM_in_feed, 1))
        res_vec$Feed_required_KG_AF[month] <-  calculate_feed_required_based_on_DM_content(res_vec$Cumulative_dry_matter_AF[month], sample(DM_in_feed, 1))
        res_vec$Feed_required_KG_AM[month] <-  calculate_feed_required_based_on_DM_content(res_vec$Cumulative_dry_matter_AM[month], sample(DM_in_feed, 1))
        
        res_vec$Feed_required_KG[month] <- sum(res_vec$Feed_required_KG_JF[month],
                                               res_vec$Feed_required_KG_JM[month],
                                               res_vec$Feed_required_KG_SubAF[month],
                                               res_vec$Feed_required_KG_SubAM[month],
                                               res_vec$Feed_required_KG_AF[month],
                                               res_vec$Feed_required_KG_AM[month])
        
        ## KG of Feed required for - partially grazed donkeys, where grazing is supplemented with additional fodder; and 
        res_vec$Purchased_Feedpartial_KG_JF[month] <- res_vec$Feed_required_KG_JF[month] * prpn_lskeepers_partial_feed * lskeepers_partial_feed_prpn_fd_purch
        res_vec$Purchased_Feedpartial_KG_JM[month] <- res_vec$Feed_required_KG_JM[month] * prpn_lskeepers_partial_feed * lskeepers_partial_feed_prpn_fd_purch
        res_vec$Purchased_Feedpartial_KG_SubAF[month] <- res_vec$Feed_required_KG_SubAF[month] * prpn_lskeepers_partial_feed * lskeepers_partial_feed_prpn_fd_purch
        res_vec$Purchased_Feedpartial_KG_SubAM[month] <- res_vec$Feed_required_KG_SubAM[month] * prpn_lskeepers_partial_feed * lskeepers_partial_feed_prpn_fd_purch
        res_vec$Purchased_Feedpartial_KG_AF[month] <- res_vec$Feed_required_KG_AF[month] * prpn_lskeepers_partial_feed * lskeepers_partial_feed_prpn_fd_purch
        res_vec$Purchased_Feedpartial_KG_AM[month] <- res_vec$Feed_required_KG_AM[month] * prpn_lskeepers_partial_feed * lskeepers_partial_feed_prpn_fd_purch
        
        res_vec$Purchased_Feedpartial_KG[month]<- sum(res_vec$Purchased_Feedpartial_KG_JF[month],
                                               res_vec$Purchased_Feedpartial_KG_JM[month],
                                               res_vec$Purchased_Feedpartial_KG_SubAF[month],
                                               res_vec$Purchased_Feedpartial_KG_SubAM[month],
                                               res_vec$Purchased_Feedpartial_KG_AF[month],
                                               res_vec$Purchased_Feedpartial_KG_AM[month])
        
        ## KG of Feed required that is purchased - primarily fodder-fed, where donkeys are mainly provided with fodder at home
        res_vec$Purchased_Feed_KG_JF[month] <- res_vec$Feed_required_KG_JF[month] * prpn_lskeepers_all_handfeed * lskeepers_all_handfeed_prpn_fd_purch
        res_vec$Purchased_Feed_KG_JM[month] <- res_vec$Feed_required_KG_JM[month] * prpn_lskeepers_all_handfeed * lskeepers_all_handfeed_prpn_fd_purch
        res_vec$Purchased_Feed_KG_SubAF[month] <- res_vec$Feed_required_KG_SubAF[month] * prpn_lskeepers_all_handfeed * lskeepers_all_handfeed_prpn_fd_purch
        res_vec$Purchased_Feed_KG_SubAM[month] <- res_vec$Feed_required_KG_SubAM[month] * prpn_lskeepers_all_handfeed * lskeepers_all_handfeed_prpn_fd_purch
        res_vec$Purchased_Feed_KG_AF[month] <- res_vec$Feed_required_KG_AF[month] * prpn_lskeepers_all_handfeed * lskeepers_all_handfeed_prpn_fd_purch
        res_vec$Purchased_Feed_KG_AM[month] <- res_vec$Feed_required_KG_AM[month] * prpn_lskeepers_all_handfeed * lskeepers_all_handfeed_prpn_fd_purch
        
        res_vec$Purchased_Feed_KG[month]<- sum(res_vec$Purchased_Feed_KG_JF[month],
                                               res_vec$Purchased_Feed_KG_JM[month],
                                               res_vec$Purchased_Feed_KG_SubAF[month],
                                               res_vec$Purchased_Feed_KG_SubAM[month],
                                               res_vec$Purchased_Feed_KG_AF[month],
                                               res_vec$Purchased_Feed_KG_AM[month])
        
        ## KG of non-purchased (foraged feed) used by system
        res_vec$Foraged_Feed_KG_JF[month] <- res_vec$Feed_required_KG_JF[month] - res_vec$Purchased_Feedpartial_KG_JF[month] - res_vec$Purchased_Feed_KG_JF[month]
        res_vec$Foraged_Feed_KG_JM[month] <- res_vec$Feed_required_KG_JM[month] - res_vec$Purchased_Feedpartial_KG_JM[month] - res_vec$Purchased_Feed_KG_JM[month]
        res_vec$Foraged_Feed_KG_SubAF[month] <- res_vec$Feed_required_KG_SubAF[month] - res_vec$Purchased_Feedpartial_KG_SubAF[month] - res_vec$Purchased_Feed_KG_SubAF[month]
        res_vec$Foraged_Feed_KG_SubAM[month] <- res_vec$Feed_required_KG_SubAM[month] - res_vec$Purchased_Feedpartial_KG_SubAM[month] - res_vec$Purchased_Feed_KG_SubAM[month]
        res_vec$Foraged_Feed_KG_AF[month] <- res_vec$Feed_required_KG_AF[month] - res_vec$Purchased_Feedpartial_KG_AF[month] - res_vec$Purchased_Feed_KG_AF[month]
        res_vec$Foraged_Feed_KG_AM[month] <- res_vec$Feed_required_KG_AM[month] - res_vec$Purchased_Feedpartial_KG_AM[month] - res_vec$Purchased_Feed_KG_AM[month]
        
        res_vec$Foraged_Feed_KG[month] <- sum(res_vec$Foraged_Feed_KG_JF[month],
                                              res_vec$Foraged_Feed_KG_JM[month],
                                              res_vec$Foraged_Feed_KG_SubAM[month],
                                              res_vec$Foraged_Feed_KG_SubAF[month],
                                              res_vec$Foraged_Feed_KG_AF[month],
                                              res_vec$Foraged_Feed_KG_AM[month])
        
        ## Total value (at market value per kg) of non-purchased feed
        feed_cost_run <- sample(Feed_cost_kg, 1)
        
        res_vec$Foraged_Feed_Value_JF[month] <- res_vec$Foraged_Feed_KG_JF[month] * feed_cost_run
        res_vec$Foraged_Feed_Value_JM[month] <- res_vec$Foraged_Feed_KG_JM[month] * feed_cost_run
        res_vec$Foraged_Feed_Value_SubAF[month] <- res_vec$Foraged_Feed_KG_SubAF[month] * feed_cost_run
        res_vec$Foraged_Feed_Value_SubAM[month] <- res_vec$Foraged_Feed_KG_SubAM[month] * feed_cost_run
        res_vec$Foraged_Feed_Value_AF[month] <- res_vec$Foraged_Feed_KG_AF[month] * feed_cost_run
        res_vec$Foraged_Feed_Value_AM[month] <- res_vec$Foraged_Feed_KG_AM[month] * feed_cost_run
        
        res_vec$Foraged_Feed_Value[month] <- sum(res_vec$Foraged_Feed_Value_JF[month],
                                                 res_vec$Foraged_Feed_Value_JM[month],
                                                 res_vec$Foraged_Feed_Value_SubAF[month],
                                                 res_vec$Foraged_Feed_Value_SubAM[month],
                                                 res_vec$Foraged_Feed_Value_AF[month],
                                                 res_vec$Foraged_Feed_Value_AM[month])
      }
      
        if (species == "swine"){
          res_vec$Feed_required_KG_GF[month] <-  calculate_feed_required_based_on_DM_content(res_vec$Cumulative_dry_matter_GF[month], sample(DM_in_feed, 1))
          res_vec$Feed_required_KG_GM[month] <-  calculate_feed_required_based_on_DM_content(res_vec$Cumulative_dry_matter_GM[month], sample(DM_in_feed, 1))
          res_vec$Feed_required_KG_GltF[month] <-  calculate_feed_required_based_on_DM_content(res_vec$Cumulative_dry_matter_GltF[month], sample(DM_in_feed, 1))
          res_vec$Feed_required_KG_GltM[month] <-  calculate_feed_required_based_on_DM_content(res_vec$Cumulative_dry_matter_GltM[month], sample(DM_in_feed, 1))
          
          res_vec$Feed_required_KG[month] <- res_vec$Feed_required_KG[month]+
            res_vec$Feed_required_KG_GF[month]+
            res_vec$Feed_required_KG_GM[month]+
            res_vec$Feed_required_KG_GltF[month]+
            res_vec$Feed_required_KG_GltM[month]
          
          res_vec$Purchased_Feed_KG_GF[month] <- res_vec$Feed_required_KG_GF[month] * prpn_lskeepers_purch_feed * prpn_feed_paid_for
          res_vec$Purchased_Feed_KG_GM[month] <- res_vec$Feed_required_KG_GM[month] * prpn_lskeepers_purch_feed * prpn_feed_paid_for
          res_vec$Purchased_Feed_KG_GltF[month] <- res_vec$Feed_required_KG_GltF[month] * prpn_lskeepers_purch_feed * prpn_feed_paid_for
          res_vec$Purchased_Feed_KG_GltM[month] <- res_vec$Feed_required_KG_GltM[month] * prpn_lskeepers_purch_feed * prpn_feed_paid_for
          
          res_vec$Purchased_Feed_KG[month] <- res_vec$Purchased_Feed_KG[month] +
            res_vec$Purchased_Feed_KG_GF[month] +
            res_vec$Purchased_Feed_KG_GM[month] +
            res_vec$Purchased_Feed_KG_GltF[month] +
            res_vec$Purchased_Feed_KG_GltM[month]
          
          res_vec$Foraged_Feed_KG_GF[month] <- res_vec$Feed_required_KG_GF[month] - res_vec$Purchased_Feed_KG_GF[month]
          res_vec$Foraged_Feed_KG_GM[month] <- res_vec$Feed_required_KG_GM[month] - res_vec$Purchased_Feed_KG_GM[month]
          res_vec$Foraged_Feed_KG_GltF[month] <- res_vec$Feed_required_KG_GltF[month] - res_vec$Purchased_Feed_KG_GltF[month]
          res_vec$Foraged_Feed_KG_GltM[month] <- res_vec$Feed_required_KG_GltM[month] - res_vec$Purchased_Feed_KG_GltM[month]
          
          res_vec$Foraged_Feed_KG_KG[month] <- res_vec$Foraged_Feed_KG_KG[month] +
            res_vec$Foraged_Feed_KG_GF[month] +
            res_vec$Foraged_Feed_KG_GM[month] +
            res_vec$Foraged_Feed_KG_GltF[month] +
            res_vec$Foraged_Feed_KG_GltM[month]
          
          res_vec$Foraged_Feed_Value_GF[month] <- res_vec$Foraged_Feed_KG_GF[month] * feed_cost_run
          res_vec$Foraged_Feed_Value_GM[month] <- res_vec$Foraged_Feed_KG_GM[month] * feed_cost_run
          res_vec$Foraged_Feed_Value_GltF[month] <- res_vec$Foraged_Feed_KG_GltF[month] * feed_cost_run
          res_vec$Foraged_Feed_Value_GltM[month] <- res_vec$Foraged_Feed_KG_GltM[month] * feed_cost_run
          
          res_vec$Foraged_Feed_Value[month] <- res_vec$Foraged_Feed_Value[month] +
            res_vec$Foraged_Feed_Value_GF[month] +
            res_vec$Foraged_Feed_Value_GM[month]+
            res_vec$Foraged_Feed_Value_GltF[month] +
            res_vec$Foraged_Feed_Value_GltM[month]
          
          res_vec$ME_required_MJ_GF[month] <-  Total_ME_MJ_GF + calculate_ME_required(liveweight_GF, GF, MEreqPerKG_GF, timestep_Nof_days)
          res_vec$ME_required_MJ_GM[month] <-  Total_ME_MJ_GM + calculate_ME_required(liveweight_GM, GM, MEreqPerKG_GM, timestep_Nof_days)
          res_vec$ME_required_MJ_GltF[month] <-  Total_ME_MJ_GltF + calculate_ME_required(liveweight_GltF, GltF, MEreqPerKG_GltF, timestep_Nof_days)
          res_vec$ME_required_MJ_GltM[month] <-  Total_ME_MJ_GltM + calculate_ME_required(liveweight_GltM, GltM, MEreqPerKG_GltM, timestep_Nof_days)
          
          res_vec$ME_required_MJ <- res_vec$ME_required_MJ + 
            res_vec$ME_required_MJ_GF[month] +
            res_vec$ME_required_MJ_GM[month] +
            res_vec$ME_required_MJ_GltF[month] +
            res_vec$ME_required_MJ_GltM[month]
            
           }
      
      if (species == "cattle"){
        res_vec$Feed_required_KG_Ox[month] <-  calculate_feed_required_based_on_DM_content(res_vec$Cumulative_dry_matter_Ox[month], sample(DM_in_feed, 1))
        res_vec$Purchased_Feed_KG_Ox[month] <- res_vec$Feed_required_KG_Ox[month] * prpn_lskeepers_purch_feed * prpn_feed_paid_for
        res_vec$Foraged_Feed_KG_Ox[month] <- res_vec$Feed_required_KG_Ox[month] - res_vec$Purchased_Feed_KG_Ox[month]
        res_vec$Foraged_Feed_Value_Ox[month] <- res_vec$Foraged_Feed_KG_Ox[month] * feed_cost_run
        
        res_vec$Feed_required_KG[month] <- res_vec$Feed_required_KG[month] + res_vec$Feed_required_KG_Ox[month]
        res_vec$Purchased_Feed_KG[month] <- res_vec$Purchased_Feed_KG[month] + res_vec$Purchased_Feed_KG_Ox[month]
        res_vec$Foraged_Feed_KG[month] <- res_vec$Foraged_Feed_KG[month] + res_vec$Foraged_Feed_KG_Ox[month]
        res_vec$Foraged_Feed_Value[month] <- res_vec$Foraged_Feed_Value[month] + res_vec$Foraged_Feed_Value_Ox[month]
        
        res_vec$ME_required_MJ_Ox[month] <-  Total_ME_MJ_Ox + calculate_ME_required(liveweight_Ox, Ox, MEreqPerKG_Ox, timestep_Nof_days)
        Total_ME_MJ_Ox <- res_vec$ME_required_MJ_Ox[month]
        
        res_vec$ME_required_MJ <- res_vec$ME_required_MJ + res_vec$ME_required_MJ_Ox[month]
      }
      
      
      ### Financial value seciion ### 
      
    if (species == "cattle" || species == "smallruminants" || species == "poultry" || species == "equids" || species == "swine") {
      
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
     
      if (species == "swine") {
        fin_val_GF <- sample(fvGF, 1)
        fin_val_GM <- sample(fvGM, 1)
        fin_val_GltF <- sample(fvGltF, 1)
        fin_val_GltM <- sample(fvGltM, 1)
        
        res_vec$Value_offtake_GF[month] <- fin_val_GF * Offtake_GF
        Value_offtake_GF <- res_vec$Value_offtake_GF[month]
        
        res_vec$Value_offtake_GM[month] <- fin_val_GM * Offtake_GM  
        Value_offtake_GM <- res_vec$Value_offtake_GM[month]
       
        res_vec$Value_offtake_GltF[month] <- fin_val_GltF * Offtake_GltF
        Value_offtake_GltF <- res_vec$Value_offtake_GltF[month]
        
        res_vec$Value_offtake_GltM[month] <- fin_val_GltM * Offtake_GltM  
        Value_offtake_GltM <- res_vec$Value_offtake_GltM[month]
        
        res_vec$Value_offtake[month] <- res_vec$Value_offtake[month] +
          res_vec$Value_offtake_GF[month] +
          res_vec$Value_offtake_GM[month] +
          res_vec$Value_offtake_GltF[month] +
          res_vec$Value_offtake_GltM[month]
        
      }
        
        
       if (species == "cattle") {
         fin_val_Ox <- sample(fvOx, 1)
         res_vec$Value_offtake_Ox[month] <- fin_val_Ox * Offtake_Ox
         
         Value_offtake_Ox <- res_vec$Value_offtake_Ox[month]
         res_vec$Value_offtake[month] <- res_vec$Value_offtake[month] + res_vec$Value_offtake_Ox[month]
       }
    
    if (species == "cattle" || species == "smallruminants" || species == "poultry" || species == "equids" || species == "swine") {
      
      Value_offtake <- res_vec$Value_offtake[month] 
      
      res_vec$Value_herd_increase_JF[month] <- (JF - N_JF) * fin_val_JF
      Value_herd_inc_JF <- res_vec$Value_herd_increase_JF[month]
      
      res_vec$Value_herd_increase_JM[month] <- (JM - N_JM) * fin_val_JM
      Value_herd_inc_JM <- res_vec$Value_herd_increase_JM[month]
      
      res_vec$Value_herd_increase_SubAF[month] <- (SubAF - N_SubAF) * fin_val_SubAF
      Value_herd_inc_SubAF <- res_vec$Value_herd_increase_SubAF[month]
      
      res_vec$Value_herd_increase_SubAM[month] <- (SubAM - N_SubAM) * fin_val_SubAM
      Value_herd_inc_SubAM <- res_vec$Value_herd_increase_SubAM[month]
      
      res_vec$Value_herd_increase_AF[month] <- (AF - N_AF) * fin_val_AF
      Value_herd_inc_AF <- res_vec$Value_herd_increase_AF[month]
      
      res_vec$Value_herd_increase_AM[month] <- (AM - N_AM) * fin_val_AM
      Value_herd_inc_AM <- res_vec$Value_herd_increase_AM[month]
      
      res_vec$Value_herd_increase[month] <- sum(res_vec$Value_herd_increase_JF[month],
                                                res_vec$Value_herd_increase_JM[month],
                                                res_vec$Value_herd_increase_SubAF[month],
                                                res_vec$Value_herd_increase_SubAM[month],
                                                res_vec$Value_herd_increase_AF[month],
                                                res_vec$Value_herd_increase_AM[month])
      
    }
      
      if (species == "swine") {
        res_vec$Value_herd_increase_GF[month] <- (GF - N_GF) * fin_val_GF
        Value_herd_inc_GF <- res_vec$Value_herd_increase_GF[month]
        
        res_vec$Value_herd_increase_GM[month] <- (GM - N_GM) * fin_val_GM
        Value_herd_inc_GM <- res_vec$Value_herd_increase_GM[month]
        
        res_vec$Value_herd_increase_GltF[month] <- (GltF - N_GltF) * fin_val_GltF
        Value_herd_inc_GltF <- res_vec$Value_herd_increase_GltF[month]
        
        res_vec$Value_herd_increase_GltM[month] <- (GltM - N_GltM) * fin_val_GltM
        Value_herd_inc_GltM <- res_vec$Value_herd_increase_GltM[month]
        
        res_vec$Value_herd_increase[month] <- res_vec$Value_herd_increase[month] +
          res_vec$Value_herd_increase_GF[month] +
          res_vec$Value_herd_increase_GM[month] +
          res_vec$Value_herd_increase_GltF[month] +
          res_vec$Value_herd_increase_GltM[month]
      }
      
      if (species == "cattle") {
        res_vec$Value_herd_increase_Ox[month] <- (Ox - N_Ox) * fin_val_Ox
        Value_herd_inc_Ox <- res_vec$Value_herd_increase_Ox[month]
        res_vec$Value_herd_increase[month] <- res_vec$Value_herd_increase[month] + res_vec$Value_herd_increase_Ox[month]
      }
    
    if (species == "cattle" || species == "smallruminants" || species == "poultry" || species == "equids" || species == "swine") {
      
      Value_herd_inc <- res_vec$Value_herd_increase[month]
      
      res_vec$Total_value_increase[month] <- Value_herd_inc + Value_offtake ## Total_value already includes oxen
      res_vec$Total_value_increase_JF[month] <- Value_herd_inc_JF 
      res_vec$Total_value_increase_JM[month] <- Value_herd_inc_JM 
      res_vec$Total_value_increase_SubAF[month] <- Value_herd_inc_SubAF + Value_offtake_SubAF
      res_vec$Total_value_increase_SubAM[month] <- Value_herd_inc_SubAM + Value_offtake_SubAM
      res_vec$Total_value_increase_AF[month] <- Value_herd_inc_AF + Value_offtake_AF
      res_vec$Total_value_increase_AM[month] <- Value_herd_inc_AM + Value_offtake_AM
      
    }
      
      if (species == "swine") {
        res_vec$Total_value_increase_GF[month] <- Value_herd_inc_GF + Value_offtake_GF
        res_vec$Total_value_increase_GM[month] <- Value_herd_inc_GM + Value_offtake_GM
        res_vec$Total_value_increase_GltF[month] <- Value_herd_inc_GltF + Value_offtake_GltF
        res_vec$Total_value_increase_GltM[month] <- Value_herd_inc_GltM + Value_offtake_GltM
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
      
    if (species == "cattle" || species == "smallruminants" || species == "poultry" || species == "equids" || species == "swine") {
        
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
    
        ## Labour
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
        
      if (species == "swine") {
        res_vec$Feed_cost_GF[month] <- Feed_GF + GF * sample(Expenditure_on_feed_GF, 1) * timestep_Nof_days
        Feed_GF <- res_vec$Feed_cost_GF[month]
       
        res_vec$Feed_cost_GM[month] <- Feed_GM + GM * sample(Expenditure_on_feed_GM, 1) * timestep_Nof_days
        Feed_GM <- res_vec$Feed_cost_GM[month]
        
        res_vec$Feed_cost_GltF[month] <- Feed_GltF + GltF * sample(Expenditure_on_feed_GltF, 1) * timestep_Nof_days
        Feed_GltF <- res_vec$Feed_cost_GltF[month]
        
        res_vec$Feed_cost_GltM[month] <- Feed_GltM + GltM * sample(Expenditure_on_feed_GltM, 1) * timestep_Nof_days
        Feed_GltM <- res_vec$Feed_cost_GltM[month]
        
        res_vec$Feed_cost[month] <- res_vec$Feed_cost[month] + 
          res_vec$Feed_cost_GF[month] +
          res_vec$Feed_cost_GM[month] +
          res_vec$Feed_cost_GltF[month] +
          res_vec$Feed_cost_GltM[month] 
          
        Feed <- res_vec$Feed_cost[month]
        
        res_vec$Labour_cost_GF[month] <- Labour_GF + GF * (sample(Labour_cost_head_GF, 1)/Num_timesteps) * lab_non_health
        Labour_GF <- res_vec$Labour_cost_GF[month]
        
        res_vec$Labour_cost_GM[month] <- Labour_GM + GM * (sample(Labour_cost_head_GM, 1)/Num_timesteps) * lab_non_health
        Labour_GM <- res_vec$Labour_cost_GM[month]
        
        res_vec$Labour_cost_GltF[month] <- Labour_GltF + GltF * (sample(Labour_cost_head_GltF, 1)/Num_timesteps) * lab_non_health
        Labour_GltF <- res_vec$Labour_cost_GltF[month]
        
        res_vec$Labour_cost_GltM[month] <- Labour_GltM + GltM * (sample(Labour_cost_head_GltM, 1)/Num_timesteps) * lab_non_health
        Labour_GltM<- res_vec$Labour_cost_GltM[month]
        
        res_vec$Labour_cost[month] <- res_vec$Labour_cost[month] + 
          res_vec$Labour_cost_GF[month] +
          res_vec$Labour_cost_GM[month] +
          res_vec$Labour_cost_GltF[month] +
          res_vec$Labour_cost_GltM[month]
          
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
      
        if (species == "cattle" || species == "poultry" || species == "smallruminants" || species == "swine") {
    
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
        
      if (species == "swine") {
        res_vec$Health_cost_GF[month] <- Health_GF + GF * (sample(Health_exp_prev_GF, 1)/Num_timesteps) + GF * (sample(Health_exp_treatment_GF, 1)/Num_timesteps)
        res_vec$Health_cost_GM[month] <- Health_GM + GM * (sample(Health_exp_prev_GM, 1)/Num_timesteps) + GM * (sample(Health_exp_treatment_GM, 1)/Num_timesteps)
        res_vec$Health_cost_GltF[month] <- Health_GltF + GltF * (sample(Health_exp_prev_GltF, 1)/Num_timesteps) + GltF * (sample(Health_exp_treatment_GltF, 1)/Num_timesteps)
        res_vec$Health_cost_GltM[month] <- Health_GltM + GltM * (sample(Health_exp_prev_GltM, 1)/Num_timesteps) + GltM * (sample(Health_exp_treatment_GltM, 1)/Num_timesteps)
        
        res_vec$Health_cost[month] <- res_vec$Health_cost[month] +
          res_vec$Health_cost_GF[month] +
          res_vec$Health_cost_GM[month] +
          res_vec$Health_cost_GltF[month] +
          res_vec$Health_cost_GltM[month]
        
        Health = res_vec$Health_cost[month]
      }
      
                                                                                                                                                                                                                                                                                                                                                                                                                                             
      if (species == "cattle") {
        res_vec$Health_cost_Ox[month] <- Health_Ox + Ox * (sample(Health_exp_prev_Ox, 1)/Num_timesteps) + Ox * (sample(Health_exp_treatment_Ox, 1)/Num_timesteps) 
        Health_Ox <- res_vec$Health_cost_Ox[month]                                                                                
        res_vec$Health_cost[month] <-  res_vec$Health_cost[month] + res_vec$Health_cost_Ox[month]
        Health = res_vec$Health_cost[month]
        
      } 
        
      if (species == "equids") {
        ### GERMA THINKS THIS IS ALL WRONG BECAUSE DOESNT TAKE INTO ACCOUNT PROPORTION USING EACH TREATMENT ###
        ### RE DO USING CODE BELOW THAT IS HEALTHCOST BY TYPE ###
        
        
        
        ### Do separately for each health cost type for future exploration these outputs can be analysed
        ## vet
        res_vec$Health_cost_vet_JF[month] <- Health_vet_JF + (JF  *  (sample(Health_exp_vet_JF, 1)/Num_timesteps)* prop_health_care_vet) 
        res_vec$Health_cost_vet_JM[month] <- Health_vet_JM + (JM  *  (sample(Health_exp_vet_JM, 1)/Num_timesteps) * prop_health_care_vet)
        res_vec$Health_cost_vet_SubAF[month] <- Health_vet_SubAF + (SubAF  * (sample(Health_exp_vet_SubAF, 1)/Num_timesteps)* prop_health_care_vet) 
        res_vec$Health_cost_vet_SubAM[month] <- Health_vet_SubAM + (SubAM  * (sample(Health_exp_vet_SubAM, 1)/Num_timesteps)* prop_health_care_vet) 
        res_vec$Health_cost_vet_AF[month] <- Health_vet_AF + (AF  * (sample(Health_exp_vet_AF, 1)/Num_timesteps) * prop_health_care_vet)
        res_vec$Health_cost_vet_AM[month] <- Health_vet_AM + (AM  * (sample(Health_exp_vet_AM, 1)/Num_timesteps) * prop_health_care_vet)
        
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
        res_vec$Health_cost_trad_JF[month] <- Health_trad_JF + (JF * (sample(Health_exp_trad_JF, 1)/Num_timesteps) * prop_health_care_tradhealer)
        res_vec$Health_cost_trad_JM[month] <- Health_trad_JM + (JM * (sample(Health_exp_trad_JM, 1)/Num_timesteps) * prop_health_care_tradhealer)  
        res_vec$Health_cost_trad_SubAF[month] <- Health_trad_SubAF + (SubAF * (sample(Health_exp_trad_SubAF, 1)/Num_timesteps) * prop_health_care_tradhealer)
        res_vec$Health_cost_trad_SubAM[month] <- Health_trad_SubAM + (SubAM * (sample(Health_exp_trad_SubAM, 1)/Num_timesteps) * prop_health_care_tradhealer) 
        res_vec$Health_cost_trad_AF[month] <- Health_trad_AF + (AF * (sample(Health_exp_trad_AF, 1)/Num_timesteps) * prop_health_care_tradhealer)
        res_vec$Health_cost_trad_AM[month] <- Health_trad_AM + (AM * (sample(Health_exp_trad_AM, 1)/Num_timesteps) * prop_health_care_tradhealer)
        
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
        res_vec$Health_cost_selftx_JF[month] <- Health_selftx_JF + (JF * (sample(Health_exp_selftx_JF, 1)/Num_timesteps) * prop_health_care_selftx) 
        res_vec$Health_cost_selftx_JM[month] <- Health_selftx_JM + (JM * (sample(Health_exp_selftx_JM, 1)/Num_timesteps) * prop_health_care_selftx) 
        res_vec$Health_cost_selftx_SubAF[month] <- Health_selftx_SubAF + (SubAF * (sample(Health_exp_selftx_SubAF, 1)/Num_timesteps) * prop_health_care_selftx)
        res_vec$Health_cost_selftx_SubAM[month] <- Health_selftx_SubAM + (SubAM * (sample(Health_exp_selftx_SubAM, 1)/Num_timesteps) * prop_health_care_selftx)  
        res_vec$Health_cost_selftx_AF[month] <- Health_selftx_AF + (AF * (sample(Health_exp_selftx_AF, 1)/Num_timesteps) * prop_health_care_selftx)
        res_vec$Health_cost_selftx_AM[month] <- Health_selftx_AM + (AM * (sample(Health_exp_selftx_AM, 1)/Num_timesteps) * prop_health_care_selftx)
        
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
        res_vec$Health_cost_combined_JF[month] <- Health_combined_JF + (JF * (sample(Health_exp_combined_JF, 1)/Num_timesteps) * prop_health_care_combined)
        res_vec$Health_cost_combined_JM[month] <- Health_combined_JM + (JM * (sample(Health_exp_combined_JM, 1)/Num_timesteps) * prop_health_care_combined) 
        res_vec$Health_cost_combined_SubAF[month] <- Health_combined_SubAF + (SubAF * (sample(Health_exp_combined_SubAF, 1)/Num_timesteps) * prop_health_care_combined) 
        res_vec$Health_cost_combined_SubAM[month] <- Health_combined_SubAM + (SubAM * (sample(Health_exp_combined_SubAM, 1)/Num_timesteps) * prop_health_care_combined)
        res_vec$Health_cost_combined_AF[month] <- Health_combined_AF + (AF * (sample(Health_exp_combined_AF, 1)/Num_timesteps) * prop_health_care_combined) 
        res_vec$Health_cost_combined_AM[month] <- Health_combined_AM + (AM * (sample(Health_exp_combined_AM, 1)/Num_timesteps) * prop_health_care_combined)
        
        
        
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
      
        
        ## All health costs combined by summing across different typea of health cost
       
        res_vec$Health_cost[month] <- res_vec$Health_cost_vet[month] + res_vec$Health_cost_trad[month] +  res_vec$Health_cost_selftx[month] + res_vec$Health_cost_combined[month]
        
        res_vec$Health_cost_JF[month] <- Health_vet_JF + Health_trad_JF + Health_selftx_JF + Health_combined_JF
        res_vec$Health_cost_JM[month] <- Health_vet_JM + Health_trad_JM + Health_selftx_JM + Health_combined_JM
        res_vec$Health_cost_SubAF[month] <- Health_vet_SubAF + Health_trad_SubAF + Health_selftx_SubAF + Health_combined_SubAF
        res_vec$Health_cost_SubAM[month] <- Health_vet_SubAM + Health_trad_SubAM + Health_selftx_SubAM + Health_combined_SubAM
        res_vec$Health_cost_AF[month] <- Health_vet_AF + Health_trad_AF + Health_selftx_AF + Health_combined_AF
        res_vec$Health_cost_AM[month] <- Health_vet_AM + Health_trad_AM + Health_selftx_AM + Health_combined_AM 
     
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
      
      
    ## GEMMA CHANGED these numbers from [1] to set numbers at time zero (eg. N_JF_t0), like infrastructure calculations
    ## below it is better to use the defined starting population numbers rather than the numbers at timestep[1]
    
      if (species == "cattle" || species == "smallruminants" || species == "poultry" || species == "equids" || species == "swine") {
        
        res_vec$Capital_cost_JF[month] <- N_JF * fin_val_JF * Interest_rate 
        Capital_JF <- res_vec$Capital_cost_JF[month]
     
        res_vec$Capital_cost_JM[month] <- N_JM * fin_val_JM * Interest_rate  
        Capital_JM <- res_vec$Capital_cost_JM[month]
     
        res_vec$Capital_cost_SubAF[month] <-N_SubAF * fin_val_SubAF * Interest_rate  
        Capital_SubAF = res_vec$Capital_cost_SubAF[month]
     
        res_vec$Capital_cost_SubAM[month] <- N_SubAM * fin_val_SubAM * Interest_rate  
        Capital_SubAM <- res_vec$Capital_cost_SubAM[month]
     
        res_vec$Capital_cost_AF[month] <- N_AF * fin_val_AF * Interest_rate  
        Capital_AF <- res_vec$Capital_cost_AF[month]
     
        res_vec$Capital_cost_AM[month] <- N_AM * fin_val_AM * Interest_rate  
        Capital_AM <- res_vec$Capital_cost_AM[month]
     
        res_vec$Capital_cost[month] <- sum(res_vec$Capital_cost_JF[month],
                                       res_vec$Capital_cost_JM[month],
                                       res_vec$Capital_cost_SubAF[month],
                                       res_vec$Capital_cost_SubAM[month],
                                       res_vec$Capital_cost_AF[month],
                                       res_vec$Capital_cost_AM[month])
        
        Capital <- res_vec$Capital_cost[month]
      }
     
      if (species == "swine") {
        res_vec$Capital_cost_GF[month] <- N_GF * fin_val_GF * Interest_rate  
        Capital_GF <- res_vec$Capital_cost_GF[month]
        
        res_vec$Capital_cost_GM[month] <- N_GM * fin_val_GM * Interest_rate  
        Capital_GM <- res_vec$Capital_cost_GM[month]
       
         res_vec$Capital_cost_GltF[month] <- N_GltF * fin_val_GltF * Interest_rate  
        Capital_GltF <- res_vec$Capital_cost_GltF[month]
        
        res_vec$Capital_cost_GltM[month] <- N_GltM * fin_val_GltM * Interest_rate  
        Capital_GltM <- res_vec$Capital_cost_GltM[month]
        
        res_vec$Capital_cost[month] <- res_vec$Capital_cost[month] +
          res_vec$Capital_cost_GF[month] +
          res_vec$Capital_cost_GM[month] +
          res_vec$Capital_cost_GltF[month] +
          res_vec$Capital_cost_GltM[month] 
        }
      
      
     if (species == "cattle") {
       res_vec$Capital_cost_Ox[month] <- N_Ox * fin_val_Ox * Interest_rate
       Capital_Ox <- res_vec$Capital_cost_Ox[month]
       res_vec$Capital_cost[month] <- res_vec$Capital_cost[month] + res_vec$Capital_cost_Ox[month]
       Capital <- res_vec$Capital_cost[month]
     }
        
    
        
    if (species == "cattle" || species == "smallruminants" || species == "poultry" || species == "equids"|| species == "swine") {
    
       ### For equids this is fixed cost including barn and cart maintenance
     res_vec$Infrastructure_cost_JF[month] <- N_JF * sample(Infrastructure_per_head_JF, 1)
     res_vec$Infrastructure_cost_JM[month] <- N_JM * sample(Infrastructure_per_head_JM, 1)
     res_vec$Infrastructure_cost_SubAF[month] <- N_SubAF * sample(Infrastructure_per_head_SubAF, 1)
     res_vec$Infrastructure_cost_SubAM[month] <- N_SubAM * sample(Infrastructure_per_head_SubAM, 1)
     res_vec$Infrastructure_cost_AF[month] <- N_AF * sample(Infrastructure_per_head_AF, 1)
     res_vec$Infrastructure_cost_AM[month] <- N_AM * sample(Infrastructure_per_head_AM, 1)
     
     res_vec$Infrastructure_cost[month] <- sum(res_vec$Infrastructure_cost_JF[month],
                                               res_vec$Infrastructure_cost_JM[month],
                                               res_vec$Infrastructure_cost_SubAF[month],
                                               res_vec$Infrastructure_cost_SubAM[month],
                                               res_vec$Infrastructure_cost_AF[month],
                                               res_vec$Infrastructure_cost_AM[month])
    }
     
      if (species == "swine") {
        res_vec$Infrastructure_cost_GF[month] <- N_GF * sample(Infrastructure_per_head_GF, 1)
        res_vec$Infrastructure_cost_GM[month] <- N_GM * sample(Infrastructure_per_head_GM, 1)
        res_vec$Infrastructure_cost_GltF[month] <- N_GltF * sample(Infrastructure_per_head_GltF, 1)
        res_vec$Infrastructure_cost_GltM[month] <- N_GltM * sample(Infrastructure_per_head_GltM, 1)
        
        res_vec$Infrastructure_cost[month] <- res_vec$Infrastructure_cost[month] + 
          res_vec$Infrastructure_cost_GF[month] +
          res_vec$Infrastructure_cost_GM[month] +
          res_vec$Infrastructure_cost_GltF[month] +
          res_vec$Infrastructure_cost_GltM[month]
      }
      
     if (species == "cattle") {
       res_vec$Infrastructure_cost_Ox[month] <- N_Ox * sample(Infrastructure_per_head_Ox, 1)
       res_vec$Infrastructure_cost[month] <- res_vec$Infrastructure_cost[month] + res_vec$Infrastructure_cost_Ox[month]
     }
      
      ## ACCESSORY COST for equids
     if (species == "equids") {
        
        ### For accessory cost is feeder, harness, ropes etc
        res_vec$Accessory_cost_JF[month] <- N_JF * sample(Acce_cost_head_JF, 1)
        res_vec$Accessory_cost_JM[month] <- N_JM * sample(Acce_cost_head_JM, 1)
        res_vec$Accessory_cost_SubAF[month] <- N_SubAF * sample(Acce_cost_head_SubAF, 1)
        res_vec$Accessory_cost_SubAM[month] <- N_SubAM * sample(Acce_cost_head_SubAM, 1)
        res_vec$Accessory_cost_AF[month] <- N_AF * sample(Acce_cost_head_AF, 1)
        res_vec$Accessory_cost_AM[month] <- N_AM * sample(Acce_cost_head_AM, 1)
        
        res_vec$Accessory_cost[month] <- sum(res_vec$Accessory_cost_JF[month],
                                                  res_vec$Accessory_cost_JM[month],
                                                  res_vec$Accessory_cost_SubAF[month],
                                                  res_vec$Accessory_cost_SubAM[month],
                                                  res_vec$Accessory_cost_AF[month],
                                                  res_vec$Accessory_cost_AM[month])
      }
      
        # Total expenditure
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
        
        res_vec$Total_expenditure[month] <- res_vec$Feed_cost[month] + res_vec$Health_cost[month] + Labour + Capital + res_vec$Infrastructure_cost[month] + res_vec$Accessory_cost[month]
        
        res_vec$Total_expenditure_JF[month] <- Feed_JF + res_vec$Health_cost_JF[month] + Labour_JF + Capital_JF + res_vec$Infrastructure_cost_JF[month] + res_vec$Accessory_cost_JF[month]
        res_vec$Total_expenditure_JM[month] <- Feed_JM + res_vec$Health_cost_JM[month] + Labour_JM + Capital_JM + res_vec$Infrastructure_cost_JM[month] + res_vec$Accessory_cost_JM[month]
        res_vec$Total_expenditure_SubAF[month] <- Feed_SubAF + res_vec$Health_cost_SubAF[month] + Labour_SubAF + Capital_SubAF + res_vec$Infrastructure_cost_SubAF[month] + res_vec$Accessory_cost_SubAF[month]
        res_vec$Total_expenditure_SubAM[month] <- Feed_SubAM + res_vec$Health_cost_SubAM[month] + Labour_SubAM + Capital_SubAM + res_vec$Infrastructure_cost_SubAM[month] + res_vec$Accessory_cost_SubAM[month]
        res_vec$Total_expenditure_AF[month] <- Feed_AF + res_vec$Health_cost_AF[month] + Labour_AF + Capital_AF + res_vec$Infrastructure_cost_AF[month] + res_vec$Accessory_cost_AF[month]
        res_vec$Total_expenditure_AM[month] <- Feed_AM + res_vec$Health_cost_AM[month] + Labour_AM + Capital_AM + res_vec$Infrastructure_cost_AM[month] + res_vec$Accessory_cost_AM[month]
        
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
    res_mat$milk_ME_MJ[i, ] <- res_vec$milk_ME_MJ
    res_mat$milk_DM_req[i, ] <- res_vec$milk_DM_req
    ## milk VALUEADDED
    res_mat$Value_milk[i, ] <- res_vec$Value_milk 

    # res_mat$Quantity_Wool[i, ] <- res_vec$Quantity_Wool
    
    res_mat$Cumulative_dry_matter[i, ] <- res_vec$Cumulative_dry_matter
    
    res_mat$Cumulative_dry_matter_JF[i, ] <- res_vec$Cumulative_dry_matter_JF
    res_mat$Cumulative_dry_matter_JM[i, ] <- res_vec$Cumulative_dry_matter_JM
    res_mat$Cumulative_dry_matter_SubAF[i, ] <- res_vec$Cumulative_dry_matter_SubAF
    res_mat$Cumulative_dry_matter_SubAM[i, ] <- res_vec$Cumulative_dry_matter_SubAM
    res_mat$Cumulative_dry_matter_AF[i, ] <- res_vec$Cumulative_dry_matter_AF 
    res_mat$Cumulative_dry_matter_AM[i, ] <- res_vec$Cumulative_dry_matter_AM
    
    ## 
    res_mat$Cumulative_Methane_Prod_KG_JF[i, ] <- res_vec$Cumulative_dry_matter_JF * sample(Methane_per_kgDMI_JF, 1)
    res_mat$Cumulative_Methane_Prod_KG_JM[i, ] <- res_vec$Cumulative_dry_matter_JM * sample(Methane_per_kgDMI_JM, 1)
    res_mat$Cumulative_Methane_Prod_KG_SubAF[i, ] <- res_vec$Cumulative_dry_matter_SubAF * sample(Methane_per_kgDMI_SubAF, 1)
    res_mat$Cumulative_Methane_Prod_KG_SubAM[i, ] <- res_vec$Cumulative_dry_matter_SubAM * sample(Methane_per_kgDMI_SubAM, 1)
    res_mat$Cumulative_Methane_Prod_KG_AF[i, ] <- res_vec$Cumulative_dry_matter_AF * sample(Methane_per_kgDMI_AF, 1)
    res_mat$Cumulative_Methane_Prod_KG_AM[i, ] <- res_vec$Cumulative_dry_matter_AM * sample(Methane_per_kgDMI_AM, 1)
    
    res_mat$Cumulative_Methane_Prod_KG[i, ] <- res_mat$Cumulative_Methane_Prod_KG_JF[i, ] + 
      res_mat$Cumulative_Methane_Prod_KG_JM[i, ] +
      res_mat$Cumulative_Methane_Prod_KG_SubAF[i, ] +
      res_mat$Cumulative_Methane_Prod_KG_SubAM[i, ] +
      res_mat$Cumulative_Methane_Prod_KG_AF[i, ] +
      res_mat$Cumulative_Methane_Prod_KG_AM[i, ]
    
    
    res_mat$Feed_required_KG[i, ] <- res_vec$Feed_required_KG
    res_mat$Feed_required_KG_JF[i, ] <- res_vec$Feed_required_KG_JF
    res_mat$Feed_required_KG_JM[i, ] <- res_vec$Feed_required_KG_JM
    res_mat$Feed_required_KG_SubAF[i, ] <- res_vec$Feed_required_KG_SubAF
    res_mat$Feed_required_KG_SubAM[i, ] <- res_vec$Feed_required_KG_SubAM
    res_mat$Feed_required_KG_AF[i, ] <- res_vec$Feed_required_KG_AF
    res_mat$Feed_required_KG_AM[i, ] <- res_vec$Feed_required_KG_AM
    
    res_mat$Purchased_Feed_KG[i, ] <- res_vec$Purchased_Feed_KG
    res_mat$Purchased_Feed_KG_JF[i, ] <- res_vec$Purchased_Feed_KG_JF
    res_mat$Purchased_Feed_KG_JM[i, ] <- res_vec$Purchased_Feed_KG_JM
    res_mat$Purchased_Feed_KG_SubAF[i, ] <- res_vec$Purchased_Feed_KG_SubAF
    res_mat$Purchased_Feed_KG_SubAM[i, ] <- res_vec$Purchased_Feed_KG_SubAM
    res_mat$Purchased_Feed_KG_AF[i, ] <- res_vec$Purchased_Feed_KG_AF
    res_mat$Purchased_Feed_KG_AM[i, ] <- res_vec$Purchased_Feed_KG_AM
    
    res_mat$Foraged_Feed_KG[i, ] <- res_vec$Foraged_Feed_KG
    res_mat$Foraged_Feed_KG_JF[i, ] <- res_vec$Foraged_Feed_KG_JF
    res_mat$Foraged_Feed_KG_JM[i, ] <- res_vec$Foraged_Feed_KG_JM
    res_mat$Foraged_Feed_KG_SubAF[i, ] <- res_vec$Foraged_Feed_KG_SubAF
    res_mat$Foraged_Feed_KG_SubAM[i, ] <- res_vec$Foraged_Feed_KG_SubAM
    res_mat$Foraged_Feed_KG_AF[i, ] <- res_vec$Foraged_Feed_KG_AF
    res_mat$Foraged_Feed_KG_AM[i, ] <- res_vec$Foraged_Feed_KG_AM
    
    res_mat$Foraged_Feed_Value[i, ] <- res_vec$Foraged_Feed_Value
    res_mat$Foraged_Feed_Value_JF[i, ] <- res_vec$Foraged_Feed_Value_JF
    res_mat$Foraged_Feed_Value_JM[i, ] <- res_vec$Foraged_Feed_Value_JM
    res_mat$Foraged_Feed_Value_SubAF[i, ] <- res_vec$Foraged_Feed_Value_SubAF
    res_mat$Foraged_Feed_Value_SubAM[i, ] <- res_vec$Foraged_Feed_Value_SubAM
    res_mat$Foraged_Feed_Value_AF[i, ] <- res_vec$Foraged_Feed_Value_AF
    res_mat$Foraged_Feed_Value_AM[i, ] <- res_vec$Foraged_Feed_Value_AM
    
    res_mat$ME_required_MJ[i, ] <- res_vec$ME_required_MJ + res_vec$milk_ME_MJ
    res_mat$ME_required_MJ_JF[i, ] <- res_vec$ME_required_MJ_JF
    res_mat$ME_required_MJ_JM[i, ] <- res_vec$ME_required_MJ_JM
    res_mat$ME_required_MJ_SubAF[i, ] <- res_vec$ME_required_MJ_SubAF
    res_mat$ME_required_MJ_SubAM[i, ] <- res_vec$ME_required_MJ_SubAM
    res_mat$ME_required_MJ_AF[i, ] <- res_vec$ME_required_MJ_AF + res_vec$milk_ME_MJ
    res_mat$ME_required_MJ_AM[i, ] <- res_vec$ME_required_MJ_AM
    
    res_mat$Value_offtake[i, ] <- res_vec$Value_offtake

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
      res_mat$Cumulative_Methane_Prod_KG_Ox[i, ] <- res_mat$Cumulative_dry_matter_Ox[i, ] * sample(Methane_per_kgDMI_Ox, 1)
      res_mat$Cumulative_Methane_Prod_KG[i, ] <- res_mat$Cumulative_Methane_Prod_KG[i, ] + 
        res_mat$Cumulative_Methane_Prod_KG_Ox[i, ]
        
      res_mat$Feed_required_KG_Ox[i, ] <- res_vec$Feed_required_KG_Ox
      res_mat$Purchased_Feed_KG_Ox[i, ] <- res_vec$Purchased_Feed_KG_Ox
      res_mat$Foraged_Feed_KG_Ox[i, ] <- res_vec$Foraged_Feed_KG_Ox
      res_mat$Foraged_Feed_Value_Ox[i, ] <- res_vec$Foraged_Feed_Value_Ox
      res_mat$ME_required_MJ_Ox[i, ] <- res_vec$ME_required_MJ_Ox

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
    
    ### Add SWINE Matrices to fill
    
    if (species == "swine") {
      res_mat$Num_GF[i, ] <- res_vec$Num_GF
      res_mat$Deaths_GF[i, ] <- res_vec$Deaths_GF
      res_mat$Total_mortality_GF[i, ] <- res_vec$Total_mortality_GF
      res_mat$Quantity_liveweight_kg_GF[i, ] <- res_vec$Quantity_liveweight_kg_GF
      res_mat$Num_offtake_GF[i, ] <- res_vec$Num_offtake_GF
      res_mat$Offtake_liveweight_kg_GF[i, ] <- res_vec$Offtake_liveweight_kg_GF
      res_mat$Pop_growth_GF[i, ] <- res_vec$Pop_growth_GF
      res_mat$Quantity_manure_GF[i, ] <- res_vec$Quantity_manure_GF
      res_mat$Value_manure_GF[i, ] <- res_vec$Value_manure_GF ## added here
      res_mat$Quantity_hides_GF[i, ] <- res_vec$Quantity_hides_GF
      res_mat$Value_hides_GF[i, ] <- res_vec$Value_hides_GF ## added here
      res_mat$Cumulative_dry_matter_GF[i, ] <- res_vec$Cumulative_dry_matter_GF
      res_mat$Cumulative_Methane_Prod_KG_GF[i, ] <- res_mat$Cumulative_dry_matter_GF[i, ] * sample(Methane_per_kgDMI_GF, 1)
      
      res_mat$Feed_required_KG_GF[i, ] <- res_vec$Feed_required_KG_GF
      res_mat$Purchased_Feed_KG_GF[i, ] <- res_vec$Purchased_Feed_KG_GF
      res_mat$Foraged_Feed_KG_GF[i, ] <- res_vec$Foraged_Feed_KG_GF
      res_mat$Foraged_Feed_Value_GF[i, ] <- res_vec$Foraged_Feed_Value_GF
      res_mat$ME_required_MJ_GF[i, ] <- res_vec$ME_required_MJ_GF
      
      res_mat$Value_offtake_GF[i, ] <- res_vec$Value_offtake_GF
      res_mat$Value_herd_increase_GF[i, ] <- res_vec$Value_herd_increase_GF
      res_mat$Total_value_increase_GF[i, ] <- res_vec$Total_value_increase_GF
      res_mat$Feed_cost_GF[i, ] <- res_vec$Feed_cost_GF
      res_mat$Labour_cost_GF[i, ] <- res_vec$Labour_cost_GF
      res_mat$Health_cost_GF[i, ] <- res_vec$Health_cost_GF
      res_mat$Capital_cost_GF[i, ] <- res_vec$Capital_cost_GF
      res_mat$Infrastructure_cost_GF[i, ] <- res_vec$Infrastructure_cost_GF
      res_mat$Total_expenditure_GF[i, ] <- res_vec$Total_expenditure_GF     
      
      res_mat$Num_GM[i, ] <- res_vec$Num_GM
      res_mat$Deaths_GM[i, ] <- res_vec$Deaths_GM
      
      res_mat$Total_mortality_GM[i, ] <- res_vec$Total_mortality_GM
      res_mat$Quantity_liveweight_kg_GM[i, ] <- res_vec$Quantity_liveweight_kg_GM
      res_mat$Num_offtake_GM[i, ] <- res_vec$Num_offtake_GM
      res_mat$Offtake_liveweight_kg_GM[i, ] <- res_vec$Offtake_liveweight_kg_GM
      res_mat$Pop_growth_GM[i, ] <- res_vec$Pop_growth_GM
      res_mat$Quantity_manure_GM[i, ] <- res_vec$Quantity_manure_GM
      res_mat$Value_manure_GM[i, ] <- res_vec$Value_manure_GM ## added here
      res_mat$Quantity_hides_GM[i, ] <- res_vec$Quantity_hides_GM
      res_mat$Value_hides_GM[i, ] <- res_vec$Value_hides_GM ## added here
      res_mat$Cumulative_dry_matter_GM[i, ] <- res_vec$Cumulative_dry_matter_GM
      res_mat$Cumulative_Methane_Prod_KG_GM[i, ] <- res_mat$Cumulative_dry_matter_GM[i, ] * sample(Methane_per_kgDMI_GM, 1)
      
      res_mat$Feed_required_KG_GM[i, ] <- res_vec$Feed_required_KG_GM
      res_mat$Purchased_Feed_KG_GM[i, ] <- res_vec$Purchased_Feed_KG_GM
      res_mat$Foraged_Feed_KG_GM[i, ] <- res_vec$Foraged_Feed_KG_GM
      res_mat$Foraged_Feed_Value_GM[i, ] <- res_vec$Foraged_Feed_Value_GM
      res_mat$ME_required_MJ_GM[i, ] <- res_vec$ME_required_MJ_GM
      
      res_mat$Value_offtake_GM[i, ] <- res_vec$Value_offtake_GM
      res_mat$Value_herd_increase_GM[i, ] <- res_vec$Value_herd_increase_GM
      res_mat$Total_value_increase_GM[i, ] <- res_vec$Total_value_increase_GM
      res_mat$Feed_cost_GM[i, ] <- res_vec$Feed_cost_GM
      res_mat$Labour_cost_GM[i, ] <- res_vec$Labour_cost_GM
      res_mat$Health_cost_GM[i, ] <- res_vec$Health_cost_GM
      res_mat$Capital_cost_GM[i, ] <- res_vec$Capital_cost_GM
      res_mat$Infrastructure_cost_GM[i, ] <- res_vec$Infrastructure_cost_GM
      res_mat$Total_expenditure_GM[i, ] <- res_vec$Total_expenditure_GM
      
      ## Gilt Females
      res_mat$Num_GltF[i, ] <- res_vec$Num_GltF
      res_mat$Deaths_GltF[i, ] <- res_vec$Deaths_GltF
      
      res_mat$Total_mortality_GltF[i, ] <- res_vec$Total_mortality_GltF
      res_mat$Quantity_liveweight_kg_GltF[i, ] <- res_vec$Quantity_liveweight_kg_GltF
      res_mat$Num_offtake_GltF[i, ] <- res_vec$Num_offtake_GltF
      res_mat$Offtake_liveweight_kg_GltF[i, ] <- res_vec$Offtake_liveweight_kg_GltF
      res_mat$Pop_growth_GltF[i, ] <- res_vec$Pop_growth_GltF
      res_mat$Quantity_manure_GltF[i, ] <- res_vec$Quantity_manure_GltF
      res_mat$Value_manure_GltF[i, ] <- res_vec$Value_manure_GltF ## added here
      res_mat$Quantity_hides_GltF[i, ] <- res_vec$Quantity_hides_GltF
      res_mat$Value_hides_GltF[i, ] <- res_vec$Value_hides_GltF ## added here
      res_mat$Cumulative_dry_matter_GltF[i, ] <- res_vec$Cumulative_dry_matter_GltF
      res_mat$Cumulative_Methane_Prod_KG_GltF[i, ] <- res_mat$Cumulative_dry_matter_GltF[i, ] * sample(Methane_per_kgDMI_GltF, 1)
      
      res_mat$Feed_required_KG_GltF[i, ] <- res_vec$Feed_required_KG_GltF
      res_mat$Purchased_Feed_KG_GltF[i, ] <- res_vec$Purchased_Feed_KG_GltF
      res_mat$Foraged_Feed_KG_GltF[i, ] <- res_vec$Foraged_Feed_KG_GltF
      res_mat$Foraged_Feed_Value_GltF[i, ] <- res_vec$Foraged_Feed_Value_GltF
      res_mat$ME_required_MJ_GltF[i, ] <- res_vec$ME_required_MJ_GltF
      
      res_mat$Value_offtake_GltF[i, ] <- res_vec$Value_offtake_GltF
      res_mat$Value_herd_increase_GltF[i, ] <- res_vec$Value_herd_increase_GltF
      res_mat$Total_value_increase_GltF[i, ] <- res_vec$Total_value_increase_GltF
      res_mat$Feed_cost_GltF[i, ] <- res_vec$Feed_cost_GltF
      res_mat$Labour_cost_GltF[i, ] <- res_vec$Labour_cost_GltF
      res_mat$Health_cost_GltF[i, ] <- res_vec$Health_cost_GltF
      res_mat$Capital_cost_GltF[i, ] <- res_vec$Capital_cost_GltF
      res_mat$Infrastructure_cost_GltF[i, ] <- res_vec$Infrastructure_cost_GltF
      res_mat$Total_expenditure_GltF[i, ] <- res_vec$Total_expenditure_GltF     
      
      ## Gilt Males
      res_mat$Num_GltM[i, ] <- res_vec$Num_GltM
      res_mat$Deaths_GltM[i, ] <- res_vec$Deaths_GltM
      
      res_mat$Total_mortality_GltM[i, ] <- res_vec$Total_mortality_GltM
      res_mat$Quantity_liveweight_kg_GltM[i, ] <- res_vec$Quantity_liveweight_kg_GltM
      res_mat$Num_offtake_GltM[i, ] <- res_vec$Num_offtake_GltM
      res_mat$Offtake_liveweight_kg_GltM[i, ] <- res_vec$Offtake_liveweight_kg_GltM
      res_mat$Pop_growth_GltM[i, ] <- res_vec$Pop_growth_GltM
      res_mat$Quantity_manure_GltM[i, ] <- res_vec$Quantity_manure_GltM
      res_mat$Value_manure_GltM[i, ] <- res_vec$Value_manure_GltM ## added here
      res_mat$Quantity_hides_GltM[i, ] <- res_vec$Quantity_hides_GltM
      res_mat$Value_hides_GltM[i, ] <- res_vec$Value_hides_GltM ## added here
      res_mat$Cumulative_dry_matter_GltM[i, ] <- res_vec$Cumulative_dry_matter_GltM
      res_mat$Cumulative_Methane_Prod_KG_GltM[i, ] <- res_mat$Cumulative_dry_matter_GltM[i, ] * sample(Methane_per_kgDMI_GltM, 1)
      
      res_mat$Feed_required_KG_GltM[i, ] <- res_vec$Feed_required_KG_GltM
      res_mat$Purchased_Feed_KG_GltM[i, ] <- res_vec$Purchased_Feed_KG_GltM
      res_mat$Foraged_Feed_KG_GltM[i, ] <- res_vec$Foraged_Feed_KG_GltM
      res_mat$Foraged_Feed_Value_GltM[i, ] <- res_vec$Foraged_Feed_Value_GltM
      res_mat$ME_required_MJ_GltM[i, ] <- res_vec$ME_required_MJ_GltM
      
      res_mat$Value_offtake_GltM[i, ] <- res_vec$Value_offtake_GltM
      res_mat$Value_herd_increase_GltM[i, ] <- res_vec$Value_herd_increase_GltM
      res_mat$Total_value_increase_GltM[i, ] <- res_vec$Total_value_increase_GltM
      res_mat$Feed_cost_GltM[i, ] <- res_vec$Feed_cost_GltM
      res_mat$Labour_cost_GltM[i, ] <- res_vec$Labour_cost_GltM
      res_mat$Health_cost_GltM[i, ] <- res_vec$Health_cost_GltM
      res_mat$Capital_cost_GltM[i, ] <- res_vec$Capital_cost_GltM
      res_mat$Infrastructure_cost_GltM[i, ] <- res_vec$Infrastructure_cost_GltM
      res_mat$Total_expenditure_GltM[i, ] <- res_vec$Total_expenditure_GltM
      
      res_mat$Cumulative_Methane_Prod_KG[i, ] <- res_mat$Cumulative_Methane_Prod_KG[i, ] + 
        res_mat$Cumulative_Methane_Prod_KG_GF[i, ] +
        res_mat$Cumulative_Methane_Prod_KG_GM[i, ] +
        res_mat$Cumulative_Methane_Prod_KG_GltF[i, ] +
        res_mat$Cumulative_Methane_Prod_KG_GltM[i, ] 

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
      
      res_mat$donkey_power_income[i, ] <- res_vec$donkey_power_income
      res_mat$donkey_power_income_SubAF[i, ] <- res_vec$donkey_power_income_SubAF
      res_mat$donkey_power_income_SubAM[i, ] <- res_vec$donkey_power_income_SubAM
      res_mat$donkey_power_income_AF[i, ] <- res_vec$donkey_power_income_AF
      res_mat$donkey_power_income_AM[i, ] <- res_vec$donkey_power_income_AM
      
      res_mat$working_hours_commercial[i, ] <- res_vec$working_hours_commercial
      res_mat$working_hours_commercial_AF[i, ] <- res_vec$working_hours_commercial_AF
      res_mat$working_hours_commercial_AM[i, ] <- res_vec$working_hours_commercial_AM
      
      res_mat$working_hours_unpaid[i, ] <- res_vec$working_hours_unpaid
      res_mat$working_hours_unpaid_SubAF[i, ] <- res_vec$working_hours_unpaid_SubAF
      res_mat$working_hours_unpaid_SubAM[i, ] <- res_vec$working_hours_unpaid_SubAM      
      res_mat$working_hours_unpaid_AF[i, ] <- res_vec$working_hours_unpaid_AF
      res_mat$working_hours_unpaid_AM[i, ] <- res_vec$working_hours_unpaid_AM
      
      res_mat$working_hours_combined[i, ] <- res_vec$working_hours_combined
      res_mat$working_hours_combined_SubAF[i, ] <- res_vec$working_hours_combined_SubAF
      res_mat$working_hours_combined_SubAM[i, ] <- res_vec$working_hours_combined_SubAM      
      res_mat$working_hours_combined_AF[i, ] <- res_vec$working_hours_combined_AF
      res_mat$working_hours_combined_AM[i, ] <- res_vec$working_hours_combined_AM
      
      res_mat$working_hours[i, ] <- res_vec$working_hours
      
    }
    
    
  } # end nruns loop

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
  
  if (species == "cattle") {
    Total_number_change_Ox <- res_mat$Num_offtake_Ox + res_mat$Pop_growth_Ox
    Total_number_change <- res_mat$Total_number_change + res_mat$Total_number_change_Ox
  }
  
  if (species == "swine"){
    Total_number_change_GF <- res_mat$Num_offtake_GF + res_mat$Pop_growth_GF
    Total_number_change_GM <- res_mat$Num_offtake_GM + res_mat$Pop_growth_GM
    Total_number_change_GltF <- res_mat$Num_offtake_GltF + res_mat$Pop_growth_GltF
    Total_number_change_GltM <- res_mat$Num_offtake_GltM + res_mat$Pop_growth_GltM
    
    Total_number_change <- res_mat$Total_number_change + 
      Total_number_change_GF +
      Total_number_change_GM +
      Total_number_change_GltF +
      Total_number_change_GltM
  }
  
  if (species == "cattle" || species == "smallruminants" || species == "poultry" || species == "swine") {
  

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
  
  if (species == "swine") {
    res_mat$Production_value_herd_offtake_hide_manure_GF <- res_mat$Total_value_increase_GF + res_mat$Value_manure_GF + res_mat$Value_hides_GF 
    res_mat$Production_value_herd_offtake_hide_manure_GM <- res_mat$Total_value_increase_GM + res_mat$Value_manure_GM + res_mat$Value_hides_GM
    res_mat$Production_value_herd_offtake_hide_manure_GltF <- res_mat$Total_value_increase_GltF + res_mat$Value_manure_GltF + res_mat$Value_hides_GltF 
    res_mat$Production_value_herd_offtake_hide_manure_GltM <- res_mat$Total_value_increase_GltM + res_mat$Value_manure_GltM + res_mat$Value_hides_GltM
    
    res_mat$Production_value_herd_offtake_hide_manure  <- res_mat$Production_value_herd_offtake_hide_manure +
      res_mat$Production_value_herd_offtake_hide_manure_GF +
      res_mat$Production_value_herd_offtake_hide_manure_GM +
      res_mat$Production_value_herd_offtake_hide_manure_GltF +
      res_mat$Production_value_herd_offtake_hide_manure_GltM
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
    
    res_mat$Production_value_herd_offtake_hide_manure_JF <- res_mat$Total_value_increase_JF + res_mat$Value_manure_JF
    res_mat$Production_value_herd_offtake_hide_manure_JM <- res_mat$Total_value_increase_JM + res_mat$Value_manure_JM
    res_mat$Production_value_herd_offtake_hide_manure_SubAF <- res_mat$Total_value_increase_SubAF + res_mat$Value_manure_SubAF + res_mat$Value_hides_SubAF + res_mat$donkey_power_income_SubAF
    res_mat$Production_value_herd_offtake_hide_manure_SubAM <- res_mat$Total_value_increase_SubAM + res_mat$Value_manure_SubAM + res_mat$Value_hides_SubAM + res_mat$donkey_power_income_SubAM
    res_mat$Production_value_herd_offtake_hide_manure_AF <- res_mat$Total_value_increase_AF + res_mat$Value_manure_AF + res_mat$Value_hides_AF + res_mat$donkey_power_income_AF
    res_mat$Production_value_herd_offtake_hide_manure_AM <- res_mat$Total_value_increase_AM + res_mat$Value_manure_AM + res_mat$Value_hides_AM + res_mat$donkey_power_income_AM
    
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
  
  if (species == "swine") {
    res_mat$Gross_margin_GF <- res_mat$Production_value_herd_offtake_hide_manure_GF - res_mat$Total_expenditure_GF
    res_mat$Gross_margin_GM <- res_mat$Production_value_herd_offtake_hide_manure_GM - res_mat$Total_expenditure_GM
    res_mat$Gross_margin_GltF <- res_mat$Production_value_herd_offtake_hide_manure_GltF - res_mat$Total_expenditure_GltF
    res_mat$Gross_margin_GltM <- res_mat$Production_value_herd_offtake_hide_manure_GltM - res_mat$Total_expenditure_GltM
  }
  
  
  if (species == "equids") {
    
    res_mat$Enterprise_budget <- res_mat$Gross_margin - res_mat$Infrastructure_cost
  }

  
  ### output results to a CSV file ###
  

   ## output from last month (cumulative total) ##
    
    apply_last_column <- function(mat) {
      mat_last_column <- mat[, ncol(mat), drop = TRUE]  
      mat_last_column
    }
    
    mat_list <- lapply(res_mat, apply_last_column)
    
    df_cumulative_total <- as.data.frame(do.call(rbind, mat_list))
    
    rownames(df_cumulative_total) <- names(mat_list)
    colnames(df_cumulative_total) <- paste("Run", 1:ncol(df_cumulative_total))
    df_cumulative_total
    
    
    ## Summary statistics ##
    
    apply_summary_last_column <- function(mat) {
      mat_summary <- summary(mat[, ncol(mat)])
      mat_sd <- sd(mat[, ncol(mat)])
      mat_summary <- c(mat_summary, SD = mat_sd)
    }
    
    summary_list <- lapply(res_mat, apply_summary_last_column)
    
    df_summary <- as.data.frame(do.call(rbind, summary_list))
    
    df_summary$Variable <- rownames(df_summary)
    
    df_summary <- df_summary[, c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.", "SD")]
    df_summary # summary statistics across all months
    
    list(
      cumulative_total = df_cumulative_total,  
      summary = df_summary      
    )
  
} # end function