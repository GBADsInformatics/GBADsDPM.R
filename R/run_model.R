run_model <- function() {
  model_env <- new.env()  # create a new environment
  
  add_variable <- function(env, variable_name) {
    env[[variable_name]] <- 0
  }
  
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
  
  # vectors of categories for animals and cost types
  animal_categories <- c("NF", "NM", "JF", "JM", "AF", "AM", "O")
  cost_categories <- c("Liveweight_kg", "Offtake", "Offtake_Liveweight", "Manure_kg", 
                       "Hides", "Milk", "Meat_kg", "Wool", "draught_income",
                       "Cumulutive_DM", "Monthly_Dry_Matter",
                       "popultation_growth_rate", "Monthly_growth_rate", "monthly_pop_growth",
                       "Value_offt", "Value_herd_inc",
                       "Feed", "Labour", "Health", "Capital")
  
  # Create empty variables
  for (animal in animal_categories) {
    for (cost in cost_categories) {
      variable_name <- paste(cost, "_", animal, sep = "")
      model_env[[variable_name]] <- 0
    }
  }
  
  return(model_env)
  
}


