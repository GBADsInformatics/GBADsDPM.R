calculate_biomass <- function() {

  # Initial values
  NF <- N_NF_t0 * lwNF
  NM <- N_NM_t0 * lwNM
  JF <- N_JF_t0 * lwJF
  JM <- N_JM_t0 * lwJM
  AF <- N_AF_t0 * lwAF
  AM <- N_AM_t0 * lwAM
  
  res <- list("Neonatal Females" = NF,
              "Neonatal Males" = NM,
              "Juvenile Females" = JF,
              "Juvenile Males" = JM,
              "Adult Femles" = AF,
              "Adult Males" = AM)
  return(res)

}


calculate_biomass()

