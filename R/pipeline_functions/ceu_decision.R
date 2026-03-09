ceu_decision <- function(heart, ec1, ec2, ec3) {
  
  # -------------------------------
  # Step 1: Merge all relevant datasets
  # -------------------------------
  datasets <- list(heart, ec1, ec2, ec3)
  
  heart <- reduce(datasets, inner_join, by = "ENCOUNTERID") |> 
    left_join(mrn, by = "PATID") |> 
    left_join(test_cohort |> select(ENCOUNTERID, RESULT_DATE), by = "ENCOUNTERID") |> 
    relocate(MRN, .before = ENCOUNTERID) |> 
    relocate(PATID, .before = everything()) |> 
    relocate(RESULT_DATE, .after = ENCOUNTERID)
  
  # -------------------------------
  # Step 2: Compute preliminary CEU eligibility Decision
  # -------------------------------
  decision <- heart |> 
    mutate(
      # Determine Decision based on troponin and HEART score
      Decision = case_when(
        trop == "Normal" & HEART <= 3 ~ "Does not qualify.",
        trop == "Normal" & HEART >= 4 & HEART <= 6 & ekg_score == 0 ~ "Does not qualify.",
        trop == "Normal" & HEART >= 4 & HEART <= 6 & ekg_score > 0 ~ "Qualifies.",
        trop == "Normal" & HEART >= 7 ~ "Qualifies.",
        trop == "Abnormal" & HEART <= 6 ~ "Qualifies.",
        trop == "Abnormal" & HEART >= 7 ~ "Does not qualify."
      ),
      
      # Override Decision if any exclusion criteria are met (EC1-EC3)
      Decision = case_when(
        EC1 == "EC1" ~ "Does not qualify.",
        EC2 == "EC2" ~ "Does not qualify.",
        EC3 == "EC3" ~ "Does not qualify.",
        TRUE ~ Decision
      ),
      
      # -------------------------------
      # Step 3: Generate suggested Instructions
      # -------------------------------
      Instructions = case_when(
        EC1 == "EC1" ~ "EC1, Admit.",
        EC2 == "EC2" ~ "EC2, Admit.",
        EC3 == "EC3" ~ "EC3, Admit.",
        Decision == "Qualifies." ~ "CEU",
        trop == "Normal" & HEART <= 3 ~ "Discharge.",
        trop == "Normal" & HEART >= 4 & HEART <= 6 & ekg_score == 0 ~ "Discharge with follow-up within 72 hours.",
        trop == "Abnormal" & HEART >= 7 ~ "Admit.",

      )
    )
  
  # -------------------------------
  # Step 4: Return final encounter-level CEU decision table
  # -------------------------------
  # Outcome columns include:
  # - ENCOUNTERID, PATID, MRN, RESULT_DATE
  # - HEART score and components
  # - EC1, EC2, EC3 flags
  # - Decision: "Qualifies", or "Does not qualify."
  # - Instructions: clinical guidance based on decision
  return(decision)
}
