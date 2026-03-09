heart_rf_calculation <- function(cohort_df, conditions_df) {
  
  # -------------------------------
  # Step 0: Define ICD code groups for risk factors
  # -------------------------------
  htn_codes <- c("I10", "I11", "I12", "I13", "I15", "I16", "I1A")
  dm_codes <- c("E08", "E09", "E10", "E11", "E13")
  atherosclerotic_codes <- c("I70", "I25", "G45", "I63", "I65", "I66", "I73.9", 
                             "T82.21", "T82.3", "Z86.73", "Z86.74")
  
  # -------------------------------
  # Step 1: Join patient conditions to cohort encounters
  # -------------------------------
  # Determine if condition is active at time of encounter
  # - valid = condition onset is before or on time of encounter
  # - valid = condition not resolved before time of encounter
  risk_factors <- cohort_df |> 
    left_join(conditions_df |> select(-ENCOUNTERID), by = "PATID") |> 
    mutate(
      valid = (is.na(ONSET_DATE) | ONSET_DATE <= RESULT_DATE) &
        (is.na(RESOLVE_DATE) | RESOLVE_DATE >= RESULT_DATE),
      
      # -------------------------------
      # Step 2: Flag individual risk factors
      # -------------------------------
      hypertension = if_else(
        valid & grepl(paste0("^(", paste(htn_codes, collapse="|"), ")"), CONDITION),
        "yes", "no"
      ),
      
      hypercholesterolemia = if_else(
        valid & grepl("^E78", CONDITION),
        "yes", "no"
      ),
      
      diabetes = if_else(
        valid & grepl(paste0("^(", paste(dm_codes, collapse="|"), ")"), CONDITION),
        "yes", "no"
      ),
      
      obesity = if_else(
        valid & (
          (grepl("^E66", CONDITION) & CONDITION != "E66" & CONDITION != "E66.3") |
            grepl("^Z68\\.3|^Z68\\.4|^Z68\\.54", CONDITION)
        ),
        "yes", "no"
      ),
      
      smoking = if_else(
        valid & (
          (grepl("^F17\\.21", CONDITION) & CONDITION != "F17.211") |
            grepl("^U07\\.0", CONDITION)
        ),
        "yes", "no"
      ),
      
      fam_history = if_else(
        valid & grepl("^Z82\\.4", CONDITION),
        "yes", "no"
      ),
      
      atherosclerotic = if_else(
        valid & grepl(paste0("^(", paste0(atherosclerotic_codes, collapse="|"), ")"), CONDITION),
        "yes", "no"
      )
    )
  
  # -------------------------------
  # Step 3: Collapse to encounter level
  # -------------------------------
  # If any condition flagged "yes" for that encounter, mark as "yes"
  rf <- risk_factors |> 
    group_by(ENCOUNTERID) |> 
    summarize(
      hypertension = if_else(any(hypertension == "yes"), "yes", "no"),
      hypercholesterolemia = if_else(any(hypercholesterolemia == "yes"), "yes", "no"),
      diabetes = if_else(any(diabetes == "yes"), "yes", "no"),
      obesity = if_else(any(obesity == "yes"), "yes", "no"),
      smoking = if_else(any(smoking == "yes"), "yes", "no"),
      fam_history = if_else(any(fam_history == "yes"), "yes", "no"),
      atherosclerotic = if_else(any(atherosclerotic == "yes"), "yes", "no"),
      .groups = "drop"
    ) |> 
    
    # -------------------------------
    # Step 4: Assign H.E.A.R.T. risk factor score
    # -------------------------------
    mutate(
      rf_score = case_when(
        atherosclerotic == "yes" ~ 2,
        rowSums(across(c(hypertension, hypercholesterolemia, diabetes,
                         obesity, smoking, fam_history),
                       ~ . == "yes")) == 0 ~ 0,
        rowSums(across(c(hypertension, hypercholesterolemia, diabetes,
                         obesity, smoking, fam_history),
                       ~ . == "yes")) %in% 1:2 ~ 1,
        TRUE ~ 2
      )
    ) |> 
    select(ENCOUNTERID, rf_score)
  
  return(rf)
}
