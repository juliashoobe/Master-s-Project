create_cohort <- function(encounters_df, labs_df, notes_df) {
  
  # Step 1: Keep only high-sensitivity troponin labs relevant to protocol
  # (baseline, 1-hour, 3-hour, and retest)
  lab_values <- labs_df |> 
    filter(
      RAW_LAB_NAME %in% c("hsTnI Baseline", "hsTnI 1 hr", "hsTnI 3 hr", 
                          "hsTnI Retest"
    )
    )
  
  # Step 2: Create an encounter-level troponin abnormality flag
  # If *any* troponin result within an encounter is abnormal, 
  # the entire encounter is labeled "Abnormal"
  trop_flag <- labs_df |> 
    group_by(ENCOUNTERID) |> 
    summarize(
      trop = if_else(any(RAW_RESULT == "Abnormal"), "Abnormal", "Normal"),
      .groups = "drop"
    )
  
  # Step 3: Build cohort
  cohort <- encounters_df |> 
    select(-PATID) |> 
    
    # Restrict encounters that have qualifying troponin labs
    inner_join(lab_values, by = "ENCOUNTERID") |> 
    
    # Restrict to encounters that have clinical notes available
    filter(ENCOUNTERID %in% notes_df$ENCOUNTERID) |> 
    
    # Within each encounter, select the single highest troponin value
    group_by(ENCOUNTERID) |> 
    slice_max(RESULT_NUM, n = 1, with_ties = FALSE) |> 
    ungroup() |> 
    select(PATID, ENCOUNTERID, RESULT_DATE, RESULT_TIME, RESULT_NUM, RAW_RESULT) |> 
    
    # Attach the encounter-level troponin abnormality flag
    left_join(trop_flag |>  select(ENCOUNTERID, trop),  by = "ENCOUNTERID")
  
  return(cohort)
}
