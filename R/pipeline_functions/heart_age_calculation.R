heart_age_calculation <- function(cohort_df, demographics_df) {
  
  # -------------------------------
  # Step 1: Merge cohort with demographic data
  # -------------------------------
  # cohort_df: encounter-level info
  # demographics_df: patient-level info
   heart_age <- cohort_df |> 
     left_join(demographics_df |> 
                 select(PATID, BIRTH_DATE), by = "PATID") |> 
     
     # -------------------------------
     # Step 2: Calculate patient age at time of encounter
     # -------------------------------
     mutate(AGE = floor(time_length(interval(BIRTH_DATE, RESULT_DATE), "years"))) |>
     select(-BIRTH_DATE) |> 
     
     # -------------------------------
     # Step 3: Assign H.E.A.R.T. age component score
    # -------------------------------
     mutate(
       age_score = case_when(
         AGE < 45 ~ 0,
         AGE >= 45 & AGE < 65 ~ 1,
         AGE >= 65 ~ 2
       )
     ) |> 
     select(ENCOUNTERID, age_score)
   
   return(heart_age)
}
