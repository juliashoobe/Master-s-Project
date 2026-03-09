heart_trop_calculation <- function(cohort_df) {
  
  # -------------------------------
  # Step 1: Assign H.E.A.R.T. troponin score
  # -------------------------------
  heart_trop <- cohort_df |> 
    mutate(
      trop_score = (
        case_when(
          RESULT_NUM < 6 ~ 0,
          RESULT_NUM >= 6 & RESULT_NUM <= 51 ~ 1,
          RESULT_NUM > 51 ~ 2
        )
      )
    ) |> 
    select(ENCOUNTERID, RESULT_NUM, trop, trop_score)
  
  return(heart_trop)
}
