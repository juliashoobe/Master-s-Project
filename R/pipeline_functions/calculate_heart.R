calculate_heart <- function(history, ekg, age, rf, trop) {
  
  # -------------------------------
  # Step 1: Combine all H.E.A.R.T. component datasets
  # -------------------------------
  datasets <- list(history, ekg, age, rf, trop)
  
  # -------------------------------
  # Step 2: Merge all datasets
  # -------------------------------
  heart <- reduce(datasets, inner_join, by = "ENCOUNTERID") |> 
    
    # -------------------------------
    # Step 3: Compute total H.E.A.R.T score
    # -------------------------------
    mutate(
      HEART = history_score + ekg_score + age_score + rf_score + trop_score
    )
}
