combine_ec1 <- function(structured_df, notes_df) {
  
  # -------------------------------
  # Step 1: Merge structured and notes-based EC1 determinations
  # -------------------------------
  # structured_df: contains EC1_structured (from diagnosis + meds)
  # notes_df: contains EC1_notes (from LLM analysis of clinical notes)
  combined <- structured_df |> 
    left_join(notes_df, by = "ENCOUNTERID") |> 
    
    
  # -------------------------------
  # Step 2: Determine final EC1 classification
  # -------------------------------
  # An encounter is EC1 if either structured or notes-based assessment
  # indicates EC1; otherwise it is 'Included'
    mutate(
      EC1 = case_when(
       EC1_structured == "EC1" | EC1_notes == "EC1" ~ "EC1",
       is.na(EC1_structured) ~ EC1_notes,
       EC1_structured == "Included" & EC1_notes == "No EKG result found." ~ "No EKG result found.",
       TRUE ~ "Included"
      )
      ) |> 
    select(PATID, ENCOUNTERID, EC1)
  
  return(combined)
}
