ruleout1_structured <- function(cohort_df, diagnosis_df, med_admin_df) {
  
  # -------------------------------
  # DX-based rule-out 
  # -------------------------------
  # Identify encounters with ICD-10 diagnoses consistent with ACS
  # (I21, I22, I24), which automatically triggers EC1 exclusion
  ruleout_dx <- diagnosis_df |> 
    filter(ENCOUNTERID %in% cohort_df$ENCOUNTERID) |> 
    group_by(ENCOUNTERID) |> 
    summarize(
      EC1_dx = if_else(any(str_detect(DX, "^I2(1|2|4)")), "EC1", "Included"),
      .groups = "drop"
    )
  
  # -------------------------------
  # Heparin-based rule-out
  # -------------------------------
  # Identify encounters receiving IV heparin within 6 hours after the 
  # index troponin result
  heparin_list <- c(
    "HEPARIN (PORCINE) IN D5W 25,000 UNIT/250 ML IV ACS/AMI",
    "HEPARIN (PORCINE) IN D5W 25,000 UNIT/250 ML IV AFIB/VALVE",
    "HEPARIN (PORCINE) IN D5W 25,000 UNIT/250 ML IV DVT/PE",
    "HEPARIN (PORCINE) IN D5W 25,000 UNIT/250 ML IV OTHER",
    "HEPARIN (PORCINE) 25,000 UNIT/250 ML (100 UNIT/ML) IN DEXTROSE 5 % IV",
    "HEPARIN (PORCINE) IN D5W 25,000 UNIT/250 ML IV TIA/STROKE",
    "HEPARIN (PORCINE) IN 1/2NS 25,000 UNIT/250 ML IV DVT/PE",
    "HEPARIN (PORCINE) IN D5W 25,000 UNIT/250 ML IV PEDS"
  )
  
  ruleout_heparin <- cohort_df |> 
    mutate(
      result_dt = ymd_hm(paste(RESULT_DATE, RESULT_TIME))
    ) |> 
    left_join(med_admin_df |> 
                select(-PATID) |> 
                mutate(
                  medadmin_dt = ymd_hm(paste(MEDADMIN_START_DATE, MEDADMIN_START_TIME))
                ), 
              by = "ENCOUNTERID") |> 
    filter(MEDADMIN_ROUTE == "INTRAVENOUS",
           medadmin_dt >= result_dt,
           medadmin_dt <= result_dt + hours(6)
           ) |> 
    group_by(ENCOUNTERID) |> 
    summarize(
      EC1_heparin = if_else(
        any(RAW_MEDADMIN_MED_NAME %in% heparin_list), 
                            "EC1", 
                            "Included"
        ),
      .groups = "drop"
    )
  # -------------------------------
  # Combine Structured Components
  # -------------------------------
  # An encounter is classified as EC1 if it meets *either* the diagnosis-based 
  # or heparin-based exclusion criteria
  structured_ec1 <- cohort_df |> 
    left_join(ruleout_dx, by = "ENCOUNTERID") |> 
    left_join(ruleout_heparin, by = "ENCOUNTERID") |> 
    mutate(
      EC1_structured = if_else(EC1_dx == "EC1" | EC1_heparin == "EC1", 
                               "EC1", 
                               "Included")
    ) |> 
    select(-EC1_dx, -EC1_heparin)
  
  return(structured_ec1)
}
