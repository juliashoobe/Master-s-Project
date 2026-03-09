ruleout2_notes <- function(cohort_df, notes_df) {
  
  # -------------------------------
  # Step 0: Define EC2 LLM Prompt
  # -------------------------------
  # Prompt instructs LLM to extract most recent vital signs
  # from the PHYSICAL EXAM or VITALS section of latest note for each encounter.
  # Also provides detailed rules for assessing bradycardia-related symptoms.
  ec2_prompt <- "
  You are an elite, expert-level medical care provider who follows exact instructions. You are evaluating clinical notes written by emergency department clinicians. 
  
  TEMPORAL REASONING RULES:
  - If a note begins with 'Initial Provider Assessment', treat this as the earliest clinical note for the encounter.
  - If timestamps appear in the text (e.g., '22:15', 'later', 'after nitro', 'on arrival'), use them to determine the order of events.
  - Notes are otherwise presented in chronological order after preprocessing.
  - The final clinical status for decision-making is taken from the last note chronologically. 

  TASK
  Your task is to extract the most recent patient vital sign measurements associated with a single ENCOUNTERID. 
  - Heart rate / pulse (may be written as 'pulse', 'HR', 'heart rate', etc.)
  - Respiratory rate (may be written as 'RR', 'resp', 'respiratory rate', etc.)
  - Temperature in Celsius (may be written as 'temp', 'temperature', 'T', etc.)
  - Oxygen saturation (may be written as 'SpO2', 'O2 saturation', etc.)
  - Blood pressure (may be written as 'BP', 'blood pressure', etc.)
    - Blood pressure should be formatted as `systolic/diastolic` (e.g., 120/80)
    - If BP is not documented in the note, output NA for that line. Do not guess values.

  ADDITIONAL TASK - SYMPTOM ASSESSMENT
  This section applies only if the most recent heart rate is < 50.
  Determine whether the bradycardia is symptomatic, using the following rules:

  DEFINITIONS
  - A symptom counts only if the clinician documents that the patient is currently experiencing it.
  - Ignore symptoms that are denied, absent, historical, or explicitly not present.
  - Ignore general statements such as 'bradycardia present' unless a symptom is directly attributed to it.
  - Explicit statements such as 'asymptomatic', 'denies', 'no', or 'without' symptoms indicate absence of symptoms.

  STEP 1 - MAJOR SYMPTOMS
  Determine if the clinician documents any of the following CURRENT symptoms that could be attributed to bradycardia:
  fatigue, weakness, exercise intolerance, dizziness, lightheadedness, syncope, presyncope, shortness of breath, chest discomfort, chest pain, palpitations, altered mental status, hypotension, left jaw pain, left arm pain, lower extremity swelling, diaphoresis, or any other symptoms explicitly attributed to bradycardia.
  If ANY of these symptoms are present, output: 'symptomatic'.

  STEP 2 - ATYPICAL SYMPTOMS WITH RISK MODIFIERS
  If none of the above are present, determine if the patient currently has nausea or epigastric pain.
  If YES, and the patient is a woman, has diabetes, or is older than 50, output: 'symptomatic'.

  STEP 3 - OTHERWISE
  If none of the above conditions are met, output: 'asymptomatic'.
  If HR ≥ 50, output: 'not assessed'.

  OUTPUT RULE
  Output six lines, in order:
  - HR
  - RR
  - Temp
  - SpO2
  - BP
  - Symptomaticity
  
  OUTPUT FORMAT
  - For vitals: numeric value or NA
  - BP must be in systolic/diastolic format or NA
  - For Symptomaticity: 'symptomatic', 'asymptomatic', or 'not assessed'
  - No units, no explanation, no extra text
  - If a vital is missing, output NA. Do not leave blank lines

  CONSTRAINTS
  - Only output the six lines above
  - Do not output any extra text or commentary
  - Follow the order exactly.

  DO NOT MAKE MISTAKES."
  
  # Helper function: builds full LLM prompt for note
  build_prompt_ec2 <- function(notes_text) {
    paste0(ec2_prompt, "\n\nCLINICAL NOTE:\n", notes_text)
  }
  
  # -------------------------------
  # Step 1: Process notes per encounter
  # -------------------------------
  notes_ec2 <- notes_df |> 
    filter(ENCOUNTERID %in% cohort_df$ENCOUNTERID) |> 
    group_by(ENCOUNTERID) |> 
    group_split() |> 
    
    # Step 1a: Query LLM for vitals and bradycardia assessment
    map_dfr(function(df) {
      vitals_text <- azure_chat(df, build_prompt = build_prompt_ec2)
      
      # Step 1b: Split LLM output into separate lines
      lines <- strsplit(vitals_text, "\n")[[1]] |> 
        trimws() |> 
        purrr::discard(~ .x == "")
      
      # Step 1c: Extract BP components
      bp_raw <- lines[5]
      systolic <- as.numeric(sub("/.*", "", bp_raw)) # extract systolic
      diastolic <- as.numeric(sub(".*/", "", bp_raw)) # extract diastolic
      
      # Step 1d: Build tibble with vitals and LLM output
      tibble(
        ENCOUNTERID = df$ENCOUNTERID[1],
        
        HR = as.numeric(lines[1]),
        RR = as.numeric(lines[2]),
        Temp = as.numeric(lines[3]),
        SpO2 = as.numeric(lines[4]),
        BP = bp_raw,
        Systolic = systolic,
        Diastolic = diastolic,
        
        Symptomaticity = lines[6],
        raw_llm_output = vitals_text
      )
    }) |> 
    
  # -------------------------------
  # Step 3: Assign EC2 flag based on vitals
  # -------------------------------
  # Encounters flagged as EC2 if any vials are missing or fall outside pre-specified range
    mutate(
      EC2 = case_when(
        HR > 150 ~ "EC2",
        HR < 50 & Symptomaticity == "symptomatic" ~ "EC2",
        Temp < 36 | Temp >= 39.8 ~ "EC2",
        RR < 10 | RR >= 30 ~ "EC2",
        SpO2 < 88 ~ "EC2",
        Systolic < 90 | Systolic >= 180 ~ "EC2",
        Diastolic < 60 | Diastolic >= 120 ~ "EC2",
        TRUE ~ "Included"
      )
    ) |> 
    select(ENCOUNTERID, EC2)
  
  return(notes_ec2)
}
