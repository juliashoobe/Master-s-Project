ruleout1_notes <- function(cohort_df, notes_df) {
  
  # -------------------------------
  # Step 0: Define EC1 LLM Prompt
  # -------------------------------
  # Prompt instructs LLM to evaluate clinical notes for evidence of acute MI or 
  # other acute ischemic cardiac events. Outputs only 'EC1' or 'Included'
  ec1_prompt <- "
  You are an elite, expert-level medical care provider who follows exact instructions. You are evaluating clinical notes written by emergency department clinicians.

  TEMPORAL REASONING RULES:
  - If a note begins with 'Initial Provider Assessment', treat this as the earliest clinical note for the encounter.
  - If timestamps appear in the text (e.g., '22:15', 'later', 'after nitro', 'on arrival'), use them to determine the order of events.
  - Notes are otherwise presented in chronological order after preprocessing.
  - The final clinical status for decision-making is taken from the last note chronologically. 

  TASK
  Your task is to determine whether the note indicates that the patient has evidence of an acute     
    myocardial infarction (MI) or an acute ischemic cardiac event.

  INSTRUCTIONS
  1. Read all note text for the encounter. Notes may describe ECG/EKG findings, diagnostic impressions, or clinical statements about myocardial infarction or acute ischemia.
  2. If any of the following are indicated:
    a. The patient has evidence of an acute myocardial infarction (MI), and/or
    b. The patient has another acute ischemic cardiac event, and/or
    c. ECG/EKG shows acute ischemic changes,
  then you must output exactly the text: 'EC1'.
  3. If none of the above are indicated but the note mentions an ECG/EKG result,  output exactly: 'Included'.
  4. If the note does NOT mention any ECG/EKG result, output exactly: 'No EKG result found.'
  

  CONSTRAINTS
  - Do not output anything else, except one of the three valid outputs.
  - Do not explain, provide reasoning, or add narrative text.
  - Do not output labels.
  - Do not output JSON.

  OUTPUT FORMAT
  Either: 'EC1', 'Included', or 'No EKG result found.'

  DO NOT MAKE MISTAKES."
  
  # Helper function: builds full LLM prompt for note
  build_prompt_ec1 <- function(notes_text) {
    paste0(ec1_prompt, "\n\nCLINICAL NOTE:\n", notes_text)
  }
  
  # -------------------------------
  # Step 1: Process notes per encounter
  # -------------------------------
  notes_ec1 <- notes_df |> 
    
    # restrict to cohort encounters
    filter(ENCOUNTERID %in% cohort_df$ENCOUNTERID) |> 
    group_by(ENCOUNTERID) |> 
    
    # split into list of data frames, one per encounter
    group_split() |> 
    map_dfr(function(df) {
      
      # Step 1a: Query LLM to determine if EC1 criteria are met
      tibble(
        ENCOUNTERID = df$ENCOUNTERID[1],
        EC1_notes = azure_chat(df, build_prompt = build_prompt_ec1)
      )
    })
  
  return(notes_ec1)
}
