ruleout3_notes <- function(cohort_df, notes_df) {
  
  # -------------------------------
  # Step 0: Define EC3 LLM Prompt
  # -------------------------------
  # Prompt instructs LLM to evaluate clinical notes for evidence of significant
  # chest pain refractory to medical management. 
  ec3_prompt <- "
  You are an elite, expert-level medical care provider who follows exact instructions. You are evaluating clinical notes written by emergency department clinicians.

  TEMPORAL REASONING RULES:
  - If a note begins with 'Initial Provider Assessment', treat this as the earliest clinical note for the encounter.
  - If timestamps appear in the text (e.g., '22:15', 'later', 'after nitro', 'on arrival'), use them to determine the order of events.
  - Notes are otherwise presented in chronological order after preprocessing.
  - The final clinical status for decision-making is taken from the last note chronologically. 

  TASK
  Your task is to determine whether the note indicates that the patient has significant chest pain refractory to medical management.

  INSTRUCTIONS
  1. Read all note text for the encounter. Notes may describe the patient's response to medications, worsening symptoms, persistent pain, or diagnostic impressions.
  2. Apply the following rules in order:
    a. If the note indicates that the patient has significant chest pain refractory to medical management, output exactly: EC3.
    b. If, and only if, the note does not indicate significant chest pain refractory to medical management, output exactly: Included.

  CONSTRAINTS
  - Only output the exact text according to the rules above.
  - Do not explain, provide reasoning, or add narrative text.
  - Do not output labels.
    - Do not output JSON.
- Do not combine outputs; output only one of the valid statements per ENCOUNTERID.

  OUTPUT FORMAT
  Either:
  'EC3' OR 'Included'

  DO NOT MAKE MISTAKES."
  
  # Helper function: builds full LLM prompt for note
  build_prompt_ec3 <- function(notes_text) {
    paste0(ec3_prompt, "\n\nCLINICAL NOTE:\n", notes_text)
  }
  
  # -------------------------------
  # Step 1: Process notes per encounter
  # -------------------------------
  notes_ec3 <- notes_df |> 
    filter(ENCOUNTERID %in% cohort_df$ENCOUNTERID) |> 
    group_by(ENCOUNTERID) |> 
    group_split() |> 
    map_dfr(function(df) {
      tibble(
        ENCOUNTERID = df$ENCOUNTERID[1],
        # Step 1a: Query LLM for EC3 assessment
        EC3 = azure_chat(df, build_prompt = build_prompt_ec3) |> 
          str_replace_all("^'+|'+$", "")
      )
    })
  
  return(notes_ec3)
}
