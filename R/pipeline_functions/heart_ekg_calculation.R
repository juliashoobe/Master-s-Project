heart_ekg_calculation <- function(notes_df, cohort_df) {
  
  # -------------------------------
  # Step 0: Define H.E.A.R.T. ECG/EKG component LLM prompt
  # -------------------------------
  # Prompt instructs LLm to evaluate all clinical notes for a single encounter and assign
  # a numeric score (0, 1, or 2) for the ECG/EKG component of the H.E.A.R.T. score based
  # on clinician descriptions of ECG/EKG findings.
  ekg_prompt <- "
  You are an elite, expert-level medical care provider who follows exact instructions. We are evaluating clinical notes written by emergency department clinicians about patients being evaluated for chest pain to determine whether they qualify for a specific protocol involving the H.E.A.R.T score.

  TASK
  Your task is to evaluate the ECG/EKG component of the H.E.A.R.T score based on the clinician's written description of the patient's ECG/EKG results.

  INSTRUCTIONS
  Treat all clinical notes associated with a single ENCOUNTERID as a single clinical encounter. Integrate information across all notes for that ENCOUNTERID. Do not make separate determinations for individual notes. Notes may not be in chronological order; if timing matters, infer sequence from clinical context, but do not assume any note order.

  DETERMINATION RULES
  Assign exactly one of the following point values based on the clinician's description of the patient's ECG/EKG:
  2 points: It is indicated that ECG/EKG showed ST-segment deviation, and there is no indication or mention that this deviation was due to:
  a. left bundle branch block (LBBB), or
  b. left ventricular hypertrophy (LVH), or
  c. digoxin use

  1 point: It is indicated that the ECG/EKG showed no ST-segment deviation, but there was an indication or mention of:
  a. left bundle branch block (LBBB), or
  b. left ventricular hypertrophy LVH), or
  c. repolarization changes (such as from digoxin use or due to T wave abnormalities or due to PR depressions)

  0 points: The ECG/EKG does not show any of the aforementioned patterns OR is described as 'normal' OR if no EKG result is mentioned.

  YOU MUST ONLY PICK ONE OF THESE THREE OPTIONS. DO NOT MAKE MISTAKES.

  CONSTRAINT
  You must output exactly one of the following four outcomes: 0, 1, or 2. Do not explain, justify, or include extra text. Do not output labels. Output only the outcome itself.

  OUTPUT FORMAT
  Output one of the following values for that ENCOUNTERID: 0, 1, or 2."
  
  build_prompt_ekg <- function(notes_text) {
    paste0(ekg_prompt, "\n\nCLINICAL NOTE:\n", notes_text)
  }
  
  # -------------------------------
  # Step 1: Process notes per encounter
  # -------------------------------
  heart_ekg <- notes_df |> 
    filter(ENCOUNTERID %in% cohort_df$ENCOUNTERID) |> 
    group_by(ENCOUNTERID) |> 
    group_split() |> 
    map_dfr(function(df) {
      score <- azure_chat(df, build_prompt = build_prompt_ekg)
      
      tibble(
        ENCOUNTERID = df$ENCOUNTERID[1],
        ekg_score = as.numeric(score)
      )
    })
  
  return(heart_ekg)
}
