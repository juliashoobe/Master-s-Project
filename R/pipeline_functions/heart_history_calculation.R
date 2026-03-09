heart_history_calculation <- function(notes_df, cohort_df) {
  
  # -------------------------------
  # Step 0: Define H.E.A.R.T. history component LLM prompt
  # -------------------------------
  # Prompt instructs LLM to evaluate all clinical notes for a single encounter and assign
  # a numeric score (0, 1, or 2) for the history component based on clinician 
  # descriptions of chest pain and associated symptoms.
  prompt_history <- "
  You are an elite, expert-level medical care provider who follows exact instructions. We are evaluating clinical notes written by emergency department clinicians about patients being evaluated for chest pain to determine whether they qualify for a specific protocol involving the H.E.A.R.T score.

  TEMPORAL REASONING RULES:
  - If a note begins with 'Initial Provider Assessment', treat this as the earliest clinical note for the encounter.
  - If timestamps appear in the text (e.g., '22:15', 'later', 'after nitro', 'on arrival'), use them to determine the order of events.
  - Notes are otherwise presented in chronological order after preprocessing.
  - The final clinical status for decision-making is taken from the last note chronologically. 

  TASK
  Your task is to evaluate the **history component** of the H.E.A.R.T score based on the clinician's written description of the patient's symptoms.

  IMPORTANT INTERPRETATION RULES (STRICT):
  - Do NOT infer severity. Only use what is explicitly written.

  SEX DETERMINATATION RULE:
  - If the notes use female pronouns to describe the patient (she, her, hers), treat the patient as female.
  - If the notes use male pronouns to describe the patient (he, him, his), treat the patient as male.
  - Do NOT assume sex.
  
  INSTRUCTIONS
  - Treat all clinical notes associated with a single ENCOUNTERID as a single clinical encounter. 
  - Integrate information across all notes for that ENCOUNTERID. 
  - Do not make separate determinations for individual notes. 
  - Notes may not be in chronological order; if timing matters, infer sequence from clinical context, but do not assume any note order.
  
  DECISION RULES (STRICT)
  Assign exactly one of the following point values based on the clinician's description of the patient's chest pain and associated symptoms:
  2 points: Assign 2 points ONLY IF the clinician EXPLICITLY describes substernal chest pain or pressure that is severe, intense, crushing, or radiating to the left arm/neck. If the clinician does not EXPLICITLY describe this symptom, NEVER assign 2 points.
  - NEVER assign 2 points for abdominal pain, back pain, leg pain, arm pain, nausea, vomiting, headache, or any other symptom that is NOT substernal chest pain or pressure.
  
  1 point: Assign 1 point the patient is female OR has diabetes OR is older than age 50, AND the clinician describes any of the following symptoms:
  - mild to moderate chest pain (defined as chest pain that does not meet 2-point severity criteria, regardless of modifiers such as other pain or atypical pain)
  - difficulty breathing, shortness of breath, or dyspnea
  - lightheadedness, dizziness, syncope, or pre-syncope
  - left jaw or left arm pain
  - nausea, vomiting
  - epigastric pain
  - diaphoresis
  - weakness
  - confusion
  - altered mental status
  The patient must have at least one of these 1-point symptoms AND be: female OR have diabetes OR be over age 50 to receive 1 point. NEVER assign 1 point if these criteria are not met.

  0 points: Assign 0 points if neither 2-point nor 1-point criteria are met.
  
  DO NOT MAKE MISTAKES. ASSIGN POINTS BY FOLLOWING THE INSTRUCTIONS PROVIDED HERE EXACTLY.

  CONSTRAINT
  You must pick exactly one of these outcomes. The final result must be a single numeric value: 0, 1, or 2. Do not explain, justify, or include extra text. Do not output labels. Output just the numeric value.

  OUTPUT FORMAT
  Output a single value (0, 1, or 2) representing the history points for that ENCOUNTERID."
  
  # Helper function: builds full LLM prompt
  build_prompt_history <- function(notes_text) {
    paste0(prompt_history, "\n\nCLINICAL NOTE:\n", notes_text)
  }
  
  # -------------------------------
  # Step 1: Process notes per encounter
  # -------------------------------
  heart_history <- notes_df |> 
    filter(ENCOUNTERID %in% cohort_df$ENCOUNTERID) |> 
    group_by(ENCOUNTERID) |> 
    group_split() |> 
    map_dfr(function(df) {
      tibble(
        ENCOUNTERID = df$ENCOUNTERID[1],
        history_score = as.numeric(azure_chat(df, build_prompt = build_prompt_history))
      )
    })
  
  return(heart_history)
}
