# Master's Project
## Integration of DUH EHR Data and LLM-Based Clinical Note Analysis to Determine Chest Pain CEU Protocol Eligibility
### Overview
This project develops an AI-driven clinical decision support system to identify ED patients who may be eligible for the Duke University Hospital (DUH) Chest Pain Clinical Evaluation Unit (CEU) protocol. The system integrates structured EHR data and LLM-extracted components from provider notes to generate eligibility alterts that support clinician-decision making, without replacing clinical judgement. 

### Workflow
The `decision_pipeline.Rmd` implements the end-to-end CEU eligibility workflow, combining structured EHR data and LLM-extracted information from provider notes.
1. Load Data
   Data are pulled from **Duke's Clinical Research Data Mart (CRDM)** (SQL-based internal EHR repository).
   For the project, the following data types are used:
   - Encounters - all DUH ED visits between July 1, 2024 - June 30, 2025
   - Labs - troponin lab values (LAB3234, LAB3235, LAB3236, LAB3237)
   - Demographics - birth date, sex, etc.
   - Diagnosis & Conditions - structured ICD codes for diagnosis records and medical history
   - Procedures & Medication Administration - structured flags for relevant procedures & medications
   - Provider Notes - unstructured clinical documentation for LLM extraction
2. Cohort Selection
   - `create_cohort.R`
   - Builds encounter-level cohort with relevant troponin labs and patient flags
   - Excludes encounters used for pipeline testing
3. Azure AI Setup
   - Azure OpenAI endpoint configured for LLM calls (`gpt-5`)
   - Used to extract HEART score components and protocol-relevant rule-out criteria from unstructured notes
4. Exclusion Criteria (Rule-Out Logic)
   - EC1 - Acute MI/Ischemia Pattern
       - `ruleout1_structured.R`: structured diagnoses and medication flags
       - `ruleout1_notes.R`: LLM-based analysis of provider notes
       - `combine_ec1.R`: combines structured and LLM outputs into a single EC1 flag
    - EC2 - Unstable Vital Signs
        - `ruleout2_notes.R`: LLM-based extraction of most recent vital signs from notes
    - EC3 - Refractory Chest Pain Concerning for ACS
        - `ruleout3_notes.R`: LLM-based extraction from notes describing concerning chest pain
5. HEART Score Calculation
     - History (`heart_history_calculation.R`) - LLM-based from notes
     - ECG/EKG (`heart_ekg_calculation.R`) - LLM-based from notes
     - Age (`heart_age_calculation.R`) - structured demographics
     - Risk Factors (`heart_rf_calculation.R`) - structured conditions
     - Initial Troponin (`heart_trop_calculation.R`) - structured lab values
     - Total HEART Score (`calculate_heart.R`) - adds the points from each individual score
6. CEU Eligibility Decision
   - `ceu_decision.R`
   - Combines HEART score and exclusion criteria flags (EC1 - EC3)
   - Outputs eligibility recommendation for clinicians
7. Validation
   - Functions are located in `validation_functions/` and evaluate model performance against manual chart review
   - Validation Types & Functions:
       - Continuous Outcomes - `continuous_validation.R`
           - Compares numeric values like troponin and HEART score
           - Metrics: mean-squared error, mean-absolute error
       - Binary Outcomes - `binary_validation.R`
           - Eligible or not eligible
           - Metrics: sensitivity, specificity, positive predictive value, negative predictive value, confusion matrix
       - Three-Class Outcomes - `three_class_validation.R`
           - For multi-category outcomes
           - Metrics: confusion matrix, Cohen's kappa
       - Ordinal Outcomes - `ordinal_validation.R`
           - Outcomes with natural ordering
           - Metrics: weighted Cohen's kappa
       - Final Multiclass Aggregation - `final_multiclass_validation.R`
           - Qualifies or does not qualify
           - Metrics: confusion matrix
