azure_chat <- function(notes_df, build_prompt) {
  
  notes_df <- notes_df |> 
    mutate(
      starts_IPA = grepl("^INITIAL PROVIDER ASSESSMENT", REPORT_TEXT, ignore.case = TRUE)
    ) |> 
    arrange(
      ENCOUNTERID,
      desc(starts_IPA),
      row_number()
    ) |> 
    mutate(
      header = case_when(
        starts_IPA ~ "[INITIAL PROVIDER ASSESSMENT]",
        TRUE ~ paste0("[NOTE ", row_number(), "]")
      ),
      formatted = paste0(header, "\n", REPORT_TEXT)
    )
  
  formatted_notes <- notes_df |> 
    pull(formatted) |> 
    paste(collapse = "\n\n")
  
  full_prompt <- build_prompt(formatted_notes)
  
  # Sends request to Azure
  response <- httr::POST(
    url = completions_endpoint,
    httr::add_headers(
      "api-key" = key,
      "Content-Type" = "application/json"
    ),
    body = list(
      messages = list(
        list(
          role = "user",
          content = full_prompt
        )
      ),
      temperature = 1,
      max_completion_tokens = 5000
    ),
    encode = "json"
  )
  
  # Parses response
  parsed <- content(response, as = "parsed", type = "application/json")
  
  # Extracts models answer
  trimws(parsed$choices[[1]]$message$content)
}
