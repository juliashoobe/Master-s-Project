prepare_validation_data <- function(chart_df, pipeline_df, variable = NULL) {
  
  merged_df <- chart_df |> 
    inner_join(pipeline_df, by = "ENCOUNTERID",
               suffix = c("_chart", "_pipe"))
  
  # if no variable specified, return merged (for final multiclass)
  if (is.null(variable)) {
    return(merged_df)
  }
  
  chart_var <- merged_df[[paste0(variable, "_chart")]]
  pipe_var <- merged_df[[paste0(variable, "_pipe")]]
  
  return(list(
    chart = chart_var,
    pipe = pipe_var,
    merged = merged_df
  ))
}
