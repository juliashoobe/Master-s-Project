final_multiclass_validation <- function(chart_df, pipeline_df) {
  
  # merge datasets
  merged_df <- prepare_validation_data(chart_df, pipeline_df)
  
  # create aligned factors
  chart_var <- factor(merged_df$Decision_chart, 
                      levels = c("Does not qualify.", "Qualifies"))
  pipe_var  <- factor(merged_df$Decision_pipe, 
                      levels = c("Does not qualify.", "Qualifies"))
  
  cm <- confusionMatrix(
    data = pipe_var,
    reference = chart_var,
    positive = "Qualifies"
  )
  
  results <- cm$table
  
  return(results)
}
