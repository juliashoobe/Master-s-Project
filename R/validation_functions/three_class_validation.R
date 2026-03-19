three_class_validation <- function(chart_df, pipeline_df, variable) {
  
  vars <- prepare_validation_data(chart_df, pipeline_df, variable)
  
  all_levels <- c("EC1", "Included", "No EKG result found.")
  
  chart_var <- factor(vars$chart, levels = all_levels)
  pipe_var <- factor(vars$pipe, levels = all_levels)
  
  # confusion matrix
  cm <- confusionMatrix(
    data = pipe_var,
    reference = chart_var
    )

  # cohen's kappa
  cohen_k <- cm$overall["Kappa"]
  
  result <- list(
    confusion_matrix = cm$table,
    cohen_kappa = cohen_k
    )
  
  return(result)
}
