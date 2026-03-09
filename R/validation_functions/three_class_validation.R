three_class_validation <- function(chart_df, pipeline_df, variable) {
  
  # confusion matrix
  cm <- confusionMatrix(
    data = pipeline_df[[variable]], 
    reference = chart_df[[variable]]
    )

  # cohen's kappa
  cohen_k <- cm$overall["Kappa"]
  
  result <- list(
    confusion_matrix = cm$table,
    cohen_kappa = cohen_k
    )
  
  return(result)
}
