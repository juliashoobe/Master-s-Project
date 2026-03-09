binary_validation <- function(chart_df, pipeline_df, variable) {
  
  # confusion matrix 
  cm <- confusionMatrix(
    data = pipeline_df[[variable]], 
    reference = chart_df[[variable]]
  )
  
  # sensitivity
  sensitivity <- cm$byClass["Sensitivity"]
  
  # specificity
  specificity <- cm$byClass["Specificity"]
  
  # ppv
  ppv <- cm$byClass["Pos Pred Value"]
  
  # npv
  npv <- cm$byClass["Neg Pred Value"]
  
  metrics <- data.frame(
    Sensitivity = sensitivity,
    Specificity = specificity,
    PPV = ppv,
    NPV = npv
  )
  
  results <- list(
    metrics = metrics,
    confusion_matrix = cm$table
  )
  
  return(results)
}
