binary_validation <- function(chart_df, pipeline_df, variable) {
  
  vars <- prepare_validation_data(chart_df, pipeline_df, variable)
  
  all_levels <- union(unique(vars$chart), unique(vars$pipe))
  
  chart_var <- factor(vars$chart, levels = all_levels)
  pipe_var <- factor(vars$pipe, levels = all_levels)
  
  # confusion matrix 
  cm <- confusionMatrix(
    data = pipe_var, 
    reference = chart_var
  )
  
  results <- list(
    metrics = data.frame(
      # sensitivity
      Sensitivity = cm$byClass["Sensitivity"],
      
      # specificity
      Specificity = cm$byClass["Specificity"],
      
      # positive predictive value
      PPV = cm$byClass["Pos Pred Value"],
      
      # negative predictive value
      NPV = cm$byClass["Neg Pred Value"]
    ),
    confusion_matrix = cm$table
  )
  
  return(results)
}
