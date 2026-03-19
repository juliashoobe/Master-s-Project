ordinal_validation <- function(chart_df, pipeline_df, variable) {

  vars <- prepare_validation_data(chart_df, pipeline_df, variable)
  
  chart_var <- factor(vars$chart, levels = 0:2, ordered = TRUE)
  pipe_var <- factor(vars$pipe, levels = 0:2, ordered = TRUE)
  
  # compute weighted kappa (quadratic by default; "S" = squared)
  kappa <- kappa2(
    data.frame(chart_var, pipe_var),
    weight = "squared"
  )
  
  result <- kappa$value
  
  return(result)
}
