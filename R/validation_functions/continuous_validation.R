continuous_validation <- function(chart_df, pipeline_df, variable) {
  
  vars <- prepare_validation_data(chart_df, pipeline_df, variable)
  
  valid <- complete.cases(vars$chart, vars$pipe)
  
  result <- list(
    # mean square error
    MSE = mse(vars$chart[valid], vars$pipe[valid]),
    
    # mean absolute error
    MAE = mae(vars$chart[valid], vars$pipe[valid])
  )
  
  return(result)
}
