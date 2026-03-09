continuous_validation <- function(chart_df, pipeline_df, variable) {
  
  # mean squared error
  mse <- mse(chart_df[[variable]], pipeline_df[[variable]])
  
  # mean absolute error
  mae <- mae(chart_df[[variable]], pipeline_df[[variable]])
  
  result <- list(
    MSE = mse,
    MAE = mae
  )
  
  return(result)
}
