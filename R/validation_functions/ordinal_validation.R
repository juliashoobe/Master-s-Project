ordinal_validation <- function(chart_df, pipeline_df, variable) {

  # compute weighted kappa
  kappa <- kappa2(
    data.frame(
      factor(chart_df[[variable]], levels = 0:2),
      factor(pipeline_df[[variable]], levels = 0:2)
      ),
    weight = "S")
  
  result <- kappa$value
  
  return(result)
}
