merge_on_weights <- function(dt, weights_path, num_replicates = 100) {
  dt_weights <- fread(weights_path, select = 1:(num_replicates + 2))
  dt_weights <- melt(
    dt_weights,
    id.vars = "H_ID", variable.name = "Replicate", value.name = "Weight"
  )
  weight_prefix <- "P_Weight_2018TaxYearIntegrated_Rep"
  dt_weights[
    , Replicate := stringr::str_remove(Replicate, weight_prefix) %>% as.numeric()
  ]
  
  dt <- merge(dt, dt_weights, by = "H_ID", allow.cartesian = TRUE)
  
  return(dt)
}