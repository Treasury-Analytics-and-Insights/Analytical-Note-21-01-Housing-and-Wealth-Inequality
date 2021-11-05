add_quantiles <- function(dt, equivalisation_variable, quantile_probs) {
  dt[
    , Net_Worth_quantile := calc_quantiles(
      Net_Worth/get(equivalisation_variable), H_People*Weight, quantile_probs
    ), by = Replicate
    ]
  dt[, Net_Worth_quantile := quantile_probs[Net_Worth_quantile + 1]]
  return(dt)
}