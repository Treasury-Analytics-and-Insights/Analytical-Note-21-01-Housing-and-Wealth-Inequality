#' Calculate Gini coefficients for a decomposition of a total population
#' into two mutually exclusive sub-populations a and b.
#'
#' @param x Values of the total population
#' @param a Values of sub-population a
#' @param b Values of sub-population b
#' @param w_x Weights for the total population
#' @param w_a Weights for sub-population a
#' @param w_b Weights for sub-population b
#' @param a_name Name of sub-population a
#' @param b_name Name of sub-population b
#'
#' @return A nested list. First level contains Gini coefficient types.
#'        Second level contains total, within-group, and between-group components.
#' @export
#'
#' @examples
calc_gini_decomposition <- function(x, a, b, w_x, w_a, w_b, a_name, b_name) {
  G_x <- weighted_gini(x, w_x) # Total population
  G_aa <- weighted_gini(a, w_a) # Sub-population a
  G_bb <- weighted_gini(b, w_b) # Sub-population b
  
  # Populations
  N_x <- sum(w_x)
  N_a <- sum(w_a)
  N_b <- sum(w_b)
  
  # Totals (note abs, this includes negative values in the normalisation)
  X <- sum(w_x*abs(x))
  A <- sum(w_a*abs(a))
  B <- sum(w_b*abs(b))
  
  # Means
  mu_x <- X / N_x
  mu_a <- A / N_a
  mu_b <- B / N_b
  mu_ab <- (mu_a + mu_b) / 2
  
  # Gini factors
  lambda_aa <- (N_a*(N_a - 1)) / (N_x*(N_x - 1)) *  (mu_a / mu_x)
  lambda_bb <- (N_b*(N_b - 1)) / (N_x*(N_x - 1)) *  (mu_b / mu_x)
  lambda_ab <-     2*(N_a*N_b) / (N_x*(N_x - 1)) * (mu_ab / mu_x)
  
  # Extract G_ab from the other Gini components
  G_ab <- (G_x - (lambda_aa*G_aa + lambda_bb*G_bb)) / lambda_ab
  
  # Absolute Gini coefficients (average gap between all pairs)
  AG_x <- G_x * (2*mu_x)
  AG_aa <- G_aa * (2*mu_a)
  AG_bb <- G_bb * (2*mu_b)
  AG_ab <- G_ab * (2*mu_ab)
  
  # The lambda formulae can be derived from the following identity:
  # AG_x*(N_x*(N_x - 1)) = (
  #   AG_aa*(N_a*(N_a - 1)) + AG_bb*(N_b*(N_b - 1)) +
  #   AG_ab*(N_a*N_b) + AG_ba*(N_a*N_b)
  # ),
  # i.e. the sum of absolute differences equals the sum of
  # "within a", "within b", "between a & b", and "between b & a" terms.
  # Note the last two terms are equal by symmetry,
  # so can be written as 2*AG_ab*(N_a*N_b),
  # and the factor of two is absorbed into lambda_ab.
  
  out_names <- c(
    "Total",
    sprintf("%s", a_name),
    sprintf("%s", b_name),
    sprintf("%s and %s", a_name, b_name)
  )
  
  out_relative <- list(G_x, G_aa, G_bb, G_ab)
  names(out_relative) <- out_names
  
  out_absolute <- list(AG_x, AG_aa, AG_bb, AG_ab)
  names(out_absolute) <- out_names
  
  out_mean <- list(mu_x, mu_a, mu_b, mu_ab)
  names(out_mean) <- out_names
  
  out_max_gap <- list(2*mu_x, 2*mu_a, 2*mu_b, 2*mu_ab)
  names(out_max_gap) <- out_names
  
  out_lambda <- list(1, lambda_aa, lambda_bb, lambda_ab)
  names(out_lambda) <- out_names
  
  out <- list(
    "Relative" = out_relative,
    "Absolute" = out_absolute,
    "Mean" = out_mean,
    "Max_Gap" = out_max_gap,
    "Lambda" = out_lambda
  )
  
  return(out)
}