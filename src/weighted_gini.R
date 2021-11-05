weighted_gini <- function(x, weights = rep(1, length = length(x))) {
  delta <- mean_absolute_difference(x, weights)
  
  # Mean - use absolute values to normalise negative values according to
  # Raffinetti, Emanuela, Elena Siletti, and Achille Vernizzi.
  # "Analyzing the effects of negative and non-negative values on
  # income inequality: evidence from the survey of household income
  # and Wealth of the Bank of Italy (2012)."
  # Social Indicators Research 133.1 (2017): 185-207.
  mu <- sum(weights*abs(x)) / sum(weights)
  
  # Gini is ratio of Mean absolute difference to twice the mean
  out <- delta / (2*mu)
  
  return(out)
}

mean_absolute_difference <- function(x, weights = rep(1, length = length(x))) {
    ox <- order(x)
    x <- x[ox]
    weights <- weights[ox]
    N <- sum(weights)
    p <- cumsum(weights)
    # Mean absolute difference - note factor of (N - 1) which ensures
    # we count *distinct* pairs in x.
    delta <- sum((2*p - N - weights)*weights*x) / (N*(N - 1)/2)
    return(delta)
}