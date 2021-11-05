calc_quantiles <- function(x, weights, probs = seq(0, 1, 0.1))
{
  i <- order(x)
  weights <- weights[i]
  
  y <- shift(cumsum(weights)/sum(weights), 1, fill=0)
  q <- cut(y, probs, labels = FALSE, include.lowest=TRUE, right=FALSE)
  
  j <- order(i)
  q <- q[j]
  
  return(q)
}
