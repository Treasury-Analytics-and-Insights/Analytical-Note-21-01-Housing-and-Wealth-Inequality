replicates_to_MoE <- function(replicates, group_by = NULL) {
  MoE <- replicates[, .(
    Value = Value[1],
    Margin_Of_Error = 1.96*sqrt(sum((Value - Value[1])^2)/(.N - 1)),
    Population = Population[1],
    Sample = Sample[1],
    Rounding_Rule = Rounding_Rule[1]
  ), by = group_by]
  return(MoE)
}
suppress_MoE <- function(dt) {
  dt[Population < 3e3 | Sample < 10, ":="(Value = NA, Margin_Of_Error = NA)]
  return(dt)
}
DEFAULT_ROUNDING_RULES <- list(
  "Gini" = 0.1/100,
  "Relative" = 0.1/100,
  "Absolute" = 0.01/1e-6,
  "Max_Gap" = 0.01/1e-6,
  "Mean" = 0.01/1e-6,
  "Lambda" = 0.001,
  "Population" = 1e3,
  "Percentage" = 0.1/100,
  "Total_Value" = 1e3
)
round_MoE <- function(dt, rounding_rules = DEFAULT_ROUNDING_RULES, pop_rounding = 1e3) {
  dt[, Population := round_half_up_to(Population, pop_rounding)]
  cols_to_round <- c("Value", "Margin_Of_Error")
  these_rounding_rules <- dt[, unique(Rounding_Rule)]
  # Check we have all the rules we need
  missing_rules <- setdiff(these_rounding_rules, names(rounding_rules))
  if (length(missing_rules) > 0) {
    msg <- sprintf("No rounding rule given for %s\n", missing_rules)
    logging::logerror(msg)
    stop(msg)
  }
  for (rounding_rule in these_rounding_rules) {
    round_to <- rounding_rules[[rounding_rule]]
    dt[Rounding_Rule == rounding_rule
      , (cols_to_round) := lapply(.SD, round_half_up_to, round_to),
      .SDcols = cols_to_round
    ]
  }
  return(dt)
}