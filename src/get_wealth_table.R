get_wealth_table <- function(
  dt,
  replicate_var = "Replicate",
  group_vars = c("Replicate", "H_Owns_Housing")
) {
  wealth_table <- dt[, .(
    Households = sum(Weight),
    Net_Worth = sum(Weight*Net_Worth),
    Population = sum(Weight),
    Sample = .N
  ), by = c(replicate_var, group_vars)]
  
  total_pops <- dt[, .(
    Total_Wealth = sum(Weight*Net_Worth),
    Total_Households = sum(Weight)
  ), by = replicate_var]
  
  wealth_table <- merge(wealth_table, total_pops, by = replicate_var)
  
  wealth_table[, ":="(
    Prop_Households = Households / Total_Households,
    Prop_Wealth = Net_Worth / Total_Wealth
  )]
  
  wealth_table_long <- melt(
    wealth_table,
    id.vars = c(replicate_var, group_vars, "Population", "Sample"),
    variable.name = "Variable", value.name = "Value"
  )
  
  wealth_table_long[
    Variable %in% c("Households", "Total_Households"),
    Rounding_Rule := "Population"
  ]
  wealth_table_long[
    Variable %in% c("Net_Worth", "Total_Wealth"),
    Rounding_Rule := "Total_Value"
  ]
  wealth_table_long[
    Variable %in% c("Prop_Households", "Prop_Wealth"),
    Rounding_Rule := "Percentage"
  ]
  
  rounding_rules <- list(
    "Population" = 1e3,
    "Total_Value" = 1e3,
    "Percentage" = 1/100
  )
  
  wealth_table_moe <-
    replicates_to_MoE(wealth_table_long, c(group_vars, "Variable")) %>%
    suppress_MoE() %>% round_MoE(rounding_rules = rounding_rules)
  
  wealth_table_share <- wealth_table_moe[
    Variable %in% c("Households", "Prop_Households", "Prop_Wealth"),
    .SD,
    .SDcols = c(group_vars, "Variable", "Value", "Margin_Of_Error")
  ]
  wealth_table_share_out <-
    wealth_table_long_to_wide(wealth_table_share, group_vars)
  
  return(list("long" = wealth_table_share, "wide" = wealth_table_share_out))
}

wealth_table_long_to_wide <- function(wealth_table_share, group_vars) {
  wealth_table_share_wide <- dcast(
    wealth_table_share,
    ... ~ Variable, value.var = c("Value", "Margin_Of_Error")
  )
  wealth_table_share_out <- wealth_table_share_wide[, .(
    pop = paste0(
      scales::comma(Value_Households),
      " ± ",
      scales::comma(Margin_Of_Error_Households)
    ),
    pop_share = paste0(
      scales::percent(Value_Prop_Households, accuracy = 1),
      " ± ",
      scales::percent(Margin_Of_Error_Prop_Households, accuracy = 1)
    ),
    wealth_share = paste0(
      scales::percent(Value_Prop_Wealth, accuracy = 1),
      " ± ",
      scales::percent(Margin_Of_Error_Prop_Wealth, accuracy = 1)
    )
  ), by = group_vars]
  
  # Encode as UTF-8 so the plus/minus symbol gets read correctly
  Encoding(wealth_table_share_out$pop) <- "UTF-8"
  Encoding(wealth_table_share_out$pop_share) <- "UTF-8"
  Encoding(wealth_table_share_out$wealth_share) <- "UTF-8"
  
  setcolorder(wealth_table_share_out, group_vars)
  setorderv(wealth_table_share_out, group_vars)
  
  return(wealth_table_share_out)
}
