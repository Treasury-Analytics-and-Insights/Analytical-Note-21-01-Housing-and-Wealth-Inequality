calc_gini_decomposition_by_rate <- function(
  households,
  inflation_rates,
  variable_to_inflate,
  ownership_variable,
  equivalisation_variable = "H_MOECD_Eq_Factor"
) {
  housing_gini_decomposition <- data.table()
  for (inflation_rate in inflation_rates) {
    logging::loginfo(
      sprintf("Calculating gini decomposition for inflation rate %s", inflation_rate)
    )
    this_housing_gini_decomposition <- households[, {
      all_households_NW <- (Net_Worth + inflation_rate*get(variable_to_inflate)) / get(equivalisation_variable)
      all_weights <- H_People*Weight
      non_owner_weights <- (get(ownership_variable) == "Non-owner")*H_People*Weight
      owner_weights <- (get(ownership_variable) == "Owner")*H_People*Weight
      
      gini_decomposition <- calc_gini_decomposition(
        x = all_households_NW,
        w_x = all_weights,
        
        a = all_households_NW,
        w_a = owner_weights,
        
        b = all_households_NW,
        w_b = non_owner_weights,
        
        a_name = "Owners",
        b_name = "Non-owners"
      )
      
      out <- rbindlist(list(
        "Relative" = as.data.table(gini_decomposition[["Relative"]]),
        "Absolute" = as.data.table(gini_decomposition[["Absolute"]]),
        "Mean" = as.data.table(gini_decomposition[["Mean"]]),
        "Max_Gap" = as.data.table(gini_decomposition[["Max_Gap"]]),
        "Lambda" = as.data.table(gini_decomposition[["Lambda"]])
      ), idcol = "Gini_Type", fill = TRUE)
    }, by = Replicate]
    
    this_housing_gini_decomposition[, Inflation_Rate := inflation_rate]
    
    housing_gini_decomposition <- rbind(
      housing_gini_decomposition,
      this_housing_gini_decomposition
    )
  }
  
  population_and_sample <- households[, .(
    Population = sum(Weight), Sample = .N
  ), by = Replicate]
  
  housing_gini_decomposition <- merge(
    housing_gini_decomposition, population_and_sample, by = "Replicate"
  )
  
  housing_gini_decomposition_long_reps <- melt(
    housing_gini_decomposition,
    measure.vars = c("Total", "Owners", "Non-owners", "Owners and Non-owners"),
    variable.name = "Variable", value.name = "Value"
  )
  housing_gini_decomposition_long_reps[, Rounding_Rule := Gini_Type]
  
  gini_level_reps <- copy(housing_gini_decomposition_long_reps)
  gini_level <- replicates_to_MoE(
    gini_level_reps, c("Inflation_Rate", "Gini_Type", "Variable")
  ) %>% suppress_MoE() %>% round_MoE()
  
  gini_change_reps <- copy(housing_gini_decomposition_long_reps)
  gini_change_reps[, ":="(
    Value = Value - first(Value)
  ), by = .(Gini_Type, Replicate, Variable)]
  
  # Zero inflation rate has zero change, so drop these
  gini_change_reps <- gini_change_reps[Inflation_Rate > 0]
  
  gini_change <- replicates_to_MoE(
    gini_change_reps, c("Inflation_Rate", "Gini_Type", "Variable")
  ) %>% suppress_MoE() %>% round_MoE()
  
  gini_decomposition <- rbindlist(list(
    "Level" = gini_level,
    "Level_Change" = gini_change
  ), idcol = "Variable_Type")
  
  gini_types <- c("Relative", "Absolute", "Max_Gap", "Mean", "Lambda")
  gini_decomposition[, Gini_Type := factor(Gini_Type, levels = gini_types)]
  gini_decomposition[
    , Variable := factor(
      Variable,
      levels = c("Total", "Owners", "Non-owners", "Owners and Non-owners"),
      labels = c("Total Population", "Within Owners", "Within Non-owners", "Between Owners\n and Non-owners")
    )
  ]
  setorder(gini_decomposition, Gini_Type, Variable, Inflation_Rate)
  
  
  
  # Formatted output
  gini_decomposition[
    Gini_Type == "Relative", output_str := paste0(
      scales::percent(Value, accuracy = 0.1),
      " ± ",
      scales::percent(Margin_Of_Error, accuracy = 0.1)
    )
  ]
  gini_decomposition[
    Gini_Type %in% c("Absolute", "Max_Gap", "Mean"), output_str := paste0(
      scales::dollar(Value, accuracy = 0.01, scale = 1e-6, suffix = "M"),
      " ± ",
      scales::dollar(Margin_Of_Error, accuracy = 0.01, scale = 1e-6, suffix = "M")
    )
  ]
  gini_decomposition[
    Gini_Type %in% c("Lambda"), output_str := paste0(
      scales::comma(Value, accuracy = 0.001),
      " ± ",
      scales::comma(Margin_Of_Error, accuracy = 0.001)
    )
  ]
  
  return(gini_decomposition)
}