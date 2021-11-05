library(data.table)
library(magrittr)
source("src/merge_on_weights.R")
source("src/calc_quantiles.R")
source("src/add_quantiles.R")
source("src/weighted_gini.R")
source("src/calc_gini_decomposition.R")
source("src/calc_gini_decomposition_by_rate.R")
source("src/round_half_up_to.R")
source("src/replicate_helpers.R")

# Parameters ################################
quantile_probs <- round(seq(0, 1, 0.05),2)
inflation_rates <- c(seq(0, 0.5, 0.05), 1, 2, 3)

output_dir <- "Results/gini_decomposition"

## Variable to inflate
variable_to_inflate <- "Assets_All_Housing"
ownership_variable <- "H_Owns_Housing"
output_suffix <- "housing"

# variable_to_inflate <- "Assets_All_Property"
# ownership_variable <- "H_Owns_Property"
# output_suffix <- "property"

# variable_to_inflate <- "Assets_PropertyOrLand"
# ownership_variable <- "H_Owns_PropertyOrLand"
# output_suffix <- "property-or-land"

# variable_to_inflate <- "Assets_Shares"
# ownership_variable <- "H_Owns_Shares"
# output_suffix <- "shares"

# variable_to_inflate <- "Assets_HousingAndShares"
# ownership_variable <- "H_Owns_HousingOrShares"
# output_suffix <- "housing-or-shares"

## Equivalisation parameters
equivalisation_variable <- "H_MOECD_Eq_Factor"
output_suffix <- paste0(output_suffix, "-moecd-eq")

# equivalisation_variable <- "H_sqrt_extra_adults_Eq_Factor"
# output_suffix <- paste0(output_suffix, "-sqrt_extra_adults-eq")

# equivalisation_variable <- "H_sqrt_people_Eq_Factor"
# output_suffix <- paste0(output_suffix, "-sqrt_people-eq")

# Analysis ##################################
dt_households <- fread("data/HES18_households.csv")

# Add weights
weights_path <- file.path(
  TAWApost::IDI_PROJECT_DIR,
  "TAWA Data/Linked/20200720/HYEFU20/Weights",
  "HES18_TY18_replicates_100reps_20210111.csv"
)
dt_households <- merge_on_weights(dt_households, weights_path)

# Calculate quantiles
dt_households <-
  add_quantiles(dt_households, equivalisation_variable, quantile_probs)

#### Gini decomposition ####
gini_decomposition <- calc_gini_decomposition_by_rate(
  dt_households,
  inflation_rates = inflation_rates,
  variable_to_inflate = variable_to_inflate,
  ownership_variable = ownership_variable,
  equivalisation_variable = equivalisation_variable
)

output_filename <- sprintf("gini_decomposition_%s.csv", output_suffix)
output_path <- file.path(output_dir, output_filename)
fwrite(gini_decomposition, output_path)

# Gini level table
gini_level_table <- dcast(
  gini_decomposition[
    Variable_Type == "Level", .(Gini_Type, Inflation_Rate, Variable, output_str)
  ],
  ... ~ Variable, value.var = "output_str"
)
gini_level_table[, ":="(
  Inflation_Rate = scales::percent(Inflation_Rate, accuracy = 1)
)]

output_filename <- sprintf("gini_level_table_%s.csv", output_suffix)
output_path <- file.path(output_dir, output_filename)
fwrite(gini_level_table, output_path)

# Change in Gini table
gini_change_table <- dcast(
  gini_decomposition[
    Variable_Type == "Level_Change", .(Gini_Type, Inflation_Rate, Variable, output_str)
  ],
  ... ~ Variable, value.var = "output_str"
)
gini_change_table[, ":="(
  Inflation_Rate = scales::percent(Inflation_Rate, accuracy = 1)
)]

output_filename <- sprintf("gini_change_table_%s.csv", output_suffix)
output_path <- file.path(output_dir, output_filename)
fwrite(gini_change_table, output_path)

# Unformatted values for 10% increase
change10_table <- gini_decomposition[
  Variable_Type == "Level_Change" &
    Gini_Type == "Relative" &
    Inflation_Rate == 0.1,
  .(
    Inflation_Rate, Gini_Type, Variable, Variable_Type,
    Value,
    Margin_Of_Error
  )
]

output_filename <- sprintf("gini_10_change_%s.csv", output_suffix)
output_path <- file.path(output_dir, output_filename)
fwrite(change10_table, output_path)
