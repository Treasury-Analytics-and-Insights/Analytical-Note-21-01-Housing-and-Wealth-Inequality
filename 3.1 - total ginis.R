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
output_dir <- "Results/total_ginis"

output_suffix <- "moecd_eq_factor"
equivalisation_variable <- "H_MOECD_Eq_Factor"

# output_suffix <- "sqrt_extra_adults_eq_factor"
# equivalisation_variable <- "H_sqrt_extra_adults_Eq_Factor"

# output_suffix <- "sqrt_people_eq_factor"
# equivalisation_variable <- "H_sqrt_people_Eq_Factor"

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

# Gini for housing and non-housing wealth
housing_and_nonhousing_gini <- dt_households[, .(
  Gini_Total = weighted_gini(
    x = Net_Worth / get(equivalisation_variable),
    weights = H_People*Weight
  ),
  Gini_Housing = weighted_gini(
    x = Net_Worth_Housing / get(equivalisation_variable),
    weights = H_People*Weight
  ),
  Gini_NonHousing = weighted_gini(
    x = Net_Worth_NonHousing / get(equivalisation_variable),
    weights = H_People*Weight
  ),
  Gini_HousingAssets_Owners = weighted_gini(
    x = Assets_All_Housing / get(equivalisation_variable),
    weights = H_People*Weight*(H_Owns_Housing == "Owner")
  ),
  Gini_HousingAssets_All = weighted_gini(
    x = Assets_All_Housing / get(equivalisation_variable),
    weights = H_People*Weight
  ),
  Population = sum(Weight),
  Sample = .N
), by = Replicate]

housing_and_nonhousing_gini_long <- melt(
  housing_and_nonhousing_gini,
  id.vars = c("Population", "Sample", "Replicate"),
  variable.name = "Variable", value.name = "Value"
)
housing_and_nonhousing_gini_long[, Rounding_Rule := "Gini"]

housing_and_nonhousing_gini_Margin_Of_Error <-
  replicates_to_MoE(housing_and_nonhousing_gini_long, "Variable") %>%
  suppress_MoE() %>% round_MoE()

output_filename <- sprintf("total_ginis_%s.csv", output_suffix)
output_path <- file.path(output_dir, output_filename)
fwrite(housing_and_nonhousing_gini_Margin_Of_Error, output_path)
