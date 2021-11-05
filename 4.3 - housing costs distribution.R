library(data.table)
library(magrittr)
library(ggplot2)
source("src/merge_on_weights.R")
source("src/calc_quantiles.R")
source("src/add_quantiles.R")
source("src/round_half_up_to.R")
source("src/replicate_helpers.R")
source("src/get_wealth_table.R")

# Parameters ################################
quantile_probs <- round(seq(0, 1, 0.05),2)
equivalisation_variable <- "H_MOECD_Eq_Factor"
output_dir <- "Results/distributions"

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

# Calculate material hardship distribution by ownership of housing
housing_costs_distribution_reps <- dt_households[, .(
  Population = sum(Weight),
  Sample = .N
), keyby = .(
  Replicate, Net_Worth_quantile, H_Owns_Housing, H_Has_High_Housing_Costs
)]
housing_costs_distribution_reps[, Value := Population]

housing_costs_distribution_reps[, Rounding_Rule := "Population"]

housing_costs_distribution <- replicates_to_MoE(
  housing_costs_distribution_reps,
  c("Net_Worth_quantile", "H_Has_High_Housing_Costs", "H_Owns_Housing")
) %>% suppress_MoE() %>% round_MoE(list("Population" = 1e3))

housing_costs_distribution[
  , H_Has_High_Housing_Costs := factor(
    H_Has_High_Housing_Costs, levels = c(TRUE, FALSE)
  )
]

output_path <- file.path(output_dir, "housing_costs_distribution.csv")
fwrite(housing_costs_distribution, output_path)

# Plot
p_housing_costs <- plot_distribution_breakdown(
  housing_costs_distribution,
  ownership_var = "H_Owns_Housing",
  fill_var = "H_Has_High_Housing_Costs",
  fill_var_breaks = c(
    "High housing costs" = TRUE,
    "Not high housing costs" = FALSE
  )
)

output_filename <- "household_housing_housing_costs_distribution.png"
output_path <- file.path(output_dir, output_filename)
ggsave(output_path, p_housing_costs, width = 6, height = 3.5, dpi = 600)

# Table
housing_costs_wealth_tables <- get_wealth_table(
  dt_households, replicate_var = "Replicate",
  group_vars = c("H_Owns_Housing", "H_Has_High_Housing_Costs")
)
owners_housing_costs_wealth_tables <- get_wealth_table(
  dt_households[H_Owns_Housing == "Owner"], replicate_var = "Replicate",
  group_vars = c("H_Owns_Housing", "H_Has_High_Housing_Costs")
)
nonowners_housing_costs_wealth_tables <- get_wealth_table(
  dt_households[H_Owns_Housing == "Non-owner"], replicate_var = "Replicate",
  group_vars = c("H_Owns_Housing", "H_Has_High_Housing_Costs")
)

output_path <- file.path(output_dir, "housing_costs_wealth_table_long.csv")
fwrite(housing_costs_wealth_tables$long, output_path)

output_path <- file.path(output_dir, "housing_costs_wealth_table_wide.csv")
fwrite(housing_costs_wealth_tables$wide, output_path, bom = TRUE)

output_path <- file.path(
  output_dir, "within-owners_housing_costs_wealth_table_long.csv"
)
fwrite(owners_housing_costs_wealth_tables$long, output_path)

output_path <- file.path(
  output_dir, "within-owners_housing_costs_wealth_table_wide.csv"
)
fwrite(owners_housing_costs_wealth_tables$wide, output_path, bom = TRUE)

output_path <- file.path(
  output_dir, "within-non-owners_housing_costs_wealth_table_long.csv"
)
fwrite(nonowners_housing_costs_wealth_tables$long, output_path)

output_path <- file.path(
  output_dir, "within-non-owners_housing_costs_wealth_table_wide.csv"
)
fwrite(nonowners_housing_costs_wealth_tables$wide, output_path, bom = TRUE)
