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

# Calculate property wealth distribution
housing_wealth_distribution_reps <- dt_households[, .(
  Housing = sum(Weight*Net_Worth_Housing),
  NonHousing = sum(Weight*Net_Worth_NonHousing),
  Population = sum(Weight),
  Sample = .N
), keyby = .(Replicate, Net_Worth_quantile)]

housing_wealth_distribution_reps <- melt(
  housing_wealth_distribution_reps, measure.vars = c("Housing", "NonHousing"),
  variable.name = "Variable", value.name = "Value"
)
housing_wealth_distribution_reps[, Rounding_Rule := "Total_Wealth"]

housing_wealth_distribution_reps_percentages <-
  copy(housing_wealth_distribution_reps)
housing_wealth_distribution_reps_percentages[
  , Value := Value / sum(Value), by = .(Replicate, Net_Worth_quantile)
]
housing_wealth_distribution_reps_percentages[, Rounding_Rule := "Percentage"]

housing_wealth_distribution_reps_total <- copy(housing_wealth_distribution_reps)
housing_wealth_distribution_reps_total[
  , Value := sum(Value), by = .(Replicate, Net_Worth_quantile)
]

housing_wealth_distribution_reps <- rbindlist(list(
  "Level" = housing_wealth_distribution_reps,
  "Percentage" = housing_wealth_distribution_reps_percentages,
  "Total" = housing_wealth_distribution_reps_total
), idcol = "Variable_Type")

housing_wealth_distribution <- replicates_to_MoE(
  housing_wealth_distribution_reps,
  c("Variable", "Net_Worth_quantile", "Variable_Type")
) %>%
  suppress_MoE() %>%
  round_MoE(list("Total_Wealth" = 100e6, "Percentage" = 0.1/100))

output_path <- file.path(output_dir, "housing_wealth_distribution.csv")
fwrite(housing_wealth_distribution, output_path)

# Plot
plot_data_cols <- c("Variable", "Net_Worth_quantile", "Value", "Variable_Type")
plot_data <- dcast(
  housing_wealth_distribution[, .SD, .SDcols = plot_data_cols],
  ... ~ Variable_Type, value.var = c("Value")
)

p_wealth <- ggplot(
  data = plot_data,
  aes(x = Net_Worth_quantile, y = Level, fill = Variable)
) +
  geom_col(position = "stack") +
  scale_y_continuous(
    name = "Total Wealth",
    labels = scales::label_dollar(scale = 1e-9, suffix = "b", accuracy = 1)
  ) +
  scale_x_continuous(
    name = "Total Wealth quantile",
    labels = scales::label_percent(accuracy = 1),
    breaks = scales::pretty_breaks(10)
  ) +
  scale_fill_discrete(
    name = "",
    breaks = c("Housing", "NonHousing"),
    labels = c("Housing wealth", "Non-Housing wealth")
  ) +
  coord_cartesian(xlim = c(0.05, 1)) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")

output_filename <- "household_housing_wealth_distribution.png"
output_path <- file.path(output_dir, output_filename)
ggsave(output_path, p_wealth, width = 6, height = 4, dpi = 600)

# Table
wealth_tables <- get_wealth_table(
  dt_households, replicate_var = "Replicate",
  group_vars = c("H_Owns_Housing")
)

output_path <- file.path(output_dir, "wealth_table_long.csv")
fwrite(wealth_tables$long, output_path)

output_path <- file.path(output_dir, "wealth_table_wide.csv")
fwrite(wealth_tables$wide, output_path, bom = TRUE)
