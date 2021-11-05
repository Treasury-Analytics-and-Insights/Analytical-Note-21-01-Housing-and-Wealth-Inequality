library(data.table)
library(magrittr)
source("src/HES18_wealth_codes.R")
source("src/wealth_helpers.R")

wealth_path <- "data/IDI_household_wealth.csv"
business_path <- "data/IDI_business_wealth.csv"
trust_path <- "data/IDI_trust_wealth.csv"

concordance_path <- "data/IDI_concordance.csv"
dt_concordance <- fread(concordance_path)

# Wealth table
household_wealth <-
  fread(wealth_path) %>%
  extract_wealth_data(dt_concordance) %>%
  categorise_wealth_data(wealth_categories)

# Business wealth table
business_wealth <-
  fread(business_path) %>%
  extract_wealth_data(dt_concordance) %>%
  categorise_wealth_data(business_categories)

# Trust wealth table
trust_wealth <-
  fread(trust_path) %>%
  extract_wealth_data(dt_concordance) %>%
  categorise_wealth_data(trust_categories)

# Summarise by category
id_vars <- c("H_ID", "P_Attributes_PersonNumberInHES")

all_wealth <- rbindlist(list(
  household_wealth,
  business_wealth,
  trust_wealth
))

fwrite(all_wealth, "data/all_wealth.csv")

wealth_summary_wide <- wealth_long_to_wide(all_wealth, id_vars)
wealth_summary_wide <- add_extra_wealth_categories(wealth_summary_wide)

# Make a correction for double-counting of trust net worth as household assets
wealth_summary_wide[, ":="(
  Equity_Household_sum =
    Equity_Business_Property +
    Equity_Business_NonProperty +
    Equity_Trust +
    Equity_Other
)]
wealth_summary_wide[, ":="(
  Equity_Trust_sum =
    Equity_Trust_Financial +
    Equity_Trust_NonFinancial +
    Equity_Business_Unincorporated_Property +
    Equity_Business_Unincorporated_NonProperty +
    Equity_Business_UnListed_Property +
    Equity_Business_UnListed_NonProperty
)]
wealth_summary_wide[, ":="(
  Assets = Assets - Equity_Trust_sum
)]
wealth_summary_wide[, ":="(
  Assets_All_BusinessTrustOther =
    Assets_All_BusinessTrustOther +
    (Equity_Household_sum - Equity_Trust_sum)
)]

# Recalc Net Worth since we changed Assets
wealth_summary_wide[, ":="(
  Net_Worth = Assets - Liabilities
)]

summary_vars <- c("Assets", "Liabilities", "Net_Worth")

# Output all variables except "Equity" since we have assigned all Equity amounts
# to various Assets and Liabilities
final_vars <- names(wealth_summary_wide) %>%
  setdiff(summary_vars) %>%
  setdiff(id_vars) %>%
  .[!(. %like% "Equity")]
output_vars <- c(id_vars, summary_vars, final_vars)

wealth_summary_wide_out <- wealth_summary_wide[, .SD, .SDcols = output_vars]

# Save data
fwrite(wealth_summary_wide_out, "data/HES18_wealth_data.csv")
