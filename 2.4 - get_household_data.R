library(data.table)
library(magrittr)

# Parameters for the wealth distribution
quantile_probs <- c(seq(0, 1, 0.1))
income_quantile_probs <- quantile_probs
net_worth_quantile_probs <- quantile_probs

quantile_prob_names <- 1:10
income_quantile_prob_names <- quantile_prob_names
net_worth_quantile_prob_names <- quantile_prob_names

tawa_input_paths <- "TAWA Output/HES18_TY18_SQ.csv"

# Load TAWA data, aggregate to household level
dt_people <- fread(tawa_input_paths)

dt_households <- dt_people[, .(
  H_Income_Disposable = sum(P_Income_Disposable),
  H_Benefits_Accommodation_Abated = sum(P_Benefits_Accommodation_Abated),
  H_People_GTE_18 = sum(P_Attributes_Age >= 18),
  H_People_LT_18 = sum(P_Attributes_Age < 18),
  H_People_LT_14 = sum(P_Attributes_Age < 14),
  H_People_GTE_14 = sum(P_Attributes_Age >= 14),
  H_Dependents = sum(P_Attributes_Dependent == TRUE),
  H_NonDependents = sum(P_Attributes_Dependent == FALSE),
  H_People = .N,
  H_Families = uniqueN(F_ID)
), keyby = H_ID]

dt_households[, ":="(
  H_MOECD_Eq_Factor = 1 + 0.5*(H_People_GTE_14 - 1) + 0.3*H_People_LT_14,
  H_sqrt_extra_adults_Eq_Factor = 1 + sqrt(H_People_GTE_18 - 1),
  H_sqrt_people_Eq_Factor = sqrt(H_People)
)]

#### Wealth - Net Worth
# Load wealth
dt_wealth_people <- fread("data/HES18_wealth_data.csv")
wealth_merge_cols <- c("H_ID", "P_Attributes_PersonNumberInHES")
dt_wealth_people <- merge(
  dt_wealth_people,
  dt_people[, .SD, .SDcols = wealth_merge_cols],
  by = wealth_merge_cols,
  all = TRUE
)
dt_wealth_people[, P_Attributes_PersonNumberInHES := NULL]

# Aggregate to household level and replace any NA's with zero
dt_wealth_households <- dt_wealth_people[
  , lapply(.SD, function(x) sum(x, na.rm = TRUE)), by = "H_ID"
]

# Merge household net worth quantile
dt_households <-
  merge(dt_households, dt_wealth_households, by = c("H_ID"), all = TRUE)

#### Material hardship
dt_material_hardship <- fread("data/material_hardship.csv")
dt_households <- merge(dt_households, dt_material_hardship, by = "H_ID")

#### Housing costs
housing_vars <- c("H_HousingCosts_Total")
housing_costs <- dt_people[
  , lapply(.SD, first), .SDcols = housing_vars, by = "H_ID"
]
dt_households <- merge(dt_households, housing_costs, by = "H_ID")

# Calculate housing cost flag
dt_households[, ":="(
  H_Has_High_Housing_Costs = H_HousingCosts_Total > 0.4*H_Income_Disposable
)]

#### Calculate extra wealth variables
dt_households[, ":="(
  Net_Worth_Physical = Assets_All_Physical - Liabilities_DurableLoans,
  Net_Worth_Financial = Assets_All_Financial - (
    Liabilities_All_StudentLoans +
      Liabilities_InvestmentLoans +
      Liabilities_CreditCards
  ),
  Net_Worth_PensionFunds = Assets_All_PensionFunds,
  Net_Worth_BusinessTrustOther = Assets_All_BusinessTrustOther -
    Liabilities_All_Other
)]

# Property or Land
dt_households[, ":="(
  Net_Worth_PropertyOwnerOccupied =
    Assets_PropertyOwnerOccupied - Liabilities_PropertyOwnerOccupied,
  Net_Worth_PropertyOtherResidential =
    Assets_PropertyOtherResidential - Liabilities_PropertyOtherResidential,
  Net_Worth_PropertyOtherNonResidential =
    Assets_PropertyOtherNonResidential - Liabilities_PropertyOtherNonResidential,
  Net_Worth_PropertyLandOnly =
    Assets_PropertyLandOnly - Liabilities_PropertyLandOnly,
  # Trust
  Net_Worth_Trust_PropertyOwnerOccupied =
    Assets_Trust_PropertyOwnerOccupied -
    Liabilities_Trust_PropertyOwnerOccupied,
  Net_Worth_Trust_PropertyOtherResidential =
    Assets_Trust_PropertyOtherResidential -
    Liabilities_Trust_PropertyOtherResidential,
  Net_Worth_Trust_PropertyOtherNonResidential =
    Assets_Trust_PropertyOtherNonResidential -
    Liabilities_Trust_PropertyOtherNonResidential,
  # Business
  Net_Worth_Business_UnIncorporated_Property =
    Assets_Business_UnIncorporated_Property -
    Liabilities_Business_UnIncorporated_Property,
  Net_Worth_Business_UnListed_Property =
    Assets_Business_UnListed_Property -
    Liabilities_Business_UnListed_Property
)]
dt_households[, ":="(
  Assets_PropertyOrLand = (
    Assets_PropertyOwnerOccupied +
      Assets_PropertyOtherResidential +
      Assets_PropertyOtherNonResidential +
      Assets_PropertyLandOnly +
      Assets_Trust_PropertyOwnerOccupied +
      Assets_Trust_PropertyOtherResidential +
      Assets_Trust_PropertyOtherNonResidential +
      Assets_Business_UnIncorporated_Property +
      Assets_Business_UnListed_Property
  ),
  Liabilities_PropertyOrLand = (
    Liabilities_PropertyOwnerOccupied +
      Liabilities_PropertyOtherResidential +
      Liabilities_PropertyOtherNonResidential +
      Liabilities_PropertyLandOnly +
      Liabilities_Trust_PropertyOwnerOccupied +
      Liabilities_Trust_PropertyOtherResidential +
      Liabilities_Trust_PropertyOtherNonResidential +
      Liabilities_Business_UnIncorporated_Property +
      Liabilities_Business_UnListed_Property
  )
)]
dt_households[, ":="(
  Net_Worth_PropertyOrLand = Assets_PropertyOrLand - Liabilities_PropertyOrLand
)]

dt_households[, ":="(
  Net_Worth_Housing = Assets_All_Housing - Liabilities_All_Housing,
  Net_Worth_Property = Assets_All_Property - Liabilities_All_Property,
  Assets_HousingAndShares = Assets_All_Housing + Assets_Shares
)]
dt_households[, ":="(
  Net_Worth_NonHousing = Net_Worth - Net_Worth_Housing,
  Net_Worth_NonProperty = Net_Worth - Net_Worth_Property,
  Net_Worth_NotPropertyOrLand = Net_Worth - Net_Worth_PropertyOrLand
)]

#### Calculate wealth ownership flags
# Housing
dt_households[, H_Owns_Housing := factor(
  Assets_All_Housing != 0 | Liabilities_All_Housing != 0,
  levels = c(FALSE, TRUE), labels = c("Non-owner", "Owner")
)]

# Property
dt_households[, H_Owns_Property := factor(
  Assets_All_Property != 0 | Liabilities_All_Property != 0,
  levels = c(FALSE, TRUE), labels = c("Non-owner", "Owner")
)]

# Property or land
dt_households[, H_Owns_PropertyOrLand := factor(
  Assets_PropertyOrLand != 0 | Liabilities_PropertyOrLand != 0,
  levels = c(FALSE, TRUE), labels = c("Non-owner", "Owner")
)]

# Shares
dt_households[, H_Owns_Shares := factor(
  Assets_Shares != 0,
  levels = c(FALSE, TRUE), labels = c("Non-owner", "Owner")
)]

# Housing Or Shares
dt_households[, H_Owns_HousingOrShares := factor(
  H_Owns_Housing == "Owner" | H_Owns_Shares == "Owner",
  levels = c(FALSE, TRUE), labels = c("Non-owner", "Owner")
)]

# Save output
fwrite(dt_households, "data/HES18_households.csv")
