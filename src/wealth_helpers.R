extract_wealth_data <- function(wealth, concordance) {
  # The purpose of this function is to merge on H_ID
  # while extracting particular wealth columns
  wealth <- wealth[, .(
    snz_hes_hhld_uid,
    snz_hes_uid,
    P_Attributes_PersonNumberInHES = People_No,
    DVClassCode, DVAL, DVAmount
  )]
  
  # Merge concordance for H_ID
  wealth <- merge(
    wealth,
    concordance[, .(snz_hes_hhld_uid, H_ID)],
    by = "snz_hes_hhld_uid"
  )
  
  return(wealth)
}

# NOTE: Modifies input data.table in-place
categorise_wealth_data <- function(wealth, wealth_categories) {
  for (ii in seq_along(wealth_categories)) {
    category <- names(wealth_categories)[ii]
    rule <- wealth_categories[[ii]]
    wealth[eval(rule), Category := category]
  }
  return(wealth)
}

exclude_redundant_aggregate_equity <- function(wealth) {
  wealth <- wealth[!(DVClassCode %in% redundant_aggregate_equity_wealth_codes)]
  return(wealth)
}

wealth_long_to_wide <- function(wealth, id_vars) {
  wealth_summary <- wealth[, .(Value = sum(DVAmount)), by = c(id_vars, "Category")]
  
  wealth_assets <- wealth[
    DVAL == "A", .(Category = "Assets", Value = sum(DVAmount)), by = c(id_vars)
  ]
  wealth_liabilities <- wealth[
    DVAL == "L", .(Category = "Liabilities", Value = sum(DVAmount)), by = c(id_vars)
  ]
  
  wealth_summary <- rbindlist(list(
    wealth_summary,
    wealth_assets,
    wealth_liabilities
  ))
  wealth_summary_wide <- dcast(
    wealth_summary,
    ... ~ Category,
    value.var = "Value", fill = 0
  )
  wealth_summary_wide[, Net_Worth := Assets - Liabilities]
  
  setcolorder(
    wealth_summary_wide,
    c(
      "H_ID", "P_Attributes_PersonNumberInHES",
      "Assets", "Liabilities", "Net_Worth"
    )
  )
  return(wealth_summary_wide)
}

add_extra_wealth_categories <- function(wealth_summary_wide) {
  # First check that all variable exist; if they do not, add them with zeros
  missing_categories <- setdiff(
    c(
      names(wealth_categories),
      names(business_categories),
      names(trust_categories)
    ),
    names(wealth_summary_wide)
  )
  if (length(missing_categories) > 0) {
    wealth_summary_wide[, (missing_categories) := 0]
  }
  
  # Calculate extra wealth categories
  #### Assets ####
  
  ## Property Assets
  # Household
  wealth_summary_wide[, ":="(
    Assets_Household_Housing =
      Assets_PropertyOwnerOccupied +
      Assets_PropertyOtherResidential,
    Assets_Household_Property =
      Assets_PropertyOwnerOccupied +
      Assets_PropertyOtherResidential +
      Assets_PropertyOtherNonResidential
    # Exclude Land-only
  )]
  # Trust
  wealth_summary_wide[, ":="(
    Assets_Trust_Housing =
      Assets_Trust_PropertyOwnerOccupied +
      Assets_Trust_PropertyOtherResidential,
    Assets_Trust_Property =
      Assets_Trust_PropertyOwnerOccupied +
      Assets_Trust_PropertyOtherResidential +
      Assets_Trust_PropertyOtherNonResidential
  )]
  # Business
  wealth_summary_wide[, ":="(
    Assets_Business_Property =
      Assets_Business_UnIncorporated_Property + 
      Assets_Business_UnListed_Property
  )]
  # All
  wealth_summary_wide[, ":="(
    Assets_All_Housing =
      Assets_Household_Housing +
      Assets_Trust_Housing,
    Assets_All_Property =
      Assets_Household_Property + 
      Assets_Business_Property +
      Assets_Trust_Property
  )]
  
  ## Physical Assets
  # Household
  wealth_summary_wide[, ":="(
    Assets_Household_Physical =
      Assets_Durables +
      Assets_Valuables +
      Assets_NonFinancial
  )]
  # All
  wealth_summary_wide[, ":="(
    Assets_All_Physical =
      Assets_Household_Physical +
      Assets_Trust_NonFinancial
  )]
  
  ## Financial Assets
  # Household
  wealth_summary_wide[, ":="(
    Assets_Household_Financial =
      Assets_Currency +
      Assets_Deposits +
      Assets_Bonds +
      Assets_Shares +
      Assets_InvestmentFunds +
      Assets_LifeInsuranceFunds +
      Assets_Loans +
      Assets_OtherFinancial
  )]
  # Business
  wealth_summary_wide[, ":="(
    Assets_Business_NonProperty =
      Assets_Business_UnIncorporated_NonProperty +
      Assets_Business_UnListed_NonProperty
  )]
  # All
  wealth_summary_wide[, ":="(
    Assets_All_Financial =
      Assets_Household_Financial +
      Assets_Business_NonProperty +
      Assets_Trust_Financial
  )]
  
  ## Pension funds
  wealth_summary_wide[, ":="(
    Assets_All_PensionFunds = Assets_PensionFunds
  )]
  
  ## Assets (Business, Trust, Other)
  wealth_summary_wide[, ":="(
    Assets_All_BusinessTrustOther =
      Assets_Trust_Business
  )]
  
  ## Equity (Business, Trust, Other) - redundant but maybe useful?
  # Business
  wealth_summary_wide[, ":="(
    Equity_Business =
      Equity_Business_Property +
      Equity_Business_NonProperty
  )]
  wealth_summary_wide[, ":="(
    Equity_All_BusinessTrustOther =
      Equity_Business +
      Equity_Trust +
      Equity_Other
  )]
  
  #### Liabilities ####
  ## Property
  # Household
  wealth_summary_wide[, ":="(
    Liabilities_Household_Housing =
      Liabilities_PropertyOwnerOccupied +
      Liabilities_PropertyOtherResidential,
    Liabilities_Household_Property =
      Liabilities_PropertyOwnerOccupied +
      Liabilities_PropertyOtherResidential +
      Liabilities_PropertyOtherNonResidential
    # Exclude Land-only
  )]
  # Business
  wealth_summary_wide[, ":="(
    Liabilities_Business_Property =
      Liabilities_Business_UnIncorporated_Property +
      Liabilities_Business_UnListed_Property
  )]
  # Trust
  wealth_summary_wide[, ":="(
    Liabilities_Trust_Housing =
      Liabilities_Trust_PropertyOwnerOccupied +
      Liabilities_Trust_PropertyOtherResidential,
    Liabilities_Trust_Property =
      Liabilities_Trust_PropertyOwnerOccupied +
      Liabilities_Trust_PropertyOtherResidential +
      Liabilities_Trust_PropertyOtherNonResidential
  )]
  # All
  wealth_summary_wide[, ":="(
    Liabilities_All_Housing =
      Liabilities_Household_Housing +
      Liabilities_Trust_Housing,
    Liabilities_All_Property =
      Liabilities_Household_Property +
      Liabilities_Business_Property +
      Liabilities_Trust_Property
  )]
  
  ## Student Loans
  ## Pension funds
  wealth_summary_wide[, ":="(
    Liabilities_All_StudentLoans = Liabilities_StudentLoans
  )]
  
  ## Other liabilities
  # Household
  wealth_summary_wide[, ":="(
    Liabilities_Household_Other =
      Liabilities_InvestmentLoans +
      Liabilities_DurableLoans +
      Liabilities_CreditCards
  )]
  # Business
  wealth_summary_wide[, ":="(
    Liabilities_BusinessOther =
      Liabilities_Business_UnIncorporated_NonProperty +
      Liabilities_Business_UnListed_NonProperty
  )]
  # Trust
  wealth_summary_wide[, ":="(
    Liabilities_TrustOther =
      Liabilities_Trust_NonFinancial +
      Liabilities_Trust_Business +
      Liabilities_Trust_Financial
  )]
  # All
  wealth_summary_wide[, ":="(
    Liabilities_All_Other =
      Liabilities_Household_Other +
      Liabilities_BusinessOther +
      Liabilities_TrustOther
  )]
  
  
  ## Trust equity (from Trust data, not household)
  wealth_summary_wide[, ":="(
    Equity_Trust_NonFinancial =
      Assets_Trust_Property - Liabilities_Trust_Property +
      Assets_Trust_NonFinancial - Liabilities_Trust_NonFinancial
  )]
  wealth_summary_wide[, ":="(
    Equity_Trust_Financial =
      Assets_Trust_Business - Liabilities_Trust_Business +
      Assets_Trust_Financial - Liabilities_Trust_Financial
  )]
  wealth_summary_wide[, ":="(
    Equity_Business_Unincorporated_Property =
      Assets_Business_UnIncorporated_Property - Liabilities_Business_UnIncorporated_Property,
    Equity_Business_Unincorporated_NonProperty =
      Assets_Business_UnIncorporated_NonProperty - Liabilities_Business_UnIncorporated_NonProperty,
    Equity_Business_UnListed_Property =
      Assets_Business_UnListed_Property - Liabilities_Business_UnListed_Property,
    Equity_Business_UnListed_NonProperty =
      Assets_Business_UnListed_NonProperty - Liabilities_Business_UnListed_NonProperty
  )]
  
  return(wealth_summary_wide)
}