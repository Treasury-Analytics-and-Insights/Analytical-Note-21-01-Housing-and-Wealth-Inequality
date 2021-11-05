wealth_categories <- list(
  #### Assets ####
  "Assets_PropertyOwnerOccupied" = quote(DVClassCode %>% startsWith("W.1.1")),
  
  "Assets_PropertyOtherResidential" = quote(DVClassCode %>% startsWith("W.1.2.1")),
  "Assets_PropertyOtherNonResidential" = quote(DVClassCode %>% startsWith("W.1.2.2")),
  "Assets_PropertyLandOnly" = quote(DVClassCode %>% startsWith("W.1.2.3")),
  
  "Assets_Durables" = quote(DVClassCode %>% startsWith("W.1.3")),
  "Assets_Valuables" = quote(DVClassCode %>% startsWith("W.1.4")),
  "Assets_NonFinancial" = quote(DVClassCode %>% startsWith("W.1.5")),
  #
  "Assets_Currency" = quote(DVClassCode == "W.2.1.1.1"),
  "Assets_Deposits" = quote(DVClassCode == "W.2.1.2.1"),
  "Assets_Bonds" = quote(DVClassCode == "W.2.2.1.1"),
  #
  "Equity_Business_Property" = quote(DVClassCode == "W.2.3.1.1"),
  "Equity_Business_NonProperty" = quote(DVClassCode == "W.2.3.1.2"),
  #
  "Assets_Shares" = quote(DVClassCode %>% startsWith("W.2.4.1")),
  #
  "Equity_Trust" = quote(DVClassCode %in% c("W.2.4.2.1", "W.2.4.2.2")),
  "Equity_Other" = quote(DVClassCode %>% startsWith("W.2.4.2.3")),
  #
  "Assets_InvestmentFunds" = quote(DVClassCode == "W.2.5.1.1"),
  "Assets_LifeInsuranceFunds" = quote(DVClassCode == "W.2.6.1.1"),
  "Assets_PensionFunds" = quote(DVClassCode %>% startsWith("W.2.7.2")),
  
  "Assets_Loans" = quote(DVClassCode %>% startsWith("W.2.8.1")),
  "Assets_OtherFinancial" = quote(DVClassCode %>% startsWith("W.2.8.2.1")),
  
  #### Liabilities ####
  "Liabilities_PropertyOwnerOccupied" = quote(DVClassCode %>% startsWith("W.3.1")),
  "Liabilities_PropertyOtherResidential" = quote(DVClassCode %>% startsWith("W.3.2.1")),
  "Liabilities_PropertyOtherNonResidential" = quote(DVClassCode %>% startsWith("W.3.2.2")),
  "Liabilities_PropertyLandOnly" = quote(DVClassCode %>% startsWith("W.3.2.3")),
  
  "Liabilities_InvestmentLoans" = quote(DVClassCode %>% startsWith("W.3.3")),
  "Liabilities_DurableLoans" = quote(DVClassCode %>% startsWith("W.3.4")),
  "Liabilities_StudentLoans" = quote(DVClassCode %>% startsWith("W.3.5")),
  "Liabilities_CreditCards" = quote(DVClassCode %>% startsWith("W.3.6"))
)

business_categories <- list(
  #### Assets ####
  "Assets_Business_UnIncorporated_Property" = quote(DVClassCode %>% startsWith("B.1.1.1")),
  "Assets_Business_UnListed_Property" = quote(DVClassCode %>% startsWith("B.1.2.1")),
  "Assets_Business_UnIncorporated_NonProperty" = quote(DVClassCode %>% startsWith("B.1.1.2")),
  "Assets_Business_UnListed_NonProperty" = quote(DVClassCode %>% startsWith("B.1.2.2")),
  
  #### Liabilities ####
  "Liabilities_Business_UnIncorporated_Property" = quote(DVClassCode %>% startsWith("B.2.1.1")),
  "Liabilities_Business_UnListed_Property" = quote(DVClassCode %>% startsWith("B.2.2.1")),
  "Liabilities_Business_UnIncorporated_NonProperty" = quote(DVClassCode %>% startsWith("B.2.1.2")),
  "Liabilities_Business_UnListed_NonProperty" = quote(DVClassCode %>% startsWith("B.2.2.2"))
)

trust_categories <- list(
  #### Assets ####
  # Business
  "Assets_Trust_Business" = quote(DVClassCode %>% startsWith("T.1.1")),
  # Property
  "Assets_Trust_PropertyOwnerOccupied" = quote(DVClassCode %>% startsWith("T.1.2.1")),
  "Assets_Trust_PropertyOtherResidential" = quote(DVClassCode %>% startsWith("T.1.2.2")),
  "Assets_Trust_PropertyOtherNonResidential" = quote(DVClassCode %>% startsWith("T.1.2.3")),
  # Non-property
  "Assets_Trust_NonFinancial" = quote(DVClassCode %>% startsWith("T.1.2.4")),
  "Assets_Trust_Financial" = quote(DVClassCode %>% startsWith("T.1.2.5")),
  
  #### Liabilities ####
  # Business
  "Liabilities_Trust_Business" = quote(DVClassCode %>% startsWith("T.2.1")),
  # Property
  "Liabilities_Trust_PropertyOwnerOccupied" = quote(DVClassCode %>% startsWith("T.2.2.1")),
  "Liabilities_Trust_PropertyOtherResidential" = quote(DVClassCode %>% startsWith("T.2.2.2")),
  "Liabilities_Trust_PropertyOtherNonResidential" = quote(DVClassCode %>% startsWith("T.2.2.3")),
  # Non-property
  "Liabilities_Trust_NonFinancial" = quote(DVClassCode %>% startsWith("T.2.2.4")),
  "Liabilities_Trust_Financial" = quote(DVClassCode %>% startsWith("T.2.2.5"))
)

wealth_codes <- list(
  #Principal residence
  "W.1.1.1.1" =	"Principal residence",
  #Other owner-occupied dwellings
  "W.1.1.2.1" = "Other owner-occupied dwellings",	#Not collected
  
  #Other residential real estate
  "W.1.2.1.1" =	"Holiday homes excluding timeshares",
  "W.1.2.1.2" =	"Timeshares",
  "W.1.2.1.3" =	"Residential investment (rental) real estate",
  "W.1.2.1.4" =	"Other non-investment residential real estate",
  #Non-residential real estate (including commercial)
  "W.1.2.2.1" =	"Non-residential real estate (including commercial)",
  # Land only
  "W.1.2.3.1" =	"Land only",
  
  #Vehicles
  "W.1.3.1.1" = "Vehicles",
  #Other consumer durables
  "W.1.3.2.1" = "Other consumer durables",
  
  # Valuables
  "W.1.4.1.1" = "Valuables",
  
  # Intellectual property
  # "W.1.5.1.1" =	"Intellectual property", 	#Not collected
  # Other household non-financial assets
  "W.1.5.2.1" =	"Other household non-financial assets",
  
  
  # Currency
  "W.2.1.1.1" =	"Currency",
  # Deposits
  "W.2.1.2.1" =	"Deposits",
  
  # Bonds and other debt securities
  "W.2.2.1.1" = "Bonds and other debt securities",
  
  # Equity in own unincorporated enterprises
  "W.2.3.1.1" =	"Property equity in own unincorporated enterprises",
  "W.2.3.1.2" =	"Non-property equity in own unincorporated enterprises",
  
  # Shares in corporations
  "W.2.4.1.1" =	"Shares in listed corporations",
  "W.2.4.1.2" =	"Shares in unlisted corporations",
  # Other equity
  "W.2.4.2.1" =	"Non-financial equity held in a family trust",
  "W.2.4.2.2"	= "Financial equity held in a family trust",
  "W.2.4.2.3"	= "Other equity not held in family trust",
  
  # Mutual funds and other investment funds
  "W.2.5.1.1" =	"Mutual funds and other investment funds",
  
  # Life insurance funds
  "W.2.6.1.1" =	"Life insurance funds",
  # Annuities
  # "W.2.6.2.1" =	"Annuities",	#Not collected
  
  # Social insurance pension funds
  # W.2.7.1.1	Social insurance pension funds 	#Not collected
  # Private pension funds
  "W.2.7.2.1" =	"KiwiSaver pension funds",
  "W.2.7.2.2" =	"Non-KiwiSaver pension funds",
  
  # Loans made to trusts, other households or entities
  "W.2.8.1.1" =	"Loans made to trusts",
  "W.2.8.1.2" =	"Loans made to other households",
  "W.2.8.1.3" =	"Loans made to other entities",
  # Other household financial assets
  "W.2.8.2.1" =	"Other household  financial assets",
  
  
  # Principal residence loans
  "W.3.1.1.1" =	"Principal residence loans",
  # Other owner-occupied dwellings loans
  # W.3.1.2.1	Other owner-occupied dwellings loans	#Not collected
  
  # Other residential real estate loans
  "W.3.2.1.1" =	"Holiday homes excluding timeshares loans",
  # "W.3.2.1.2" =	"Timeshares loans",	#Not collected
  "W.3.2.1.3" =	"Residential investment (rental) real estate loans",
  "W.3.2.1.4" =	"Other non-investment residential real estate loans",
  # Non-residential real estate (including commercial) loans
  "W.3.2.2.1" =	"Non-residential real estate (including commercial) loans",
  # Land only loans
  "W.3.2.3.1" =	"Land only loans",
  
  # Financial asset loans	Financial asset loans
  "W.3.3.1.1" =	"Financial asset loans",
  # Valuables loans
  "W.3.3.2.1" =	"Valuables loans",
  # Intellectual property and other non-financial asset loans
  "W.3.3.3.1" =	"Intellectual property loans	Not collected",
  # Other investment loans
  "W.3.3.4.1" =	"Other investment loans",
  
  # Vehicle loans
  "W.3.4.1.1" =	"Vehicle loans",
  # Other consumer durable loans
  "W.3.4.2.1" =	"Other consumer durable loans",
  
  # Education loans
  "W.3.5.1.1" =	"Education loans",
  
  # Other  loans and liabilities
  "W.3.6.1.1" =	"Credit card debt",
  "W.3.6.1.2" =	"Other consumer credit loans and liabilities"
)

redundant_aggregate_equity_wealth_codes <- c(
  "W.2.3.1.1", # "Property equity in own unincorporated enterprises: B.1.1.1 - B.2.1.1"
  "W.2.3.1.2", # "Non-property equity in own unincorporated enterprises: B.1.1.2 - B.2.1.2"
  "W.2.4.2.1", # "Non-financial equity held in a family trust: T.1.2.{1,2,3,4} - T.2.2.{1,2,3,4}"
  "W.2.4.2.2", # "Financial equity held in a family trust: (T.1.2.5 - T.2.2.5) + (T.1.1.1. - T.2.1.1)"
  "W.2.4.2.3"  # "Non-trust Other Equity: B.1.2.2 - B.2.2.2
)

business_wealth_codes <- list(
  #### Assets ####
  ## Unincorporated enterprises ##
  # Property assets in own unincorporated enterprises
  "B.1.1.1.1" =	"Farm related property assets in own unincorporated enterprises",
  "B.1.1.1.2" =	"Non-farm related property assets in own unincorporated enterprises",
  # Non-property assets in own unincorporated enterprises
  "B.1.1.2.1" = "Farm related non-property assets in own unincorporated enterprises",
  "B.1.1.2.2" = "Non-farm related non- property assets in own unincorporated enterprises",
  ## Incorporated but unlisted enterprises ##
  # Property assets in unlisted incorporated enterprises
  "B.1.2.1.1" = "Farm related property assets in unlisted incorporated enterprises",
  "B.1.2.1.2" = "Non-farm related property assets in unlisted incorporated enterprises",
  # Non-property assets in unlisted incorporated enterprises 	
  "B.1.2.2.1" = "Farm related non-property assets in unlisted incorporated enterprises",
  "B.1.2.2.2" = "Non-farm related non- property assets in unlisted incorporated enterprises",
  
  #### Liabilities ####
  ## Unincorporated enterprises ##
  # Property liabilities in own unincorporated enterprises 	
  "B.2.1.1.1" = "Farm related property liabilities in own unincorporated enterprises",
  "B.2.1.1.2" = "Non-farm related property liabilities in own unincorporated enterprises",
  # Non-property liabilities in own unincorporated enterprises 	
  "B.2.1.2.1" = "Farm related non-property liabilities in own unincorporated enterprises",
  "B.2.1.2.2" = "Non-farm related non- property liabilities in own unincorporated enterprises",
  ## Incorporated but unlisted enterprises ##
  # Property assets in unlisted incorporated enterprises 	
  "B.2.2.1.1" = "Farm related property liabilities in unlisted incorporated enterprises",
  "B.2.2.1.2" = "Non-farm related property liabilities in unlisted incorporated enterprises",
  # Non-property liabilities in unlisted incorporated enterprises 	
  "B.2.2.2.1" = "Farm related non-property liabilities in unlisted incorporated enterprises",
  "B.2.2.2.2" = "Non-farm related non- property liabilities in unlisted incorporated enterprises"
)

trust_wealth_codes <- list(
  #### Assets ####
  # Assets of businesses related to household
  "T.1.1.1.1" = "Farm assets in businesses related to household",
  "T.1.1.1.2" = "Non-farm assets in businesses related to household",
  # Owner-occupied property
  "T.1.2.1.1" = "Owner-occupied property",
  # Other residential real estate
  "T.1.2.2.1" = "Non-investment residential property including land",
  "T.1.2.2.2" = "Investment residential (rental) real estate",
  # Non-residential real estate (including commercial)
  "T.1.2.3.1" = "Non-residential real estate (including commercial)",
  # Other non-financial non-property assets
  "T.1.2.4.1" = "Other non-financial non-property assets",
  # Financial assets
  "T.1.2.5.1" = "Financial assets",
  
  #### Liabilities ####
  # Liabilities of businesses related to household
  "T.2.1.1.1" = "Farm loans in businesses related to household",
  "T.2.1.1.2" = "Non-farm loans in businesses related to household",
  # Owner-occupied property loans
  "T.2.2.1.1" = "Owner-occupied property loans",
  # Other residential real estate loans
  "T.2.2.2.1" = "Non-investment residential property including land loans",
  "T.2.2.2.2" = "Investment residential (rental) real estate loans",
  # Non-residential real estate (including commercial) loans
  "T.2.2.3.1" = "Non-residential real estate (including commercial) loans",
  # Other non-financial non-property loans
  "T.2.2.4.1" = "Other non-financial non-property loans",
  # Loans for financial assets
  "T.2.2.5.1" = "Loans for financial assets"
)