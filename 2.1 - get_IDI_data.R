source("src/get_IDI_helpers.R")
source("src/TAWArun_setup.R")

# Wealth data
household_wealth_SQL_query <- "
SELECT [snz_hes_hhld_uid]
      ,[snz_hes_uid]
      ,[person_nbr] AS [People_No]
      ,[asset_or_liab_code] AS [DVAL]
      ,[wealth_class_code] AS [DVClassCode]
      ,[amount_nbr] AS [DVAmount]
  FROM [IDI_Adhoc].[clean_read_HES].[hes_networth_wealth]"

IDI_household_wealth <- read_sql_table(household_wealth_SQL_query)
fwrite(IDI_household_wealth, "data/IDI_household_wealth.csv")

business_SQL_query <- "
SELECT [snz_hes_hhld_uid]
      ,[snz_hes_uid]
      ,[person_nbr] AS [People_No]
      ,[asset_or_liab_code] AS [DVAL]
      ,[business_class_code] AS [DVClassCode]
      ,[amount_nbr] AS [DVAmount]
  FROM [IDI_Adhoc].[clean_read_HES].[hes_networth_business]"

IDI_business_wealth <- read_sql_table(business_SQL_query)
fwrite(IDI_business_wealth, "data/IDI_business_wealth.csv")

trust_SQL_query <- "
SELECT [snz_hes_hhld_uid]
      ,[snz_hes_uid]
      ,[person_nbr] AS [People_No]
      ,[asset_or_liab_code] AS [DVAL]
      ,[trust_class_code] AS [DVClassCode]
      ,[amount_nbr] AS [DVAmount]
  FROM [IDI_Adhoc].[clean_read_HES].[hes_networth_trust]"

IDI_trust_wealth <- read_sql_table(trust_SQL_query)
fwrite(IDI_trust_wealth, "data/IDI_trust_wealth.csv")

# Material hardship data
material_wellbeing_query <- "
SELECT [snz_hes_hhld_uid]
      ,[snz_hes_uid]
      ,[HES_Year]
      ,[People_no]
      ,[Sex]
      ,[DVAge]
      ,[DVUnder15]
      ,[Month_of_birth]
      ,[DVEthnicity]
      ,[DVEthnicity_desc]
      ,[DVYearsInNZ]
      ,[DVCOBTreasury]
      ,[DVHQual]
      ,[DVHQual_desc]
      ,[DVStudyTime]
      ,[DVCurEdInstMain]
      ,[DVFTPTStatus]
      ,[DVLFStatus]
      ,[DVLFStatus_desc]
      ,[Retired]
      ,[ASAREA]
      ,[NucleusNumber]
      ,[Ref_Person]
      ,[Rel_to_RefPerson]
      ,[DVRoleInFN]
      ,[DVFamRel]
      ,[DVFam_NuminFamNuc]
      ,[DVDepChild]
      ,[DVFam_ParentRole]
      ,[DVFam_ChildRole]
      ,[DVFam_WithPartner]
      ,[DVFam_DepStat]
      ,[DVLFTotHrsWkd]
      ,[DVMonth]
      ,[DVQuarter]
      ,[DVYear]
      ,[FinalWgt]
      ,[Total_Person_RegInc]
      ,[Total_Person_AllInc]
      ,[Age_imputed]
      ,[Jobs_imputed]
      ,[Benefits_imputed]
      ,[Investments_imputed]
      ,[MWMPerson]
      ,[MWMqMeal]
      ,[MWMqGoodBed]
      ,[MWMqGoodShoes]
      ,[MWMqSuitableClothes]
      ,[MWMqInsurance]
      ,[MWMqCarOrVan]
      ,[MWMqComputerNet]
      ,[MWMqFamilyMeal]
      ,[MWMqGiveGifts]
      ,[MWMqLocalHols]
      ,[MWMqOSHoliday]
      ,[MWMqNoFruit]
      ,[MWMqLessMeat]
      ,[MWMqOldClothes]
      ,[MWMqNoDoctor]
      ,[MWMqNoDentist]
      ,[MWMqFewerLocalTrips]
      ,[MWMqLessHobby]
      ,[MWMqFeelCold]
      ,[MWMqOldAppliance]
      ,[MWMqOldFurniture]
      ,[MWMqDampOrMould]
      ,[MWMqKeepHouseWarm]
      ,[MWMqLimitClothesShoes]
      ,[MWMqSpot300]
      ,[MWMqUnexpected500]
      ,[MWMqUnexpected1500]
      ,[MWMqPersonalMoney]
      ,[MWMqUtilities]
      ,[MWMqCarLate]
      ,[MWMqRentMortLate]
      ,[MWMqBorrow]
      ,[MWMqGotHelp]
      ,[MWMqIncomeEnough]
      ,[MWMqLifeSatisfied]
      ,[MWMReason_NoMeal]
      ,[MWMReason_NoGoodBed]
      ,[MWMReason_NoGoodShoes]
      ,[MWMReason_NoSuitableClothes]
      ,[MWMReason_NoInsurance]
      ,[MWMReason_NoCarOrVan]
      ,[MWMReason_NoComputerNet]
      ,[MWMReason_NoFamilyMeal]
      ,[MWMReason_NoGiveGifts]
      ,[MWMReason_NoLocalHols]
      ,[MWMReason_NoOSHoliday]
      ,[DVSampleYearMonth]
  FROM [IDI_Adhoc].[clean_read_HES].[hes_person_1718]
"

IDI_material_wellbeing <- read_sql_table(material_wellbeing_query)
fwrite(IDI_material_wellbeing, "data/IDI_material_wellbeing.csv")

# Concordance giving H_ID
concordance_H_ID_address <- fread(
  DB_PATH, select = c("H_ID", "H_Attributes_AddressString", "Period")
)[, .(H_Attributes_AddressString = H_Attributes_AddressString[1]), by = "H_ID"]

concordance_query <- "
SELECT [hes_year]
    ,[snz_hes_hhld_uid]
    ,[NEWADRS] as H_Attributes_AddressString
  FROM [IDI_Adhoc].[clean_read_HES].[hes_concordance_NEWADRS]
  WHERE hes_year = '1718' AND NEWADRS IS NOT NULL"

concordance_address_snz_hes_hhld_uid <- read_sql_table(concordance_query)

concordance <- merge(
  concordance_H_ID_address,
  concordance_address_snz_hes_hhld_uid,
  by = "H_Attributes_AddressString"
)
fwrite(concordance, "data/IDI_concordance.csv")
