library(data.table)
library(magrittr)

material_hardship_path <- "data/IDI_material_wellbeing.csv"
material_hardship <- fread(material_hardship_path)

concordance_path <- "data/IDI_concordance.csv"
dt_concordance <- fread(concordance_path)

material_hardship <- merge(
  material_hardship,
  dt_concordance[, .(snz_hes_hhld_uid, H_ID)],
  by = "snz_hes_hhld_uid"
)

DEP17_vars <- list(
  ## Enforced lack of essentials
  # 2 == No, 12 == because cost
  "MWMqMeal" = quote(MWMqMeal == "2" & MWMReason_NoMeal == "12"),
  "MWMqGoodShoes" = quote(MWMqGoodShoes == "2" & MWMReason_NoGoodShoes == "12"),
  "MWMqSuitableClothes" = quote(MWMqSuitableClothes == "2" & MWMReason_NoSuitableClothes == "12"),
  "MWMqInsurance" = quote(MWMqSuitableClothes == "2" & MWMReason_NoInsurance == "12"),
  "MWMqGiveGifts"= quote(MWMqGiveGifts == "2" & MWMReason_NoGiveGifts == "12"),
  ## Economising behaviours
  # 13 == "A lot"
  "MWMqNoFruit" = quote(MWMqNoFruit == "13"),
  "MWMqLessMeat" = quote(MWMqLessMeat == "13"),
  "MWMqNoDoctor" = quote(MWMqNoDoctor == "13"),
  "MWMqNoDentist" = quote(MWMqNoDentist == "13"),
  "MWMqFewerLocalTrips" = quote(MWMqFewerLocalTrips == "13"),
  "MWMqFeelCold" = quote(MWMqFeelCold == "13"),
  "MWMqOldAppliance" = quote(MWMqOldAppliance == "13"),
  ## Restrictions
  # 14 == "very limited"
  "MWMqLimitClothesShoes" = quote(MWMqLimitClothesShoes == "14"),
  # 2 == "no"
  "MWMqUnexpected500" = quote(MWMqUnexpected500 == "2"),
  ## Financial stress and vulnerability
  # 13 == "more than once"
  "MWMqUtilities" = quote(MWMqUtilities == "13"),
  "MWMqCarLate" = quote(MWMqCarLate == "13"),
  "MWMqBorrow" = quote(MWMqBorrow == "13")
)
material_hardship_DEP17 <- copy(material_hardship)
material_hardship_DEP17[, DEP17 := 0]
for (ii in seq_along(DEP17_vars)) {
  DEP17_var <- names(DEP17_vars)[ii]
  DEP17_var_condition <- DEP17_vars[[DEP17_var]]
  material_hardship_DEP17[eval(DEP17_var_condition), DEP17 := DEP17 + 1]
}

# Summarise to household level
material_hardship_DEP17 <-
  material_hardship_DEP17[, .(DEP17 = sum(DEP17)), by = H_ID]

material_hardship_DEP17[, H_Has_Material_Hardship := DEP17 >= 6]
material_hardship_DEP17[, H_Has_Severe_Material_Hardship := DEP17 >= 9]

fwrite(material_hardship_DEP17, "data/material_hardship.csv")
