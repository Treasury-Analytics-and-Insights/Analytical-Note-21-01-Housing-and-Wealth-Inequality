source("src/TAWArun_setup.R")
# Setting this to TRUE runs TAWA in test mode.
# When you have set everything up,
# change this to FALSE to run TAWA for real.
DRY_RUN <- FALSE

# Main options of run_scenarios
TERMINAL_VALUES <- c(
  "P_Attributes_PersonNumberInHES",
  "P_ID",
  "F_ID",
  "H_ID",
  "P_Attributes_Dependent",
  "P_Attributes_Age",
  "P_Income_Disposable",
  "H_HousingCosts_Total"
)
  
SRC_DIR <- "tawaproc/period"

# Scenario parameters: {filename = scenario_names}
SCENARIO_LIST <- list(
  "Parameters/Parameters_HYEFU20.xlsx" = c("SQ")
)

# Run TAWA, in aggregated period mode
TAWArun::run_TAWA(
  scenario_list = SCENARIO_LIST,
  db_path = DB_PATH,
  src_dir = SRC_DIR,
  inflators_file = INFLATORS_PATH,
  inflators_mapping_file = INFLATORS_MAPPING_PATH,
  survey = SURVEY,
  terminal_values = TERMINAL_VALUES,
  aggregate_period_data = TRUE,
  dry_run = DRY_RUN
)
