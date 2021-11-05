# Helper script to setup a standard TAWArun runscript
TAR_settings <- yaml::read_yaml("TAR_settings.yaml")

# Get Inflator and Mapping files
INFLATORS_PATH <- TAR_settings[["INFLATORS_PATH"]]
INFLATORS_MAPPING_PATH <- TAR_settings[["INFLATORS_MAPPING_PATH"]]

SURVEY <- TAR_settings[["SURVEY"]]

# Get TAWA input database
DB_PATH <- unlist(TAR_settings[["DB_PATH"]])
