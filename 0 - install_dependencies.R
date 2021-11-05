# Dependencies to install, found with
# attachment::att_from_rscripts(".")

to_install <- c(
  # "TAWArun", "TAWApost", # Packages made by the Analytics & Insights TAWA team
  # These are available upon request to IDI users at the discretion of A&I.
  "DBI", "odbc", # For reading in IDI data from the SQL server
  "data.table", "magrittr", # Used by every script
  "yaml", "openxlsx", # For loading and saving
  "ggplot2", "scales", # For plotting
  "logging", "stringr", "janitor" # Utility packages
)
for (i in to_install) {
  message(paste("looking for ", i))
  if (!requireNamespace(i)) {
    message(paste("     installing", i))
    install.packages(i)
  }
}