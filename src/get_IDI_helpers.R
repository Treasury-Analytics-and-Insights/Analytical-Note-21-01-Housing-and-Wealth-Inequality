library(data.table)
library(magrittr)
library(DBI)

get_IDI_connection <- function(database = "IDI_Adhoc") {
  IDI_connection <- dbConnect(
    odbc::odbc(),
    Driver = "ODBC Driver 17 for SQL Server",
    Server = "PRTPRDSQL36.stats.govt.nz",
    Port = "1433",
    Database = database,
    Trusted_Connection = "Yes"
  )
  return(IDI_connection)
}

read_sql_table <- function(SQL_query, database = "IDI_Adhoc"){
  connection_channel <- get_IDI_connection(database)
  SQL_query_result <- setDT(dbGetQuery(connection_channel, SQL_query))
  dbDisconnect(connection_channel)
  return(SQL_query_result)
}