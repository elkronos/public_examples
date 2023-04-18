# Install and load required packages
install_and_load_packages <- function(packages) {
  for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package)
    }
    library(package, character.only = TRUE)
  }
}

install_and_load_packages(c("RMySQL", "RSQLite", "RPostgreSQL", "RODBC", "DBI"))

# Function to connect to the database
# Arguments:
#   - db_type: The type of the database. Supported types are 'mysql', 'sqlite', 'postgresql', and 'odbc'.
#   - host, user, password, dbname, port: Connection parameters for 'mysql' and 'postgresql'.
#   - dsn: Connection parameter for 'odbc'.
#   - ...: Additional connection parameters.
connect_to_database <- function(db_type, host = NULL, user = NULL, password = NULL, dbname = NULL, port = NULL, dsn = NULL, options = NULL) {
  if (!db_type %in% c("mysql", "sqlite", "postgresql", "odbc")) {
    stop("Unsupported database type.")
  }
  
  connection <- NULL
  
  if (db_type == "mysql") {
    connection <- DBI::dbConnect(RMySQL::MySQL(), host = host, user = user, password = password, dbname = dbname, port = port, options)
  } else if (db_type == "sqlite") {
    connection <- DBI::dbConnect(RSQLite::SQLite(), dbname)
  } else if (db_type == "postgresql") {
    connection <- DBI::dbConnect(RPostgreSQL::PostgreSQL(), host = host, user = user, password = password, dbname = dbname, port = port, options)
  } else if (db_type == "odbc") {
    connection <- DBI::dbConnect(odbc::odbc(), dsn = dsn, uid = user, pwd = password, options)
  }
  
  return(connection)
}

# Connect to databases
mysql_connection <- connect_to_database("mysql", host = "localhost", user = "username", password = "password", dbname = "mydb", port = 3306)
sqlite_connection <- connect_to_database("sqlite", dbname = "mydb.sqlite")
postgresql_connection <- connect_to_database("postgresql", host = "localhost", user = "username", password = "password", dbname = "mydb", port = 5432)
odbc_connection <- connect_to_database("odbc", dsn = "mydsn", user = "username", password = "password")

# Disconnect from databases
DBI::dbDisconnect(mysql_connection)
DBI::dbDisconnect(sqlite_connection)
DBI::dbDisconnect(postgresql_connection)
DBI::dbDisconnect(odbc_connection)
