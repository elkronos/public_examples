# Load library
library(mockery)

# Simulates a connection to a database 
mock_conn <- create_mock("DBI::Connection", .default = NULL)

# Set up expectations for the behavior of the mock object
expect_called(mock_conn, "dbGetQuery")
expect_return(mock_conn$dbGetQuery("SELECT * FROM customers"), customer_data)

# Switch your functions over to the DB
set_ref(mock_conn, "DBI::dbConnect")
get_customer_data() 
