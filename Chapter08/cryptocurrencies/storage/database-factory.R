#
# Database Factory (Function)
#

# ---- database-factory-all

source("./csv-files.R")

database_factory <- function(db_setup, table_names) {
    table_names <- table_names[[db_setup[["environment"]]]]
    if (db_setup[["name"]] == "CSVFiles") {
        return(CSVFiles$new(table_names))
    } else {
        stop("Unknown database name")
    }
}
