#
# Storage (R6 Class)
#

# ---- storage-all

source("./database-factory.R")

Storage <- R6Class(
    "Storage",
    public = list(
        initialize = function(settings) {
            private$read_db <- database_factory(
                settings[["storage"]][["read"]],
                settings[["storage"]][["table_names"]]
            )
            private$write_dbs <- lapply(
                settings[["storage"]][["write"]],
                database_factory,
                settings[["storage"]][["table_names"]]
            )
        },
        read_exchanges = function() {
            return(private$read_db$read_exchanges())
        },
        read_users = function() {
            return(private$read_db$read_users(self))
        },
        read_wallets = function(email) {
            return(private$read_db$read_wallets(email))
        },
        read_all_wallets = function() {
            return(private$read_db$read_all_wallets())
        },
        read_analysis_assets = function(email) {
            return(private$read_db$read_analysis_assets(email))
        },
        write_user = function(user) {
            for (db in private$write_dbs) { db$write_user(user) }
        },
        write_wallets = function(wallets) {
            for (db in private$write_dbs) { db$write_wallets(wallets) }
        },
        write_assets = function(assets) {
            for (db in private$write_dbs) { db$write_assets(assets) }
        },
        write_markets = function(markets) {
            for (db in private$write_dbs) { db$write_markets(markets) }
        }
    ),
    private = list(read_db = NULL, write_dbs = list())
)
