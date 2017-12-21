#
# Database (R6 Class)
#

# ---- database-all

Database <- R6Class(
    "Database",
    public = list(
        set_table_names = function(table_names) {
            private$table_names <- table_names
        },
        get_table_names = function() {
            return(private$table_names)
        },
        read_exchanges = function() list(),
        read_users = function(storage) list(),
        read_wallets = function(email) list(),
        read_all_wallets = function() list(),
        read_analysis_assets = function(email) list(),
        write_user = function(user) {},
        write_wallet = function(wallet) {},
        write_assets = function(assets) {},
        write_markets = function(markets) {}
    ),
    private = list(table_names = list())
)
