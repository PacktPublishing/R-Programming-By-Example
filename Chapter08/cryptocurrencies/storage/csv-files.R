#
# CSV Files (R6 Class)
#

# ---- csv-files-sources

source("../assets/exchanges/exchange.R", chdir = TRUE)
source("../users/user.R", chdir = TRUE)
source("./database.R")

# ---- csv-files-dir

DIR <- "./csv-files/"

# ---- csv-files-csv-files

CSVFiles <- R6Class(
    "CSVFiles",
    inherit = Database,
    public = list(
        initialize = function(table_names) {
            super$set_table_names(table_names)
            initialize_csv_files(table_names)
        },
        read_exchanges = function() {
            return(list(Exchange$new("CoinMarketCap")))
        },
        read_users = function(storage) {
            data <- private$read_csv("users")
            return(lapply(data$email, user_constructor, storage))
        },
        read_wallets = function(email) {
            data <- private$read_csv("wallets")
            wallets <- NULL
            if (nrow(data) >= 1) {
                for (i in 1:nrow(data)) {
                    if (data[i, "email"] == email) {
                        wallets <- c(wallets, list(Wallet$new(
                            data[i, "email"], data[i, "symbol"],
                            data[i, "address"], data[i, "note"])))

                    }
                }
            } else { wallets <- list() }
            return(wallets)
        },
        read_all_wallets = function() {
            data <- private$read_csv("wallets")
            wallets <- NULL
            if (nrow(data) >= 1) {
                for (i in 1:nrow(data)) {
                    wallets <- c(wallets, list(Wallet$new(
                        data[i, "email"], data[i, "symbol"],
                        data[i, "address"], data[i, "note"])))
                }
            } else { wallets <- list() }
            return(wallets)
        },
        write_user = function(user) {
            data <- private$read_csv("users")
            new_row <- as.data.frame(dataS3(user))
            print(new_row)
            if (private$user_does_not_exist(user, data)) {
                data <- rbind(data, new_row)
            }
            private$write_csv("users", data)
        },
        write_wallets = function(wallets) {
            data <- private$read_csv("wallets")
            for (wallet in wallets) {
                new_row <- as.data.frame(wallet$data())
                print(new_row)
                if (private$wallet_does_not_exist(wallet, data)) {
                    data <- rbind(data, new_row)
                }
            }
            private$write_csv("wallets", data)
        },
        write_assets = function(assets) {
            data <- private$read_csv("assets")
            for (asset in assets) {
                new_row <- as.data.frame(dataS4(asset))
                print(new_row)
                data <- rbind(data, new_row)
            }
            private$write_csv("assets", data)
        },
        write_markets = function(markets) {
            data <- private$read_csv("markets")
            for (market in markets) {
                new_row <- as.data.frame(market$data())
                print(new_row)
                data <- rbind(data, new_row)
            }
            private$write_csv("markets", data)
        }
    ),
    private = list(
        read_csv = function(table_name) {
            return(read.csv(private$file(table_name), stringsAsFactors = FALSE))
        },
        write_csv = function(table_name, data) {
            write.csv(data, file = private$file(table_name), row.names = FALSE)
        },
        file = function(table_name) {
            return(paste(
                DIR, super$get_table_names()[[table_name]], ".csv", sep = ""))
        },
        user_does_not_exist = function(user, data) {
            if (dataS3(user)[["email"]] %in% data$email) {
                return(FALSE)
            }
            return(TRUE)
        },
        wallet_does_not_exist = function(wallet, data) {
            current_addresses <- data[
                data$email == wallet$get_email() &
                data$symbol == wallet$get_symbol(),
                "address"
            ]
            if (wallet$get_address() %in% current_addresses) {
                return(FALSE)
            }
            return(TRUE)
        }
    )
)

# ---- csv-files-initialize-csv-files

initialize_csv_files <- function(table_names) {
    dir.create(DIR, showWarnings = FALSE)
    for (table in table_names) {
        filename <- paste(DIR, table, ".csv", sep = "")
        if (!file.exists(filename)) {
            data <- empty_dataframe(table)
            write.csv(data, file = filename, row.names = FALSE)
        }
    }
}

# ---- csv-files-empty-dataframe

empty_dataframe <- function(table) {
    if (grepl("assets", table))  {
        return(empty_assets())
    } else if (grepl("markets", table)) {
        return(empty_markets())
    } else if (grepl("users", table)) {
        return(empty_users())
    } else if (grepl("wallets", table)) {
        return(empty_wallets())
    } else {
        stop("Unknown table name")
    }
}

# ---- csv-files-empty-dataframes

empty_assets <- function() {
    return(data.frame(
        email = character(),
        timestamp = character(),
        name = character(),
        symbol = character(),
        total = numeric(),
        address = character()
    ))
}

empty_markets <- function() {
    return(data.frame(
        timestamp = character(),
        name = character(),
        symbol = character(),
        rank = numeric(),
        price_btc = numeric(),
        price_usd = numeric()
    ))
}

empty_users <- function() {
    return(data.frame(
        email = character()
    ))
}

empty_wallets <- function() {
    return(data.frame(
        email = character(),
        symbol = character(),
        address = character(),
        note = character()
    ))
}
