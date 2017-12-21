#
# Wallet (R6 Class)
#

# ---- wallet-all

source("./requesters/wallet-requester-factory.R", chdir = TRUE)

Wallet <- R6Class(
    "Wallet",
    public = list(
        initialize = function(email, symbol, address, note) {
            private$requester <- wallet_requester_factory(symbol, address)
            private$email <- email
            private$symbol <- symbol
            private$address <- address
            private$note <- note
        },
        data = function() {
            return(list(
                email = private$email,
                symbol = private$symbol,
                address = private$address,
                note = private$note
            ))
        },
        get_email = function() {
            return(as.character(private$email))
        },
        get_symbol = function() {
            return(as.character(private$symbol))
        },
        get_address = function() {
            return(as.character(private$address))
        },
        update_assets = function(timestamp, storage) {
            private$timestamp <- timestamp
            storage$write_assets(private$assets())
        }
    ),
    private = list(
        timestamp = NULL,
        requester = NULL,
        email = NULL,
        symbol = NULL,
        address = NULL,
        note = NULL,
        assets = function() {
            return(lapply(private$requester$assets(), private$insert_metadata))
        },
        insert_metadata = function(asset) {
            timestamp(asset) <- unclass(private$timestamp)
            email(asset) <- private$email
            return(asset)
        }
    )
)
