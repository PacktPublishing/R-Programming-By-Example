#
# Exchange (R6 Class)
#

# ---- exchange-all

source("./requesters/exchange-requester-factory.R", chdir = TRUE)

Exchange <- R6Class(
    "Exchange",
    public = list(
        initialize = function(name) {
            private$requester <- exchange_requester_factory(name)
        },
        update_markets = function(timestamp, storage) {
            private$timestamp <- unclass(timestamp)
            storage$write_markets(private$markets())
        }
    ),
    private = list(
        requester = NULL,
        timestamp = NULL,
        markets = function() {
            return(lapply(private$requester$markets(), private$insert_metadata))
        },
        insert_metadata = function(market) {
            market$set_timestamp(private$timestamp)
            return(market)
        }
    )
)
