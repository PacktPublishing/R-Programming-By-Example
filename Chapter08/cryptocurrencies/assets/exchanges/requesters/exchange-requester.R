#
# Exchange Requester (R6 Class)
#

# ---- exchange-requester-all

source("../../../utilities/requester.R")

KNOWN_ASSETS = list(
    "BTC" = "Bitcoin",
    "LTC" = "Litecoin"
)

ExchangeRequester <- R6Class(
    "ExchangeRequester",
    public = list(
        markets = function() list()
    ),
    private = list(
        requester = Requester$new(),
        create_market = function(resp) NULL,
        request = function(URL) {
            return(private$requester$request(URL))
        }
    )
)
