#
# Exchange Requester Factory (Function)
#

# ---- exchange-requester-factory-all

source("./coinmarketcap-requester.R")

exchange_requester_factory <- function(name) {
    if (name == "CoinMarketCap") {
        return(CoinMarketCapRequester$new())
    } else {
        stop("Unknown exchange name")
    }
}
