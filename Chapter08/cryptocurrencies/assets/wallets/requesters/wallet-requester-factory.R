#
# Wallet Requester Factory (Function)
#

# ---- wallet-requester-factory-all

source("./btc-requester.R")
source("./ltc-requester.R")

wallet_requester_factory <- function(symbol, address) {
    if (symbol == "BTC") {
        return(BTCRequester$new(address))
    } else if (symbol == "LTC") {
        return(LTCRequester$new(address))
    } else {
        stop("Unknown symbol")
    }
}
