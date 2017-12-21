#
# CoinMarketCap Requester (R6 Class)
#

# ---- coinmarketcap-requester-all

source("./exchange-requester.R")
source("../market.R")

CoinMarketCapRequester <- R6Class(
    "CoinMarketCapRequester",
    inherit = ExchangeRequester,
    public = list(
        markets = function() {
            data <- private$clean(private$request(private$URL))
            return(apply(data, 1, private$create_market))
        }
    ),
    private = list(
        URL = "https://api.coinmarketcap.com/v1/ticker",
        create_market = function(row) {
            timestamp <- NULL
            return(Market$new(
                timestamp,
                row[["name"]],
                row[["symbol"]],
                row[["rank"]],
                row[["price_btc"]],
                row[["price_usd"]]
            ))
        },
        clean = function(data) {
            data$price_usd <- as.numeric(data$price_usd)
            data$price_btc <- as.numeric(data$price_btc)
            data$rank <- as.numeric(data$rank)
            return(data)
        }
    )
)
