#
# Market (R6 Class)
#

# ---- market-all

Market <- R6Class(
    "Market",
    public = list(
        initialize = function(timestamp, name, symbol, rank, price_btc, price_usd) {
            private$timestamp <- timestamp
            private$name      <- name
            private$symbol    <- symbol
            private$rank      <- rank
            private$price_btc <- price_btc
            private$price_usd <- price_usd
        },
        data = function() {
            return(list(
                timestamp = private$timestamp,
                name      = private$name,
                symbol    = private$symbol,
                rank      = private$rank,
                price_btc = private$price_btc,
                price_usd = private$price_usd
            ))
        },
        set_timestamp = function(timestamp) {
            private$timestamp <- timestamp
        },
        get_symbol = function() {
            return(private$symbol)
        },
        get_rank = function() {
            return(private$rank)
        },
        get_price = function(base) {
            if (base == 'btc') {
                return(private$price_btc)
            } else if (base == 'usd') {
                return(private$price_usd)
            }
        }
    ),
    private = list(
        timestamp = NULL,
        name = "",
        symbol = "",
        rank = NA,
        price_btc = NA,
        price_usd = NA
    )
)
