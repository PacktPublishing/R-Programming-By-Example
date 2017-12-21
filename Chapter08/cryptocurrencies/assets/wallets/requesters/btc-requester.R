#
# Bitcoin Requester (R6 Class)
#

# ---- btc-requester-all

source("./wallet-requester.R")
source("../../asset.R")

BTCRequester <- R6Class(
    "BTCRequester",
    inherit = WalletRequester,
    public = list(
        initialize = function(address) {
            private$address <- address
        },
        assets = function() {
            total <- as.numeric(private$request(private$url()))
            if (total > 0) { return(list(private$create_asset(total))) }
            return(list())
        }
    ),
    private = list(
        address = "",
        url = function(address) {
            return(paste(
                "https://chainz.cryptoid.info/btc/api.dws",
                "?q=getbalance",
                "&a=",
                private$address,
                sep = ""
            ))
        },
        create_asset = function(total) {
            return(new(
                "Asset",
                email = "",
                timestamp = "",
                name = "Bitcoin",
                symbol = "BTC",
                total = total,
                address = private$address
            ))
        }
    )
)
