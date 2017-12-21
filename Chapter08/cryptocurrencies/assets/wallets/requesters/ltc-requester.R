#
# Litecoin Requester (R6 Class)
#

# ---- ltc-requester-all

source("./wallet-requester.R")
source("../../asset.R")

LTCRequester <- R6Class(
    "LTCRequester",
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
                "https://chainz.cryptoid.info/ltc/api.dws",
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
                name = "Litecoin",
                symbol = "LTC",
                total = total,
                address = private$address
            ))
        }
    )
)
