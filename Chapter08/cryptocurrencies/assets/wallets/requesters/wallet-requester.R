#
# Wallet Requester (R6 Class)
#

# ---- wallet-requester-all

source("../../../utilities/requester.R")

WalletRequester <- R6Class(
    "WalletRequester",
    public = list(
        assets = function() list()
    ),
    private = list(
        requester = Requester$new(),
        create_asset = function() NULL,
        url = function(address) "",
        request = function(URL) {
            return(private$requester$request(URL))
        }
    )
)
