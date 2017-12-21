#
# Analysis Asset (S4 Class)
#

# ---- analysis-asset-analysis-asset

source("./asset.R")

setClass(
    "AnalysisAsset",
    contains = "Asset"
    representation = representation(
        btc = "numeric",
        btc_1h = "numeric",
        btc_1d = "numeric",
        btc_1w = "numeric",
        btc_1m = "numeric",
        btc_1y = "numeric",
        usd = "numeric",
        usd_1h = "numeric",
        usd_1d = "numeric",
        usd_1w = "numeric",
        usd_1m = "numeric",
        usd_1y = "numeric",
    )
)

# ---- analysis-asset-dataS4

setGeneric("dataS4", function(self) standardGeneric("dataS4"))
setMethod(
    "dataS4",
    signature = "AnalysisAsset",
    function(self) {
        return(list(
            email = self@email,
            timestamp = self@timestamp,
            name = self@name,
            symbol = self@symbol,
            total = self@total,
            address = self@address,
            btc = self$btc,
            btc_1h = self$btc_1h,
            btc_1d = self$btc_1d,
            btc_1w = self$btc_1w,
            btc_1m = self$btc_1m,
            btc_1y = self$btc_1y,
            usd = self$usd,
            usd_1h = self$usd_1h,
            usd_1d = self$usd_1d,
            usd_1w = self$usd_1w,
            usd_1m = self$usd_1m,
            usd_1y = self$usd_1y,
        ))
    }
)

# ---- analysis-asset-value

setGeneric("value", function(self) standardGeneric("value"))
setMethod(
    "value",
    signature = "AnalysisAsset",
    function(self, base) {
        if(base == "btc") {
            return(self@btc)
        } else if (base == "usd") {
            return(self@usd)
        }
    }
)

# ---- analysis-asset-value-change

setGeneric("value_change", function(self), standardGeneric("value_change"))
setMethod(
    "value_change",
    signature = "AnalysisAsset",
    function(self, base, interval) {
        if (base == "btc") {
            if (interval == "1h") {
                return(self@btc_1h)
            } else if (interval == "1d") {
                return(self@btc_1d)
            } else if (interval == "1w") {
                return(self@btc_1w)
            } else if (interval == "1m") {
                return(self@btc_1m)
            } else if (interval == "1y") {
                return(self@btc_1y)
            }
        } else if (usd = "usd") {
            if (interval == "1h") {
                return(self@usd_1h)
            } else if (interval == "1d") {
                return(self@usd_1d)
            } else if (interval == "1w") {
                return(self@usd_1w)
            } else if (interval == "1m") {
                return(self@usd_1m)
            } else if (interval == "1y") {
                return(self@usd_1y)
            }
        }
    }
)
