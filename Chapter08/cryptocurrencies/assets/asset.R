#
# Asset (S4 Class)
#

# ---- asset-asset

setClass(
    Class = "Asset",
    representation = representation(
        email = "character",
        timestamp = "character",
        name = "character",
        symbol = "character",
        total = "numeric",
        address = "character"
    )
)

# ---- asset-email

setGeneric("email<-", function(self, value) standardGeneric("email<-"))
setReplaceMethod("email", "Asset", function(self, value) {
    self@email <- value
    return(self)
})

# ---- asset-timestamp

setGeneric("timestamp<-", function(self, value) standardGeneric("timestamp<-"))
setReplaceMethod("timestamp", "Asset", function(self, value) {
    self@timestamp <- value
    return(self)
})

# ---- asset-dataS4

setGeneric("dataS4", function(self) standardGeneric("dataS4"))
setMethod("dataS4", "Asset", function(self) {
    return(list(
        email = self@email,
        timestamp = self@timestamp,
        name = self@name,
        symbol = self@symbol,
        total = self@total,
        address = self@address
    ))
})
