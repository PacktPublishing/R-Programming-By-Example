#
# Requester (R6 Class)
#

# ---- requester-requester

library(jsonlite)

Requester <- R6Class(
    "Requester",
    public = list(
        request = function(URL) {
            return(fromJSON(URL))
        }
    )
)
