#
# Update Markets (Function)
#

# ---- update-markets-all

library(R6)
library(methods)

source("../storage/storage.R", chdir = TRUE)
source("../utilities/time-stamp.R")
source("../settings.R")

update_markets_loop <- function(minutes_interval) {
    storage = Storage$new(SETTINGS)
    exchanges <- storage$read_exchanges()
    repeat  {
        timestamp = now.TimeStamp()
        for (exchange in exchanges) {
            exchange$update_markets(timestamp, storage)
        }
        Sys.sleep(minutes_interval * 60)
    }
}

update_markets_loop(60)
