#
# Update Assets (Function)
#

# ---- update-assets-all

library(R6)
library(methods)

source("../storage/storage.R", chdir = TRUE)
source("../utilities/time-stamp.R")
source("../settings.R")

update_assets_loop <- function(minutes_interval) {
    storage = Storage$new(SETTINGS)
    repeat {
        users <- storage$read_users()
        timestamp = now.TimeStamp()
        lapply(users, update_assets, timestamp)
        Sys.sleep(minutes_interval * 60)
    }
}

update_assets_loop(60)
