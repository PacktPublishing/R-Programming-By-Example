#
# Create User Data
#

# ---- create-user-data-all

library(R6)
library(methods)

source("../storage/storage.R", chdir = TRUE)
source("../users/user.R")
source("../settings.R")

storage = Storage$new(SETTINGS)

user_1 <- user_constructor("1@somewhere.com", storage)
user_1 <- new_wallet(user_1, "BTC", "3D2oetdNuZUqQHPJmcMDDHYoqkyNVsFk9r", "")
user_1 <- new_wallet(user_1, "LTC", "LdP8Qox1VAhCzLJNqrr74YovaWYyNBUWvL", "")
save(user_1)

user_2 <- user_constructor("2@somewhere.com", storage)
user_2 <- new_wallet(user_2, "BTC", "16rCmCmbuWDhPjWTrpQGaU3EPdZF7MTdUk", "")
user_2 <- new_wallet(user_2, "LTC", "LbGi4Ujj2dhcMdiS9vaCpWxtayBujBQYZw", "")
save(user_2)

# ---- create-admin-data

# source("../users/admin.R", chdir = TRUE)

# admin <- admin_constructor("3@somewhere.com", storage)
# admin <- new_wallet(admin, "BTC", "0ASD11BHEKi4NmSeJPNVTiED99XpUJrFIDM", "")
# # update_assets(admin, now.TimeStamp())
# save(admin)
