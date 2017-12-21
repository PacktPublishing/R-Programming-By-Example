#
# Admin (S3 Class)
#

# ---- admin-all

source("./user.R")

admin_constructor <- function(storage, email) {
    admin <- user_constructor(storage, email)
    class(admin) <- c("Admin", class(admin))
    admin <- get_wallets(admin)
    return(admin)
}

get_wallets.Admin <- function(admin) {
    admin$wallets <- admin$storage$read_all_wallets()
    return(admin)
}

new_wallet.Admin <- function(admin, symbol, address, note) {
    stop("Admins may not have own wallets")
}

update_assets.Admin <- function(admin, timestamp) {
    stop("Admins may do not have own assets")
}

save.Admin <- function(admin) {
    admin$storage$write_user(user)
}
