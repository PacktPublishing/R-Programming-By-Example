#
# User (S3 Class)
#

# ---- user-user-contructor

source("../assets/wallets/wallet.R", chdir = TRUE)

user_constructor <- function(email, storage) {
    if (!valid_email(email)) { stop("Invalid email") }
    user <- list(storage = storage, email = email, wallets = list())
    class(user) <- "User"
    user <- get_wallets(user)
    return(user)
}

valid_email <- function(string) {
    if (grepl("@", string) && grepl(".", string)) { return(TRUE) }
    return(FALSE)
}

# ---- user-get-wallets-user

get_wallets.User <- function(user) {
    user$wallets <- user$storage$read_wallets(user$email)
    return(user)
}

get_wallets <- function(object) {
    UseMethod("get_wallets")
}

# ---- user-new-wallet-user

new_wallet.User <- function(user, symbol, address, note) {
    if (length(user$wallets) >= 1) {
        for (wallet in user$wallets) {
            if (wallet$get_symbol() == symbol &
                wallet$get_address() == address) {
                return(user)
            }
        }
    }
    wallet <- Wallet$new(user$email, symbol, address, note)
    user$wallets <- c(user$wallets, list(wallet))
    return(user)
}

new_wallet <- function(object, symbol, address, note) {
    UseMethod("new_wallet")
}

# ---- user-update-assets-user

update_assets.User <- function(user, timestamp) {
    for (wallet in user$wallets) {
        wallet$update_assets(timestamp, user$storage)
    }
}

update_assets <- function(object, timestamp) {
    UseMethod("update_assets")
}

# ---- user-save-user

save.User <- function(user) {
    user$storage$write_user(user)
    user$storage$write_wallets(user$wallets)
}

save <- function(object) {
    UseMethod("save")
}

# ---- user-dataS3-user

dataS3.User <- function(user) {
    return(list(email = user$email))
}

dataS3 <- function(object) {
    UseMethod("dataS3")
}
