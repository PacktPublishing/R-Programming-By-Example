#
# Chapter 09 - Improving Performance: Simple Moving Average (SMA), slow
#
# R Programming by Example
# Omar Trejo Navarro
# Packt Publishing
# Mexico City
# 2017
#

# ---- sma-slow-1

sma_slow_1 <- function(period, symbol, data) {
    result <- data.frame(sma=numeric())
    for(end in 1:nrow(data)) {
        position <- end
        sma <- NA
        n_accumulated <- 0
        period_prices <- data.frame(price=numeric())
        if (data[end, "symbol"] == symbol) {
            while(n_accumulated < period & position >= 1) {
                if (data[position, "symbol"] == symbol) {
                    period_prices <- rbind(
                        period_prices,
                        data.frame(price=data[position, "price_usd"])
                    )
                    n_accumulated <- n_accumulated + 1
                }
                position <- position - 1
            }
            if (n_accumulated == period) {
                sma <- 0
                for (price in period_prices$price) {
                    sma <- sma + price
                }
                sma <- sma / period
            } else {
                sma <- NA
            }
            result <- rbind(result, data.frame(sma=sma))
        }
    }
    return(result)
}

# ---- sma-slow-2

# Removed inner dataframe

sma_slow_2 <- function(period, symbol, data) {
    result <- data.frame(sma=numeric())
    for(end in 1:nrow(data)) {
        position <- end
        sma <- NA
        n_accumulated <- 0
        period_prices <- NULL
        if (data[end, "symbol"] == symbol) {
            while(n_accumulated < period & position >= 1) {
                if (data[position, "symbol"] == symbol) {
                    period_prices <- c(period_prices, data[position, "price_usd"])
                    n_accumulated <- n_accumulated + 1
                }
                position <- position - 1
            }
            if (n_accumulated == period) {
                sma <- 0
                for (price in period_prices) {
                    sma <- sma + price
                }
                sma <- sma / period
            } else {
                sma <- NA
            }
            result <- rbind(result, data.frame(sma=sma))
        }
    }
    return(result)
}

# ---- sma-slow-3

# Removed outer dataframe

sma_slow_3 <- function(period, symbol, data) {
    result <- NULL
    for(end in 1:nrow(data)) {
        position <- end
        sma <- NA
        n_accumulated <- 0
        period_prices <- NULL
        if (data[end, "symbol"] == symbol) {
            while(n_accumulated < period & position >= 1) {
                if (data[position, "symbol"] == symbol) {
                    period_prices <- c(period_prices, data[position, "price_usd"])
                    n_accumulated <- n_accumulated + 1
                }
                position <- position - 1
            }
            if (n_accumulated == period) {
                sma <- 0
                for (price in period_prices) {
                    sma <- sma + price
                }
                sma <- sma / period
            } else {
                sma <- NA
            }
            result <- c(result, sma)
        }
    }
    return(result)
}

# ---- sma-slow-4

# Vectorized mean period_prices

sma_slow_4 <- function(period, symbol, data) {
    result <- NULL
    for(end in 1:nrow(data)) {
        position <- end
        sma <- NA
        n_accumulated <- 0
        period_prices <- NULL
        if (data[end, "symbol"] == symbol) {
            while(n_accumulated < period & position >= 1) {
                if (data[position, "symbol"] == symbol) {
                    period_prices <- c(period_prices, data[position, "price_usd"])
                    n_accumulated <- n_accumulated + 1
                }
                position <- position - 1
            }
            if (n_accumulated == period) {
                sma <- mean(period_prices)
            } else {
                sma <- NA
            }
            result <- c(result, sma)
        }
    }
    return(result)
}

# ---- sma-slow-5

# Removed unnecessary period_prices

sma_slow_5 <- function(period, symbol, data) {
    result <- NULL
    for(end in 1:nrow(data)) {
        position <- end
        sma <- 0
        n_accumulated <- 0
        if (data[end, "symbol"] == symbol) {
            while(n_accumulated < period & position >= 1) {
                if (data[position, "symbol"] == symbol) {
                    sma <- sma + data[position, "price_usd"]
                    n_accumulated <- n_accumulated + 1
                }
                position <- position - 1
            }
            if (n_accumulated == period) {
                sma <- sma / period
            } else {
                sma <- NA
            }
            result <- c(result, sma)
        }
    }
    return(result)
}

# ---- sma-slow-6

# Filtered data beforehand

sma_slow_6 <- function(period, symbol, data) {
    data <- data[data$symbol == symbol, ]
    result <- NULL
    for(end in 1:nrow(data)) {
        position <- end
        sma <- 0
        while(end - position < period & position >= 1) {
            sma <- sma + data[position, "price_usd"]
            position <- position - 1
        }
        if (end - position == period) {
            sma <- sma / period
        } else {
            sma <- NA
        }
        result <- c(result, sma)
    }
    return(result)
}

# ---- sma-slow-7

# Vectorized sma

sma_slow_7 <- function(period, symbol, data) {
    data <- data[data$symbol == symbol, ]
    result <- NULL
    for(end in 1:nrow(data)) {
        start <- end - period + 1
        if (start >= 1) {
            sma <- mean(data[start:end, "price_usd"])
        } else {
            sma <- NA
        }
        result <- c(result, sma)
    }
    return(result)
}
