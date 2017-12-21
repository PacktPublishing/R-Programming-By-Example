#
# Chapter 09 - Improving Performance: Data Simulation
#
# R Programming by Example
# Omar Trejo Navarro
# Packt Publishing
# Mexico City
# 2017
#

# ---- data-simulation-all

source("../chapter-08/cryptocurrencies/utilities/time-stamp.R")

library(lubridate)

N <- 60 * 24 * 365

simulate_market <- function(name, symbol, now, n, base, sd, x) {
    dates <- seq(now - minutes(n - 1), now, by = "min")
    ts <- unlist(lapply(lapply(dates, time_to_timestamp.TimeStamp), unclass))
    price_usd <- simulate_prices(n, base, sd, x)
    data <- data.frame(timestamp = ts, price_usd = price_usd)
    data$name <- name
    data$symbol <- symbol
    return(data)
}

simulate_prices <- function(n, base, sd, x) {
    ts <- arima.sim(list(15, 15, 15), n = n, sd = sd)
    quadratic_model <- base + (x - 1) * base / (n^2) * (1:n)^2
    return(as.numeric(ts + quadratic_model))
}

now <- Sys.time()
btc <- simulate_market("Bitcoin", "BTC", now, N, 8000, 8, 2)
ltc <- simulate_market("Litecoin", "LTC", now, N, 80, 0.08, 1.5)

data <- rbind(btc, ltc)
data <- data[order(data$timestamp), ]

write.csv(data, "./data.csv", row.names = FALSE)

# ---- data-simulation-graphs

s <- sample(1:nrow(btc), 1000)

plot(btc[s[order(s)], "price_usd"], xlab="Minutes", ylab="Price", xaxt='n')
title(main="Bitcoin price simulation for 1 year")
lines(btc[s[order(s)], "price_usd"])

plot(btc[1:60, "price_usd"], xlab="Minutes", ylab="Price", xaxt='n')
title(main="Bitcoin price simulation for 1 hour")
lines(btc[1:60, "price_usd"])
