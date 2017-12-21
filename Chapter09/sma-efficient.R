#
# Chapter 09 - Improving Performance: Simple Moving Average (SMA), efficient
#
# R Programming by Example
# Omar Trejo Navarro
# Packt Publishing
# Mexico City
# 2017
#

# ---- sma-efficient-1

sma_efficient_1 <- function(period, symbol, data) {
    data <- data[data$symbol == symbol, ]
    return(unlist(lapply(1:nrow(data), sma_from_position_1, period, data)))
}

sma_from_position_1 <- function(end, period, data) {
    start <- end - period + 1
    return(ifelse(start >= 1, mean(data[start:end, "price_usd"]), NA))
}

# ---- sma-efficient-2

sma_efficient_2 <- function(period, symbol, data) {
    data <- data[data$symbol == symbol, "price_usd"]
    return(unlist(lapply(1:length(data), sma_from_position_2, period, data)))
}

sma_from_position_2 <- function(end, period, data) {
    start <- end - period + 1
    return(ifelse(start >= 1, sum(data[start:end]) / period, NA))
}
