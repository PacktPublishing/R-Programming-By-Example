#
# Chapter 09 - Improving Performance: Simple Moving Average (SMA), parallel
#
# R Programming by Example
# Omar Trejo Navarro
# Packt Publishing
# Mexico City
# 2017
#

# ---- sma-parallel-inefficient

library(parallel)

sma_parallel_inefficient <- function(period, symbol, data) {
    data <- as.numeric(data[data$symbol == symbol, "price_usd"])
    cluster <- makeCluster(detectCores())
    result <- unlist(parLapply(
        cluster, 1:length(data), sma_from_position_2, period, data))
    stopCluster(cluster)
    return(result)
}

# ---- sma-parallel

sma_parallel <- function(period, symbol, data, cluster) {
    data <- as.numeric(data[data$symbol == symbol, "price_usd"])
    return(unlist(parLapply(
        cluster, 1:length(data), sma_from_position_2, period, data)))
}
