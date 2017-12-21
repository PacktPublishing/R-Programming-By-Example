#
# Chapter 09 - Improving Performance: Simple Moving Average (SMA), delegated
#
# R Programming by Example
# Omar Trejo Navarro
# Packt Publishing
# Mexico City
# 2017
#

# ---- sma-delegated-cpp

library(Rcpp)

sourceCpp("./sma-delegated-cpp.cpp")

sma_delegated_cpp <- function(period, symbol, data) {
    data <- as.numeric(data[which(data$symbol == symbol), "price_usd"])
    return(sma_cpp(period, data))
}

# ---- sma-delegated-fortran

system("R CMD SHLIB sma-delegated-fortran.f")
dyn.load("sma-delegated-fortran.so")

sma_delegated_fortran <- function(period, symbol, data) {
    data <- data[which(data$symbol == symbol), "price_usd"]
    n <- length(data)
    results <- .Fortran(
        "sma_fortran",
        period = as.integer(period),
        dataa = as.single(data),
        smas = single(n),
        n = as.integer(n)
    )
    return(as.numeric(results$smas))
}
