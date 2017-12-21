#
# Chapter 09 - Improving Performance: Main
#
# R Programming by Example
# Omar Trejo Navarro
# Packt Publishing
# Mexico City
# 2017
#

# TODO: Assume order and data for each minute
# TODO: If necessary add a pre-allocation example

# ---- main-requirements

library(parallel)

source("./functions.R")

data_original <- read.csv("./data.csv")
data <- data_original[1:100, ]

# ---- main-start-cluster

cluster <- makeCluster(detectCores())

# ---- main-sma-testing-parameters

times <- 100
period <- 5
symbol <- "BTC"

# ---- main-sma-testing-performance-single

performance <- microbenchmark(
    # sma_1 <- sma_slow_1(period, symbol, data),
    # sma_2 <- sma_slow_2(period, symbol, data),
    # sma_3 <- sma_slow_3(period, symbol, data),
    # sma_4 <- sma_slow_4(period, symbol, data),
    sma_5 <- sma_slow_5(period, symbol, data),
    # sma_6 <- sma_slow_6(period, symbol, data),
    # sma_7 <- sma_slow_7(period, symbol, data),
    # sma_8 <- sma_efficient_1(period, symbol, data),
    # sma_9 <- sma_efficient_2(period, symbol, data),
    # sma_10 <- sma_parallel_inefficient(period, symbol, data),
    # sma_11 <- sma_parallel(period, symbol, data, cluster),
    # sma_12 <- sma_delegated_fortran(period, symbol, data),
    # sma_13 <- sma_delegated_cpp(period, symbol, data),
    times = times,
    unit = "us"
)
summary(performance)$median
autoplot(performance)

# ---- main-sma-testing-simple-timestamp

system.time(sma_slow_1(period, symbol, data_original[1:10000, ]))

# ---- main-sma-testing-performance-full

sizes <- c(10, 100, 1000, 10000)
results <- sma_performance(symbol, period, data_original, sizes, cluster, times)
graph_sma_performance(results, sizes)

# ---- main-sma-testing-correctness

all(sma_1$sma == sma_2$sma, na.rm = TRUE)
all(sma_1$sma - sma_3  <= 0.001, na.rm = TRUE)
all(sma_1$sma - sma_4  <= 0.001, na.rm = TRUE)
all(sma_1$sma - sma_5  <= 0.001, na.rm = TRUE)
all(sma_1$sma - sma_6  <= 0.001, na.rm = TRUE)
all(sma_1$sma - sma_7  <= 0.001, na.rm = TRUE)
all(sma_1$sma - sma_8  <= 0.001, na.rm = TRUE)
all(sma_1$sma - sma_9  <= 0.001, na.rm = TRUE)
all(sma_1$sma - sma_10 <= 0.001, na.rm = TRUE)
all(sma_1$sma - sma_11 <= 0.001, na.rm = TRUE)
all(sma_1$sma - sma_12 <= 0.001, na.rm = TRUE)
all(sma_1$sma - sma_13 <= 0.001, na.rm = TRUE)

# ---- main-stop-cluster

stopCluster(cluster)

# ---- main-profiling

data <- data_original[1:10000, ]

Rprof()
# sma_1 <- sma_slow_1(period, symbol, data)
# sma_2 <- sma_slow_2(period, symbol, data)
# sma_3 <- sma_slow_3(period, symbol, data)
# sma_4 <- sma_slow_4(period, symbol, data)
# sma_5 <- sma_slow_5(period, symbol, data)
# sma_6 <- sma_slow_6(period, symbol, data)
# sma_7 <- sma_slow_7(period, symbol, data)
# sma_8 <- sma_efficient_1(period, symbol, data)
sma_9 <- sma_efficient_2(period, symbol, data)
# sma_10 <- sma_parallel_inefficient(period, symbol, data)
# sma_11 <- sma_parallel(period, symbol, data, cluster)
# sma_12 <- sma_delegated_fortran(period, symbol, data)
# sma_13 <- sma_delegated_cpp(period, symbol, data)
Rprof(NULL)
summaryRprof()
