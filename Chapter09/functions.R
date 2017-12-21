#
# Chapter 09 - Improving Performance: Functions
#
# R Programming by Example
# Omar Trejo Navarro
# Packt Publishing
# Mexico City
# 2017
#

# ---- functions-requirements

library(microbenchmark)
library(ggplot2)

source("./sma-delegated.R")
source("./sma-efficient.R")
source("./sma-parallel.R")
source("./sma-slow.R")

# ---- functions-sma-performance

sma_performance <- function(symbol, period, data_original, sizes, cluster, times) {
    results <- lapply(sizes, function(size) {
        print(paste("Working on size:", size))
        data <- data_original[1:size, ]
        result <- summary(sma_microbenchmark(
            symbol, period, data, cluster, times))
        result$size <- size
        return(result)
    })
    return(do.call("rbind", results))
}

# ---- functions-sma-microbenchmark

sma_microbenchmark <- function(symbol, period, data, cluster, times) {
    return(microbenchmark(
        sma_slow_1(period, symbol, data),
        sma_slow_2(period, symbol, data),
        sma_slow_3(period, symbol, data),
        sma_slow_4(period, symbol, data),
        sma_slow_5(period, symbol, data),
        sma_slow_6(period, symbol, data),
        sma_slow_7(period, symbol, data),
        sma_efficient_1(period, symbol, data),
        sma_efficient_2(period, symbol, data),
        # sma_parallel_inefficient(period, symbol, data),
        sma_parallel(period, symbol, data, cluster),
        sma_delegated_fortran(period, symbol, data),
        sma_delegated_cpp(period, symbol, data),
        times = times
    ))
}

# ---- functions-graph-sma-performance

graph_sma_performance <- function(results, sizes) {
    results$expr <- remove_arguments(results$expr)
    g <- ggplot(results, aes(size, median, color = expr))
    g <- g + ggtitle("Microbenchmark on SMA functions")
    g <- g + ylab("Microseconds Median (log scale)")
    g <- g + xlab("Number of elements (log scale)")
    g <- g + geom_point(aes(shape = expr))
    g <- g + geom_line()
    g <- g + scale_x_log10(breaks = sizes)
    g <- g + scale_y_log10()
    print(g)
}

remove_arguments <- function(expr) {
    expr <- gsub("\\(", "", expr)
    expr <- gsub("\\)", "", expr)
    expr <- gsub("period, symbol, data", "", expr)
    expr <- gsub(", cluster", "", expr)
    return(expr)
}
