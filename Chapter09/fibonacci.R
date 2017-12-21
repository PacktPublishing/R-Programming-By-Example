#
# Chapter 09 - Improving Performance: Fibonacci
#
# R Programming by Example
# Omar Trejo Navarro
# Packt Publishing
# Mexico City
# 2017
#

# ---- fibonacci-recursive

fibonacci_recursive <- function(n) {
    if (n <= 1) { return(n) }
    return(fibonacci_recursive_1(n - 1) +
           fibonacci_recursive_1(n - 2))
}

# ---- fibonacci-sequential

fibonacci_sequential <- function(n) {
    if (n <= 2) { return(1) }
    f <- integer(n)
    f[1] <- 1
    f[2] <- 1
    for (i in 3:n) {
        f[i] <- f[i - 2] + f[i - 1]
    }
    return(f[n])
}
