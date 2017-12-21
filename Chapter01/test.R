
l1_norm <- function(x, y = 0) sum(abs(x - y))

l2_norm <- function(x, y = 0) sum((x - y)^2)

distance <- function(x, y = 0, norm = "l2") {
    if (norm == "l2") {
        return(l2_norm(x, y))
    } else if (norm == "l1") {
        return(l1_norm(x, y))
    } else {
        stop("Invalid norm option")
    }
}

distance <- function(x, y = 0, norm = "l2") {
    if (norm == "l2") {
        return(l2_norm(x, y))
    } else {
        return(l1_norm(x, y))
    }
}

a <- c(1, 2, 3)
b <- c(4, 5, 6)

distance(a, b, "l1")
distance(a, b, "this will produce an error")

distance(a, b)
distance(a, b, "l2")
distance(a, b, "l1")
distance(a, b, "l1 will also be used in this case")
